; Реализация на основе классических POSIX-механизмов

(add-to-load-path
  (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-31)
             (srfi srfi-41)
             (common)
             (options))

(define to-read car)
(define to-write cdr)

(define (event-pipe) (let ((p (pipe))) (setvbuf (to-write p) 'none) p))

(define signal-pipe
  (let* ((p (event-pipe))
         (o (to-write p)))
    (sigaction SIGCHLD (lambda (n) (write-char #\D o)) SA_NOCLDSTOP)
    (sigaction SIGINT SIG_DFL)
    (sigaction SIGTERM SIG_DFL)
    (to-read p)))

(define (default-signals)
  (for-each
    (lambda (s) (sigaction s SIG_DFL))
    (list SIGINT SIGTERM SIGCHLD)))

(define (clear-ports)
  (port-for-each
    (lambda (p)
      (let ((fd (false-if-exception (port->fdes p))))
        (when (and (integer? fd) (< 2 fd))
          (close-port p))))))

(define (notifications-pipe opts)
  (apply open-pipe* OPEN_READ (inotify-command opts)))

(define (timeout? t) (and (number? t) (not (negative? t))))
(define (counter? n) (and (integer? n) (positive? n)))

(define (selector timeout . ports)
  (let ((T (and (timeout? timeout) timeout)))
    (rec (poll) (catch 'system-error
                       (lambda () (car (select ports '() '() T)))
                       (lambda er (if (= EINTR (system-error-errno er))
                                      (poll)
                                      (apply throw er)))))))

(define (readable-port? t p) (compose not null? (selector t p))) 

(define (ready-ports . ports)
  (if (null? ports)
      stream-null
      (let ((poll (apply selector #f ports)))
        (stream-let cycle ((P '()))
          (if (null? P)
              (cycle (poll))
              (stream-cons (car P) (cycle (cdr P))))))))

(define (drainer p read-proc limit timeout)
  (let ((readable? (readable-port? timeout p))
        (next (if (counter? limit) 1- (const 1)))
        (N (if (counter? limit) limit 1)))
    (lambda () (let loop ((n N)
                          (R '()))
                 (if (not (and (positive? n) (readable?)))
                     (values #f R)
                     (let ((v (read-proc p)))
                       (if (eof-object? v)
                           (values #t R)
                           (loop (next n) (cons v R)))))))))

(define (events opts)
  (let* ((s signal-pipe)
         (n (notifications-pipe opts))
         (s-drain (drainer s read-char 32 0))
         (n-drain (drainer n read-line 32 0.1)))
    (stream-map (lambda (p)
                  (cond ((equal? p s) (cons #:signal
                                            (call-with-values s-drain cons)))
                        ((equal? p n) (cons #:notification
                                            (call-with-values n-drain cons)))
                        (else (cons #:unknown-port (cons p '())))))
                (ready-ports s n))))


(define (fork-command opts)
  (let-values (((R W) (let ((p (event-pipe))) (values (to-read p) (to-write p)))))
    (let ((cmd (options:command opts))
          (p (primitive-fork)))
      (if (zero? p)
          (begin
            (default-signals)
            (close-output-port W)
            (unless (char=? #\G (read-char R)) (exit 1))
            (clear-ports)
            (dump-error "worker is clean to go. PID: ~a~%" (getpid))
            (apply execlp (car cmd) cmd))
          (begin 
            (close-input-port R)
            (setpgid p p)
            (write-char #\G W)
            (close-output-port W)
            p)))))

(define kind car)
(define eof? cadr)
(define content cddr)

(define (signal-ok? chars) (every (lambda (c) (char=? #\D c)) chars))

(define (wait-for pid)
  (let ((wait (lambda () (false-if-exception (waitpid WAIT_ANY WNOHANG)))))
    (lambda (pid)
      (let loop ((p (wait)))
        (and (pair? p) (or (= pid (car p))
                           (loop (wait))))))))

(define (signal-step opts e loop R pid rerun?)
  (cond
    ; Так не должно быть. Не возвращаемся в цикл, возвращаем pid на внешний
    ; уровень обработки
    ((or (eof? e)
         (not (signal-ok? (content e)))) 
     (dump-error "unexpected signal event: ~a~%" e)
     pid)

    ; Если задача завершена, нужно проверить, следует ли её перезапустить, и
    ; сделать это в случае необходимости
    ((wait-for pid)
     (dump-error "worker with PID finished: ~a~%" pid)
     (loop R (if rerun? (fork-command opts) 0) #f))

    ; Сигналы нормальные, но задача не выполнена, продолжаем цикл
    (else (loop R pid rerun?))))

(define (any-triggers? opts strings)
  (fold (lambda (s result) (let ((t? (trigger? opts s)))
                             (format #t "~a ~a~%" s (if t? "triggered" "skipping"))
                             (if t? #t result)))
        #f
        (reverse strings)))

(define (notification-step opts e loop R pid rerun?)
  (cond
    ; Так не должно быть. Не возвращаемся в цикл, возвращаем pid на внешний
    ; уровень обработки
    ((eof? e)
     (dump-error "unexpected notification event: ~a~%" e)
     pid)

    ; То, ради чего всё затевалось. Если файлы обновились, а процесс их
    ; обработки затянулся, надо его прервать. Здесь два варианта развития
    ; событий.
    ;
    ; 1. Процесс не запущен (pid = 0), тогда надо его запустить, со
    ; сброшенным флагом rerun?. Реакция на изменения уже запущена.
    ;
    ; 2. Процесс запущен (pid > 0), тогда надо прервать его (kill pid SIGINT), и
    ; продолжить цикл с установленным rerun? Когда дочерний процесс прервётся,
    ; этому процессу поступит сигнал SIGCHLD и после некоторой обработки
    ; управление перейдёт во вторую ветку процедуры signal-step, где процесс
    ; реакции на обновления будет запущен снова.
    ;
    ; Дополнение: если мы уже находимся в процессе перезапуска процесса, то
    ; ничего делать не нужно.
    ((any-triggers? opts (content e)) (if (positive? pid)
                                          (if rerun?
                                              (begin
                                                (dump-error "interruption in progress for PID: ~a~%" pid)
                                                (loop R pid rerun?))
                                              (begin 
                                                (dump-error "interrupting worker with PID: ~a~%" pid)
                                                (kill (- pid) SIGINT)
                                                (loop R pid #t)))
                                          (loop R (fork-command opts) #f)))

    ; Событие нормальное, но нет ничего интересного. Продолжаем цикл
    (else (loop R pid rerun?))))

(define (main-loop opts)
  (let loop ((E (events opts))
             (pid 0)
             (rerun? #f))
    ; Следующий элемент потока вычисляется в (stream-car E). R -- это ссылка на
    ; остаток потока.
    (let ((e (stream-car E))
          (R (stream-cdr E)))
      ((case (kind e)
         ((#:signal) signal-step)
         ((#:notification) notification-step)
         (else (lambda a 
                 (dump-error "unexpected event structure: ~a~%" e)
                 pid)))
       opts e loop R pid rerun?))))

(main-loop (parse-options (cdr (command-line))))
