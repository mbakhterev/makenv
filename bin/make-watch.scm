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

; Quite stable code

(define to-read car)
(define to-write cdr)

(define (notifications-pipe opts)
  (apply open-pipe* OPEN_READ (inotify-command opts)))

(define (event-pipe) (let ((p (pipe))) (setvbuf (to-write p) 'none) p))

(define (pipe? p) (and (pair? p)
                       (input-port? (to-read p))
                       (output-port? (to-write p))))

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

(define (timeout? t) (and (number? t) (not (negative? t))))
(define (counter? n) (and (integer? n) (positive? n)))

(define (fork-command opts)
  (let* ((P (event-pipe))
         (R (to-read P))
         (W (to-write P))
         (cmd (options:command opts))
         (p (primitive-fork)))
    (if (zero? p)
        (begin
          (default-signals)
          (close-output-port W)
          (unless (char=? #\G (read-char R)) (exit 1))
          (clear-ports)
          (tune-env! opts)
          (dump-error "worker is clean to go. PID: ~a~%" (getpid))
          (apply execlp (car cmd) cmd))
        (begin 
          (close-input-port R)
          (setpgid p p)
          (write-char #\G W)
          (close-output-port W)
          p))))

(define (wait-for pid)
  (let ((wait (lambda () (false-if-exception (waitpid WAIT_ANY WNOHANG)))))
    (lambda (pid)
      (let loop ((p (wait)))
        (and (pair? p) (or (= pid (car p))
                           (loop (wait))))))))

(define decode-signals
  (let ((signum (lambda (c) (case c
                              ((#\D) SIGCHLD)
                              ((#\T) SIGTERM)
                              ((#\I) SIGINT)
                              (else 0)))))
    (lambda (E)
      (fold (lambda (c R) (logior R (ash 1 (signum c))))
            0
            E))))

(define (signals-ok? v) (and (not (logbit? 0 v)) (positive? v)))

; Semistable pices of code

; FIXME: Code is not thread friendly.
(define signal-pipe
  (let ((p #f))
    (lambda ()
      (if (pipe? p)
          (to-read p)
          (begin
            (set! p (event-pipe))
            (let ((o (to-write p)))
              (sigaction SIGINT (lambda (n) (write-char #\I o)))
              (sigaction SIGCHLD (lambda (n) (write-char #\D o)) SA_NOCLDSTOP)
              (sigaction SIGTERM (lambda (n) (write-char #\T o)))
              (to-read p)))))))

; Сигнала с номером 0 не бывает. Поэтому можно использовать эту позицию в
; битвекторе, как признак неожиданного символа в event-канале от обработчиков
; сигналов.

(define (signals-present? v) (or (bitvector-ref v SIGCHLD)
                                 (bitvector-ref v SIGINT)
                                 (bitvector-ref v SIGTERM)))

; Unstable code

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
                     (cons #f R)
                     (let ((v (read-proc p)))
                       (if (eof-object? v)
                           (cons #t R)
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

(define eof? car)
(define content cdr)

(define (resignal s)
  (default-signals)
  (let ((p (getpid))) (kill (if (= p (getpgrp)) (- p) p) s)))
 
(define (child-step opts pid rerun? signals lost? loop)
  (cond
    ; Если наш процесс не завершился, просто продолжаем цикл. 
    ((not (wait-for pid)) (loop pid rerun? signals))

    ; Если процесс завершился, потому что его остановили по сигналу в signals,
    ; то мы должны теперь сами себя остановить одним из этих сигналов. Приоритет
    ; отдаём SIGTERM, если был. Причина остановки по сигналу отмечается в
    ; signals
    ((logbit? SIGTERM signals) (resignal SIGTERM))
    ((logbit? SIGINT signals) (resignal  SIGINT))

    ; Может быть так, что у нас отвалился канал с уведомлениями. В этом случае
    ; нужно выйти из цикла. Дочерний процесс завершён, можно вернуть 0.
    (lost? 0)

    (else (loop (if rerun? (fork-command opts) 0) #f 0))))

(define (int-step opts pid rerun? signals loop)
  (dump-error "SIGINT~%")
  ; Если дочерний процесс запущен, надо отправить ему прерывающий сигнал, а
  ; потом дождаться, когда он будет завершён в child-step. Иначе, можно
  ; завершиться
  (if (positive? pid)
      (begin (kill (- pid) SIGINT)
             (loop pid #f (logior signals (ash 1 SIGINT))))
      (resignal SIGINT)))

(define (term-step opts pid rerun? signals loop)
  (dump-error "SIGTERM~%")
  (if (positive? pid)
      (begin (kill (- pid) SIGTERM)
             (loop pid #f (logior signals (ash 1 SIGTERM))))
      (resignal SIGTERM)))

(define (signal-step opts e pid rerun? signals lost? loop)
  (let ((V (decode-signals (content e))))
    (if (or (eof? e)
            (not (signals-ok? V)))
        ; Если сломался канал с сигналами по какой-то причине, то ничего толком
        ; сделать уже нельзя. Сообщаем об этом и отдаём pid на внешний уровень
        ; обработки
        (begin (dump-error "unexpected signal event: ~a~%" e)
               pid)

        ; Каждая из step-процедур возвращает либо pid, и тогда цикл прерывается,
        ; либо передаёт в цикл новое состояние из pid, rerun? и signals, тогда
        ; цикл продолжается. Но перед тем, как вернуться в loop, нужно ещё
        ; пройти шаги по сигналам. Поэтому, в каждую из процедур передаётся
        ; «прослойка».
        (let* ((after-int (if (logbit? SIGCHLD V)
                              (lambda (p r? s) (child-step opts p r? s lost? loop))
                              loop))
               (after-term (if (logbit? SIGINT V)
                               (lambda (p r? s) (int-step opts p r? s after-int))
                               after-int)))
          (if (logbit? SIGTERM V)
              (term-step opts pid rerun? signals loop)
              (after-term pid rerun? signals))))))

(define (any-triggers? opts strings)
  (fold (lambda (s result) (let ((t? (trigger? opts s)))
                             (format #t "~a ~a~%" s (if t? "triggered" "skipping"))
                             (if t? #t result)))
        #f
        (reverse strings)))

(define (notification-step opts e pid rerun? signals loop)
  (cond
    ; Notification-канал утерян. Сообщаем и сигнализируем об этом. Во всех
    ; других ветках канал в порядке.
    ((eof? e) (dump-error "unexpected notification event: ~a~%" e)
              (if (positive? pid)
                  (loop pid rerun? #t)
                  0))

    ; То, ради чего всё затевалось. Если файлы обновились, а процесс их
    ; обработки затянулся, надо его прервать. Здесь два варианта развития
    ; событий.
    ;
    ; 1. Процесс не запущен (pid = 0), тогда надо его запустить, со
    ; сброшенным флагом rerun?. Реакция на изменения уже запущена.
    ;
    ; 2. Процесс запущен (pid > 0), тогда надо прервать его группу (kill (- pid)
    ; SIGINT), и продолжить цикл с установленным rerun?. Когда дочерний процесс
    ; прервётся, поступит сигнал SIGCHLD, и после некоторой обработки управление
    ; перейдёт во вторую ветку процедуры child-signal-step, где процесс реакции на
    ; обновления будет запущен снова.
    ;
    ; Дополнение: если мы уже находимся в процессе перезапуска процесса, то
    ; ничего делать не нужно.
    ((any-triggers? opts (content e))
     (if (positive? pid)
         (if (or rerun? (positive? signals))
             (begin
               (dump-error "interruption in progress. PID: ~a~%" pid)
               (loop pid rerun? #f))
             (begin 
               (dump-error "interrupting PID: ~a~%" pid)
               (kill (- pid) SIGINT)
               (loop pid #t #f)))
         (loop (fork-command opts) #f #f)))

    ; Событие нормальное, но нет ничего интересного. Канал в порядке. Продолжаем
    (else (loop pid rerun? #f))))

(define (main-loop opts)
  (let* ((n (notifications-pipe opts))
         (s (signal-pipe))
         (s-drain (drainer s read-char 32 0))
         (n-drain (drainer n read-line 32 0.1)))
    (let loop ((poll (selector #f s n))
               (P '())
               (pid 0)
               (rerun? #f)
               (signals 0)
               (lost? #f))
      (gc)
      (cond ((null? P) (loop poll (poll) pid rerun? signals lost?))

            ; Когда срабатывают сигналы, может произойти всякое. Что это всякое
            ; может вернуть?
            ;
            ; 1. Может вернуться pid и rerun? на ветке child-step. Там может
            ; быть снова запущен или не запущен worker.
            ;
            ; 2. Могут вернуться биты, описывающие зарегистрированные сигналы
            ; остановки. Зачем нам эти биты? Чтобы аккуратно дождаться
            ; завершения дочернего процесса, а потом снова вызвать указанные
            ; сигналы. Эти данные потекут в child-step. Ну, хорошо.
            ((equal? s (car P))
             (signal-step opts
                          (s-drain)
                          pid
                          rerun? 
                          signals
                          lost?
                          (lambda (p r? s) (loop poll (cdr P) p r? s lost?)))) 

            ; Когда приходит информация об изменениях в fs, то после их
            ; обработки, может вернуться информация о процессе, о необходимости
            ; его перезапустить и о потере notification-канала.
            ((equal? n (car P))
             (notification-step opts
                                (n-drain)
                                pid
                                rerun?
                                signals
                                (lambda (p r? l?) (loop (if l? (selector #f s) poll)
                                                        (cdr P) p r? signals l?))))

            (else (dump-error "unknown port: ~a~%" (car P))
                  pid)))))

(let ((p (main-loop (parse-options (cdr (command-line))))))
  (dump-error "should not be here. PID: ~a~%" p)
  (when (positive? p) (kill (- p) SIGTERM))
  (exit 1))
