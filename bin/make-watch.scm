; Реализация на основе классических POSIX-механизмов

(add-to-load-path
  (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-11)
             (srfi srfi-31)
             (srfi srfi-41)
             (common)
             (options))

(define to-read car)
(define to-write cdr)

(define signal-pipe
  (let* ((p (pipe))
         (o (to-write p)))
    (setvbuf o 'none)
    (sigaction SIGCHLD
               (lambda (n) (write-char #\D o))
               SA_NOCLDSTOP)
    (to-read p)))

(define (notifications-pipe opts)
  (apply open-pipe* OPEN_READ (inotify-command opts)))

(define (timeout? t) (and (number? t) (not (negative? t))))
(define (counter? n) (and (integer? n) (positive? n)))

; Слишком сложный код. Вынести бы отсюда сигналы и select-ы. Это можно сделать,
; если сразу выдавать из poll. Но проблема в том, что этот цикл по ports тогда
; просто переедет в selector. Поэтому здесь проще просто выдавать события в виде
; поток. Тогда не придётся перевызывать процедуру и страдать с каррированием и
; промежуточными контекстами.

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

(define (fork-command cmd)
  (let ((p (primitive-fork)))
    (if (not (zero? p))
        p
        (apply execlp (car cmd) cmd))))

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
    ((wait-for pid) (loop R (if rerun? (fork-command opts) 0)) #f)

    ; Сигналы нормальные, но задача не выполнена, продолжаем цикл
    (else (loop R pid rerun?))))

(define (any-triggers? opts strings)
  (fold (lambda (s result) (let ((t? (trigger? opts s)))
                             (format "~s ~s~%" (if t? "triggered" "skipping"))
                             (if t? #t result)))
        #f
        strings))

(define (notification-step opts e loop R pid rerun?)
  (cond
    ; Так не должно быть. Не возвращаемся в цикл, возвращаем pid на внешний
    ; уровень обработки
    ((eof? e)
     (dump-error "unexpected notification event: ~a~%" e)
     pid)

    ; То, ради чего всё затевалось. Если изменились интересные файлы, надо
    ; прервать текущий процесс обработки. Он может быть долгим, и
    ; тратить время на ожидание не хочется, так как его результат уже не будет
    ; актуальными.
    ;
    ; Алгоритм прерывания: если процесс запущен. FIXME: Не завершено
    ((any-triggers? opts (content e)) pid)

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
       opts e loop R pid rerun?)))
  )

; (define (main-loop opts s)
;   (let* ((fork-cmd (lambda () (let ((cl (options:command opts))
;                                     (p (primitive-fork)))
;                                 (if (not (zero? p)) p (apply execlp (head cl) cl)))))
;          ; Структура состояния свёртки -- пара (pid есть-неучтённая-работа?)
;          (pid car)
;          (job? cdr)
;          (proc (lambda (st e)
;                  (case e
;                    ; Завершился дочерний процесс
;                    ((#:done) (if (not (= (pid st)
;                                          (pid (waitpid WAIT_ANY WNOHANG))))
;                                ; Это не рабочий процесс, менять нечего
;                                st
;                                ; Завершился рабочий процесс. Его нужно
;                                ; повторно запустить, если есть новая работа.
;                                ; Флаг о наличии работы надо сбросить. Если
;                                ; работ нет, ничего не запускаем, аннулируем pid
;                                ; в состоянии
;                                (cons (if (job? st) (fork-cmd) 0) #f)))
;                    ((#:triggered) (if (not (zero? (pid st)))
;                                     ; Не обнаружено завершение рабочего
;                                     ; процесса, запоминаем что есть работа
;                                     (cons (pid st) #t)
;                                     ; Иначе запускаем процесс, отмечая, что
;                                     ; работы учтены
;                                     (cons (fork-cmd) #f)))
;                    (else (error "Unexpected event:" e)))))
;          (final (stream-fold proc '(0 . #f) s)))
;     ; Если в конце оказался запущенным некоторый процесс, нужно дождаться его
;     ; завершения
;     (when (not (zero? (pid final)))
;       (waitpid (pid final)))))

; (let ((opts (parse-options (tail (command-line)))))
;   (tune-env! opts)
;   (main-loop opts (events opts)))

(main-loop (parse-options (cdr (command-line))))
