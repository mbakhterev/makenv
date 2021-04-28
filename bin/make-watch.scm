; Реализация на основе классических POSIX-механизмов

(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
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
                       (lambda () (car (select ports '() '() t)))
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

(define (port-drainer read-proc limit p)
  (let ((readable? (readable-port? 0.01 p))
        (next (if (counter? limit) 1- (const 1)))
        (N (if (counter? limit) limit 1)))
    (lambda () (let cycle ((n N)
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
         (s-drain (port-drainer ))

         )
    (stream-map (lambda (p)
                  (cond ((equal? p s) (if (eq? #\D (read-char p))
                                          #:child
                                          #:unexpected-signal-char))
                        ((equal? p n) (drain-triggers opts p))
                        (else #:unknown-port)))
                (ready-ports s n))))


(define ())

(define (events opts)
  (let* ((s signal-pipe)
         (n (notifications-pipe opts))
         (poll (readable 0 s n)))
    (stream-let cycle ((ports (poll)))
      (if (null? P)
          (cycle (poll))
          (let ((p (car P))
                (r (cdr P)))
            (cond ((equal? p s) (let ((c (read-char p)))
                                  (if (eq? #\D c)
                                      (stream-cons #:child (cycle r))
                                      (error "unexpected signal char:" c))))
                  ((equal? p n) (let ((v (read-line p)))
                                  (if (eof-object? v)
                                      (begin (close-pipe p) stream-null)
                                      (let ((t? (trigger? opts v)))
                                        (dump-error "~a: ~a~%"
                                                    v
                                                    (if t? "triggered" "skipping"))
                                        (if t?
                                            (cycle r)
                                            (cycle r))))))
                  (else (error "unknown port:" p))))))))

(define (fork-command cmd)
  (let ((p (primitive-fork)))
    (if (not (zero? p))
        p
        (apply execlp (head cmd) cmd))))

(define head car)
(define tail cdr)

(define (events opts)
  (let ((read-end car)
        (write-end cdr)
        (signal-pipe (pipe))
        (p-watch (apply open-pipe* OPEN_READ (inotify-command opts))))
    (letrec ((p-signal (read-end signal-pipe))
             (poll (let ((l (list p-signal p-watch)))
                     (lambda () (catch 'system-error
                                       (lambda () (head (select l '() '() #f)))
                                       ; Если select прерван сигналом, то повторяем
                                       ; вызов
                                       (lambda er (if (= EINTR (system-error-errno er))
                                                    (poll)
                                                    (apply throw er))))))))
      (sigaction SIGCHLD
                 (let ((p (write-end signal-pipe)))
                   (lambda (n) (write-char #\D p) (force-output p)))
                 SA_NOCLDSTOP)
      (stream-let loop ((ports '()))
        (if (null? ports)
          ; Если нет готовых к чтению портов, вызываем poll и продолжаем цикл
          (loop (poll))
          ; Иначе разбираемся с первым портом p в списке 
          (let ((p (head ports))
                (r (tail ports)))
            (cond ((equal? p-signal p) (let ((c (read-char p)))
                                         (if (eq? #\D c)
                                           (stream-cons #:done (loop r))
                                           (error "Unexpected signal char:" c))))
                  ((equal? p-watch p) (let ((info (read-line p)))
                                        (if (eof-object? info)
                                          (begin (close-pipe p) stream-null)
                                          (let ((t? (trigger? opts info)))
                                            (dump-error "~a: ~a~%"
                                                        info
                                                        (if t? "triggered" "skipped"))
                                            (if t?
                                              (stream-cons #:triggered (loop r))
                                              (loop r))))))
                  (else (error "Unexpected port:" p)))))))))

(define (main-loop opts s)
  (let* ((fork-cmd (lambda () (let ((cl (options:command opts))
                                    (p (primitive-fork)))
                                (if (not (zero? p)) p (apply execlp (head cl) cl)))))
         ; Структура состояния свёртки -- пара (pid есть-неучтённая-работа?)
         (pid car)
         (job? cdr)
         (proc (lambda (st e)
                 (case e
                   ; Завершился дочерний процесс
                   ((#:done) (if (not (= (pid st)
                                         (pid (waitpid WAIT_ANY WNOHANG))))
                               ; Это не рабочий процесс, менять нечего
                               st
                               ; Завершился рабочий процесс. Его нужно
                               ; повторно запустить, если есть новая работа.
                               ; Флаг о наличии работы надо сбросить. Если
                               ; работ нет, ничего не запускаем, аннулируем pid
                               ; в состоянии
                               (cons (if (job? st) (fork-cmd) 0) #f)))
                   ((#:triggered) (if (not (zero? (pid st)))
                                    ; Не обнаружено завершение рабочего
                                    ; процесса, запоминаем что есть работа
                                    (cons (pid st) #t)
                                    ; Иначе запускаем процесс, отмечая, что
                                    ; работы учтены
                                    (cons (fork-cmd) #f)))
                   (else (error "Unexpected event:" e)))))
         (final (stream-fold proc '(0 . #f) s)))
    ; Если в конце оказался запущенным некоторый процесс, нужно дождаться его
    ; завершения
    (when (not (zero? (pid final)))
      (waitpid (pid final)))))

(let ((opts (parse-options (tail (command-line)))))
  (tune-env! opts)
  (main-loop opts (events opts)))
