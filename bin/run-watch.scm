; Реализация на основе классических POSIX-механизмов

(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-41)
             (common))

(define (events opts)
  (define to-read car)
  (define to-write cdr) 

  (define signal-pipe (pipe))
  (define p-signal (to-read signal-pipe))
  (define p-watch (let ((cmd (inotify-command opts)))
                    (format #t "inotify command: ~s~%" cmd)
                    (apply open-pipe* OPEN_READ cmd)))

  (define (poll) (catch 'system-error
                        (lambda () (car (select (list p-signal p-watch) '() '() #f)))
                        (lambda err (if (= EINTR (system-error-errno err))
                                      (poll)
                                      (apply throw err)))))

  (define-stream (events-stream ports)
    (if (null? ports)
      ; Если закончились готовые к чтению порты, вызываем poll и продолжаем
      (events-stream (poll))
      ; В противном случае, разбираемся, с первым портом в списке
      (let ((p (car ports))
            (rest (cdr ports)))
        (cond ((equal? p-signal p) (let ((c (read-char p)))
                                     (if (eq? #\D c)
                                       (stream-cons #:done (events-stream rest))
                                       (error "Unexpected signal char:" c))))
              ((equal? p-watch p) (let ((info (read-line p)))
                                    (if (eof-object? info)
                                      (begin
                                        (close-pipe p)
                                        stream-null)
                                      (let ((t? (trigger? opts info)))
                                        (dump-error "~a: ~a~%" info (if t? "triggered" "skipped"))
                                        (if t?
                                          (stream-cons #:triggered (events-stream rest))
                                          (events-stream rest))))))
              (else (error "Unexpected port:" p))))))

  (sigaction SIGCHLD
             (let ((p (to-write signal-pipe))) (lambda (n) (write-char #\D p) (force-output p)))
             SA_NOCLDSTOP)
  
  (events-stream '()))

(define (main-loop opts s)
  (define cmdline (options:command opts))

  (define (run-cmd) (let ((p (primitive-fork)))
                      (if (not (zero? p))
                        p 
                        (begin (tune-env! opts)
                               (apply execlp (car cmdline) cmdline)))))

  ; Состояние -- это пара (pid есть-ли-работа?)
  (define pid car)
  (define job? cdr)

  (define (step st e)
    (case e
      ; Завершился некоторый дочерний процесс
      ((#:done) (if (not (= (pid st)
                            (pid (waitpid WAIT_ANY WNOHANG))))
                  ; Если это не рабочий процесс, то ничего не меняется
                  st
                  ; Иначе, если есть некоторая работа, запускаем новый рабочий
                  ; процесс, сбрасывая флаг о наличии работ. Если работы никакой
                  ; нет, то обнуляем в состоянии информацию о текущем процессе
                  (cons (if (job? st) (run-cmd) 0) #f)))
      ; Появилась некоторая работа
      ((#:triggered) (if (not (= 0 (pid st)))
                       ; Если рабочий процесс не завершён, то запоминаем, что у
                       ; нас есть новая работа
                       (cons (pid st) #t)
                       ; Если завершён, то нужно его запустить
                       (cons (run-cmd) #f)))
      (else (error "Unexpected event:" e)))) 

  (stream-fold step '(0 . #f) s))

(let ((opts (collect-options (cdr (command-line)))))
  (main-loop opts (events opts)))
