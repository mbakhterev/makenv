; Реализация на основе классических POSIX-механизмов

(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-41)
             (common))

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

(let ((opts (collect-options (tail (command-line)))))
  (tune-env! opts)
  (main-loop opts (events opts)))
