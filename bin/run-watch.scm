; Реализация на основе классических POSIX-механизмов

(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-41)
             (common))

(define-stream (events ))

(define (main-loop opts)
  (define to-read car)
  (define to-write cdr)
  (define of-child first)
  (define of-watch second)
 
  (let* ((p (pipe))
         (p-child (to-read p))
         (p-watch (apply open-pipe* OPEN_READ (inotify-command opts)))
         (poll (lambda () (catch 'system-error
                                 (lambda () (first (select (list p-child p-watch) '() '() #f)))
                                 (lambda err (if (= EINTR (system-error-errno err))
                                               (select (list p-child p-watch) '() '() #f)
                                               (apply throw err))))))
         ; Основная идея: у нас есть поток событий, который упорядочивается
         ; select-ом, мы бежим по цепочке этих событий, обновляя состояние,
         ; которое определит ту операцию, которую нужно выполнить 
         (next-state-from-pipe
           (lambda (pipe state)
             (let ((info (read-line pipe)))
               (if (eof-object? info)
                 (begin (close-pipe pipe)
                        #:done)
                 (let ((t? (trigger? opts info)))
                   (dump-error "~a: ~a~%" info (if t? "triggered" "skipped"))
                   (if (not t?)
                     state
                     (case state
                       ((#:idle) #:restart)
                       ()
                       )
                     )
                   )
                 )
               )
             ))
         (next-state (lambda (port state)
                       (cond
                         ((equal? p-watch port)
                          (let ((info (read-line port)))
                            (if (eof-object? info)
                              (begin (close-pipe port)
                                     #:done)
                              )
                            
                            (if (not t?)
                              state
                              )
                            )
                          )

                         ((equal? p-child port))
                         (else (error "Unexpected:" port)))
                       ))
         )
    (let loop ((state #:idle)
               (events (poll)))
      (fold (lambda (p st)))
      )
    )
  )
