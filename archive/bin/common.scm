(define-module (common)
               #:use-module (srfi srfi-1)
               #:use-module (options) 
               #:export (dump-error
                         tune-env!
                         inotify-command
                         trigger?))

(define dump-error
  (let ((p (current-error-port)))
    (lambda (fmt . args) (apply format p fmt args))))

(define (tune-env! opts)
  (setenv "BDIR" (options:target opts))
  (setenv "TCN" (options:toolchain opts))
  (when (options:debug? opts) (setenv "DBG" "Y")))

(define (inotify-command opts)
    (apply list
           "inotifywait" "-rm" "-e" "close_write" "--format" "%:e %f"
           (options:directories opts)))

(define (trigger? opts str)
  (any (lambda (s) (string-suffix? s str)) (options:suffixes opts)))
