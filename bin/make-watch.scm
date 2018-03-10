(use-modules (ice-9 popen) (ice-9 rdelim) (ice-9 futures))

(define (collect-options arguments)
  (lambda () 
    (let lp ((mode #:none)
             (arguments arguments)
             (suffixes '())
             (directories '()))
      (if (null? arguments)
        (values suffixes directories)
        (let ((a (car arguments)))
          (if (eq? #\- (string-ref a 0))
            (cond
              ((string= "-s" a) (lp #:sfx (cdr arguments) suffixes directories))
              ((string= "-d" a) (lp #:dir (cdr arguments) suffixes directories))
              (else (throw 'parse-options (format #f "unknown option: ~a" a))))
            (case mode
              ((#:sfx) (lp mode (cdr arguments) (cons a suffixes) directories))
              ((#:dir) (lp mode (cdr arguments) suffixes (cons a directories)))
              (else (throw 'parse-options (format #f "no classifier for: ~a" a))))))))))

(define (watch-loop sfxs dirs)
  (define (wrap s) (format #f " '~a'" s)) 

  (define inotify-command
    (apply string-append "inotifywait -rm -e close_write --format '%:e %f'" (map wrap dirs)))
  
  (define (trigger? str)
    (not (and-map (lambda (x) (not (string-suffix? x str))) sfxs)))

  (define (make-thread thrd str)
    ; Запускать make снова нужно в случае, если строчка зацепила один из
    ; отслеживаемых суффиксов, и либо рабочий поток не создан, либо создан и
    ; закончился. Видимо, thread-join! вызывать не нужно. Потоки и так
    ; собираются
    (format (current-error-port) "~a: ~a~%" str (if (trigger? str) "triggered" "skipping"))
    (cond ((not (trigger? str)) thrd)

          ((or (not (thread? thrd)) (thread-exited? thrd))
           (call-with-new-thread (lambda () (system "make -r BDIR=/tmp/sci TCN=tex"))))
          
          (else thrd)))

  (let ((p (open-input-pipe inotify-command)))
    (let loop ((info (read-line p))
               (thrd #f))
      (if (not (eof-object? info))
        (loop (read-line p) (make-thread thrd info))))
    (close-pipe p)))

(let* ((args (command-line))
       (prog (car args)))
  (catch 'parse-options
         (lambda () (call-with-values (collect-options (cdr args)) watch-loop))
         (lambda (key . args) (format (current-error-port) "~a: ~a~%" prog (car args)))))
