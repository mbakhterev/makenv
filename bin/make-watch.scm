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

(let* ((args (command-line))
       (prog (car args)))
  (catch 'parse-options
         (lambda ()
           (call-with-values
             (collect-options (cdr args))
             (lambda (sfx dir) (format #t "sfx: ~s; dir: ~s~%" sfx dir))))
         (lambda (key . args)
           (format (current-error-port) "~a: ~a~%" prog (car args)))))
