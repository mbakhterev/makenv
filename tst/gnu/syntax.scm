(define-syntax make-echo-2
  (let ((rename (lambda (ns)
                  (map (lambda (n) (string->symbol (string-append "echo-" (syntax->datum n)))) ns))))
    (lambda (x)
      (syntax-case x ()
        ((make-echo-2 j ...)
         (with-syntax (((fn ...) (datum->syntax x (rename (syntax (j ...))))))
                      (syntax (begin (define fn (lambda (target) (echo j target))) ...)))))))) 
