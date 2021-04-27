(define-module (options)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9 gnu)
               #:use-module (srfi srfi-11)
               #:use-module (common)
               #:export (parse-options
                         options:directories
                         options:suffixes
                         options:target
                         options:toolchain
                         options:debug?
                         options:command))

; Да, вот так вот мы объявляем структуры в Scheme. И это боль и печаль. Хотелось
; бы обойтись без структуры в данном случае, но она уместна.

(define-immutable-record-type Options
  (options directories suffixes target toolchain debug? command)
  options?
  (directories options:directories set-options:directories)
  (suffixes    options:suffixes    set-options:suffixes)
  (target      options:target      set-options:target)
  (toolchain   options:toolchain   set-options:toolchain)
  (debug?      options:debug?      set-options:debug?)
  (command     options:command     set-options:command))

(define (flag? a) (and (= 2 (string-length a)) (eq? #\- (string-ref a 0))))

(define (dash a) (string #\- a))
(define (undash a) (and (flag? a) (string-ref a 1)))

; with-unset проверяет, нужно ли проверить, что значение не установлено, и если
; нужно, проверяет, не установлено ли. Если по результатам проверки значение
; установлено, with-unset вызывает ошибку. Иначе, передаёт список в продолжение.

(define (with-unset am unset? v k)
  (if (or (string-null? am)
          (unset? v))
      (k v)
      (error am v)))

(define (gather-list flag get set already-message)
  (lambda (args opts)
    (with-unset already-message null? (get opts)
                (lambda (l)
                  (let-values (((words rest) (span (compose not flag?) args)))
                    (if (null? words)
                        (error "no parameter list for:" (dash flag))
                        (flags rest (set opts (fold cons l words)))))))))

(define (gather-rest flag get set already-message)
  (lambda (args opts)
    (with-unset already-message null? (get opts)
                (lambda (l)
                  (if (null? args)
                      (error "no parameter list for:" (dash flag))
                      (set opts (reverse (fold cons l args))))))))

(define (gather-string flag get set already-message)
  (lambda (args opts)
    (with-unset already-message string-null? (get opts)
                (lambda (s)
                  (if (null? args)
                      (error "no parameter for:" (dash flag))
                      (flags (cdr args) (set opts (car args))))))))

(define (gather-boolean flag get set already-message)
  (lambda (args opts)
    (with-unset already-message (lambda (v) (and (boolean? v) (not v))) (get opts)
                (lambda (b)
                  (flags args (set opts #t))))))

(define (flags arguments options)
  (if (null? arguments)
      options
      ((case (undash (car arguments))
         ((#\s) (gather-list #\s
                             options:suffixes
                             set-options:suffixes
                             ""))
         ((#\d) (gather-list #\d
                             options:directories
                             set-options:directories
                             ""))
         ((#\r) (gather-rest #\r
                             options:command
                             set-options:command
                             ""))
         ((#\B) (gather-string #\B
                               options:target
                               set-options:target
                               "build directory is already specified:"))
         ((#\T) (gather-string #\T
                               options:toolchain
                               set-options:toolchain
                               "toolchain is already specified:"))
         ((#\D) (gather-boolean #\D
                                options:debug?
                                set-options:debug?
                                "debug mode is already enabled:"))
         (else (error "unknown option:" (car arguments))))
       (cdr arguments) options)))

(define defaultize identity)

(define parse-options
  (let ((unspecified (options '() '() "" "" #f '())))
    (compose defaultize flags (lambda (arguments) (values arguments unspecified)))))
