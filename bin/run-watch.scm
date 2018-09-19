(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 atomic)
             (ice-9 threads)
             (ice-9 receive)
             (srfi srfi-9 gnu))

(define-immutable-record-type Options
  (options suffixes directories target toolchain debug? command)
  options?
  (suffixes options:suffixes set-options:suffixes)
  (directories options:directories set-options:directories)
  (target options:target set-options:target)
  (toolchain options:toolchain set-options:toolchain)
  (debug? options:debug? set-options:debug?)
  (command options:command set-options:command))

; Можно использовать стандартные процедуры разбора опций, например, getopt-long.
; Но в этих стандартных процедурах не реализуема логика, в которой ключи
; изменяют режимы разбора аргументов. Не хочется, ведь, вместо «-s a b c d»
; писать «-s a -s b -s c -s d». Поэтому самодельный разбор опций
(define (collect-options arguments)
  ; Замена неопределённых значений на значения по-умолчанию
  (define (defaultize o)
    (let ((sfxs (options:suffixes o))
          (dirs (options:directories o))
          (tgt (options:target o))
          (tcn (options:toolchain o))
          (cmd (options:command o)))
      (set-fields o
                  ((options:suffixes) (if (null? sfxs) '("") sfxs))
                  ((options:directories) (if (null? dirs) '(".") dirs))
                  ((options:toolchain) (if (string-null? tcn) "toolchain" tcn))
                  ((options:target) (if (string-null? tgt) "/tmp/bdir" tgt))
                  ((options:command) (if (null? cmd) '("make") cmd)))))

  (define (flag? a) (and (= 2 (string-length a))
                         (eq? #\- (string-ref a 0))))
  
  ; Тело процедуры collect-options
  (defaultize
    (let loop ((mode #:none)
               (args arguments)
               (opts (options '() '() "" "" #f '())))
      (if (null? args)
        opts
        (let ((a (car args))
              (rest (cdr args)))
          (if (flag? a)
            (case (string-ref a 1)
              ((#\s) (loop #:sfx rest opts))
              ((#\d) (loop #:dir rest opts))
              ((#\r) (loop #:cmd rest opts))
              ((#\D) (loop #:none rest (set-options:debug? opts #t)))
              ((#\B) (let ((tgt (options:target opts))) (if (string-null? tgt)
                                                          (loop #:tgt rest opts)
                                                          (error "build dir is already specified:" tgt))))
              ((#\T) (let ((tcn (options:toolchain opts))) (if (string-null? tcn)
                                                             (loop #:tcn rest opts)
                                                             (error "toolchain is already specified:" tcn))))
              (else (error "unknown option:" a)))
            (case mode
              ((#:sfx) (loop mode
                             rest
                             (set-options:suffixes opts (cons a (options:suffixes opts)))))
              ((#:dir) (loop mode
                             rest
                             (set-options:directories opts (cons a (options:directories opts)))))
              ((#:tcn) (loop #:none rest (set-options:toolchain opts a)))
              ((#:tgt) (loop #:none rest (set-options:target opts a)))
              ((#:cmd) (set-options:command opts args))
              (else (error "no classifier for:" a)))))))))

(write (collect-options (cdr (command-line))))
(newline)
