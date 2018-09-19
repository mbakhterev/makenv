(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 atomic)
             (ice-9 threads)
             (ice-9 receive)
             (srfi srfi-1)
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

(define (watch-loop opts)
  (define inotify (apply list
                         "inotifywait" "-rm" "-e" "close_write" "--format" "%:e %f"
                         (options:directories opts)))

  (define (trigger? str) (any (lambda (s) (string-suffix? s str)) (options:suffixes opts)))

  ; Протокол простой. Семафор на трёх состояниях: (< nothing-to-do accepted
  ; need-to-run). Основной поток, следящий за сообщениями от inotifywait
  ; тянет значения вверх. Поток, в котором запускается команда, тянет значение
  ; вниз.  Более подробно в заметке Сб мар 10 20:47:31 +05 2018
  (define state (make-atomic-box #:nothing-to-do))

  ; Процедура для запуска в потоке, запускающем команду по событию
  (define (run-loop)
    ; Сначала убеждаемся, что протокол соблюдается. В начале этого цикла должен
    ; быть запрос на перезапуск
    (let ((st (atomic-box-ref state)))
      (when (not (eq? #:need-to-run st ))
        (error "Protocol violation. Expecting #:need-to-run. Got:" st)))

    ; Со стороны основного потока #:net-to-run не может измениться ни на что,
    ; поэтому можно сбросить его в #:accepted без cas-операции и выполнить одну
    ; итерацию сборки. Результат не важен, поэтому без проверок
    (apply system* (options:command opts))

    ; #:accepted может быть переведён снова в need-to-run, поэтому флаг
    ; меняем через cas
    (let ((v (atomic-box-compare-and-swap! state #:accepted #:nothing-to-do)))
      ; Если операция удалось, то основной поток увидит значение #:nothing-to-do
      ; и то перезапустит run-loop. Если не удалась, значит, семафор переброшен
      ; в #:need-to-run и нам надо повторять цикл
      (when (not (eq? #:accepted v)) (run-loop)))) 

  ; Тело процедуры watch-loop
  (setenv "BDIR" (options:target opts))
  (setenv "TCN" (options:toolchain opts))
  (when (options:debug? opts) (setenv "DBG" "Y"))
   
  (let ((p (apply open-pipe* (options:command opts)))))


  )
