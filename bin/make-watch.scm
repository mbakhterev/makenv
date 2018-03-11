(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 atomic)
             (ice-9 threads)
             (srfi srfi-9 gnu))

(define-immutable-record-type Options
  (options suffixes directories target toolchain debug?)
  options?
  (suffixes options:suffixes)
  (directories options:directories)
  (target options:target)
  (toolchain options:toolchain)
  (debug? options:debug?))


(define (collect-options arguments)
  (lambda () 
    (let lp ((mode #:none)
             (arguments arguments)
             (opts (options '() '() "" "" #f)))
      (if (null? arguments)
        (if (not (or (null? (options:suffixes opts))
                     (null? (options:directories opts))
                     (string-null? (options:target opts))))
          opts
          (throw 'parse-options (format #f "not enough options to run: ~a" opts)))
        (let ((a (car arguments)))
          (if (eq? #\- (string-ref a 0))
            (cond
              ((string= "-s" a) (lp #:sfx (cdr arguments) opts))
              ((string= "-d" a) (lp #:dir (cdr arguments) opts))
              ((string= "-D" a) (lp #:none (cdr arguments) (set-field opts (options:debug?) #t)))

              ((string= "-B" a) (if (string-null? (options:target opts))
                                  (lp #:tgt (cdr arguments) opts)
                                  (throw 'parse-options
                                         (format #f "build dir is already specified: ~a"
                                                 (options:target opts)))))

              ((string= "-T" a) (if (string-null? (options:toolchain opts))
                                  (lp #:tcn (cdr arguments) opts)
                                  (throw 'parse-options
                                         (format #f "toolchain is already specified: ~a"
                                                 (options:toolchain opts)))))

              (else (throw 'parse-options (format #f "unknown option: ~a" a))))
            (case mode
              ((#:sfx) (lp mode
                           (cdr arguments)
                           (set-field opts (options:suffixes) (cons a (options:suffixes opts)))))
               
              ((#:dir) (lp mode
                           (cdr arguments)
                           (set-field opts (options:directories) (cons a (options:directories opts)))))

              ((#:tcn) (lp #:none (cdr arguments) (set-field opts (options:toolchain) a)))
              ((#:tgt) (lp #:none (cdr arguments) (set-field opts (options:target) a)))

              (else (throw 'parse-options (format #f "no classifier for: ~a" a))))))))))

(define (watch-loop opts)
  (define sfxs (options:suffixes opts))
  (define dirs (options:directories opts))

  (define (wrap s) (format #f " '~a'" s)) 

  (define inotify-command
    (apply string-append "inotifywait -rm -e close_write --format '%:e %f'" (map wrap dirs)))

  (define make-command
    (string-append
      (let ((v (options:toolchain opts))) (if (null? v) "" (format #f "TCN='~a' " v)))
      (let ((v (options:target opts))) (if (null? v) "" (format #f "BDIR='~a' " v)))
      "make -r"))
  
  (define (trigger? str)
    (not (and-map (lambda (x) (not (string-suffix? x str))) sfxs)))

  ; Протокол простой. Некий аналог светофора на трёх состояниях: nothing-to-do <
  ; accepted < need-to-remake. Основной поток, следящий за сообщениями от
  ; inotifywait тянет значения вверх, future, в котором запускается make тянет
  ; значения вниз. Более подробно в заметке Сб мар 10 20:47:31 +05 2018

  (define synchro (make-atomic-box #:nothing-to-do))

  (define (remaking-loop)
    ; Убеждаемся сначала, что протокол соблюдается, вначале этого цикла должен
    ; быть запрос на пересборку
    (if (not (eq? #:need-to-remake (atomic-box-ref synchro)))
      (error "protocol violation: expecting #:need-to-remake"))

    ; Со стороны основного потока #:need-to-remake не может изменится ни на что,
    ; поэтому можно без cas-операции сбросить его в #:accepted и выполнить одну
    ; итерацию сборки
    (atomic-box-set! synchro #:accepted)
    ; (system "make -r BDIR=/tmp/sci TCN=tex")
    (system make-command)

    ; Далее нужно действовать осторожнее. accepted может быть переведён снова в
    ; need-to-remake, поэтому меняем через cas
    (let ((v (atomic-box-compare-and-swap! synchro #:accepted #:nothing-to-do)))
      ; Если предыдущее значение было #:accepted операция удалась, и можно
      ; завершать работу. Иначе необходимо повторить цикл
      (if (not (eq? #:accepted v)) (remaking-loop))))

  (let ((p (open-input-pipe inotify-command)))
    (let loop ((info (read-line p)))
      (if (not (eof-object? info))
        (begin
          (format (current-error-port) "~a: ~a~%" info (if (trigger? info) "triggered" "skipping"))
          (if (trigger? info)
            ; Необходимо обеспечить перезапуск make. Анализируем состояние
            (case (atomic-box-ref synchro)
              ; Future не подхватит уже ничего, поэтому нужно запускать новое
              ((#:nothing-to-do)
               (atomic-box-set! synchro #:need-to-remake)
               (call-with-new-thread remaking-loop))

              ; Флаг уже установлен, и future должно это заметить, нечего делать
              ((#:need-to-remake) '())

              ; Состояние accepted может поменяться на nothing-to-do, поэтому
              ; аккуратно меняем его через cas-гонку. Если гонка выиграна, то
              ; future это заметит, если проиграна, то надо создать новое,
              ; потому что текущее завершится. Убеждаемся перед этим в
              ; корректности протокола
              ((#:accepted) (let ((v (atomic-box-compare-and-swap! synchro #:accepted #:need-to-remake)))
                              (if (not (eq? #:accepted v))
                                (begin
                                  (if (not (eq? #:nothing-to-do v))
                                    (error "protocol violation: expecting #:nothing-to-do, got:" v))
                                  (atomic-box-set! synchro #:need-to-remake)
                                  (call-with-new-thread remaking-loop)))))))
          (loop (read-line p)))))
    (close-pipe p)))

(let* ((args (command-line))
       (prog (car args)))
  (catch 'parse-options
         (lambda () (call-with-values (collect-options (cdr args)) watch-loop))
         (lambda (key . args) (format (current-error-port) "~a: ~a~%" prog (car args)))))
