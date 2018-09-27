(add-to-load-path (let ((fn (current-filename))) (if (string? fn) (dirname fn) ".")))

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 atomic)
             (ice-9 threads)
             (common))

(define (watch-loop opts)
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
    (atomic-box-set! state #:accepted)
    (apply system* (options:command opts))

    ; #:accepted может быть переведён снова в need-to-run, поэтому флаг
    ; меняем через cas
    (let ((v (atomic-box-compare-and-swap! state #:accepted #:nothing-to-do)))
      ; Если операция удалось, то основной поток увидит значение #:nothing-to-do
      ; и то перезапустит run-loop. Если не удалась, значит, семафор переброшен
      ; в #:need-to-run и нам надо повторять цикл
      (when (not (eq? #:accepted v))
        ; (dump-error "R: Repeating command~%")
        (run-loop)))) 

  (define (run-thread)
    ; (dump-error "M: New run-thread~%")
    (atomic-box-set! state #:need-to-run)
    (call-with-new-thread run-loop))

  ; Тело процедуры watch-loop
  (tune-env! opts)
   
  (let ((p (apply open-pipe* OPEN_READ (inotify-command opts))))
    (let loop ((info (read-line p)))
      (if (eof-object? info)
        (close-pipe p)
        (let ((t? (trigger? opts info)))
          (dump-error "~a: ~a~%" info (if t? "triggered" "skipped"))
          (when t?
            (case (atomic-box-ref state)
              ; Вариант, когда поток запуска команд ничего не собирается
              ; подхватывать. Нужно его перезапустить, предварительно установив
              ; флаг о наличии работы. Результат -- созданный поток --
              ; игнорируем. Потенциальный FIXME.
              ((#:nothing-to-do) (run-thread))

              ; Флаг уже установлен, и поток запуска команд об этом знает,
              ((#:need-to-run) ; (dump-error "M: Run-thread should repeat command~%")
                               '())

              ; Состояние #:accepted может упасть в #:nothing-to-do. Поэтому его
              ; надо поднимать в #:need-to-run через cas-гонку. Если гонка
              ; выиграна, то рабочий поток должен это заметить. Если нет, то
              ; рабочий поток будет завершаться, и его нужно перезапустить
              ((#:accepted) (let ((v (atomic-box-compare-and-swap! state #:accepted #:need-to-run)))
                              (when (not (eq? #:accepted v))
                                (when (not (eq? #:nothing-to-do v))
                                  (error "protocol violation: expecting #:nothing-to-do, got:" v))
                                (run-thread))))))
          (loop (read-line p)))))))

(watch-loop (collect-options (cdr (command-line))))
