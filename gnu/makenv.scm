(use-modules (ice-9 format))

; Процедура убеждающаяся в доступности директории по указанному пути. Если
; компоненты пути не созданы, она их создаёт. Аналог mkdir -p 

(define (ensure-path! path)
  ; Вспомогательные процедуры

  ; Процедура для разбиения пути на элементы
  (define (split-path path)
    (let ((items (filter (compose not string-null?)
                         (string-split path file-name-separator?))))
      (if (absolute-file-name? path)
        (cons file-name-separator-string items)
        items)))

  ; Проверка корректности пути. Разрешаем только те, что не ведут обратно вверх,
  ; и не содержат повторений текущей директории.
  (define (path-correct? items)
    (and (not (equal? ".." (car items)))
         (and-map (lambda (i) (not (or (equal? i ".") (equal? i "..")))) (cdr items))))

  ; Процедура меняющая текущую директорию вдоль path. Возвращает либо остаток
  ; пути, либо #f, как индикация ошибки: невозможность пройти вдоль пути.
  (define (into-dirs! path)
    ; Обработчик ошибок от chdir. Логика такая: если соответствующей записи не
    ; существует, то всё хорошо, нужно создать цепочку оставшихся директорий,
    ; передающуюся в path; в остальных случаях непреодолимая ошибка.
    (define (handler items)
      (lambda error
        (let ((errno (system-error-errno error)))
          (cond ((eqv? ENOENT errno) items)
                (else (apply throw error))))))

    (let loop ((items path))
      (if (null? items)
        '()
        (let ((v (catch 'system-error (lambda () (chdir (car items))) (handler items))))
          (if (not (unspecified? v)) v (loop (cdr items)))))))

  ; Процедура для создания остатка пути. Обработка ошибок вынесена в
  ; нижеследующий catch
  (define (make-dirs! path)
    (let loop ((items path))
      (if (not (null? items))
        (let ((i (car items)))
          (mkdir i #o700)
          (chdir i)
          (loop (cdr items))))))

  ; Универсальный обработчик исключения на два случая жизни: на случай системных
  ; ошибок, и на случай внутренних для ensure-path! ошибок. Для того, чтобы
  ; сломать вызывающий make в нужном месте, возвращается строчка "false",
  ; интерпретация которой приведёт к прерыванию исполнения цепочки команд
  ; рецепта

  (define (handler key . args)
    (case (symbol->keyword key)
      ((#:system-error)
       (let ((errstr (strerror (system-error-errno (cons key args)))))
         (format (current-error-port) "ensure-path!: ~a: ~a~%" errstr path)))

      ((#:internal)
       (format (current-error-port) "ensure-path!: ~a: ~a~%" (car args) path)))

    "false")

  ; Основная работа. Нужно гарантировать вызов (chdir cwd) по завершении работы,
  ; вне зависимости от возникших ошибок. Поэтому код обёрнут в два catch.
  ; Внешний ловит ошибки getcwd.

  (catch
    'system-error
    (lambda ()
      (let* ((cwd (getcwd))
             (items (split-path path))
             (r (catch
                  #t
                  (lambda () (if (path-correct? items)
                               (begin (make-dirs! (into-dirs! items))
                                      "true")
                               (throw 'internal "incorrect path")))
                  handler)))
        (chdir cwd)
        r))
    handler))

; Запоминание некоторых координат в файловой системе для последующего
; использования

; Базовая директория makenv. Определение по пути до текущего файла.
(define base (dirname (current-filename)))

; Корень дерева исходных файлов. Определяется по координатам самого первого
; make-файла в списке
(define root (dirname (gmk-expand "$(firstword $(MAKEFILE_LIST))")))

; Процедура вывода информации о выполняемом сценарии. Чтобы имитировать
; покомандное выполнение рецептов придётся делать в стиле свободной монадки с
; двойной передачей и интерпретацией данных. TODO: придётся так же впоследствии
; формировать команду в зависимости от используемой оболочки.

(define (echo job target) (format #f "echo '\t~a\t~a'" job target))
