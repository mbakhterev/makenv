(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

; Вычисление описания системной ошибки по информации об исключении
(define (error-string fn-name path key args)
  (case (symbol->keyword key)
    ((#:system-error)
     (let ((errstr (strerror (system-error-errno (cons 'system-error args)))))
       (format #f "~a: ~a: ~a" fn-name path errstr)))
    ((#:internal)
     (format #f "~a: ~a: ~a" fn-name path (car args)))))

; Процедура для разбиения пути на элементы
(define (split-path path)
  (let ((items (filter (compose not string-null?)
                       (string-split path file-name-separator?))))
    (if (absolute-file-name? path)
      (cons file-name-separator-string items)
      items))) 

; Остановка make по ошибке. Возврат "$(error ...)" не срабатывает -- make
; интерпретирует возвращаемые строки как правила.

(define (gmk-error fmt . vals)
  (gmk-eval (string-append "$(error " (apply format #f fmt vals) ")")))

; Переменные для хранения директории, в которой всё собирается.
(define bdir "")
(define bdir-items '())

; Плюс набор переменных для часто используемых целевых директорий

(define B "")
(define I "")
(define T "")
(define L "")

; Процедура запоминает целевую директорию для сборки, предварительно проверяя её
; доступность. Если проверка не пройдена, то bdir-set! вызывает ошибку в make.

(define (bdir-set! path)
  (define (handler key . args)
    (gmk-error (error-string 'bdir-set! path key args)))

  (catch
    #t
    (lambda () (let ((st (stat path)))
                 (if (and (eqv? 'directory (stat:type st))
                          (eqv? #o700 (logand #o700 (stat:mode st))))
                   (begin (set! bdir path)
                          (set! bdir-items (split-path path))
                          (let ((/ file-name-separator-string))
                            (set! B (string-append bdir / "bin"))
                            (set! L (string-append bdir / "lib"))
                            (set! I (string-append bdir / "include"))
                            (set! T (string-append bdir / "tst"))))
                   (throw 'internal "is not accessible (700) directory"))))
    handler))

; Процедура убеждающаяся в доступности директории по указанному пути. Если
; компоненты пути не созданы, она их создаёт. Аналог mkdir -p 

(define (ensure-path! path)
  ; Вспомогательные процедуры

  ; Проверка корректности пути. Разрешаем только те, что не ведут обратно вверх,
  ; и не содержат повторений текущей директории.

  (define (path-correct? items)
    (and (not (equal? ".." (car items)))
         (and-map (lambda (i) (not (or (equal? i ".") (equal? i "..")))) (cdr items))))

  ; Перемотка элементов пути вдоль bdir. Каждый корректный путь должен
  ; начинаться с префикса, равного bdir: мы не хотим портить дерево директорий
  ; пользователю и писать в места, о которых она не догадывается. Если префиксы
  ; не совпадают, процедура возвращает #f, если совпадают, то возвращается
  ; остаток пути.

  (define (rewind-path path)
    (let loop ((p path) (b bdir-items))
      (cond ((null? b) p)
            ((or (null? p) (not (string=? (car p) (car b)))) #f)
            (else (loop (cdr p) (cdr b))))))

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
  ; ошибок, и на случай внутренних ошибок, отмечаемых символом 'internal. Для
  ; того, чтобы сломать вызывающий make в нужном месте, возвращается строчка
  ; "false", интерпретация которой приведёт к прерыванию исполнения цепочки
  ; команд рецепта

  (define (handler key . args)
    (format (current-error-port) "~s" (error-string 'ensure-path! path key args))
    "false")

  ; Основная работа. Нужно гарантировать вызов (chdir cwd) по завершении работы,
  ; вне зависимости от возникших ошибок. Поэтому код обёрнут в два catch.
  ; Внешний ловит ошибку getcwd.

  (catch
    'system-error
    (lambda ()
      (let* ((cwd (getcwd))
             (items (rewind-path (split-path path)))
             (r (catch
                  #t
                  (lambda () (if (and (list? items)
                                      (path-correct? items))
                               (begin 
                                 (chdir bdir)
                                 (make-dirs! (into-dirs! items))
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

; Положение вспомогательного скрипта

(define runscm-path (string-append base file-name-separator-string "run.scm"))

; Корень дерева исходных файлов. Определяется по координатам самого первого
; make-файла в списке
(define root (dirname (gmk-expand "$(firstword $(MAKEFILE_LIST))")))

; Определение пути до текущего файла
(define (nodepath) (dirname (gmk-expand "$(lastword $(MAKEFILE_LIST))")))
(define (bitspath) (string-append (gmk-expand "$(bits)") file-name-separator-string (nodepath)))

; Процедура вывода информации о выполняемом сценарии. Чтобы имитировать
; покомандное выполнение рецептов придётся делать в стиле свободной монадки
; (возвращаем команду, которую будет интерпретировать make) с двойной передачей
; и интерпретацией данных. TODO: придётся так же впоследствии формировать
; команду в зависимости от используемой оболочки.

(define (echo job target) (format #f "echo '\t~a\t~a'" job target))

(define-syntax make-echoes
  (let ((rename (lambda (ns) (map (lambda (n) (string->symbol (string-append "echo-" (syntax->datum n))))
                                  ns))))
    (lambda (x)
      (syntax-case x ()
        ((make-echoes j ...)
         (with-syntax (((fn ...) (datum->syntax x (rename (syntax (j ...))))))
           (syntax (begin (define fn (lambda (target) (echo j target))) ...))))))))

; FIXME: названия могут быть не самыми удачными
(make-echoes "install"
             "dep" "dep/gen" "dep-c++" "c" "c/gen" "c++" "h" "h/gen"
             "link" "lib"
             "tex" "xtex" "tex/cnv" "xtex/cp" "sty/cnv")

; Процедура для проверки определённости всех переменных, перечисленных по именам
; через пробел. 

(define (check-vars group-name variables)
  (let* ((vs (string-split variables char-set:whitespace))
         (undefined-var? (lambda (v) (string=? "undefined" (gmk-expand (format #f "$(origin ~a)" v)))))
         (undefined (filter undefined-var? vs)))
    (if (not (null? undefined))
      (gmk-error "~a: undefined variables: ~a" group-name undefined))))

; (for-each (lambda (v) (display v) (display (gmk-expand (format #f "$(origin ~a)" v))) (newline)) vs) 

; Процедура возвращает подстроку без расширения - суффикса, начинающегося с «.»
(define (drop-ext str)
  (let ((dotpos (or (string-rindex str #\.) (string-length str))))
    (substring/read-only str 0 dotpos))) 

; Перевод списка файлов в другой список с добавкой префикса и заменой
; расширения. Если префикс или расширение заданы пустыми строками, то
; соответствующее преобразование не осуществляется

(define (reform paths prefix ext)
  ; Порождение функции редактирования, дабы не считать if для каждого пути
  (define (mk-transform prefix ext)
    (compose (if (string-null? prefix) identity (lambda (str) (string-append prefix
                                                                             file-name-separator-string
                                                                             str)))
             (if (string-null? ext) identity (lambda (str) (string-append (drop-ext str) "." ext)))))

  ; Make должна автоматически подхватить список и превратить его в строку слов
  (map (mk-transform prefix ext)
       (filter (compose not string-null?) (string-split paths char-set:whitespace))))

; Специальные варианты reform. Для уменьшения потока данных между guile и make.
(define (f2o prefix paths) (reform paths prefix "o"))
(define (f2d paths) (reform paths "" "d"))

; Функция должна прочитать результат cmd (которая должна быть командой запуска
; компилятора с опциями для генерации зависимостей исходного .c или .cpp файла),
; и модифицировать его с учётом makenv. Для этого нужно открыть два порта и
; обработать, следовательно, два исключения. Неудобно. Возможно, я
; перестраховщик

(define (fix-deps cmd target)
  ; Префикс, который надо добавить к целям в .d-файле, определяется по пути к
  ; цели.
  (define dir (dirname target))

  ; Функция обработки одной строки. Не хочется в ней работать с портами, поэтому
  ; она обрабатывает только строки. Параметр state - то, в каком состоянии
  ; работает процедура: #:start -- начало make-правила, ожидание строки,
  ; разделённой двоеточием на цель и предпосылки; #:copy -- копирование строк,
  ; описывающих предпосылки, связанных вместе символом переноса строки \.
  ; Функция возвращает пару из следующего состояния и преобразованной (если
  ; нужно) строки.

  (define (fix-string state str)
    (define (next-state str) (if (eqv? #\\ (string-ref str (- (string-length str) 1))) #:copy #:start))

    (define (reformat str colpos)
      (let* ((objname (substring/read-only str 0 colpos))
             (justname (drop-ext objname))
             (prereqs (substring/read-only str colpos)))
        (format #f "~a/~a ~a/~a.d~a" dir objname dir justname prereqs)))

    (case state
      ((#:start) (let ((colpos (string-index str #\:)))
                   (if (not colpos)
                     (vector #:no-target str)
                     (vector (next-state str) (reformat str colpos)))))
      ((#:copy) (vector (next-state str) str))))

  ; Генерация параметризованного обработчика исключений для красивых сообщений
  ; об ошибках

  (define (handler item on-error)
    (lambda (key . args)
      (format (current-error-port) "~s" (error-string 'ensure-path! item key args))
      on-error))

  (define (intercepting-apply fn arg) (catch 'system-error (lambda () (fn arg)) (handler arg #f)))

  (let* ((t (intercepting-apply open-output-file target))
         (c (intercepting-apply open-input-pipe cmd)))
    (cond ((not (port? t)) "false")
          ((not (port? c)) (close-port t) "false")
          (else (let ((r (catch
                           'system-error
                           (lambda ()
                             (let lp ((st #:start)
                                      (l (read-line c)))
                               (if (eof-object? l)
                                 "true"
                                  (match (fix-string st l)
                                    (#(state str)
                                     (if (eqv? state #:no-target)
                                       "false"
                                       (begin
                                         (format t "~a~%" str)
                                         (lp state (read-line c)))))))))
                           (handler (string-append "| " cmd) "false"))))
                  (close-port t)
                  (close-port c)
                  r)))))

; Правила makenv устроены так, что обычно структура деревьев исходных файлов
; повторяется целевыми. Иногда требуется иное поведение: копирование
; заголовочных файлов для библиотек в специальную include-директорию, или
; копирование стилей и списка библиотек в место компиляции tex-файлов. Шаблоны
; таких правил генерируют x-route процедуры. Я старался минимизировать число
; необходимых параметров. Результаты этих процедур необходимо пропускать через
; $(eval ...) по месту вызова.

(define (h-route target)
  (define headline
    (let ((\ file-name-separator-string))
      (string-append I \ target \ "%.h: " (nodepath) \ "%.h")))

  (with-output-to-string
    (lambda ()
      (format #t "~a~%~/~a~%~/~a~%~/~a~%"
              headline
              "@ $(guile (echo-h \"$@\"))"
              "@ $(guile (ensure-path! \"$(@D)\"))"
              "@ install -m 755 '$<' '$@'"))))

; (format #t "~a~%~/~a~%~/~a~%~/~a~%"
;               (headline target source)
;               "@ $(guile (echo-h \"$@\"))"
;               "@ $(guile (ensure-path! \"$(@D)\"))"
;               "@ install -m 755 '$<' '$@'")
; 
; (define (bib-route source)
;   (define (headline source)
;     (let ((\ file-name-separator-string))
;       (string-append (bitspath) \ "%.bib: " source \ "%.bib")))
; 
;   (with-output-to-string
;     (lambda ()
;       (format #t "~a~%~/~a~%~/~a~%~/~a~%"
;               (headline source)
;               "@ $(guile (echo-bib/cnv \"$@\"))"
;               "@ $(guile (ensure-path! \"$(@D)\"))"
;               "@ iconv -t $(texcode) < '$<' > '$@'"))))
 
(define (tex-route source ext)
  (define headline
    (let ((\ file-name-separator-string))
      (string-append (bitspath) \ "%." ext ": "  source \ "%." ext)))

  (with-output-to-string
    (lambda ()
      (format #t "~a~%~/~a~%~/~a~%~/~a~%"
              headline
              "@ $(guile (echo-sty/cnv \"$@\"))"
              "@ $(guile (ensure-path! \"$(@D)\"))"
              "@ iconv -t $(texcode) < '$<' > '$@'"))))
