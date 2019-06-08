(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 receive)
             (srfi srfi-42))

; (define gmk-expand (lambda (v) ""))
; (define gmk-eval (lambda (v) '()))

(define dump-error
  (let ((p (current-error-port)))
    (lambda (fmt . args) (apply format p fmt args))))

; Вычисление описания системной ошибки по err -- информации об исключении
(define (error-string fn-name path err)
  (format #f "~a: ~a: ~a"
          fn-name path
          (cond ((eq? 'system-error (car err)) (strerror (system-error-errno err)))
                ((eq? 'internal (car err)) (cadr err)))))

; Составление пути из отдельных компонент
(define join-path
  (let ((s file-name-separator-string))
    (lambda arguments
      (let ((args (filter (compose not string-null?) arguments)))
        (if (string=? s (car args))
          (string-join (cdr args) s 'prefix)
          (string-join args s))))))

; Процедура для разбиения пути на элементы 
(define split-path
  (let ((s file-name-separator-string))
    (lambda (str)
      (let ((path (filter (compose not string-null?)
                          (string-split str file-name-separator?))))
        (if (not (string-prefix? s str))
          path
          (cons s path))))))

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
(define D "")

; Процедура запоминает целевую директорию для сборки, предварительно проверяя её
; доступность. Если проверка не пройдена, то bdir-set! вызывает ошибку в make.

(define (bdir-set! path)
  (define handler (lambda err (gmk-error (error-string 'bdir-set! path err))))

  ; Проверка структуры stat на соответствие типу и режиму доступа 
  (define mode-rwx #o700)  
  (define (stat-match? st type mode) (and (eqv? type (stat:type st))
                                          (eqv? mode (logand mode (stat:mode st)))))

  (catch
    #t
    (lambda () (if (stat-match? (stat path) 'directory mode-rwx)
                 (begin (set! bdir path)
                        (set! bdir-items (split-path path))
                        (set! B (join-path bdir "bin"))
                        (set! L (join-path bdir "lib"))
                        (set! I (join-path bdir "include"))
                        (set! T (join-path bdir "tst"))
                        (set! D (join-path bdir "txt")))
                 (throw 'internal "is not accessible (700) directory")))
    handler))

; Процедура убеждающаяся в доступности директории по указанному пути. Если
; компоненты пути не созданы, она их создаёт. Аналог mkdir -p
(define (ensure-path! path)
  ; Вспомогательные процедуры

  ; Проверка корректности пути. Разрешаем только те, что не ведут обратно вверх,
  ; и не содержат повторений текущей директории.
  (define (path-correct? items)
    (and (not (string=? ".." (car items)))
         (and-map (lambda (i) (not (or (string=? i ".")
                                       (string=? i ".."))))
                  (cdr items))))

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
      (lambda err
        ; (dump-error "handler: items: ~s~%" items)
        (let ((errno (system-error-errno err)))
          (cond ((eqv? ENOENT errno) items)
                (else (apply throw err))))))

    (let loop ((items path))
      ; (dump-error "changing dir to: ~s~%" (car path))
      (if (null? items)
        '()
        (let ((v (catch 'system-error (lambda () (chdir (car items))) (handler items))))
          ; (dump-error "items: ~s~%" v)
          (if (not (unspecified? v))
            v
            (loop (cdr items)))))))

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
  (define handler
    (lambda err
      (format (current-error-port) "~s" (error-string 'ensure-path! path err))
      "false"))

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

; Корень дерева исходных файлов. Определяется по координатам самого первого
; make-файла в списке
(define root (dirname (gmk-expand "$(firstword $(MAKEFILE_LIST))")))

; Определение пути до текущего файла
(define (nodepath) (dirname (gmk-expand "$(lastword $(MAKEFILE_LIST))")))
(define (bitspath) (join-path (gmk-expand "$(bits)") (nodepath)))

; Процедура вывода информации о выполняемом сценарии. Чтобы имитировать
; покомандное выполнение рецептов придётся делать в стиле свободной монадки
; (возвращаем команду, которую будет интерпретировать make) с двойной передачей
; и интерпретацией данных. TODO: придётся так же впоследствии формировать
; команду в зависимости от используемой оболочки.
(define (echo job target) (format #f "echo '~/~a~/~a'" job target))

(define-syntax make-echoes
  (let ((rename (lambda (ns)
                  (map (lambda (n) (string->symbol (string-append "echo-" (syntax->datum n)))) ns))))
    (lambda (x)
      (syntax-case x ()
        ((make-echoes j ...)
         (with-syntax (((fn ...) (datum->syntax x (rename (syntax (j ...))))))
           (syntax (begin (define fn (lambda (target) (echo j target))) ...))))))))

; FIXME: названия могут быть не самыми удачными
(make-echoes "install"
             "dep" "dep/gen" "dep-c++" "c" "c/gen" "c++" "h" "h/gen"
             "link" "lib"
             "tex" "xtex" "tex/cnv" "xtex/cp" "biber"
             "cp" "pix"
             "asm" "elf" "bin" "hex" "o")

; Процедура для проверки определённости всех переменных, перечисленных по именам
; через пробел. 
(define (check-vars group-name variables)
  (let* ((vs (string-split variables char-set:whitespace))
         (undefined-var? (lambda (v) (string=? "undefined" (gmk-expand (format #f "$(origin ~a)" v)))))
         (undefined (filter undefined-var? vs)))
    (if (not (null? undefined))
      (gmk-error "~a: undefined variables: ~a" group-name undefined))))

; Процедура возвращает подстроку без расширения - суффикса, начинающегося с «.»
(define (drop-ext str)
  (let ((dotpos (or (string-rindex str #\.) (string-length str))))
    (substring/read-only str 0 dotpos))) 

; Перевод списка файлов в другой список с добавкой префикса и заменой
; расширения. Если префикс или расширение заданы пустыми строками, то
; соответствующее преобразование не осуществляется

(define (reform prefix paths ext)
  ; Порождение функции редактирования, дабы не считать if для каждого пути
  (define (mk-transform prefix ext)
    (compose (if (string-null? prefix) identity (lambda (str) (join-path prefix str)))
             (if (string-null? ext) identity (lambda (str) (string-append (drop-ext str) "." ext)))))

  ; Make должна автоматически подхватить список и превратить его в строку слов
  (map (mk-transform prefix ext)
       (filter (compose not string-null?) (string-split paths char-set:whitespace))))

; Специальные варианты reform.

; Добавка к путям префикса и замена расширения на .o
(define (f2o prefix paths) (reform prefix paths "o"))

; Замена расширения путей на .d
(define (f2d paths) (reform "" paths "d"))

; Добавка к путям префиксов
(define (f2p prefix paths) (reform prefix paths ""))

; Функция должна прочитать результат cmd (которая должна быть командой запуска
; компилятора с опциями для генерации зависимостей исходного .c или .cpp файла),
; и модифицировать его с учётом makenv. Для этого нужно открыть два порта и
; обработать, следовательно, два исключения. Неудобно. Возможно, я
; перестраховщик
(define (fix-deps cmd target)
  ; Префикс, добавляемый к целям в .d-файле, определяется по пути к цели.
  (define dir (dirname target))

  ; Функция обработки одной строки. Не хочется в ней работать с портами, поэтому
  ; она обрабатывает только строки. Параметр state - то, в каком состоянии
  ; работает процедура: #:start -- начало make-правила, ожидание строки,
  ; разделённой двоеточием на цель и предпосылки; #:copy -- копирование строк,
  ; описывающих предпосылки, связанных вместе символом переноса строки \.
  ; Функция возвращает пару из следующего состояния и преобразованной (если
  ; нужно) строки.
  (define (fix-string state str)
    (define (next-state str)
      (if (eqv? #\\ (string-ref str (- (string-length str) 1))) #:copy #:start))

    (define (reformat str colpos)
      (let* ((objname (join-path dir (substring/read-only str 0 colpos)))
             (justname (drop-ext objname))
             (prereqs (substring/read-only str colpos)))
        (format #f "~a ~a.d~a" objname justname prereqs)))

    (case state
      ((#:start) (let ((colpos (string-index str #\:)))
                   (if (not colpos)
                     (values #:no-target str)
                     (values (next-state str) (reformat str colpos)))))
      ((#:copy) (values (next-state str) str))))

  ; Генерация параметризованного обработчика исключений для красивых сообщений
  ; об ошибках

  (define (handler item on-error)
    (lambda err
      (format (current-error-port) "~s" (error-string 'fix-deps item err))
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
                               ; (format (current-error-port) "~a~%" l)
                               (if (eof-object? l)
                                 "true"
                                  (receive (state str) (fix-string st l)
                                    (if (eq? state #:no-target)
                                      "false"
                                      (begin
                                        ; (format (current-error-port) "~a~%" str)
                                        (format t "~a~%" str)
                                        (lp state (read-line c))))))))
                           (handler (string-append "| " cmd) "false"))))
                  (close-port t)
                  (close-pipe c)
                  r)))))

; Правила makenv устроены так, что обычно структура деревьев исходных файлов
; повторяется целевыми. Иногда требуется иное поведение: копирование
; заголовочных файлов для библиотек в специальную include-директорию, или
; копирование стилей и списка библиотек в место компиляции tex-файлов. Шаблоны
; таких правил генерируют x-route процедуры. Я старался минимизировать число
; необходимых параметров. Результаты этих процедур необходимо пропускать через
; $(eval ...) по месту вызова.

(define (h-route-template headline)
  (lambda ()
    (format #t "~a~%~/~a~%~/~a~%~/~a~%"
            headline
            "@ $(guile (echo-h \"$@\"))"
            "@ $(guile (ensure-path! \"$(@D)\"))"
            "@ install -m 755 '$<' '$@'")))

(define (h-route target)
  (define headline (string-append (join-path I target "%.h") ": "
                                  (join-path (nodepath) "%.h")))

  (with-output-to-string (h-route-template headline)))

(define (h-route-subdir target subdir)
  (define headline (string-append (join-path I target "%.h") ": "
                                  (join-path (nodepath) subdir "%.h")))

  (with-output-to-string (h-route-template headline)))

(define (form-headline source ext default)
  (define (pattern e d) (string-append "%." (if (string-null? e) d e)))

  (let ((pat (pattern ext default)))
    (string-append (join-path (bitspath) pat) ": "
                   (join-path source pat))))

(define (tex-route source ext)
  (let ((reroot (lambda (str) (if (absolute-file-name? str) str (join-path root str)))))
    (with-output-to-string
      (lambda ()
        (format #t "~a~%~/~a~%~/~a~%~/~a~%"
                (form-headline (reroot source) ext "tex")
                "@ $(guile (echo-tex/cnv \"$@\"))"
                "@ $(guile (ensure-path! \"$(@D)\"))"
                "@ iconv -t $(texcode) < '$<' > '$@'")))))

(define (pix-route source ext)
  (with-output-to-string
    (lambda ()
      (format #t "~a~%~/~a~%~/~a~%~/~a~%"
              (form-headline source ext "png")
              "@ $(guile (echo-pix \"$@\"))"
              "@ $(guile (ensure-path! \"$(@D)\"))"
              "@ cp '$<' '$@'"))))

(define (tex-log path) (string-append (drop-ext path) ".log"))

; Процедура подбирает подходящий toolchain-файл по имени. Поиск сначала в
; исходной директории для исходного makefile-а, а затем в базовой директории для
; makenv
(define (tcn-path name)
  (let* ((file-name (string-append name ".mk"))
         (tcn-root (join-path root file-name))
         (tcn-base (join-path base ".." "tcn" file-name)))
    (if (access? tcn-root R_OK)
      tcn-root
      (if (access? tcn-base R_OK)
        tcn-base
        (gmk-error "no toolchain: ~a" name)))))

; Процедура проверяет, нужно ли запускать biber для вёрстки списка литературы.
; Проверка осуществляет по изменениям в: (1) bcf-файле, который создаёт компиляция
; исходного tex-файла; (2) bib-файлах из списка предпосылок -- аргументе
; biberize! Исходный tex-файл -- это первый элемент в этом списке. Изменением
; считается изменение контрольной sha-суммы
(define (biberize! prerequisites)
  (define files (string-split prerequisites char-set:whitespace))
  (define path (car files))
  (define bibs (filter (lambda (f) (string-suffix? ".bib" f)) (cdr files)))

  (define handler (lambda err
                    (format (current-error-port) "~s" (error-string 'biberize! path err))
                    "false"))

  (let* ((bcf (string-append (drop-ext path) ".bcf"))
         (bcf-sum (string-append bcf ".sum")))
    ; Если списка литературы нет, то пропускаем biber, формируя вместо команды
    ; с biber-ом команду true. Если список есть, нужно проверить, новый ли он.
    ; Для этого используется shasum. Если он не изменился, по shasum-критерию,
    ; то тоже пропускаем biber. Надеюсь в этом коде, на то, что with-конструкции
    ; закрывают порты при ошибках
    (if (not (access? bcf R_OK))
     "true"
      (catch
        'system-error
        (lambda ()
          (let ((known-sums (if (not (access? bcf-sum R_OK))
                              ""
                              (with-input-from-file bcf-sum read)))
                (new-sums (with-input-from-port
                            (apply open-pipe* OPEN_READ "shasum" bcf bibs)
                            (lambda ()
                              (let lp ((l (read-line)))
                                (if (eof-object? l)
                                  '()
                                  (cons l (lp (read-line)))))))))
            (if (equal? known-sums new-sums)
              "true"
              (begin
                (with-output-to-file bcf-sum (lambda () (write new-sums)))
                (string-append (gmk-expand "$(biber)") " " (basename bcf))))))
        handler))))

(define (undefine-vars . vars)
  (define combinations
    (let lp ((rest vars)
             (result '()))
      (if (null? rest)
        result
        (lp (cdr rest)
            (let ((words (filter (compose not string-null?)
                                 (string-split (car rest) char-set:whitespace))))
              (if (null? result)
                words
                (list-ec (:list a result) (:list b words) (string-append a "-" b))))))))

  (with-output-to-string
    (lambda ()
      (for-each (lambda (v) (format #t "undefine ~a~%" v)) combinations))))

(define (std-tex-rules target sources routes)
  (let* ((bits (bitspath))
         (t (join-path bits target))
         (s (string-join (map (lambda (p) (join-path bits p)) sources) " ")))
    (with-output-to-string
      (lambda ()
        ; (format #t "target: ~s~%sources: ~s~%bits: ~s~%" target sources bits)
        (format #t "~a: ~a~%" t s)
        (for-each (lambda (p) (format #t "~%~a" (tex-route (car p) (cdr p))))
                  routes)))))
