(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-42))

; (define gmk-expand (lambda (v) ""))
; (define gmk-eval (lambda (v) '()))

(define dump-error (let ((p (current-error-port)))
                     (lambda (fmt . args) (apply format p fmt args))))

; Вычисление описания системной ошибки по err -- информации об исключении
(define (error-string fn-name path err)
  (format #f "~a: ~a: ~a"
          fn-name path
          (cond ((eq? 'system-error (car err)) (strerror (system-error-errno err)))
                ((eq? 'internal (car err)) (cadr err)))))

(define (dump-error-string fn-name path err)
  (dump-error "~s~%" (error-string fn-name path err)))

(define (internal-error fmt . args) (list 'internal (apply format #f fmt args)))

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
(define bits "")

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
                        (set! bits (join-path bdir "bits"))
                        (set! B (join-path bdir "bin"))
                        (set! L (join-path bdir "lib"))
                        (set! I (join-path bdir "include"))
                        (set! T (join-path bdir "tst"))
                        (set! D (join-path bdir "pdf")))
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
        (let ((errno (system-error-errno err)))
          (cond ((eqv? ENOENT errno) items)
                (else (apply throw err))))))

    (let loop ((items path))
      (if (null? items)
        '()
        (let ((v (catch 'system-error (lambda () (chdir (car items))) (handler items))))
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
(define (bitspath) (join-path bits (nodepath)))

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
             "tex" "xtex" "tex/cnv" "xtex/cp" "bib" "bib/cnv"
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

(define reroot (lambda (str) (if (absolute-file-name? str) str (join-path root str))))

(define (any-route source extensions default-extension route-lambda)
  (with-output-to-string
    (lambda ()
      (for-each route-lambda
                (if (null? extensions) (list default-extension) extensions)))))

(define (tex-route source . extensions)
  (any-route source extensions "tex"
             (lambda (ext) (format #t "~a~%~/~a~%~/~a~%~/~a~%"
                                   (form-headline (reroot source) ext "tex")
                                   "@ $(guile (echo-tex/cnv \"$@\"))"
                                   "@ $(guile (ensure-path! \"$(@D)\"))"
                                   "@ iconv -t $(texcode) < '$<' > '$@'"))))

(define (bib-route source . extensions)
  (any-route source extensions "bib"
             (lambda (ext) (format #t "~a~%~/~a~%~/~a~%~/~a~%"
                                   (form-headline (reroot source) ext "tex")
                                   "@ $(guile (echo-bib/cnv \"$@\"))"
                                   "@ $(guile (ensure-path! \"$(@D)\"))"
                                   "@ iconv -t $(texcode) < '$<' > '$@'"))))

(define (png-route source . extensions)
  (any-route source extensions "png"
             (lambda (ext) (format #t "~a~%~/~a~%~/~a~%~/~a~%"
                                   (form-headline (reroot source) ext "png")
                                   "@ $(guile (echo-pix \"$@\"))"
                                   "@ $(guile (ensure-path! \"$(@D)\"))"
                                   "@ cp '$<' '$@'"))))

(define (copy-route source . extensions)
  (any-route source extensions "txt"
             (lambda (ext) (format #t "~a~%~/~a~%~/~a~%~/~a~%"
                                   (form-headline (reroot source) ext "txt")
                                   "@ $(guile (echo-cp \"$@\"))"
                                   "@ $(guile (ensure-path! \"$(@D)\"))"
                                   "@ cp '$<' '$@'"))))

(define (tex-log path) (string-append path ".out"))

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

(define (shasum files)
  (and (list? files)
       (not (null? files))
       (with-input-from-port
         (apply open-pipe* OPEN_READ "shasum" files)
         (lambda ()
           (unfold eof-object? identity (lambda x (read-line)) (read-line))))))

(define (replace-ext path ext) (string-append (drop-ext path) ext))

; Процедура проверяет, нужно ли запускать biber для вёрстки списка литературы.
; Если не нужно, то формирует команду true для make. Если нужно, то команду
; запуска biber. Проверка осуществляет по изменениям в: (1) bcf-файле,
; который создаёт компиляция исходного tex-файла по пути path; (2) bib-файлах
; из списка bibs. Изменением считается изменение контрольных sha-сумм
(define (biberize! path bibs)
  (let* ((bcf (replace-ext path ".bcf"))
         (sha-sums (string-append bcf ".sum")))
    (if (not (access? bcf R_OK))
      ; Если bcf-файл не создан при компиляции, значит, списка литературы нет,
      ; значит, пропускаем запуск biber, возвращая команду true для make
      "true"
      ; В противном случае подсчитываем суммы и сравниваем с прежними
      (let ((known-sums (if (not (access? sha-sums R_OK))
                          ""
                          (with-input-from-file sha-sums read)))
            (new-sums (shasum (cons bcf bibs))))
        (if (equal? known-sums new-sums)
          ; Если суммы совпадают, то пропускаем запуск biber
          "true"
          ; Иначе, обновляем суммы и формируем команду для запуска biber
          (begin
            (with-output-to-file sha-sums (lambda () (write new-sums)))
            (string-append (gmk-expand "$(biber)") " " (basename bcf)))))))) 

(define citations
  (let ((r (make-regexp "^\\\\(citation|bibdata|bibstyle)")))
    (lambda (aux)
      (if (not (access? aux R_OK))
        '()
        (let ((bib-lines (sort (filter (lambda (s) (regexp-exec r s))
                                       (with-input-from-file
                                         aux
                                         (lambda ()
                                           (unfold eof-object?
                                                   identity
                                                   (lambda x (read-line))
                                                   (read-line)))))
                               string<)))
          ; Чтобы bibtex не ломался, в aux-файле должны быть и citation, и
          ; bibdata, и bibstyle
          (if (not (and (< 2 (length bib-lines))
                        (string-prefix? "\\bibdata{" (car bib-lines))
                        (string-prefix? "\\bibstyle{" (cadr bib-lines))))
            '()
            bib-lines))))))

; Процедура проверяет, нужно ли запускать bibtex для вёрстки списка литературы.
; Интерфейс с bibify! предписывает при необходимости запуска bibtex возврат пары
; значений из команды и нового состояния для контроля изменений, в противном
; случае возврат пустых команды и контрольного состояния. Проверка необходимости
; запуска bibtex выполняется по изменениям в: (1) списке цитат из aux-файла,
; сформированного при компиляции исходного tex-файла по пути path; (2)
; bib-файлах списка bibs.  Изменением считается изменение множества ссылок и
; контрольных sha-сумм для bib-файлов.
(define (bibtexify! path bibs known)
  (let* ((aux (replace-ext path ".aux"))
         (refs (citations aux))
         (new (cons refs (shasum bibs))))
    (if (or (null? refs)
            (equal? known new)) 
      (values "" '())
      (let ((cmd (format #f "~a && ~a '~a'"
                         (echo-bib aux)
                         (gmk-expand "$(bibtex)")
                         (basename aux))))
        (values cmd new))))) 

(define bib-state '())

(define (bibify! prerequisites)
  (let* ((engine (gmk-expand "$(bib-engine)"))
         (files (string-split prerequisites char-set:whitespace))
         (path (car files))
         (bibs (filter (lambda (f) (string-suffix? ".bib" f)) (cdr files)))
         (state (replace-ext path ".bib-state")))
    ; (dump-error "bibify!: ~s ~s~%" bibs engine)
    (catch
      'system-error
      (lambda ()
        (unless (null? bib-state)
          (throw 'internal
                 (string-append "bib state is not empty: " bib-state)))
        (let ((bib! (cond ((string=? "biber" engine) biberize!)
                          ((string=? "bibtex" engine) bibtexify!)
                          (else (throw 'internal
                                       (string-append "unknown engine: " engine)))))
              (known (if (not (access? state R_OK)) '() (with-input-from-file state read))))
          (let-values (((cmd new) (bib! path bibs known)))
            ; (dump-error "CMD: ~s~%NEW: ~s~%" cmd new)
            (if (null? new)
              "true"
              (begin (set! bib-state (cons state new))
                     cmd)))))
      (lambda err (dump-error-string 'bibify! path err) "false"))))

(define (bibify-end!)
  (if (not (pair? bib-state))
    "true"
    (let ((path (car bib-state))
          (state (cdr bib-state)))
      (set! bib-state '())
      (format #f "echo '~s' > '~a'" state path))))

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

; FIXME: Примитивчик. 
(define (var->list var)
  ; Некий небезопасный эксперимент. Если cond не доходит до конца, будет
  ; undefined. Интересно, как это всё себя ведёт 

  (define (stringify v) (cond ((symbol? v) (symbol->string v))
                              ((string? v) v)))

  (with-input-from-string (gmk-expand (format #f "$(~a)" var))
                          (lambda ()
                            (let loop ((s (read))
                                       (R '()))
                              (if (eof-object? s)
                                  (reverse R)
                                  (let ((v (stringify s)))
                                    (if (not (string? v))
                                        '()
                                        (loop (read) (cons v R)))))))))

; FIXME: Проблема с именами, содержащими пробелы
(define (std-tex-rules var)
  (let ((bits (bitspath))
        (sources (var->list var)))
    (if (null? sources)
        (format #f "$(error Can not itemize content: $(~a))" var)
        (let* ((leader (car sources))
               (name (drop-ext leader)))
          (if (or (not (string-suffix? ".tex" leader))
                  (string-null? name))
              (format #f "$(error The first filename is not valid .tex: ~a)" leader)
              (let* ((t (join-path bits (string-append name ".pdf")))
                     (s (map (lambda (p) (join-path bits p)) sources)))
                (format #f "~a: ~a~%" t (string-join s " "))))))))

(define (std-xtex-rules var)
  (let ((bits (bitspath))
        (sources (var->list var)))
    (if (null? sources)
        (format #f "$(error Can not itemize content: $(~a))" var)
        (let* ((leader (car sources))
               (name (drop-ext leader)))
          (if (or (not (string-suffix? ".xtex" leader))
                  (string-null? name))
              (format #f "$(error The first filename is not valid .xtex: ~a)" leader)
              (let* ((t (join-path bits (string-append name ".pdf")))
                     (s (map (lambda (p) (join-path bits p)) sources)))
                (format #f "~a: ~a~%" t (string-join s " "))))))))
