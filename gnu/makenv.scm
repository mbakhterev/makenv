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

;   ; Проверка наличия и доступности директории. Проверка на существование
;   ; директории и доступность её для чтения и перечисления файлов (режим 5).
; 
;   (define (check-dir dir)
;     (let ((st (catch 'system-error (lambda () (stat dir)) (lambda error #f))))
;       (cond ((not (array? st)) #:no-stats)
; 
;             ((and (eq? 'directory (stat:type st))
;                   (logand #o500 (stat:perms st))) #:dir-ok)
;             
;             (else #:dir-bad))))
; 
;   ; Генерация следующего пути для проверки на существование директории или
;   ; для создания новой
; 
;   (define (next-path path items)
;     (if (null? items) path (let ((name (car items)))
;                              (if (eq? file-name-separator-string name)
;                                file-name-separator-string
;                                (string-append path file-name-separator-string name))))) 
; 
;   ; Процедура прохода по существующим директориям начала пути. Каждый элемент
;   ; пути проверяем при помощи check-dir. Дальше можем идти, если check-dir
;   ; вернёт #:dir-ok. В случае #:dir-bad не сможем создать путь. В случае
;   ; #:no-stats считаем, что соответствующего имени нет, и дальше надо создавать
;   ; директории
; 
;   (define (into-dirs path-items)
;     (display path-items)
;     (newline)
;     (let loop ((path "")
;                (items path-items))
;       (if (null? items)
;         ; Если в списке нет элементов, то прошлись по списку целиком
;         (cons path '())
; 
;         ; В противном случае углубляем проверки на один уровень.
;         (let ((n-path (next-path path items)))
;           (case (check-dir n-path)
;             ((#:dir-ok) (loop n-path (cdr items)))
;             ((#:no-stats) (cons path items))
;             (else #f))))))

  ; Процедура меняющая текущую директорию вдоль path. Возвращает либо остаток
  ; пути, либо #f, как индикация ошибки: невозможность пройти вдоль пути.
  (define (into-dirs! path)
    ; Обработчик ошибок от chdir. Логика такая: если соответствующей записи не
    ; существует, то всё хорошо, нужно создать цепочку оставшихся директорий,
    ; передающуюся в path; в остальных случаях непреодолимая ошибка.
    (define (handler path)
      (lambda error
        (let ((errno (system-error-errno error)))
          (cond
            ((eqv? ENOENT errno) path)
            (else 
              (format (current-error-port) "into-dirs!: ~a~%" (strerror errno))
              #f)))))

    (let loop ((items path))
      (if (null? items)
        '()
        (let ((v (catch 'system-error (lambda () (chdir (car items))) (handler items))))
          (if (unspecified? v) (loop (cdr items)) v)))))

  ; Процедура для создания остатка пути
;   (define (make-dirs! path)
;     (define handler
;       (lambda error
;         (format (current-error-port)
;                 "make-dirs!: ~a~%" (strerror (system-error-errno error)))
;         #f))
;     
;     (let loop ((items path))
;       (if (null? items)
;         #t
;         (let ((v (catch 'system-error (lambda () (mkdir (car items) #700)) handler)))
;           (if (unspecified? v) (loop (cdr items)) v)))))

  (into-dirs! (split-path path)))


