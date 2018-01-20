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

  ; Проверка наличия и доступности директории. Проверка на существование
  ; директории и доступность её для чтения и перечисления файлов (режим 6).

  (define (check-dir dir)
    (let ((st (catch 'system-error (lambda () (stat dir)) (lambda error #f))))
      (cond ((not (array? st)) #:no-stats)

            ((and (eq? 'directory (stat:type st))
                  (logand #o500 (stat:perms st))) #:dir-ok)
            
            (else #:dir-bad))))

  ; Генерация следующего пути для проверки на существование директории или
  ; для создания новой

  (define (next-path path items)
    (if (null? items) path (let ((name (car items)))
                             (if (eq? file-name-separator-string name)
                               file-name-separator-string
                               (string-append path file-name-separator-string name))))) 

  ; Процедура прохода по существующим директориям начала пути. Каждый элемент
  ; пути проверяем при помощи check-dir. Дальше можем идти, если check-dir
  ; вернёт #:dir-ok. В случае #:dir-bad не сможем создать путь. В случае
  ; #:no-stats считаем, что соответствующего имени нет, и дальше надо создавать
  ; директории

  (define (into-dirs path-items)
    (let loop ((path "")
               (items path-items))
      (if (null? items)
        ; Если в списке нет элементов, то прошлись по списку целиком
        (cons path '())

        ; В противном случае углубляем проверки на один уровень.
        (let ((n-path (next-path path items)))
          (case (check-dir n-path)
            ((#:dir-ok) (loop n-path (cdr items)))
            ((#:no-stats) (cons path items))
            (else #f))))))
  
  (into-dirs (split-path path)))

(display (ensure-path! "/tmp/a/b/c"))
(newline)
