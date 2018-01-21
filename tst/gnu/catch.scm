(catch
  'ERR
  (lambda () (catch 'ERR
                    (lambda () (throw 'ERR "hello"))
                    (lambda err
                      (display "1 ")
                      (display (cdr err))
                      (newline)
                      (apply throw err))))

  (lambda (ERR . x) (display x) (newline)))
