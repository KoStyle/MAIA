
(define (leer-ejemplos archivo)
  (let ((x archivo))
    (call-with-input-file x
     (lambda (i)
       (let* ((a (read i)))
         a)))))
         
       

(define ejemplos (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos.scm"))
(car (cdr ejemplos))

ejemplos