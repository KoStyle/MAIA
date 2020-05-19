#lang racket
(define leer-ejemplos
  (lambda (x)
    (call-with-input-file x
     (lambda (i)
       (let* ((a (read i)))
         a)))))
         
       

;(define ejemplos (leer-ejemplos "C:\\Users\\konom\\Desktop\\IA_metodos_aprendizaje\\bloque_1\\ejemplos.scm"))
;(car (cdr ejemplos))