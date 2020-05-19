#lang racket
(provide (all-defined-out))

(define anadir-ejemplo
  (lambda (lista ejemplo)
    (if (or (null? lista) (not (list? ejemplo)))
        '()
        (if (null? (list-tail lista 1))
            (cons (list-ref lista 0) (cons ejemplo '()))
            (cons (list-ref lista 0) (anadir-ejemplo (list-tail lista 1) ejemplo))))))



(define leer-ejemplos
  (lambda (x)
    (call-with-input-file x
     (lambda (i)
       (let* ((a (read i)))
         a)))))

;(define ejemplos (leer-ejemplos "C:\\Users\\konom\\Desktop\\IA_metodos_aprendizaje\\bloque_1\\ejemplos.scm"))

;(anadir-ejemplo ejemplos '(cacoso 1 3 45 2))
;ejemplos
;(car (cdr ejemplos))