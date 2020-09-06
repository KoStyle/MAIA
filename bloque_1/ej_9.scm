#lang racket
(provide (all-defined-out))

(define (anadir-ejemplo ejemplos ejemplo)
    (if (or (null? ejemplos) (not (list? ejemplo)))
        '()
        (if (null? (list-tail ejemplos 1))
            (cons (list-ref ejemplos 0) (cons ejemplo '()))
            (cons (list-ref ejemplos 0) (anadir-ejemplo (list-tail ejemplos 1) ejemplo)))))





;(define ejemplos (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos.scm"))

;(set! ejemplos (anadir-ejemplo ejemplos '(cacoso 1 3 no alto si -)))
;ejemplos
;ejemplos