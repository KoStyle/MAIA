#lang racket
(define prob_total
  (lambda (x)
    (if (null? (list-tail x 1))
        (cdr (list-ref x 0))
        (+ (cdr (list-ref x 0)) (prob_total (list-tail x 1))))))


(define encuentra_posicion
  (lambda (lista threshold acumulado)
    (set! acumulado (+ acumulado (cdr (list-ref lista 0))))
    (if (> acumulado threshold)
        (car (list-ref lista 0))
        (encuentra_posicion (list-tail lista 1) threshold acumulado))))

(define obtener-al-azar0
  (lambda (x)
    (encuentra_posicion x (* (random) (prob_total x)) 0)))

(define elementos '())
(do ((x 6000 (- x 1)))
  ((= x 0) elementos)
  (set! elementos
        (cons (obtener-al-azar0 '((a . 1) (b . 2) (c . 3))) elementos)))

(map (lambda (x)
       (cons x (count (lambda(y) (eq? x y)) elementos)))
     '(a b c))

;(obtener-al-azar0 '((a . 1) (b . 2) (c . 3)))
