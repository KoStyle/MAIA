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

(define convertir-a-cons
  (lambda (x)
    (if (null? (list-tail x 1))
        (cons (cons (list-ref x 0) 1) '())
        (cons (cons (list-ref x 0) 1) (convertir-a-cons (list-tail x 1))))))

(define obtener-al-azar
  (lambda (x)
    (when (not (pair? (list-ref x 0)))
      (set! x (convertir-a-cons x)))
    (encuentra_posicion x (* (random) (prob_total x)) 0)))

;(obtener-al-azar '((a . 1) (b . 2) (c . 3)))
;(obtener-al-azar '(a b c))

(define elementos '())
(do ((x 6000 (- x 1)))
  ((= x 0) elementos)
  (set! elementos
        (cons (obtener-al-azar '(a b c)) elementos)))

(map (lambda (x)
       (cons x (count (lambda(y) (eq? x y)) elementos)))
     '(a b c))