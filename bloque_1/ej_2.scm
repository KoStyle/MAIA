(define (siguiente lista)
  (if (list? lista)
      (if (not (null? (list-tail lista 1)))
          (cons (+ (list-ref lista 0) 1) (siguiente (list-tail lista 1)))
          (cons (+ (list-ref lista 0) 1) '()))))

(siguiente '(1 3 5))
        