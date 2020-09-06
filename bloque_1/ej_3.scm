;They need to be lists and have the same length
(define (sumas lista1 lista2)
  (let ((x lista1) (y lista2))
    (if (and (list? x) (list? y) (= (length x) (length y)))
        (if (not (null? (list-tail x 1)))
            (cons (+ (list-ref x 0) (list-ref y 0)) (sumas (list-tail x 1) (list-tail y 1)))
            (cons (+ (list-ref x 0) (list-ref y 0)) '())))))

(sumas '(1 3 5) '(2 4 6 5))
            