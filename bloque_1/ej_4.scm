(define (factorial numero)
  (if (>= numero 0)
      (if (= numero 0)
          1 
          (* numero (factorial (- numero 1))))))

(factorial 0)