#lang racket
(define leer-ejemplos
  (lambda (x)
    (call-with-input-file x
     (lambda (i)
       (let* ((a (read i)))
         a)))))

(define ejemplos (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos.scm"))

(define busca-atributo
  (lambda (atrib listatrib posicion)
    (if (or (not (number? posicion)) (not (symbol? atrib)) (not (list? listatrib)))
        '()
        (if (eqv? (car (list-ref listatrib posicion)) atrib)
            posicion
            (busca-atributo atrib listatrib (+ posicion 1))))))

(define recupera-atributo-ejemplos
  (lambda (indice ejemplos)
    (if (or (not (number? indice)) (not (list? ejemplos)))
        '()
        (if (null? (list-tail ejemplos 1))
            (cons (list-ref (car ejemplos) indice) '())
            (cons (list-ref (car ejemplos) indice) (recupera-atributo-ejemplos indice (list-tail ejemplos 1)))))))

(define (atributo nombre-atributo ejemplos)
  (let ((atrib nombre-atributo))
    (if (or (not (symbol? atrib)) (not (list? ejemplos)))
        '()
        (recupera-atributo-ejemplos (busca-atributo atrib (car ejemplos) 0) (cdr ejemplos)))))
      

(busca-atributo 'humedad (car ejemplos) 0)
(car ejemplos)
(car(cdr ejemplos))
;ejemplos
(atributo 'clase ejemplos)

(map (lambda (x)
       (cons x (count (lambda(y) (eq? x y)) (atributo 'clase ejemplos))))
     '(+ -))