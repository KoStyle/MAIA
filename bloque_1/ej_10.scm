#lang racket
(require "ej_9_inicio.scm")

(define ejemplos (leer-ejemplos "C:\\Users\\konom\\Desktop\\IA_metodos_aprendizaje\\bloque_1\\ejemplos.scm"))

(define busca-atributo
  (lambda (atrib listatrib posicion)
    (if (or (not (number? posicion)) (not (symbol? atrib)) (not (list? listatrib)))
        '()
        (if (symbol=? (car (list-ref listatrib posicion)) atrib)
            posicion
            (busca-atributo atrib listatrib (+ posicion 1))))))

(define recupera-atributo-ejemplos
  (lambda (indice ejemplos)
    (if (or (not (number? indice)) (not (list? ejemplos)))
        '()
        (if (null? (list-tail ejemplos 1))
            (cons (list-ref (car ejemplos) indice) '())
            (cons (list-ref (car ejemplos) indice) (recupera-atributo-ejemplos indice (list-tail ejemplos 1)))))))

(define atributo
  (lambda (atrib ejemplos)
    (if (or (not (symbol? atrib)) (not (list? ejemplos)))
        '()
        (recupera-atributo-ejemplos (busca-atributo atrib (car ejemplos) 0) (car (cdr ejemplos))))))
      

;(busca-atributo 'humedad (car ejemplos) 0)
;(car ejemplos)
(car(cdr ejemplos))
;ejemplos
(atributo 'eclipse_solar ejemplos)