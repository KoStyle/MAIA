;;Plantilla b1-e20-e21.scm
;;Autor: Manuel Konomi Pilkati

;;Ejercicio 20
(he-tardado 15 'b1-e20)
;; Ya que se asume que la clase esta siempre al final de un ejemplo, comparamos el ejemplo con las dos clases que estamos dando, y devolvemos el concepto inicial
;incrementando o una clase o la otra
(define (IIA1 atributos concepto-inicial ejemplo)
 (if (eqv? (car (list-ref concepto-inicial 0)) (list-ref ejemplo (- (length ejemplo) 1))) ;equals the first class
      (cons (cons (car (list-ref concepto-inicial 0)) (+ (cdr (list-ref concepto-inicial 0)) 1)) (list-tail concepto-inicial 1) )
      (cons (list-ref concepto-inicial 0) (cons (cons (car (list-ref concepto-inicial 1)) (+ (cdr (list-ref concepto-inicial 1)) 1)) '()))))

;;Ejercicio 21
(he-tardado 20 'b1-e21)
;;la funcion IA1 extrae los atributos y crea un concepto inicial a cero, la funcion recursiva actualiza el concepto inicial utilizando IIA1
; y si no quedan mas ejempls devuelve el concepto calculado, si no, se llama a si misma con el siguiente ejemplo.

(define (IA1-rec atributos concepto-inicial ejemplos)
  (let ((concepto (IIA1 atributos concepto-inicial (list-ref ejemplos 0))))
    (if (null? (list-tail ejemplos 1))
        concepto
        (IA1-rec atributos concepto (list-tail ejemplos 1)))))

(define (IA1 ejemplos)
 (let ((atributos (car ejemplos))
        (concepto-inicial '((+ . 0) (- . 0))))
    (IA1-rec atributos concepto-inicial (cdr ejemplos))))
