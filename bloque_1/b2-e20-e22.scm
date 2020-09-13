;;Plantilla b2-e20-e22.scm
;;Autor: Manuel Konomi

;;Ejercicio 20
(he-tardado 20 'b2-e20)

;Modificacion de "pasa-concepto-CL" que cuenta el numero de tests pasados y devuelve el numero
(define (pasa-concepto-TC? concepto ejemplo-sin-clase correctos)
  (if (null? concepto)
      correctos
  (if (not (pasa-test? (car concepto) (car ejemplo-sin-clase)))
      (pasa-concepto-TC? (list-tail concepto 1) (list-tail ejemplo-sin-clase 1) correctos)
      (pasa-concepto-TC? (list-tail concepto 1) (list-tail ejemplo-sin-clase 1) (+ 1 correctos)))))

;Utilizando la funcion anterior, esta funcion compara el numero de tests pasados con el primer elemento
; del concepto-TC. Si el total de aprobados es superior o igual al umbral del concepto, es match, de lo contrario nada.
; Adicionalmente se compara la longitud de los test y de los valores del ejemplo, y si todos los tests del concepto son validos.
(define (match-TC concepto-TC ejemplo-sin-clase)
  (if (or (not (= (length (cdr concepto-TC)) (length ejemplo-sin-clase))) (not (number? (car concepto-TC))))
      #f
      (if (not (validar-concepto (cdr concepto-TC) ejemplo-sin-clase))
          #f
          (>= (pasa-concepto-TC? (cdr concepto-TC) ejemplo-sin-clase 0) (car concepto-TC)))))

;;Ejercicio 21
(he-tardado 5 'b2-e21)

;Devuelve el ejemplo sin clase clasificado según el resultado de Match-TC
(define (TCi concepto-TC ejemplo-sin-clase)
  (if (match-TC concepto-TC ejemplo-sin-clase)
      (append ejemplo-sin-clase '(+))
      (append ejemplo-sin-clase '(-))))

;;Ejercicio 22
(he-tardado 40 'b2-e22)

;Genera una especialización del valor de umbral. Esto significa incrementar el valor para que sea mas especifico, o mantener la generalidad igual, bajando el umbral
; en 1, pero haciendo mas especificos los atributos.
(define (especializacion-umbral concepto-TC metadatos ejemplo)
  (let* ((len (length (cdr concepto-TC)))
         (umb (car concepto-TC))
         (newumb (if (< umb len) (+ 1 umb) umb))
         (minusumb (if (<= umb 0) 0 (- umb 1))))
    (if (= newumb len)
        '()
        (append (list (cons newumb (cdr concepto-TC))) (map (lambda (x) (cons minusumb x)) (especializaciones-CL (cdr concepto-TC) metadatos ejemplo))))))

;Esta funcion es como espe-CL-rec, genera las especializciones segun sea el test en concreto nominal o numerico, con un añadido, con indice -1 lo que hace
;es generalizar el umbral, y luego sigue operando como en la versión CL
(define (espe-TC-rec concepto-TC metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (= -1 indice)
          (append (especializacion-umbral concepto-TC metadatos ejemplo) (map (lambda (x) (cons (car concepto-TC) x)) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice))))
          (if (number? (list-ref ejemplo indice))
              (append (especializaciones-atributo-numerico (cdr concepto-TC) indice ejemplo) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice)))
              (append (especializaciones-atributo-nominal (cdr concepto-TC) indice metadatos) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice)))))))

;Llamamos a la funcion recursiva
(define (especializaciones-TC concepto-TC metadatos ejemplo)
  (delete-duplicates (espe-TC-rec concepto-TC metadatos ejemplo -1)))

