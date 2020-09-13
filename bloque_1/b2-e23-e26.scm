;;Plantilla b2-e23-e26.scm
;;Autor: Manuel Konomi

;;Ejercicio 23
(he-tardado 5 'b2-e23)
;;Recibe metaatributo y valor, si es tipo numerico devuelve el valor, y si es nominal, devuelve el indice del valor al que corresponde en la lista del metaatributo.
(define (traducir meta-atributo valor)
  (if (symbol? (list-ref meta-atributo 1))
      valor
      (index-of (list-ref meta-atributo 1) valor)))

;;Ejercicio 24
(he-tardado 15 'b2-e24)

;Funcion recursiva que va generando el concepto de forma aleatoria entre -init e init
(define (nuevo-conceptoUU-rec metadatos init)
  (if (null? metadatos)
      '()
      (cons (* init (- (* 2 (random)) 1)) (nuevo-conceptoUU-rec (cdr metadatos) init))))

;llamada a la funcion recursiva
(define (nuevo-conceptoUU metadatos init)
  (list metadatos (nuevo-conceptoUU-rec metadatos init)))

;;Ejercicio 25
(he-tardado 35 'b2-e25)

;Esta funcion traduce un ejemplo sin clase por completo, valor a valor, usando la funcion del ejercicio 23
(define (traducir-ejemplo-sin-clase metadatos ejemplo-sin-clase)
  (if (null? ejemplo-sin-clase)
      '()
      (cons (traducir (car metadatos) (car ejemplo-sin-clase)) (traducir-ejemplo-sin-clase (cdr metadatos) (cdr ejemplo-sin-clase)))))

;Esta funcion multiplica todos los elementos de list1 con sus correspondientes en la list2, generando una lista de la misma longitud
;que la lista 1 y la lista 2 en la que cada posicion contiene el producto de los valores en list1 y list2 de la misma posicion
(define (multiply-lists list1 list2)
  (if (null? list1)
      '()
      (cons (* (car list1) (car list2)) (multiply-lists (cdr list1)(cdr list2)))))

;Esta funcion suma todos los elementos de una lista
(define (sum-list lista)
  (if (null? lista)
    0
    (+ (car lista) (sum-list (cdr lista)))))

;Esta funcion decide si se ha realizado un match, para ello, traduce el ejemplo, multiplica sus valores por los valores correspondientes en conceptoUU y los suma, si estos
;supera el umbral (que se almacena como -umbral) entonces hay match, de lo contrario nada.
(define (match-LUU conceptoUU ejemplo-sin-clase)
  (let* ((eje-traducido (traducir-ejemplo-sin-clase (car conceptoUU) ejemplo-sin-clase))
         (multiplic (multiply-lists (car (cdr conceptoUU)) (append eje-traducido '(1))))
         (suma (sum-list (drop-right multiplic 1)))
         (umbral (list-ref multiplic (- (length multiplic) 1))))
    (if (>= suma (- umbral))
        #t
        #f)))

;;Ejercicio 26
(he-tardado 5 'b2-e26)

;Se genera una versión clasificada del ejemplo sin clase en base al resultado de match-LUU
(define (LUUi conceptoUU ejemplo-sin-clase)
  (if (match-LUU conceptoUU ejemplo-sin-clase)
      (append ejemplo-sin-clase '(+))
      (append ejemplo-sin-clase '(-))))
