;;Plantilla b2-e1-e5.scm
;;Autor: Manuel Konomi Pilkati

;;Ejercicio 1
(he-tardado 90 'b2-e1)

;Esta funcion devuelve un simbolo distinto según el tipo del elemento que se pasa como parametro
(define (get-type x)
  (if (and (list? x) (not (eqv? x '())))
      (if (symbol? (car x))
          'symbol
          (if (num-concept? x)
              'number
              '?))
      (if (pair? x)
          'pair
          (if (symbol? x)
              'symbol
              (if (number? x)
                  'number
                  '?)))))

;Esta funcion verifica si un test es de tipo test numerico valido
(define (num-concept? concept)
  (if (or (< (length concept) 1) (> (length concept) 2))
      #f
      (if (not (or (list? (car concept)) (number? (car concept))))
          #f
          (if (number? (car concept))
              (if (null? (list-tail concept 1))
                  #t
                  (num-concept? (list-tail concept 1)))
              (if (not (number? (car (car concept))))
                  #f
                  (if (null? (list-tail concept 1))
                      #t
                      (num-concept? (list-tail concept 1))))))))

;Esta funcion comprueba un concepto-CL completo para ver si su estructura es valida
(define (concepto-bien-formado? concepto)
  (if (not (or (eqv? (car concepto) '()) (num-concept? (car concepto)) (symbol? (car (car concepto)))))
      #f
      (if (null? (list-tail concepto 1))
          #t
          (concepto-bien-formado? (list-tail concepto 1)))))

;Esta funcion comprueba que los tipos de test se corresponden con los tipos de valores en el ejemplo sin clase que se facilita
; Es decir, que tanto concepto como el ejemplo tengan el mismo tipo de elemento en el indice 0, 1, 2, etc.
(define (validar-concepto concepto ejemplo-sin-clase)
  (if (not (concepto-bien-formado? concepto))
      #f
      (if (eqv? (car concepto) '())
          #f
          (if (and (not (eqv? (get-type (car concepto)) (get-type (car ejemplo-sin-clase)))) (not (eqv? (car (car concepto)) '*))) 
              #f
              (if (null? (list-tail concepto 1))
                  #t
                  (validar-concepto (list-tail concepto 1) (list-tail ejemplo-sin-clase 1)))))))
          
;Esta funcion devuelve true en caso de que el valor de ejemplo satisfaga el test numerico, es decir, que entre en el rango establecido
(define (test-numerico test valor-ejemplo)
  (let* ((ini (if (list? (car test)) (car (car test)) (car test)))
         (iniinc (if (list? (car test)) #t #f))
         (fin (if (= (length test) 2) (if (list? (list-ref test 1)) (car (list-ref test 1)) (list-ref test 1)) ini))
         (fininc (if (= (length test) 2) (if (list? (list-ref test 1)) #t #f) #t)))
    (if (= (length test) 2)
        (if (and iniinc fininc)
            (if (and (<= ini valor-ejemplo) (>= fin valor-ejemplo))
                #t
                #f)
            (if (and (not iniinc) fininc)
                (if (and (< ini valor-ejemplo) (>= fin valor-ejemplo))
                    #t
                    #f)
                (if (and iniinc (not fininc))
                    (if (and (<= ini valor-ejemplo) (> fin valor-ejemplo))
                        #t
                        #f)
                    (if (and (not iniinc) (not fininc))
                        (if (and (< ini valor-ejemplo) (> fin valor-ejemplo))
                            #t
                            #f)
                        '()))))
        (if (eqv? ini '*)
            #t
            (= ini valor-ejemplo)))))

;Esta funcion revuelve true si el valor de ejemplo (de tipo simbolo) cumple el test
(define (test-simbolico test valor-ejemplo)
  (if (or (eqv? (car test) valor-ejemplo) (eqv? (car test) '*))
      #t
      #f))

;Esta funcion recibe un valor de ejemplo (de cualquiera de los dos tipos) y devuelve true o false segun cumpla o no el test
(define (pasa-test? test valor-ejemplo)
  (if (number? valor-ejemplo)
      (test-numerico test valor-ejemplo)
      (test-simbolico test valor-ejemplo)))

;Esta funcion devuelve true en caso de que un ejemplo (sin clase) pasa un concepto-CL completamente
(define (pasa-concepto? concepto ejemplo-sin-clase)
  (if (not (pasa-test? (car concepto) (car ejemplo-sin-clase)))
      #f
      (if (null? (list-tail concepto 1))
          #t
          (pasa-concepto? (list-tail concepto 1) (list-tail ejemplo-sin-clase 1)))))

;Match-CL nos devuelve true si tenemos un concepto-CL y ejemplo sin clase que coinciden en longitud y en tipo de valor en cada posicion, y
;todos los valores de ejemplo-sin-clase pueden pasar los test del concepto-CL
(define (match-CL concepto-CL ejemplo-sin-clase)
  (if (not (= (length concepto-CL) (length ejemplo-sin-clase)))
      #f
      (if (not (validar-concepto concepto-CL ejemplo-sin-clase))
          #f
          (pasa-concepto? concepto-CL ejemplo-sin-clase))))

;;Ejercicio 2
(he-tardado 5 'b2-e2)
;;Esta funcion devuelve el ejemplo-sin-clase con una clasificación según el resultado de match-CL
(define (CLi concepto-CL ejemplo-sin-clase)
 (if (match-CL concepto-CL ejemplo-sin-clase)
     (append ejemplo-sin-clase (cons '+ '()))
     (append ejemplo-sin-clase (cons '- '()))))

;;Ejercicio 3
(he-tardado 5 'b2-e3)
;El mas general, todos los test aceptan cualquier cosa
;'((*)(*)(*)(*)(*)(*))
;El mas especifico (todos los test rechazan cualquier cosa)
;'(()()()()()())
;El concepto mas aproximado a un buen dia, calculado a ojo
;'((soleado)(5 31)(23 80)(*)(bajo)(no))

;;Ejercicio 4
(he-tardado 5 'b2-e4)
;;Esta funcion recibe los metadatos de un set de ejemplos y usa la estructura para generar el ejemplo mas general posible
(define (concepto-CL-mas-general metadatos)
  (if (eqv? (car (car metadatos)) 'clase)
      (if (null? (list-tail metadatos 1))
          '()
          (concepto-CL-mas-general (list-tail metadatos 1)))
      (if (null? (list-tail metadatos 1))
          (list (list '*))
          (cons (list '*) (concepto-CL-mas-general (list-tail metadatos 1))))))

;;Ejercicio 5
(he-tardado 2 'b2-e5)
;;Esta funcion es la contraparte de la anterior, a partir de los metadatos de un set de ejemplos crea el concepto mas especifico
(define (concepto-CL-mas-especifico metadatos)
  (if (eqv? (car (car metadatos)) 'clase)
      (if (null? (list-tail metadatos 1))
          '()
          (concepto-CL-mas-general (list-tail metadatos 1)))
      (if (null? (list-tail metadatos 1))
          (list '())
          (cons '() (concepto-CL-mas-especifico (list-tail metadatos 1))))))
