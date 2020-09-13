;;Plantilla b2-e9-e14.scm
;;Autor: Manuel Konomi Pilkati

;Se necesita la funcion pasa-test? de las entregas anteriores.

;;Ejercicio 9
(he-tardado 40 'b2-e9)

;Sustituye el elemento i de la lista "lista" por el elemento "elem"
(define (reemplaza-elemento lista elem i)
  (if (>= i (length lista))
      '()
      (if (= i 0)
          (cons elem (cdr lista))
          (cons (car lista) (reemplaza-elemento (cdr lista) elem (- i 1))))))

;Recupera el valor del elemento i de la lista "lista"
(define (recupera-elemento lista i)
  (if (>= i (length lista))
      '()
      (if (= i 0)
          (car lista)
          (recupera-elemento (cdr lista) (- i 1)))))

;gean-rec es una funcion recursiva que se utiliza para generar especializaciones o generalizaciones de un concepto.
;La funcion recibe el concepto-CL, una serie de elementos, y el indice del concepto donde se va a sustituir el valor actual
;por los disntots valores en elementos. Es decir, si llamamos a la funcion con el indice 0 y la lista de elementos: (lluvia tornado),
;crearemos dos conceptos nuevos, el primero sustituye la posicion 0 del concepto por "lluvia" (deja el resto igual) y el segundo sustituye la posicion
;0 por "tornado". Dependiendo de los elementos en "elementos", el resultado será especializacion o generalizacion
(define (gean-rec concepto-CL indice elementos)
  (let ((a-insertar (if (not (list? (car elementos))) (list (car elementos)) (car elementos))))
    (if (null? (list-tail elementos 1))
        (cons (reemplaza-elemento concepto-CL a-insertar indice) '())
        (cons (reemplaza-elemento concepto-CL a-insertar indice) (gean-rec concepto-CL indice (cdr elementos))))))

;Esta funcion genera especializaciones de un atributo nominal en la posición "indice" del concepto en base a lo contenido
;en los metadatos
(define (especializaciones-atributo-nominal concepto-CL indice metadatos)
  (let* ((opciones (car (cdr (recupera-elemento metadatos indice))))
         (elemento (recupera-elemento concepto-CL indice)))
    (if (eqv? elemento '())
        (list concepto-CL)
        (if (not (eqv? (car elemento) '*))
            (list (reemplaza-elemento concepto-CL '() indice))
            (gean-rec concepto-CL indice opciones)))))

;;Ejercicio 10
(he-tardado 5 'b2-e10)

;Esta funcion es la versión que generaliza un atributo nominal en lugar de especializarlo. El funcionamiento es muy parecido a la funcion anterior, simplemente
;se intercambian las posiciones de '(*) y '(). Cuando tenemos '() buscaremos en los metadatos, y cuando tengamos un simbolo lo cambiaremos por '(*)
(define (generalizaciones-atributo-nominal concepto-CL indice metadatos)
  (let* ((opciones (car (cdr (recupera-elemento metadatos indice))))
         (elemento (recupera-elemento concepto-CL indice)))
    (if (eqv? elemento '(*))
        (list concepto-CL)
        (if (not (eqv? elemento '()))
            (list (reemplaza-elemento concepto-CL '(*) indice))
            (gean-rec concepto-CL indice opciones)))))

;;Ejercicio 11
(he-tardado 30 'b2-e11)

;Esta funcion recibe un test de tipo numerico y un valor numerico de ejemplo, el resultado es un nuevo test que incluye el valor recibido.
;Se supone que valor-nuevo no existe en el rango.
(define (generaliza-rango test valor-nuevo)
  (if (eqv? test '())
      (list valor-nuevo)
      (let ((test-superior (list (car test) (list valor-nuevo)))
            (test-inferior (if (= (length test) 2) (list (list valor-nuevo) (car (cdr test))) (list (list valor-nuevo) (car test)))))
            (if (pasa-test? test-superior valor-nuevo)
                test-superior
                test-inferior))))

;Esta funcion crea una generalizacion de un atributo numerico en un concepto-CL. para eso se hace uso de la anterior funcion "generaliza-rango"
(define (generalizaciones-atributo-numerico concepto-CL indice ejemplo)
  (let* ((test (recupera-elemento concepto-CL indice))
         (valor (recupera-elemento ejemplo indice))
         (clase (recupera-elemento ejemplo (- (length ejemplo) 1))))
    (if (eqv? clase '-)
        (list concepto-CL)
        (if (not (pasa-test? test valor))
            (list (reemplaza-elemento concepto-CL (generaliza-rango test valor) indice))
            (list concepto-CL)))))

;;Ejercicio 12
(he-tardado 40 'b2-e12)

;Esta funcion crea especializaciones para atributos numericos. Esto suele significar partir en 2 el rango del atributo numerico para excluir un valor, salvo que el atributo
;no sea un rango, si no un numbero en concreto, en cuyo caso el test se convierte en '()
(define (especializaciones-atributo-numerico concepto-CL indice ejemplo)
  (let* ((test (recupera-elemento concepto-CL indice)) ;Recuperamos el test en la posicion indicada
         (valor (recupera-elemento ejemplo indice))    ;Recuperamos el valor correspondiente del ejemplo
         (clase (recupera-elemento ejemplo (- (length ejemplo) 1)))) 
    (if (eqv? clase '+)
        (list concepto-CL)
        (if (eqv? (car test) '*) ;Si la clase es negativa, excluimos el valor si esta eincluido
            (gean-rec concepto-CL indice (list (list -inf.0 valor) (list valor +inf.0))) ;especializamos (*) usando infinitos
            (if (not (pasa-test? test valor)) 
                (list concepto-CL)       ;Si hubiera sido un rango, y no entraba ya de serie, devolvemos el concepto tal cual
                (if (= (length test) 1)  ;Si el test solo tiene un numero, devolvemos '()
                    (gean-rec concepto-CL indice (list '()))
                    (let* ((iniinc (list? (car test)))                       ;flag para saber si el rango es cerrado
                           (fininc (list? (car (cdr test))))                 ;flag para saber si el rango es cerrado
                           (ini (if iniinc (car (car test)) (car test)))     ;Numero de rango incial
                           (fin (if fininc (car (car (cdr test))) (car (cdr test)))) ;Numero de rango final
                           (coinc-ini (= ini valor))                         ;Comprobamos si el valor es el inicio o el fin del rango
                           (coinc-fin (= fin valor))
                           (newini (if coinc-ini valor (car test)))          ;Recalculamos inicio y fin en base a esa informacion
                           (newfin (if coinc-fin valor (car (cdr test)))))
                      (gean-rec concepto-CL indice (list (if (eqv? newini valor) '() (list newini valor)) (if (eqv? valor newfin) '() (list valor newfin)))))))))))  ;Usamos gean-rec para crear las especializaciones con nuevos rangos

;;Ejercicio 13
(he-tardado 20 'b2-e13)

;Esta funcion devuelve una lista de conceptos-cL generalizados. Funciona recursivamente hasta que no le quedan test que generalizar
(define (gene-CL-rec concepto-CL metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (number? (list-ref ejemplo indice))
          (append (generalizaciones-atributo-numerico concepto-CL indice ejemplo) (gene-CL-rec concepto-CL metadatos ejemplo (+ 1 indice)))
          (append (generalizaciones-atributo-nominal concepto-CL indice metadatos) (gene-CL-rec concepto-CL metadatos ejemplo (+ 1 indice))))))

(define (generalizaciones-CL concepto-CL metadatos ejemplo)
  (gene-CL-rec concepto-CL metadatos ejemplo 0))

;;Ejercicio 14
(he-tardado 40 'b2-e14)

;Esta funcion es analoga a la del ejercicio 13, en este caso, especializa cada test del concepto-CL devolviendo una lista
;de conceptos especializados. Funciona recursivamente.
(define (espe-CL-rec concepto-CL metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (number? (list-ref ejemplo indice))
          (append (especializaciones-atributo-numerico concepto-CL indice ejemplo) (espe-CL-rec concepto-CL metadatos ejemplo (+ 1 indice)))
          (append (especializaciones-atributo-nominal concepto-CL indice metadatos) (espe-CL-rec concepto-CL metadatos ejemplo (+ 1 indice))))))

(define (especializaciones-CL concepto-CL metadatos ejemplo)
  (espe-CL-rec concepto-CL metadatos ejemplo 0))
