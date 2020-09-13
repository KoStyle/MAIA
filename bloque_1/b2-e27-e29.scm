;;Plantilla b2-e27-e29.scm
;;Autor: Manuel Konomi


;Dependencias: Depende de la entrega anterior y de SRFI-1

;;Ejercicio 27
(he-tardado 20 'b2-e27)
;; Habia una errata en la plantilla, llamaba a la funcion score-CL en lugar de score-TC

;Esta funcion cuenta el numero de matches de un concepto-TC H frente a un SET utilizando match-TC como indicativo
(define (count-matches-TC H SET COUNT)
  (if (null? SET)
      COUNT
      (if (match-TC H (drop-right (car SET) 1))
          (count-matches-TC H (cdr SET) (+ 1 COUNT))
          (count-matches-TC H (cdr SET) COUNT))))

;Esta funcion utiliza la misma formula que score-CL salvo que para generar el recuento de matches utiliza la funcion anterior
(define (score-TC concepto-TC PSET NSET)
 (let* ((PSETsc (cdr PSET))
        (NSETsc (cdr NSET))
        (metadata (car PSET))
        (pmatches (count-matches-TC concepto-TC PSETsc 0))
        (nunmatches (- (length NSETsc) (count-matches-TC concepto-TC NSETsc 0)))
        (total-ejem (+ (length NSETsc) (length PSETsc))))
   (/ (+ pmatches nunmatches) total-ejem)))

;;Ejercicio 28
(he-tardado 130 'b2-e28)

; Encuenta un contador en una lista de pares en el formato (X . C) donde X es el elemento que se busca, y C su contador de apariciones
(define (find-counter lista elemento)
  (if (null? lista)
      '()
      (if (eqv? (car (car lista) elemento))
          (car lista)
          (find-counter (cdr lista) elemento))))


; Esta funcion obtiene el indice de un elemento en la lista de contadores, y -1 si no existe
(define (get-index-counter lista elemento index)
  (if (>= index (length lista))
      -1
      (if (eqv? (car (list-ref lista index)) elemento)
          index
          (get-index-counter lista elemento (+ 1 index)))))

;Esta funcion crea el contador para un elemento "element" en el formato (element . n)
(define (create-counter element lista n)
  (if (null? lista)
      (cons element n)
      (if (eqv? (car lista) element)
          (create-counter element (cdr lista) (+ 1 n))
          (create-counter element (cdr lista) n))))

;Esta funcion compara si el contador c1 es mayor que c2
(define (contador>? c1 c2)
  (if (null? c2)
      #t
      (> (cdr c1) (cdr c2))))

;Esta funcion obtiene la moda (el elemento mas frecuente) en una lista simple
(define (get-moda lista contadores posicion)
  (if (>= posicion (length lista))
      (car (car (sort contadores contador>?)))
      (let* ((elem (list-ref lista posicion))
             (indice-elem (get-index-counter contadores elem 0))
             (contador (if (>= indice-elem 0) (list-ref contadores indice-elem) (create-counter elem lista 0))))
        (if (< indice-elem 0)
            (get-moda lista (cons contador contadores) (+ 1 posicion))
            (get-moda lista contadores (+ 1 posicion))))))


(define (HTC0 PSET NSET CSET HSET)
  'No-implementado)

(define (HTC ejemplos)
  'no-implementado)

;;Ejercicio 29
(he-tardado 0 'b2-e29)
;;<comentarios>
