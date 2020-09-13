;;Plantilla b2-e17-e19.scm
;;Autor: Manuel Konomi Pilkati

;Dependencias: Necesita de las entregas anteriores y de SRFI-1

;;Ejercicio 17
(he-tardado 10 'b2-e17)

;Esta funcion utiliza la formula de la literatura para calcular la puntiacion de un concepto de cara
;a clasificar los casos positivos y negativos.Cuenta matches positivos y mismatches negativos y lo divide entre el total de casos.
(define (score-CL concepto-CL PSET NSET)
 (let* ((PSETsc (cdr PSET))
        (NSETsc (cdr NSET))
        (metadata (car PSET))
        (pmatches (count-matches concepto-CL PSETsc 0))
        (nunmatches (- (length NSETsc) (count-matches concepto-CL NSETsc 0)))
        (total-ejem (+ (length NSETsc) (length PSETsc))))
   (/ (+ pmatches nunmatches) total-ejem)))

;;Ejercicio 18
(he-tardado 120 'b2-e18)

;Esta funcion genera un Open-set a partir de un concepto especializado S. Tambien calcula el nuevo CSET, ya que se pueden haber eliminado soluciones
;peores que la nueva S. Si la solucion S entra en OSET pasara a evaluarse en la siguiente vuelta del algoritmo
(define (generate-OPEN-SET CSET S OSET PSET NSET CSETOUT)
  (if (null? CSET)
      (list CSETOUT (cons S OSET))
      (let* ((C (car CSET)))
        (if (<= (cmp-concepto-CL S C) 0)
            (if (< (score-CL S PSET NSET) (score-CL C PSET NSET))
                (list CSETOUT OSET)
                (generate-OPEN-SET (cdr CSET) S OSET PSET NSET CSETOUT))
            (generate-OPEN-SET (cdr CSET) S OSET PSET NSET (cons C CSETOUT))))))

;Esta funcion actualiza el OSET y el CSET para cada solucion S en NEWSET usando la funcion anterior
(define (generate-OSET CSET NEWSET OSET PSET NSET)
  (if (null? NEWSET)
      (list CSET OSET)
      (let* ((COSET (generate-OPEN-SET CSET (car NEWSET) OSET PSET NSET '()))
             (OSET+ (list-ref COSET 1))
             (CSET+ (car COSET)))
        (generate-OSET CSET+ (cdr NEWSET) OSET+ PSET NSET))))

;Esta funcion evalua que los conceptos en SPECS tengan mayor puntiacion que H. Devuelve NEWSET con los conceptos que cumplen la condicion
(define (eval-SPECS SPECS H PSET NSET NEWSET)
  (if (null? SPECS)
      NEWSET
      (if (> (score-CL (car SPECS) PSET NSET) (score-CL H PSET NSET))
          (eval-SPECS (cdr SPECS) H PSET NSET (cons (car SPECS) NEWSET))
          (eval-SPECS (cdr SPECS) H PSET NSET NEWSET))))

;Similar a eval-hset, pero adaptado para HGS, se general los SPECS (especializaciones directas), se filtran en newsets, Si NEWSET no esta vacio
;se actualiza OSET y CSET y pasa a la recursion
(define (eval-H HSET PSET NSET OSET CSET)
  (if (null? HSET)
      (list OSET CSET)
      (let* ((H (car HSET))
             (metadata (car PSET))
             (PSETsc (cdr PSET))
             (NSETsc (cdr NSET))
             (SPECS (delete-duplicates (egs-generate-specs H metadata NSETsc)))   ;TODO maybe otros H presentes en SPECS, habria que limpiar TODOS los H cada vez que se hace el bucle?
             (NEWSET (eval-SPECS SPECS H PSET NSET '())))
        (if (eqv? NEWSET '())
            (eval-H (cdr HSET) PSET NSET OSET (cons H CSET))
            (let* ((COSET (generate-OSET CSET NEWSET OSET PSET NSET))
                   (CSET+ (car COSET))
                   (OSET+ (list-ref COSET 1)))
              (eval-H (cdr HSET) PSET NSET OSET+ CSET+))))))

;Devuelve una lista de tamaño "beamsize" de los primeros elementos de BESTSET
(define (get-best-beam BESTSET beamsize)
  (if (or (= 0 beamsize) (null? BESTSET))
      '()
      (cons (car BESTSET) (get-best-beam (cdr BESTSET) (- beamsize 1)))))


;Parte recursiva de HGS. Se calcula el OSET inicial, el CSET, el BESTSET, se saca su beamsize, se sacan los elementos de OSET en BSSET, y lo mismo con los de CSET
;En caso de que OSET este vacio, devolvemos la primera posicion de BSSET (si hay). Esta posicion es la que tiene la mayor puntuacion segun score-CL
(define (HGS0 PSET NSET CSET HSET)
  (let* ((OCSET (eval-H HSET PSET NSET '() '()))
         (OSET (delete-duplicates (car OCSET)))
         (CSET+ (delete-duplicates (list-ref OCSET 1)))
         (BESTSET (sort (append OSET CSET+) (lambda (c1 c2) (>= (score-CL c1 PSET NSET) (score-CL c2 PSET NSET)))))
         (BSSET (get-best-beam BESTSET 3))
         (BCSET (generate-exclusive-newset-rec OSET BSSET))
         (BOSET (generate-exclusive-newset-rec CSET+ BSSET)))
    (if (null? OSET)
        (if (null? BSSET)
            '()
            (car BSSET))
        (HGS0 PSET NSET BCSET BOSET))))

;Esta funcion separa ejemplos en metadatos, set positivo, set negativo, hipotesis mas general y llama a HGS0 para recuperar el mejor resultado
(define (HGS ejemplos)
  (let* ((metadata (car ejemplos))
         (PSET (cons metadata (get-PSET ejemplos)))
         (NSET (cons metadata (get-NSET ejemplos)))
         (HSET (list (concepto-CL-mas-general (car ejemplos))))
         (CSET (HGS0 PSET NSET '() HSET)))
    (if (not (null? CSET)) CSET '())))

;;Ejercicio 19
(he-tardado 80 'b2-e19)
;;Para este caso se han obtenido resultados, pero al usar un beamsize inicial de 5 los resultados generados en los pasos intermedios sigue siendo muy grande,
;tras bajar el beamsize, se obtuvieron resultados, aunque lejos de ser los mejores.
