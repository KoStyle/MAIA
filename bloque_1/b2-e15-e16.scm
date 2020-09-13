;;Plantilla b2-e15-e16.scm
;;Autor: Manuel Konomi Pilkati

;Dependencias: Se necesitas todas las entregas anteriores hasta llegar al bloque1 con obtener al azar.
;Tambien se utilizan funciones de SRFI-1 para eliminar los duplicados de las listas o eliminar conceptos padre

;;Ejercicio 15
(he-tardado 240 'b2-e15)

;Esta funcion comprueba si todos los elementos del PSET tienen match, de lo contrario devuelve false
(define (egs-test-positives PSET H)
  (if (null? PSET)
      #t
      (if (not (match-CL H (drop-right (car PSET) 1)))
          #f
          (egs-test-positives (cdr PSET) H))))

;Esta funcion comprueba que todos los elementos en NSET fallan el match. En ese caso devuelve true. Si al menos un elemento hace match, se devuelve false
(define (egs-test-negatives NSET H)
  (if (null? NSET)
      #t
      (if (match-CL H (drop-right (car NSET) 1))
          #f
          (egs-test-negatives (cdr NSET) H))))

;Esta funcion cuenta los matches de un set
(define (count-matches H SET COUNT)
  (if (null? SET)
      COUNT
      (if (match-CL H (drop-right (car SET) 1))
          (count-matches H (cdr SET) (+ 1 COUNT))
          (count-matches H (cdr SET) COUNT))))


;Esta funcion genera una lista de conceptos especializados a partir de H. Para mejorar la eficiencia se eliminan conceptos duplicados y el concepto H original
;en casod e que se haya generado.
(define (egs-generate-specs H metadata NSET)
  (if (null? NSET)
     '()
     (append (delete-duplicates (delete H (especializaciones-CL H metadata (car NSET)))) (egs-generate-specs H metadata (cdr NSET)))))
  
  
;Esta funcion evalua todos los conceptos en HSET para ver si acierta todos los ejemplos de PSET y falla todos los de NSET.
; Si falla en clasificar positivos, el concepto que se esta evaluando (H) se descarta y se pasa al siguiente de HSET.
; Si acierta todos los positivos y falla todos los negativos, se añade a CSET (conceptos que clasifican perfectamente un set)
; Si acierta todos los positivos pero no falla todos los negativos, se añade a validHSET como concepto abierto para especializarse mas.
(define (eval-hset HSET PSET NSET CSET VALIDHSET)
  (if (null? HSET)
    (list CSET VALIDHSET)
    (let* ((H (car HSET))
           (allposi (egs-test-positives PSET H))
           (allnega (egs-test-negatives NSET H)))
           (if (not allposi)
               (eval-hset (cdr HSET) PSET NSET CSET VALIDHSET)
               (if allnega
                   (eval-hset (cdr HSET) PSET NSET (cons H CSET) VALIDHSET)
                   (eval-hset (cdr HSET) PSET NSET CSET (cons H VALIDHSET)))))))

;Compara la generalidad del concepto H frente a todo un set de conceptos. True si H es mas general que absolutamente todo el set
(define (compara-generalidad>= H SET)
  (if (null? SET)
      #t
      (if (not (concepto-CL>= H (car SET)))
          #f
          (compara-generalidad>= H (cdr SET)))))

;Esta funcion comprueba que los conceptos de SPECS sean mas generales que todos los de CSET (los conceptos buenos)
(define (egs-valid-specs SPECS CSET)
  (if (null? SPECS)
      '()
      (if (compara-generalidad>= (car SPECS) CSET)
          (cons (car SPECS) (egs-valid-specs (cdr SPECS) CSET))
          (egs-valid-specs (cdr SPECS) CSET))))

;Esta funcion genera un nuevo set de conceptos especializados y validos (que cumplan con la funcion anterior) de HSET. Se eliminan los duplicados
(define (generate-newset HSET NSET CSET metadata)
  (if (null? HSET)
      '()
      (let* ((H (car HSET))
             (NEWSET (append (egs-valid-specs (egs-generate-specs H metadata NSET) CSET) (generate-newset (cdr HSET) NSET CSET metadata))))
        (delete-duplicates NEWSET))))

;Esta funcion se encarga de eliminar recursivamente las instancias de HSET de NEWSET (para que los conceptos progrenitores no esten presentes en la siguiente
;vuelta, y evitar bucles infinitos
(define (generate-exclusive-newset-rec HSET NEWSET)
  (if (null? HSET)
      NEWSET
      (let ((NEWSET+ (delete (car HSET) NEWSET)))
        (generate-exclusive-newset-rec (cdr HSET) NEWSET+))))

;Llama a la funcion anterior despuesd de generar el NEWSET
(define (generate-exclusive-newset HSET NSET CSET metadata)
  (let* ((NEWSET (generate-newset HSET NSET CSET metadata)))
    (generate-exclusive-newset-rec HSET NEWSET)))

;Devuelve los ejemplos positivos de un set de ejemplos (con cabecera). El formato de salida es el mismo: Cabecera mas ejemplos)
(define (get-PSET ejemplos)
  (if (null? ejemplos)
      '()
      (if  (eqv? (list-ref (car ejemplos) (- (length (car ejemplos)) 1)) '+)
           (cons (car ejemplos) (get-PSET (cdr ejemplos)))
           (get-PSET (cdr ejemplos)))))

;Esta funcion devuelve de la misma manera los ejemplos engativos
(define (get-NSET ejemplos)
  (if (null? ejemplos)
      '()
      (if  (eqv? (list-ref (car ejemplos) (- (length (car ejemplos)) 1)) '-)
           (cons (car ejemplos) (get-NSET (cdr ejemplos)))
           (get-NSET (cdr ejemplos)))))

;Parte recursiva de EGS. Evalua el set HSET en base a los ejemplos de PSET y NSET. Si tras la evaluacion no quedan soluciones que se puedan mejorar
;se devuelve CSET (coleccion de conceptos que clasifican todos los casos correctamente).
;Si quedan conceptos mejorables, se especializan todos los de HSET+ (conceptos validos frente a CSET) y se empieza la recursion
(define (EGS0 PSET NSET CSET HSET)
  (let* ((CHSET (eval-hset HSET (cdr PSET) (cdr NSET) CSET '()))
         (CSET+ (car CHSET))
         (HSET+ (car (cdr CHSET))))
    (if (null? HSET+)
        CSET+
        (let ((NEWSET (generate-exclusive-newset HSET+ (cdr NSET) CSET+ (car PSET))))
          (EGS0 PSET NSET CSET+ NEWSET)))))

;Esta funcion separa los metadatos, los casos positivos, los negativos, y crea el concepto mas general posible (todo comodines). Llama a EGS0 y devuelve
;una de las soluciones que devuelva al azar
(define (EGS ejemplos)
  (let* ((metadata (car ejemplos))
         (PSET (cons metadata (get-PSET ejemplos)))
         (NSET (cons metadata (get-NSET ejemplos)))
         (HSET (list (concepto-CL-mas-general (car ejemplos))))
         (CSET (EGS0 PSET NSET '() HSET)))
    (if (not (null? CSET)) (obtener-al-azar CSET) '())))

;;Ejercicio 16
(he-tardado 30 'b2-e16)
;;Estos dos ejemplos causan una explosion de conceptos tras un par de vueltas. Se ha necesitado ampliar varias veces la memoria de DrRacket para que soporte todos en memoria.
;Con 4096MB tiene suficiente para ejecutarlo entero. para los dos problemas devuelve un CSET  vacio, sin una sola solucion que satisfaga todos los casos de los sets positivo
;y negativo.
