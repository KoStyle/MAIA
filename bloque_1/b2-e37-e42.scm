;;Plantilla b2-e37-e42.scm
;;Autor: Manuel Konomi

;;Ejercicio 37
(he-tardado 25 'b2-e37)

;Funcion facilitada por el enunciado para generar un concepto NB vacio a partir de unos metadatos
(define (nuevo-conceptoNB metadatos)
  (do ((cuentas '(0))
       (valores '())
       (i 0 (+ i 1))
       )
    ((= i (- (length metadatos) 1))
     (let ((c (reverse cuentas)))
       (list (cons '+ c)(cons '- c)));valor devuelto
     )
    (set! valores (cadr (list-ref metadatos i)))
    (cond
      ((eq? valores 'numerico)
       (set! cuentas (cons '(numerico 0 0) cuentas)))
      (else ;nominales
       (set! cuentas
             (cons (map (lambda(x) (cons x 0))
                        valores)
                   cuentas)))
      )))


;Esta funcion actualiza un atributo nominal del concepto NB creando un nuevo par (simbolo . contador) cuando encuentra uno que tiene match.
;De lo contrario, devuelve todo el atributo nominal como le llego.
(define (actualiza-atributo-NB-nominal atributo-NB valor)
  (if (null? atributo-NB)
      '()
      (if (eqv? (car (car atributo-NB)) valor)
          (cons (cons (caar atributo-NB) (+ 1 (cdr (car atributo-NB)))) (cdr atributo-NB))
          (cons (car atributo-NB) (actualiza-atributo-NB-nominal (cdr atributo-NB) valor)))))

;Esta funcion actualiza un atributo de tipo numerico con un valor actualizando el acumulador de valor, y el acumulador de valor cuadrado.
(define (actualiza-atributo-NB-numerico atributo-NB valor)
  (list 'numerico (+ (list-ref atributo-NB 1) valor) (+ (list-ref atributo-NB 2) (expt valor 2))))

;Esta funcion recibe un ejemplo sin clase y una clase de un concepto NB, recorre todos los atributos actualizandolos segun sea necesario con la funcion numerica o
;la funcion nominal
(define (actualiza-clase-NB clase-NB ejemplo-sc)
  (if (null? clase-NB)
      '()
      (if (number? (car clase-NB))
          (cons (+ (car clase-NB) 1) (actualiza-clase-NB (cdr clase-NB) ejemplo-sc))
          (if (symbol? (car clase-NB))
              (cons (car clase-NB) (actualiza-clase-NB (cdr clase-NB) ejemplo-sc))
              (if (eqv? (caar clase-NB) 'numerico)
                  (cons (actualiza-atributo-NB-numerico (car clase-NB) (car ejemplo-sc)) (actualiza-clase-NB (cdr clase-NB) (cdr ejemplo-sc)))
                  (cons (actualiza-atributo-NB-nominal (car clase-NB) (car ejemplo-sc)) (actualiza-clase-NB (cdr clase-NB) (cdr ejemplo-sc))))))))
  

;Funcion pricipal, va recorriendo las clases del concepto hasta que encuentra la clase a la que pertenece el ejemplo. Cuando la encuentra (si la encuentra) la sustituye por
; el resultado de llamar a "actualiza-clase-NB" pasandole la clase y el ejemplo sin clase. Si no encuentra la clase devuelve el concepto como estaba.
(define (INB concepto-NB ejemplo)
  (if (null? concepto-NB)
      '()
      (let* ((cnb (car concepto-NB))
             (clase (list-ref ejemplo (- (length ejemplo) 1))))
        (if (eqv? (car cnb) clase)
            (cons (actualiza-clase-NB cnb (drop-right ejemplo 1)) (cdr concepto-NB))
            (cons cnb (INB (cdr concepto-NB) ejemplo))))))

;;Ejercicio 38
(he-tardado 10 'b2-e38)
;Funcion recursiva que actualiza el concepto con el primer concepto del SET y pasa a la siguiente iteracion con el resto del set. Cuando recorre todos lso ejemplos
;devuelve el concepto
(define (NB-rec CNB ejemplos-scab)
  (if (null? ejemplos-scab)
      CNB
      (NB-rec (INB CNB (car ejemplos-scab)) (cdr ejemplos-scab))))

;funcion principal. Separa los ejemplos en metadatos y ejemplos, y crea el concepto vacio. Devuelve el resultado de NB-rec
(define (NB ejemplos)
  (let* ((metadatos (car ejemplos))
         (ejemplos-scab (cdr ejemplos))
         (conceptoNB (nuevo-conceptoNB metadatos)))
    (NB-rec conceptoNB ejemplos-scab)))

;;Ejercicio 39
(he-tardado 10 'b2-e39)

;Divide el sumatorio de elemento x entre n
(define (media x n)
 (/ x n))

(define (varinza x2 m n)
  (- (/ x2 n) (expt m 2)))

;;Ejercicio 40
(he-tardado 60 'b2-e40)

;Probabilidad de un metaatributo para un valor
(define (probabilidad-num atrib n valor)
  (let* ((med (media (list-ref atrib 1) n))
         (var (varinza (list-ref atrib 2) (list-ref atrib 1) n))
         (exponente (/ (- (expt (- valor med) 2)) (* 2 var)))
         (denom (sqrt (* 2 pi var)))
         (prob (* (/ 1 denom) (exp exponente))))
    prob))

(define (probabilidad-nom atrib n valor)
  (if (null? atrib)
      0
      (if (eqv? (caar atrib) valor)
          (/ (cdar atrib) n)
          (probabilidad-nom (cdr atrib) n valor))))

(define (prob-atrib atrib n valor)
  (if (number? valor)
      (probabilidad-num atrib n valor)
      (probabilidad-nom atrib n valor)))

(define (multiplica-lista lista)
  (if (null? lista)
      1.0
      (* (car lista) (multiplica-lista (cdr lista)))))

(define (PIC metaclase ejemplo-scla)
  (let* ((meta-atribs (list-tail metaclase 2))
         (n (list-ref metaclase 1))
         (probs (map (lambda (x y) (prob-atrib x n y)) meta-atribs ejemplo-scla)))
    probs))

(define (PIC+ metaclase ejemplo-scla)
  (let* ((meta-atribs (list-tail metaclase 2))
         (n (list-ref metaclase 1))
         (probs (map (lambda (x y) (prob-atrib x n y)) meta-atribs ejemplo-scla)))
   (multiplica-lista probs)))

(define (multiplica-listas l1 l2)
  (if (null? l1)
      '()
      (cons (* (car l1) (car l2)) (multiplica-listas (cdr l1) (cdr l2)))))

(define (divide-listas l1 l2)
  (if (null? l1)
      '()
      (if (not (= (car l2) 0))
          (cons (/ (car l1) (car l2)) (multiplica-listas (cdr l1) (cdr l2)))
          (cons +inf.0 (multiplica-listas (cdr l1) (cdr l2))))))

(define (multiplica-lista-de-listas l vacumulado)
  (if (null? l)
      vacumulado
      (multiplica-lista-de-listas (cdr l) (multiplica-listas (car l) vacumulado))))
 
         

(define (PI concepto ejemplo-scla)
  (let* ((prob-clases (map (lambda (x) (PIC x ejemplo-scla)) concepto))
         (vprob-clases (multiplica-lista-de-listas prob-clases (build-list (length prob-clases) (lambda (x) 1.0)))))
    vprob-clases))

(define (PCI PC metaclase concepto ejemplo-scla)
  (let* ((probIC (PIC metaclase ejemplo-scla))
         (probI (PI concepto ejemplo-scla))
         (vnumerador (map (lambda (x) (x * PC)) probIC)))
    (divide-listas vnumerador PI)))

(define (get-metaclase concepto clase)
  (if (null? concepto)
      '()
      (if (eqv? (caar concepto) clase)
          (car concepto)
          (get-metaclase (cdr concepto) clase))))


(define (probabilidades clase concepto-NB ejemplo-sin-clase)
  (let* ((metaclase (get-metaclase concepto-NB clase))
         (PC (/ (list-ref metaclase 1) (sum-list (map (lambda (x) (list-ref x 1)) concepto-NB))))
         (vprobCI (PCI PC metaclase concepto-NB ejemplo-sin-clase)))
    vprobCI))

;;Ejercicio 41
(he-tardado -1 'b2-e41)
;;<comentarios>
;(define (NBi concepto-NB ejemplo-sin-clase)
;<codigo>)

;;Ejercicio 42
(he-tardado -1 'b2-e42)
;;<comentarios>







