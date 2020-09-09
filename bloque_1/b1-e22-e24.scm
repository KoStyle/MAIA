;;Plantilla b1-e22-e24.scm
;;Autor: Manuel Konomi

;;Ejercicio 22
;En este ejercicio he creado una funcion que calcula los aciertos dadas dos listas de ejemplos (sin cabecera) una con la clase que que sabemos
;es la correcta, otra con las clases predichas para esos mismos casos. Devuelve el numero de aciertos totales
(he-tardado 45 'b1-e22)
(define (calcula-aciertos ej-originales ej-calculados aciertos)
  (if (and (list? ej-calculados) (list? ej-originales) (= (length ej-calculados) (length ej-originales)))
      (let* ((c-original (list-ref (list-ref ej-originales 0) (- (length (list-ref ej-originales 0)) 1)))
            (c-calculada (list-ref (list-ref ej-calculados 0) (- (length (list-ref ej-calculados 0)) 1)))
            (nuevo-acierto (if (eqv? c-original c-calculada) (+ aciertos 1) aciertos)))
      (if (null? (list-tail ej-originales 1))
          nuevo-acierto
          (calcula-aciertos (list-tail ej-originales 1) (list-tail ej-calculados 1) nuevo-acierto)))
      '()))
        

;;usando la funcion anterior, lo que hacemos es convertir los ejemplos en una lista sin campo clase, los pasamos
; por el interprete y calculamos los aciertos con la funcion aterior, el resultado lo dividimos por la cantidad de ejemplso y lo pasamos a float
(define (resustitution algoritmo interprete ejemplos)
 (let* ((esencia (algoritmo ejemplos))
        (ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) (list-tail ejemplos 1)))
        (extension (map (lambda(x) (interprete esencia x)) ejemplos-sin-clase)))
   (exact->inexact (/ (calcula-aciertos (list-tail ejemplos 1) extension 0) (length extension)))))

(define (aprende-y-clasifica algoritmo interprete ejemplos benchmark)
  (let* ((esencia (algoritmo ejemplos))
        (ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) benchmark))
        (extension (map (lambda(x) (interprete esencia x)) ejemplos-sin-clase)))
    extension))

;;Leave-one-out tiene la parte principal donde se calculan todos los atributos necesarios y luego la parte recursiva. En esta ultima
; por cada ciclo se elecciona el primer elemento de la lista oneoutlist, el que usaremos de test. Se elimina este elemento de la lista completa
; de ejemplos, se transforman los ejemplos resultates a un conjunto de entrenamiento, se convierte en oneout en una lista (la funcion aprende in clasifica
; espera una lista de ejemplos, no un ejemplo suelto) Se llama a la funcion aprende y clasifica que usa entrenamiento para extraer la esencia
; y clasifica lo contenido en benchmark. Actualizamos el parametro aciertos con lo que nos diga calcula-aciertos. Si quedan elementos en la lista oneoutlist, seguimos
; la recursion, si no, devolvemos los aciertos totales.
(define (leave-one-out-rec algoritmo interprete oneoutlist ejemplos aciertos)
  (let* ((oneout (list-ref oneoutlist 0))
         (atributos (car ejemplos))
         (casos (cdr ejemplos))
         (training (eliminar_elemento casos oneout))
         (entrenamiento (list atributos training))
         (benchmark (cons oneout '()))
         (benchclasi (aprende-y-clasifica algoritmo interprete entrenamiento benchmark))
         (nuevo-aciertos (calcula-aciertos benchmark benchclasi aciertos)))
    (if (null? (list-tail oneoutlist 1))
        nuevo-aciertos
        (leave-one-out-rec algoritmo interprete (list-tail oneoutlist 1) ejemplos nuevo-aciertos))))
  
;La funcion principal divide el numero de aciertos de la recursion entre la cantidad de casos que tenemos (ya que 0 <= aciertos <= ncasos)
(define (leave-one-out algoritmo interprete ejemplos)
  (let ((aciertos 0)
        (num-eje (length (list-tail ejemplos 1)))
        (oneoutlist (list-tail ejemplos 1)))
    (exact->inexact (/ (leave-one-out-rec algoritmo interprete oneoutlist ejemplos aciertos) num-eje)))) ;dividimos entre el numero de veces que se hace leaveone out


;;Ejercicio 23
(he-tardado 10 'b1-e23)
;;Hemos llamado a la funcion aprende-y-clasifica y calculado los aciertos resultantes
(define (holdout algoritmo interprete ejemplos-aprender ejemplos-evaluar)
  (let (
        (classic (aprende-y-clasifica algoritmo interprete ejemplos-aprender ejemplos-evaluar))) ;TODO duda ejemplos evaluar (cabecera si o no?)
    (exact->inexact (/ (calcula-aciertos ejemplos-evaluar classic 0) (length ejemplos-evaluar)))))

;;Ejercicio 24
(he-tardado 60 'b1-e24)

;Recibe una lista de folds (sin cabecera) y los va uniendo recursivamente en agregado, devuelve una lista de ejemplos con cabecera "atributos"
(define (unir-folds atributos listafolds agregado)
  (let* ((nuevo-agregado (mezcla_rec (list-ref listafolds 0) agregado)))
    (if (null? (list-tail listafolds 1))
        (cons atributos nuevo-agregado)
        (unir-folds atributos (list-tail listafolds 1) nuevo-agregado))))

;Similar a leave-one-out-rec salvo que esta vez el oneoutlist es de folds y no de ejemplos. Por lo demas, el procedimiento es el mismo.
(define (cross-validation-rec algoritmo interprete oneoutlist ejemplos aciertos)
  (let* ((oneout (list-ref oneoutlist 0))
         (atributos (car ejemplos))
         (folds (cdr ejemplos))
         (training (eliminar_elemento folds oneout))
         (entrenamiento (unir-folds atributos training '()))
         (benchmark oneout)
         (benchclasi (aprende-y-clasifica algoritmo interprete entrenamiento benchmark))
         (nuevo-aciertos (calcula-aciertos benchmark benchclasi aciertos)))
    (if (null? (list-tail oneoutlist 1))
        nuevo-aciertos
        (cross-validation-rec algoritmo interprete (list-tail oneoutlist 1) ejemplos nuevo-aciertos))))

;Esta funcion crea las estructuras necesarias para llamar a cross-validation-rec, Para ello usa la funcion de folding del parametro funcionfolding.
; Se crea una version de ejemplos con nfolds, se crea una lista de folds que sera el oneoutlist, se separan los atributos y se calcula el numero de ejemplos
(define (cross-validation-generic algoritmo interprete ejemplos nfolds funcionfolding)
  (let* ((ejemplos-folded (funcionfolding nfolds ejemplos))
         (listafolds (cdr ejemplos-folded))
         (atributos (car ejemplos-folded))
         (num-eje (length (cdr ejemplos)))
         (aciertos 0))
    (exact->inexact (/ (cross-validation-rec algoritmo interprete listafolds ejemplos-folded 0) num-eje))))

;Compara la igualdad de dos listas SIMPLES
(define (listas-iguales? lista1 lista2)
  (if (not (eqv? (length lista1) (length lista2)))
      #f
      (if (not (eqv? (car lista1) (car lista2)))
          #f
          (if (null? (list-tail lista1 1))
              #t
              (listas-iguales? (list-tail lista1 1) (list-tail lista2 1))))))

;;llamamos a la funcion generica usando "folds" como funcion de folding
(define (cross-validation algoritmo interprete ejemplos n)
 (cross-validation-generic algoritmo interprete ejemplos n folds))
;;llamamos a la funcion generica usando "stratify" como funcion de folding
(define (stratified-cross-validation algoritmo interprete ejemplos n)
  (cross-validation-generic algoritmo interprete ejemplos n stratify))
