#lang racket
(require srfi/1)
(require racket/trace)
;-----------------------------------------ej8
(define (leer-ejemplos archivo)
  (let ((x archivo))
    (call-with-input-file x
     (lambda (i)
       (let* ((a (read i)))
         a)))))
;-----------------------------------------ej6
;obtiene la probabilidad total sumando todos los pesos de una lista de pares "(valor . peso)"
(define prob_total
  (lambda (x)
    (if (null? (list-tail x 1))
        (cdr (list-ref x 0))
        (+ (cdr (list-ref x 0)) (prob_total (list-tail x 1))))))



;Busca el elemento que sobrepasa el threshold y devuelve el valor (sin el peso)
(define encuentra_posicion
  (lambda (lista threshold acumulado)
    (set! acumulado (+ acumulado (cdr (list-ref lista 0))))
    (if (> acumulado threshold)
        (car (list-ref lista 0))
        (encuentra_posicion (list-tail lista 1) threshold acumulado))))

;Convierte una lista de elementos simples en una lista de pares "(elemento . peso)" donde el peso es 1 para todos
(define convertir-a-cons
  (lambda (x)
    (if (null? (list-tail x 1))
        (cons (cons (list-ref x 0) 1) '())
        (cons (cons (list-ref x 0) 1) (convertir-a-cons (list-tail x 1))))))

(define obtener-al-azar0
  (lambda (x)
    (encuentra_posicion x (* (random) (prob_total x)) 0)))

;Obtiene un elemento al azar de una lista (uniforme)
(define (obtener-al-azar lista)
  (let ((x lista))
    (if (not (pair? (list-ref x 0)))
      (set! x (convertir-a-cons x))
      (if (not (number? (cdr (list-ref x 0))))
          (set! x (convertir-a-cons x))
          '()))
    (obtener-al-azar0 x)))


(define ejemplos (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos.scm"))
(define ejemplos2 (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos2.scm"))
(define ejemplos3 (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos3.scm"))
(define lepiota (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\agaricus-lepiota.scm"))
(define ionosphere (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ionosphere.scm"))

;-----------------------------------------ej10
(define busca-atributo
  (lambda (atrib listatrib posicion)
    (if (or (not (number? posicion)) (not (symbol? atrib)) (not (list? listatrib)))
        '()
        (if (eqv? (car (list-ref listatrib posicion)) atrib)
            posicion
            (busca-atributo atrib listatrib (+ posicion 1))))))

(define recupera-atributo-ejemplos
  (lambda (indice ejemplos)
    (if (or (not (number? indice)) (not (list? ejemplos)))
        '()
        (if (null? (list-tail ejemplos 1))
            (cons (list-ref (car ejemplos) indice) '())
            (cons (list-ref (car ejemplos) indice) (recupera-atributo-ejemplos indice (list-tail ejemplos 1)))))))

(define (atributo nombre-atributo ejemplos)
  (let ((atrib nombre-atributo))
    (if (or (not (symbol? atrib)) (not (list? ejemplos)))
        '()
        (recupera-atributo-ejemplos (busca-atributo atrib (car ejemplos) 0) (cdr ejemplos)))))

;-----------------------------------------ej11
;recibe dos listas de ejemplos (sin cabecera) y concatena la segunda al final de la primera
(define mezcla_rec
  (lambda (lista1 lista2)
    (if (null? (list-tail lista1 1))
        (cons (list-ref lista1 0) lista2)
        (cons (list-ref lista1 0) (mezcla_rec (list-tail lista1 1) lista2)))))


;une la cabecera de le primera lista (se supone que son iguales) con el resultado de la funcion mezclarecursiva cogiendo las listas de ejemplos de ambas listas
(define (mezclar ejemplos1 ejemplos2)
  (let ((lista1 ejemplos1) (lista2 ejemplos2))
    (cons (list-ref lista1 0) (mezcla_rec (list-tail lista1 1) (list-tail lista2 1)))))

;-----------------------------------------ej12
(define convertir_porcentaje
  (lambda (x valor)
    (if (or (not (real? x)) (not (integer? valor)) (< x 0))
        '()
        (ceiling (* x valor)))))

(define eliminar_elemento
  (lambda (lista item)
    (if (null? lista)
        '()
        (if (eqv? (list-ref lista 0) item)
            (list-tail lista 1)
            (cons (list-ref lista 0) (eliminar_elemento (list-tail lista 1) item))))))



;devuelve un dato al azar y lo elimina de la lista original
(define separar_rec
  (lambda (tam nueva x)
    (if (or (null? x) (not (integer? tam)) (not (list? nueva)))
        '()
        (if (< (length nueva) tam)
            (let ((elemento (obtener-al-azar x)))
              (set! x (eliminar_elemento x elemento))
              (set! nueva (cons elemento nueva))
              (separar_rec tam nueva x))
            (list nueva x)))))

(define (separar proporcion ejemplos)
  (let ((perc proporcion) (ejemp ejemplos))
    (if (or (not (real? perc)) (not (list? ejemp)))
        '()
        (let ((tam (convertir_porcentaje perc (length (list-tail ejemp 1)))))  ;Calculamos el tamaño de la lista a generar
          (cons (list-ref ejemp 0) (separar_rec tam '() (list-tail ejemp 1))))))) ; devolvemos una lista con los  metadatos y dos listas de ejemplos, una con el porcentaje aproximado y otra con las sobras


;-----------------------------------------ej13
;Esta funcion recursiva concatena resultados de la funcion split con el tamaño que le decimos (el resto a la hora de repartir los ejemplos de folds se reparte entre los n primeros folds) extrayendo de forma aleatoria
;de la lista de ejemplos remains. fold_size el tamaño del fold, todo los folds que quedan por hacer, n_extra el numero de folds que quedan por tener un elemento extra.
(define folds-rec
  (lambda (remains todo n_extra fold_size)
    (if (< todo 1)
        '()
        (if (= todo 1)
            (cons remains '())
            (if (> n_extra 0)
                (let* ((split (separar_rec (+ 1 fold_size) '() remains))
                       (nuevo_fold (car split)))
                  (set! remains (list-ref split 1))
                  (cons nuevo_fold (folds-rec remains (- todo 1) (- n_extra 1) fold_size)))
                (let* ((split (separar_rec fold_size '() remains))
                       (nuevo_fold (car split)))
                  (set! remains (list-ref split 1))
                  (cons nuevo_fold (folds-rec remains (- todo 1) n_extra fold_size))))))))

(define (folds n ejemplos)
  (let ((num n) (eje ejemplos))
    (if (or (not (integer? num)) (< num 1) (not (list? eje)))
        '()
        (let ((n_extra_lists (modulo (length (list-tail eje 1)) num)) ; Calculamos el resto de dividir por el tamaño de fold (para distribuir los sobrantes en las listas)
              (len_fold (floor (/ (length (list-tail eje 1)) num))))  ; Calculamos la longitud de los folds y cuantos de ellos tendran un elemento extra
          (cons (list-ref eje 0) (folds-rec (list-tail eje 1) num n_extra_lists len_fold))))))



;-----------------------------------------ej14
(define (cuenta-simbolo simbolo lista)
  (if (pair? simbolo)
      (let ((cont (car simbolo))
            (sim (cdr simbolo)))
        (if (null? (list-tail lista 1))
            (if (eqv? (list-ref lista 0) sim)
                (+ 1 cont)
                cont)
            (if (eqv? (list-ref lista 0) sim)
                (cuenta-simbolo (cons (+ 1 cont) sim) (list-tail lista 1))
                (cuenta-simbolo (cons cont sim) (list-tail lista 1)))))
      (let ((cont 0)
            (sim simbolo))
        (if (null? (list-tail lista 1))
            (if (eqv? (list-ref lista 0) sim)
                (+ 1 cont)
                cont)
            (if (eqv? (list-ref lista 0) sim)
                (cuenta-simbolo (cons (+ 1 cont) sim) (list-tail lista 1))
                (cuenta-simbolo (cons cont sim) (list-tail lista 1)))))))

(define (ratio-clases clases lista)
  (map (lambda (x)
         (cons x (cuenta-simbolo x (recupera-atributo-ejemplos (- (length (car lista)) 1) lista)))) ;recuperamos los valores de la ultima posicion de todos los ejemplos (recibimos solo ejemplos)
       clases))

(define (calcular-ratio-ejemplo ejemplo ogratio newratio)
  (if (and (list? ejemplo) (list? ogratio) (list? newratio))
      (let ((clase (list-ref ejemplo (- (length ejemplo) 1)))) ;extraemos la clase del ejemplo (sea el simbolo que sea, existe en los ratios)
        (if (eqv? clase (car (car ogratio)))  ;si es igual al simbolo de la priemra clase, el valor devuelto es cantidad original de instancias de esa clase / cantidad actual
            (/ (cdr (car ogratio)) (cdr (car newratio))) ;De esta forma, al asignar el resultado a obten-valor, se mantiene el ratio de casos obtenidos.
            (/ (cdr (list-ref ogratio 1)) (cdr (list-ref newratio 1)))))
      '()))

(define convertir-a-cons-straty
  (lambda (x ogratio newratio)
    (if (null? (list-tail x 1))
        (cons (cons (list-ref x 0) (calcular-ratio-ejemplo (list-ref x 0) ogratio newratio)) '())
        (cons (cons (list-ref x 0) (calcular-ratio-ejemplo (list-ref x 0) ogratio newratio)) (convertir-a-cons-straty (list-tail x 1) ogratio newratio)))))

(define (obtener-al-azar-straty lista ogratio simbolos)
  (if (and (list? lista) (list? ogratio) (list? simbolos))
      (let* ((ratio-actual (ratio-clases simbolos lista))
             (conlista (convertir-a-cons-straty lista ogratio ratio-actual)))
        (obtener-al-azar conlista))
      '()))

(define separar_rec-stra
  (lambda (tam nueva x ogratio simbolos)
    (if (or (null? x) (not (integer? tam)) (not (list? nueva)))
        '()
        (if (< (length nueva) tam)
            (let ((elemento (obtener-al-azar-straty x ogratio simbolos)))
              (set! x (eliminar_elemento x elemento))
              (set! nueva (cons elemento nueva))
              (separar_rec-stra tam nueva x ogratio simbolos))
            (list nueva x)))))

(define stratify-rec
  (lambda (remains todo n_extra fold_size ogratio simbolos)
    (if (< todo 1)
        '()
        (if (= todo 1)
            (cons remains '())
            (if (> n_extra 0)
                (let* ((split (separar_rec-stra (+ 1 fold_size) '() remains ogratio simbolos))
                       (nuevo_fold (car split)))
                  (set! remains (list-ref split 1))
                  (cons nuevo_fold (stratify-rec remains (- todo 1) (- n_extra 1) fold_size ogratio simbolos)))
                (let* ((split (separar_rec-stra fold_size '() remains ogratio simbolos))
                       (nuevo_fold (car split)))
                  (set! remains (list-ref split 1))
                  (cons nuevo_fold (stratify-rec remains (- todo 1) n_extra fold_size ogratio simbolos))))))))

(define (stratify n ejemplos)
  (let ((num n) (eje ejemplos))
    (if (or (not (integer? num)) (< num 1) (not (list? eje)))
        '()
        (let ((n_extra_lists (modulo (length (list-tail eje 1)) num)) ; Calculamos el resto de dividir por el tamaño de fold (para distribuir los sobrantes en las listas)
              (len_fold (floor (/ (length (list-tail eje 1)) num)))   ; Calculamos la longitud de los folds y cuantos de ellos tendran un elemento extra
              (ogratio (ratio-clases '(+ -) (list-tail eje 1))))      ; Calculamos el ratio inicial de clases
          (cons (list-ref eje 0) (stratify-rec (list-tail eje 1) num n_extra_lists len_fold ogratio '(+ -)))))))

;---------------------ej15-21

(define (A0 ejemplos)
  (let ((ratio (ratio-clases '(+ -) (list-tail ejemplos 1))))
    (if (> (cdr (list-ref ratio 0)) (cdr (list-ref ratio 1)))
        (car (list-ref ratio 0))
        (car (list-ref ratio 1)))))

(define (A0i concepto ejemplo-sin-clase)
  (append ejemplo-sin-clase (list concepto)))

(define (A1 ejemplos)
  (ratio-clases '(+ -) (list-tail ejemplos 1)))

(define (IIA1 atributos concepto-inicial ejemplo)
  (if (eqv? (car (list-ref concepto-inicial 0)) (list-ref ejemplo (- (length ejemplo) 1))) ;equals the first class
      (cons (cons (car (list-ref concepto-inicial 0)) (+ (cdr (list-ref concepto-inicial 0)) 1)) (list-tail concepto-inicial 1) )
      (cons (list-ref concepto-inicial 0) (cons (cons (car (list-ref concepto-inicial 1)) (+ (cdr (list-ref concepto-inicial 1)) 1)) '()))))

(define (IA1-rec atributos concepto-inicial ejemplos)
  (let ((concepto (IIA1 atributos concepto-inicial (list-ref ejemplos 0))))
    (if (null? (list-tail ejemplos 1))
        concepto
        (IA1-rec atributos concepto (list-tail ejemplos 1)))))

      

(define (IA1 ejemplos)
  (let ((atributos (car ejemplos))
        (concepto-inicial '((+ . 0) (- . 0))))
    (IA1-rec atributos concepto-inicial (cdr ejemplos))))

(define (A1i concepto ejemplo-sin-clase)
  (append ejemplo-sin-clase (cons (obtener-al-azar concepto) '())))

;-----------------ej22-24
(define (calcula-aciertos ej-originales ej-calculados aciertos)
  (if (and (list? ej-calculados) (list? ej-originales) (= (length ej-calculados) (length ej-originales)))
      (let* ((c-original (list-ref (list-ref ej-originales 0) (- (length (list-ref ej-originales 0)) 1)))
            (c-calculada (list-ref (list-ref ej-calculados 0) (- (length (list-ref ej-calculados 0)) 1)))
            (nuevo-acierto (if (eqv? c-original c-calculada) (+ aciertos 1) aciertos)))
      (if (null? (list-tail ej-originales 1))
          nuevo-acierto
          (calcula-aciertos (list-tail ej-originales 1) (list-tail ej-calculados 1) nuevo-acierto)))
      '()))
        

;;<comentarios resustitution>
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
;;<comentarios leave-one-out>

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
  

(define (leave-one-out algoritmo interprete ejemplos)
  (let ((aciertos 0)
        (num-eje (length (list-tail ejemplos 1)))
        (oneoutlist (list-tail ejemplos 1)))
    (exact->inexact (/ (leave-one-out-rec algoritmo interprete oneoutlist ejemplos aciertos) num-eje)))) ;dividimos entre el numero de veces que se hace leaveone out

;;<comentarios holdout>
(define (holdout algoritmo interprete ejemplos-aprender ejemplos-evaluar)
  (let (
        (classic (aprende-y-clasifica algoritmo interprete ejemplos-aprender ejemplos-evaluar))) ;TODO duda ejemplos evaluar (cabecera si o no?)
    (exact->inexact (/ (calcula-aciertos ejemplos-evaluar classic 0) (length ejemplos-evaluar)))))

;;ejercicio 24
(define (unir-folds atributos listafolds agregado)
  (let* ((nuevo-agregado (mezcla_rec (list-ref listafolds 0) agregado)))
    (if (null? (list-tail listafolds 1))
        (cons atributos nuevo-agregado)
        (unir-folds atributos (list-tail listafolds 1) nuevo-agregado))))

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

(define (cross-validation-generic algoritmo interprete ejemplos nfolds funcionfolding)
  (let* ((ejemplos-folded (funcionfolding nfolds ejemplos))
         (listafolds (cdr ejemplos-folded))
         (atributos (car ejemplos-folded))
         (num-eje (length (cdr ejemplos)))
         (aciertos 0))
    (exact->inexact (/ (cross-validation-rec algoritmo interprete listafolds ejemplos-folded 0) num-eje))))

(define (listas-iguales? lista1 lista2)
  (if (not (eqv? (length lista1) (length lista2)))
      #f
      (if (not (eqv? (car lista1) (car lista2)))
          #f
          (if (null? (list-tail lista1 1))
              #t
              (listas-iguales? (list-tail lista1 1) (list-tail lista2 1))))))



;;<comentarios cross-validation>
(define (cross-validation algoritmo interprete ejemplos n)
 (cross-validation-generic algoritmo interprete ejemplos n folds))
;;<comentarios stratified-cross-validation>
(define (stratified-cross-validation algoritmo interprete ejemplos n)
  (cross-validation-generic algoritmo interprete ejemplos n stratify))


;--------------------------------------
;--------------BLOQUE 2----------------
;--------------------------------------

;-----ej1

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

(define (concepto-bien-formado? concepto)
  (if (not (or (eqv? (car concepto) '()) (num-concept? (car concepto)) (symbol? (car (car concepto)))))
      #f
      (if (null? (list-tail concepto 1))
          #t
          (concepto-bien-formado? (list-tail concepto 1)))))

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

(define (test-simbolico test valor-ejemplo)
  (if (or (eqv? (car test) valor-ejemplo) (eqv? (car test) '*))
      #t
      #f))

(define (pasa-test? test valor-ejemplo)
  (if (number? valor-ejemplo)
      (test-numerico test valor-ejemplo)
      (test-simbolico test valor-ejemplo)))

(define (pasa-concepto? concepto ejemplo-sin-clase)
  (if (not (pasa-test? (car concepto) (car ejemplo-sin-clase)))
      #f
      (if (null? (list-tail concepto 1))
          #t
          (pasa-concepto? (list-tail concepto 1) (list-tail ejemplo-sin-clase 1)))))

(define (match-CL concepto-CL ejemplo-sin-clase)
  (if (not (= (length concepto-CL) (length ejemplo-sin-clase)))
      #f
      (if (not (validar-concepto concepto-CL ejemplo-sin-clase))
          #f
          (pasa-concepto? concepto-CL ejemplo-sin-clase))))
    

;-----ej2

(define (CLi concepto-CL ejemplo-sin-clase)
 (if (match-CL concepto-CL ejemplo-sin-clase)
     (append ejemplo-sin-clase (cons '+ '()))
     (append ejemplo-sin-clase (cons '- '()))))


;-----ej3
;El mas general
'((*)(*)(*)(*)(*)(*))
;El mas especifico (no incluye nada)
'(()()()()()())
;El concepto mas aproximado a un buen dia
'((soleado)(5 31)(23 80)(*)(bajo)(no))

;------ej4
(define (concepto-CL-mas-general metadatos)
  (if (eqv? (car (car metadatos)) 'clase)
      (if (null? (list-tail metadatos 1))
          '()
          (concepto-CL-mas-general (list-tail metadatos 1)))
      (if (null? (list-tail metadatos 1))
          (list (list '*))
          (cons (list '*) (concepto-CL-mas-general (list-tail metadatos 1))))))

;------ej5
(define (concepto-CL-mas-especifico metadatos)
  (if (eqv? (car (car metadatos)) 'clase)
      (if (null? (list-tail metadatos 1))
          '()
          (concepto-CL-mas-general (list-tail metadatos 1)))
      (if (null? (list-tail metadatos 1))
          (list '())
          (cons '() (concepto-CL-mas-especifico (list-tail metadatos 1))))))


;-------ej6
(define (grado-generalidad? test)
  (if (eqv? test '())
      '(1 0)
      (let* ((x (car test)))
        (if (eqv? x '*)
            '(3 0)
            (if (symbol? x)
                '(2 0)
                (if (num-concept? test)
                    (list 2 (length test))    ;TODO en el futuro habra que calcular un rango, o eso sospecho
                    '(-1)))))))

(define (test-CL>= t1 t2)
  (let* ((grado1 (grado-generalidad? t1))
         (grado2 (grado-generalidad? t2)))
         (if (num-concept? t1)
             (if (not (= (car grado1) (car grado2)))
                 (>= (car grado1) (car grado2))
                 (>= (list-ref grado1 1) (list-ref grado2 1)))
             (>= (car grado1) (car grado2)))))


;--------ej7
(define (concepto-CL>= c1 c2)
  (if (not (test-CL>= (car c1) (car c2)))
      #f
      (if (null? (list-tail c1 1))
          #t
          (concepto-CL>= (list-tail c1 1) (list-tail c2 1)))))

;-------ej8
(define (cmp-concepto-CL c1 c2)
  (if (and (concepto-CL>= c1 c2) (not (concepto-CL>= c2 c1)))
      1
      (if (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1))
          0
          -1)))
          
;-------ej9
(define (reemplaza-elemento lista elem i)
  (if (>= i (length lista))
      '()
      (if (= i 0)
          (cons elem (cdr lista))
          (cons (car lista) (reemplaza-elemento (cdr lista) elem (- i 1))))))

(define (recupera-elemento lista i)
  (if (>= i (length lista))
      '()
      (if (= i 0)
          (car lista)
          (recupera-elemento (cdr lista) (- i 1)))))

(define (gean-rec concepto-CL indice elementos)
  (let ((a-insertar (if (not (list? (car elementos))) (list (car elementos)) (car elementos))))
    (if (null? (list-tail elementos 1))
        (cons (reemplaza-elemento concepto-CL a-insertar indice) '())
        (cons (reemplaza-elemento concepto-CL a-insertar indice) (gean-rec concepto-CL indice (cdr elementos))))))
  
(define (especializaciones-atributo-nominal concepto-CL indice metadatos)
  (let* ((opciones (car (cdr (recupera-elemento metadatos indice))))
         (elemento (recupera-elemento concepto-CL indice)))
    (if (eqv? elemento '())
        (list concepto-CL)
        (if (not (eqv? (car elemento) '*))
            (list (reemplaza-elemento concepto-CL '() indice))
            (gean-rec concepto-CL indice opciones)))))

;-----------ej10
(define (generalizaciones-atributo-nominal concepto-CL indice metadatos)
  (let* ((opciones (car (cdr (recupera-elemento metadatos indice))))
         (elemento (recupera-elemento concepto-CL indice)))
    (if (eqv? elemento '(*))
        (list concepto-CL)
        (if (not (eqv? elemento '()))
            (list (reemplaza-elemento concepto-CL '(*) indice))
            (gean-rec concepto-CL indice opciones)))))



;------------ej11
(define (generaliza-rango test valor-nuevo)
  (if (eqv? test '())
      (list valor-nuevo)
      (let ((test-superior (list (car test) (list valor-nuevo)))
            (test-inferior (if (= (length test) 2) (list (list valor-nuevo) (car (cdr test))) (list (list valor-nuevo) (car test)))))
        (if (pasa-test? test-superior valor-nuevo)
            test-superior
            test-inferior))))

(define (generalizaciones-atributo-numerico concepto-CL indice ejemplo)
  (let* ((test (recupera-elemento concepto-CL indice))
         (valor (recupera-elemento ejemplo indice))
         (clase (recupera-elemento ejemplo (- (length ejemplo) 1))))
    (if (eqv? clase '-)
        (list concepto-CL)
        (if (not (pasa-test? test valor))
            (list (reemplaza-elemento concepto-CL (generaliza-rango test valor) indice))
            (list concepto-CL)))))

;------------ej12
(define (especializaciones-atributo-numerico concepto-CL indice ejemplo)
  (let* ((test (recupera-elemento concepto-CL indice))
         (valor (recupera-elemento ejemplo indice))
         (clase (recupera-elemento ejemplo (- (length ejemplo) 1))))
    (if (eqv? clase '+)
        (list concepto-CL)
        (if (eqv? (car test) '*)
            (gean-rec concepto-CL indice (list (list -inf.0 valor) (list valor +inf.0)))
            (if (not (pasa-test? test valor))
                (list concepto-CL)
                (if (= (length test) 1)
                    (gean-rec concepto-CL indice (list '()))
                    (let* ((iniinc (list? (car test)))
                           (fininc (list? (car (cdr test))))
                           (ini (if iniinc (car (car test)) (car test)))
                           (fin (if fininc (car (car (cdr test))) (car (cdr test))))
                           (coinc-ini (= ini valor))
                           (coinc-fin (= fin valor))
                           (newini (if coinc-ini valor (car test)))
                           (newfin (if coinc-fin valor (car (cdr test)))))
                      (gean-rec concepto-CL indice (list (if (eqv? newini valor) '() (list newini valor)) (if (eqv? valor newfin) '() (list valor newfin)))))))))))

;-------ej13
(define (gene-CL-rec concepto-CL metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (number? (list-ref ejemplo indice))
          (append (generalizaciones-atributo-numerico concepto-CL indice ejemplo) (gene-CL-rec concepto-CL metadatos ejemplo (+ 1 indice)))
          (append (generalizaciones-atributo-nominal concepto-CL indice metadatos) (gene-CL-rec concepto-CL metadatos ejemplo (+ 1 indice))))))

(define (generalizaciones-CL concepto-CL metadatos ejemplo)
  (gene-CL-rec concepto-CL metadatos ejemplo 0))


;----------ej14
(define (espe-CL-rec concepto-CL metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (number? (list-ref ejemplo indice))
          (append (especializaciones-atributo-numerico concepto-CL indice ejemplo) (espe-CL-rec concepto-CL metadatos ejemplo (+ 1 indice)))
          (append (especializaciones-atributo-nominal concepto-CL indice metadatos) (espe-CL-rec concepto-CL metadatos ejemplo (+ 1 indice))))))

(define (especializaciones-CL concepto-CL metadatos ejemplo)
  (espe-CL-rec concepto-CL metadatos ejemplo 0))
;;Ejercicio 15

(define (egs-test-positives PSET H)
  (if (null? PSET)
      #t
      (if (not (match-CL H (drop-right (car PSET) 1)))
          #f
          (egs-test-positives (cdr PSET) H))))

(define (egs-test-negatives NSET H)
  (if (null? NSET)
      #t
      (if (match-CL H (drop-right (car NSET) 1))
          #f
          (egs-test-negatives (cdr NSET) H))))

(define (count-matches H SET COUNT)
  (if (null? SET)
      COUNT
      (if (match-CL H (drop-right (car SET) 1))
          (count-matches H (cdr SET) (+ 1 COUNT))
          (count-matches H (cdr SET) COUNT))))


(define (egs-generate-specs H metadata NSET)
  (if (null? NSET)
     '()
     (append (delete-duplicates (delete H (especializaciones-CL H metadata (car NSET)))) (egs-generate-specs H metadata (cdr NSET)))))
  
  
  

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

(define (compara-generalidad>= H SET)
  (if (null? SET)
      #t
      (if (not (concepto-CL>= H (car SET)))
          #f
          (compara-generalidad>= H (cdr SET)))))

(define (egs-valid-specs SPECS CSET)
  (if (null? SPECS)
      '()
      (if (compara-generalidad>= (car SPECS) CSET)
          (cons (car SPECS) (egs-valid-specs (cdr SPECS) CSET))
          (egs-valid-specs (cdr SPECS) CSET))))

(define (generate-newset HSET NSET CSET metadata)
  (if (null? HSET)
      '()
      (let* ((H (car HSET))
             (NEWSET (append (egs-valid-specs (egs-generate-specs H metadata NSET) CSET) (generate-newset (cdr HSET) NSET CSET metadata))))
        (delete-duplicates NEWSET))))

(define (generate-exclusive-newset-rec HSET NEWSET)
  (if (null? HSET)
      NEWSET
      (let ((NEWSET+ (delete (car HSET) NEWSET)))
        (generate-exclusive-newset-rec (cdr HSET) NEWSET+))))

(define (generate-exclusive-newset HSET NSET CSET metadata)
  (let* ((NEWSET (generate-newset HSET NSET CSET metadata)))
    (generate-exclusive-newset-rec HSET NEWSET)))

(define (get-PSET ejemplos)
  (if (null? ejemplos)
      '()
      (if  (eqv? (list-ref (car ejemplos) (- (length (car ejemplos)) 1)) '+)
           (cons (car ejemplos) (get-PSET (cdr ejemplos)))
           (get-PSET (cdr ejemplos)))))

(define (get-NSET ejemplos)
  (if (null? ejemplos)
      '()
      (if  (eqv? (list-ref (car ejemplos) (- (length (car ejemplos)) 1)) '-)
           (cons (car ejemplos) (get-NSET (cdr ejemplos)))
           (get-NSET (cdr ejemplos)))))



    
(define (EGS0 PSET NSET CSET HSET)
  (let* ((CHSET (eval-hset HSET (cdr PSET) (cdr NSET) CSET '()))
         (CSET+ (car CHSET))
         (HSET+ (car (cdr CHSET))))
    (if (null? HSET+)
        CSET+
        (let ((NEWSET (generate-exclusive-newset HSET+ (cdr NSET) CSET+ (car PSET))))
          (EGS0 PSET NSET CSET+ NEWSET)))))

(define (EGS ejemplos)
  (let* ((metadata (car ejemplos))
         (PSET (cons metadata (get-PSET ejemplos)))
         (NSET (cons metadata (get-NSET ejemplos)))
         (HSET (list (concepto-CL-mas-general (car ejemplos))))
         (CSET (EGS0 PSET NSET '() HSET)))
    (if (not (null? CSET)) (obtener-al-azar CSET) '())))
    

;---------ej17

;(define (count-matches SET H count)
;  (if (null? SET)
;      count
;      (if (not (match-CL H (drop-right (car SET) 1)))
;          (count-matches (cdr SET) count)
;          (count-matches (cdr SET) (+ count 1)))))


(define (score-CL concepto-CL PSET NSET)
 (let* ((PSETsc (cdr PSET))
        (NSETsc (cdr NSET))
        (metadata (car PSET))
        (pmatches (count-matches concepto-CL PSETsc 0))
        (nunmatches (- (length NSETsc) (count-matches concepto-CL NSETsc 0)))
        (total-ejem (+ (length NSETsc) (length PSETsc))))
   (/ (+ pmatches nunmatches) total-ejem)))

;-----------ej18

(define (generate-OPEN-SET CSET S OSET PSET NSET CSETOUT)
  (if (null? CSET)
      (list CSETOUT (cons S OSET))
      (let* ((C (car CSET)))
        (if (<= (cmp-concepto-CL S C) 0)
            (if (< (score-CL S PSET NSET) (score-CL C PSET NSET))
                (list CSETOUT OSET)
                (generate-OPEN-SET (cdr CSET) S OSET PSET NSET CSETOUT))
            (generate-OPEN-SET (cdr CSET) S OSET PSET NSET (cons C CSETOUT))))))

(define (generate-OSET CSET NEWSET OSET PSET NSET)
  (if (null? NEWSET)
      (list CSET OSET)
      (let* ((COSET (generate-OPEN-SET CSET (car NEWSET) OSET PSET NSET '()))
             (OSET+ (list-ref COSET 1))
             (CSET+ (car COSET)))
        (generate-OSET CSET+ (cdr NEWSET) OSET+ PSET NSET))))

(define (eval-SPECS SPECS H PSET NSET NEWSET)
  (if (null? SPECS)
      NEWSET
      (if (> (score-CL (car SPECS) PSET NSET) (score-CL H PSET NSET))
          (eval-SPECS (cdr SPECS) H PSET NSET (cons (car SPECS) NEWSET))
          (eval-SPECS (cdr SPECS) H PSET NSET NEWSET))))

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
                  
(define (get-best-beam BESTSET beamsize)
  (if (or (= 0 beamsize) (null? BESTSET))
      '()
      (cons (car BESTSET) (get-best-beam (cdr BESTSET) (- beamsize 1)))))

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

(define (HGS ejemplos)
  (let* ((metadata (car ejemplos))
         (PSET (cons metadata (get-PSET ejemplos)))
         (NSET (cons metadata (get-NSET ejemplos)))
         (HSET (list (concepto-CL-mas-general (car ejemplos))))
         (CSET (HGS0 PSET NSET '() HSET)))
    (if (not (null? CSET)) CSET '())))


;--------ej20
(define (pasa-concepto-TC? concepto ejemplo-sin-clase correctos)
  (if (null? concepto)
      correctos
  (if (not (pasa-test? (car concepto) (car ejemplo-sin-clase)))
      (pasa-concepto-TC? (list-tail concepto 1) (list-tail ejemplo-sin-clase 1) correctos)
      (pasa-concepto-TC? (list-tail concepto 1) (list-tail ejemplo-sin-clase 1) (+ 1 correctos)))))

(define (match-TC concepto-TC ejemplo-sin-clase)
  (if (or (not (= (length (cdr concepto-TC)) (length ejemplo-sin-clase))) (not (number? (car concepto-TC))))
      #f
      (if (not (validar-concepto (cdr concepto-TC) ejemplo-sin-clase))
          #f
          (>= (pasa-concepto-TC? (cdr concepto-TC) ejemplo-sin-clase 0) (car concepto-TC)))))

;---------------ej21

(define (TCi concepto-TC ejemplo-sin-clase)
  (if (match-TC concepto-TC ejemplo-sin-clase)
      (append ejemplo-sin-clase '(+))
      (append ejemplo-sin-clase '(-))))


;--------------ej22
(define (especializacion-umbral concepto-TC metadatos ejemplo)
  (let* ((len (length (cdr concepto-TC)))
         (umb (car concepto-TC))
         (newumb (if (< umb len) (+ 1 umb) umb))
         (minusumb (if (<= umb 0) 0 (- umb 1))))
    (if (= newumb len)
        '()
        (append (list (cons newumb (cdr concepto-TC))) (map (lambda (x) (cons minusumb x)) (especializaciones-CL (cdr concepto-TC) metadatos ejemplo))))))

(define (espe-TC-rec concepto-TC metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (= -1 indice)
          (append (especializacion-umbral concepto-TC metadatos ejemplo) (map (lambda (x) (cons (car concepto-TC) x)) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice))))
          (if (number? (list-ref ejemplo indice))
              (append (especializaciones-atributo-numerico (cdr concepto-TC) indice ejemplo) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice)))
              (append (especializaciones-atributo-nominal (cdr concepto-TC) indice metadatos) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice)))))))

(define (especializaciones-TC concepto-TC metadatos ejemplo)
  (delete-duplicates (espe-TC-rec concepto-TC metadatos ejemplo -1)))

;----------ej23
(define (traducir meta-atributo valor)
  (if (symbol? (list-ref meta-atributo 1))
      valor
      (index-of (list-ref meta-atributo 1) valor)))

;------------ej24
(define (nuevo-conceptoUU-rec metadatos init)
  (if (null? metadatos)
      '()
      (cons (* init (- (* 2 (random)) 1)) (nuevo-conceptoUU-rec (cdr metadatos) init))))

(define (nuevo-conceptoUU metadatos init)
  (list metadatos (nuevo-conceptoUU-rec metadatos init)))

;---------------ej25
(define (traducir-ejemplo-sin-clase metadatos ejemplo-sin-clase)
  (if (null? ejemplo-sin-clase)
      '()
      (cons (traducir (car metadatos) (car ejemplo-sin-clase)) (traducir-ejemplo-sin-clase (cdr metadatos) (cdr ejemplo-sin-clase)))))

(define (multiply-lists list1 list2)
  (if (null? list1)
      '()
      (cons (* (car list1) (car list2)) (multiply-lists (cdr list1)(cdr list2)))))

(define (sum-list elemList)
  (if
    (null? elemList)
    0
    (+ (car elemList) (sum-list (cdr elemList)))
  )
)

(define (match-LUU conceptoUU ejemplo-sin-clase)
  (let* ((eje-traducido (traducir-ejemplo-sin-clase (car conceptoUU) ejemplo-sin-clase))
         (multiplic (multiply-lists (car (cdr conceptoUU)) (append eje-traducido '(1))))
         (suma (sum-list (drop-right multiplic 1)))
         (umbral (list-ref multiplic (- (length multiplic) 1))))
    (if (>= suma (- umbral))
        #t
        #f)))

;----------ej26
(define (LUUi conceptoUU ejemplo-sin-clase)
  (if (match-LUU conceptoUU ejemplo-sin-clase)
      (append ejemplo-sin-clase '(+))
      (append ejemplo-sin-clase '(-))))

;------------ej27
(define (count-matches-TC H SET COUNT)
  (if (null? SET)
      COUNT
      (if (match-TC H (drop-right (car SET) 1))
          (count-matches-TC H (cdr SET) (+ 1 COUNT))
          (count-matches-TC H (cdr SET) COUNT))))

(define (score-TC concepto-TC PSET NSET)
 (let* ((PSETsc (cdr PSET))
        (NSETsc (cdr NSET))
        (metadata (car PSET))
        (pmatches (count-matches-TC concepto-TC PSETsc 0))
        (nunmatches (- (length NSETsc) (count-matches-TC concepto-TC NSETsc 0)))
        (total-ejem (+ (length NSETsc) (length PSETsc))))
   (/ (+ pmatches nunmatches) total-ejem)))

;------------ej28
(define (find-counter lista elemento)
  (if (null? lista)
      '()
      (if (eqv? (car (car lista) elemento))
          (car lista)
          (find-counter (cdr lista) elemento))))

(define (get-index-counter lista elemento index)
  (if (>= index (length lista))
      -1
      (if (eqv? (car (list-ref lista index)) elemento)
          index
          (get-index-counter lista elemento (+ 1 index)))))

(define (create-counter element lista n)
  (if (null? lista)
      (cons element n)
      (if (eqv? (car lista) element)
          (create-counter element (cdr lista) (+ 1 n))
          (create-counter element (cdr lista) n))))

(define (contador>? c1 c2)
  (if (null? c2)
      #t
      (> (cdr c1) (cdr c2))))

(define (get-moda lista contadores posicion)
  (if (>= posicion (length lista))
      (car (car (sort contadores contador>?)))
      (let* ((elem (list-ref lista posicion))
             (indice-elem (get-index-counter contadores elem 0))
             (contador (if (>= indice-elem 0) (list-ref contadores indice-elem) (create-counter elem lista 0))))
        (if (< indice-elem 0)
            (get-moda lista (cons contador contadores) (+ 1 posicion))
            (get-moda lista contadores (+ 1 posicion))))))


;(define (HTC0 PSET NSET CSET HSET)
;  <codigo>)


;---------ej30
(define (PRM concepto-UU ejemplos)
  (if (null? ejemplos)
      concepto-UU
      (let* ((ejemplos+ (if (list? (car (car ejemplos))) (cdr ejemplos) ejemplos)) ;eliminamos la cabecera si la hay
             (I (car ejemplos+))
             (I-sin-clase (drop-right I 1))
             (I-sc-trad (traducir-ejemplo-sin-clase (car concepto-UU) I-sin-clase))
             (C (list-ref I (- (length I) 1)))
             (P (list-ref (LUUi concepto-UU I-sin-clase) (- (length I) 1)))
             (S (if (and (eqv? P '-) (eqv? C '+)) 1 (if (and (eqv? P '+) (eqv? C '-)) -1 0))))
        (if (not (eqv? C P))
            (let ((newconcept  (list (car concepto-UU) (map (lambda (w v) (+ w (* v S 0.4))) (cadr concepto-UU) (append I-sc-trad '(1)))))) ;Actualizamos los pesos de la hipotesis
              (PRM newconcept (cdr ejemplos+)))
            (PRM concepto-UU (cdr ejemplos+))))))


;--------------ej31

(define (all-classified? H ejemplos)
  (if (null? ejemplos)
      #t
      (let* ((I (car ejemplos))
             (C (list-ref I (- (length I) 1)))
             (P (list-ref (LUUi H (drop-right I 1)) (- (length I) 1))))
        (if (not (eqv? C P))
            #f
            (all-classified? H (cdr ejemplos))))))

(define COUNT 1000)
(define COUNT2 0)
(define (PCP-loop H ejemplos-sc)
  (if (<= COUNT2 0)
      H
      (let* ((bien-clasificado (all-classified? H ejemplos-sc)))
        (if bien-clasificado
            H
            (let ()
              (set! COUNT2 (- COUNT2 1))
              (PCP-loop (PRM H ejemplos-sc) ejemplos-sc))))))

(define (PCP ejemplos)
  (let* ((metadatos (car ejemplos))
         (ejemplos-sc (cdr ejemplos))
         (H (nuevo-conceptoUU metadatos 1)))
    (set! COUNT2 COUNT)
    (PCP-loop H ejemplos-sc)))


;------------------ej32

;Si el valor devuelto > 0 la clase es positiva, si es < 0 es negativa
(define (predict-LUU conceptoUU ejemplo-sin-clase)
  (let* ((eje-traducido (traducir-ejemplo-sin-clase (car conceptoUU) ejemplo-sin-clase))
         (multiplic (multiply-lists (car (cdr conceptoUU)) eje-traducido))
         (suma (sum-list multiplic)))
    (/ 1 (+ 1 (exp (/ (- suma) 4)))))) ;Funcion sigmoide. Temos que hacer Umbral (siempre 0.5, en el interpretre) menos el resto. Al sumar todo de golpe nosotros hacemos el resto-umbra, deahi el cambio de signo


(define (calcular-SSE-LUU H ISET SSEacumulado)
  (if (null? ISET)
      SSEacumulado
      (let* ((I (car ISET))
             (C (list-ref I (- (length I) 1)))
             (O (if (eqv? C '+) 1 0))
             (P (predict-LUU H (drop-right I 1)))
             (SSE+ (+ SSEacumulado (expt (- O P) 2))))
        (calcular-SSE-LUU H (cdr ISET) SSE+))))

(define (calcular-vector-gradiente H ISET grad)
  (if (null? ISET)
      grad
      (let* ((I (car ISET)) 
             (I-sc (drop-right I 1)) ;ejemplo sin clase
             (I-sc-trad (traducir-ejemplo-sin-clase (car H) I-sc)) ;ejemplo traducido
             (C (list-ref I (- (length I) 1))) ;literal de la clase
             (O (if (eqv? C '+) 1 0)) ;observed class (1 para positivo 0 negativo)
             (P (predict-LUU H I-sc)) ;predicted class(salida de una sigmoide entre 1 y 0)
             (ita 0.05)   ;learning factor
             (e (* P (- 1 P) (- O P))) ;error
             (grad-parcial (map (lambda (x) (* ita e x)) I-sc-trad))
             (nueva-grad (map (lambda (x y) (+ x y)) grad-parcial grad)))
        (calcular-vector-gradiente H (cdr ISET) nueva-grad))))

(define (LMS-loop H ISET vec-delta-old)
  (if (<= COUNT2 0)
      H
      (let* ((min-err 0.01)  ;En terminos de MSE
             (MSE (/ (calcular-SSE-LUU H ISET 0) (length ISET)))
             (alfa 0.05))
        (if (< MSE min-err)
            H
            (let* ((GRAD (calcular-vector-gradiente H ISET (build-list (length (cadr H)) (lambda (x) 0.0))))
                   (vec-delta-new (map (lambda (x y) (+ x (* alfa y))) GRAD vec-delta-old))  ;Se calcula el nuevo vector del ta segun la gradiente y la inercia
                   (H+ (list (car H) (map (lambda (x y) (+ x y)) (cadr H) vec-delta-new))))  ;Se actualiza la hipotesis con el nuevo vector delta
              (set! COUNT2 (- COUNT2 1))
              (LMS-loop H+ ISET vec-delta-new))))))
        


(define (LMS ejemplos)
  (let* ((metadatos (car ejemplos))
         (ejemplos-sc (cdr ejemplos))
         (H (nuevo-conceptoUU metadatos 0.2))
         (H+ (list (car H) (drop-right (cadr H) 1))) ;eliminamos el umbral, está definido de forma constante en la sigmoide
         (vec-delta-anterior (build-list (length (cadr H+)) (lambda (x) 0.0))))
    (set! COUNT2 COUNT)
    (let ((Hnew (LMS-loop H+ ejemplos-sc vec-delta-anterior)))
      (list (car Hnew) (append (cadr Hnew) '(-0.5))))))
         
(define (LUUi-LMS conceptoUU ejemplo-sin-clase)
  (if (> (predict-LUU conceptoUU ejemplo-sin-clase) 0.5)
      (append ejemplo-sin-clase '(+))
      (append ejemplo-sin-clase '(-))))
;------------ej34

(define (distancia-valor valor1 valor2)
  (if (and (number? valor1) (number? valor2))
      (- valor1 valor2)
      (if (eqv? valor1 valor2)
          0
          1)))


(define (distancia-rec ej1 ej2)
  (if (null? ej1)
      0
      (+ (expt (distancia-valor (car ej1) (car ej2)) 2) (distancia-rec (cdr ej1) (cdr ej2)))))

(define (distancia ejemplo-sin-clase ejemplo)
  (let* ((ejsc ejemplo-sin-clase)
         (ej2 (if (< (length ejsc) (length ejemplo)) (drop-right ejemplo 1) ejemplo)))
    (sqrt (distancia-rec ejsc ej2))))


;----------------ej35

(define (IBi-rec concepto-sc ejsc vecino-hasta-ahora)
  (if (null? concepto-sc)
      (append ejsc (list-tail vecino-hasta-ahora (- (length vecino-hasta-ahora) 1)))
      (let* ((vecino-actual (if (eqv? '() vecino-hasta-ahora) (car concepto-sc) vecino-hasta-ahora))
             (vecino-maybe (car concepto-sc))
             (distancia-actual (distancia ejsc vecino-actual))
             (distancia-maybe (distancia ejsc vecino-maybe)))
        (if (< distancia-maybe distancia-actual)
            (IBi-rec (cdr concepto-sc) ejsc vecino-maybe)
            (IBi-rec (cdr concepto-sc) ejsc vecino-actual)))))

(define (IBi concepto-IB ejemplo-sin-clase)
  (if (list? (car (car concepto-IB)))
      (IBi-rec (cdr concepto-IB) ejemplo-sin-clase '())  ;quitamos la cabecera si la tiene
      (IBi-rec concepto-IB ejemplo-sin-clase '())))


;-----------ej36
(define (match-IB concepto-IB ejemplo-sin-clase)
  (let ((resultado (IBi concepto-IB ejemplo-sin-clase)))
    (if (eqv? (list-ref resultado (- (length resultado) 1)) '-)
        #f
        #t)))

;--------ej37
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


(define (actualiza-atributo-NB-nominal atributo-NB valor)
  (if (null? atributo-NB)
      '()
      (if (eqv? (car (car atributo-NB)) valor)
          (cons (cons (caar atributo-NB) (+ 1 (cdr (car atributo-NB)))) (cdr atributo-NB))
          (cons (car atributo-NB) (actualiza-atributo-NB-nominal (cdr atributo-NB) valor)))))

(define (actualiza-atributo-NB-numerico atributo-NB valor)
  (list 'numerico (+ (list-ref atributo-NB 1) valor) (+ (list-ref atributo-NB 2) (expt valor 2))))

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
  

(define (INB concepto-NB ejemplo)
  (if (null? concepto-NB)
      '()
      (let* ((cnb (car concepto-NB))
             (clase (list-ref ejemplo (- (length ejemplo) 1))))
        (if (eqv? (car cnb) clase)
            (cons (actualiza-clase-NB cnb (drop-right ejemplo 1)) (cdr concepto-NB))
            (cons cnb (INB (cdr concepto-NB) ejemplo))))))

;-----------Ej38

(define (NB-rec CNB ejemplos-scab)
  (if (null? ejemplos-scab)
      CNB
      (NB-rec (INB CNB (car ejemplos-scab)) (cdr ejemplos-scab))))
(define (NB ejemplos)
  (let* ((metadatos (car ejemplos))
         (ejemplos-scab (cdr ejemplos))
         (conceptoNB (nuevo-conceptoNB metadatos)))
    (NB-rec conceptoNB ejemplos-scab)))

;------ej39
;Divide el sumatorio de elemento x entre n
(define (media x n)
 (/ x n))

(define (varinza x2 m n)
  (- (/ x2 n) (expt m 2)))

;-------------------ej40

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
    
        

(distancia '(0 cocos 1 1) '(0 0 2 5 -))
(distancia '(0 cocos 1 1) '(0 cocos 2 5 -))
(distancia '(0 0 1 1) '(0 0 2 5 -))

(define megaejemplos (mezclar ejemplos ejemplos2))
(define PSET (get-PSET megaejemplos))
(define NSET (get-NSET megaejemplos))
(define HSET (list (concepto-CL-mas-general (car ejemplos))))
(define H (car HSET))
(define metadatos (car ejemplos))
(define PSETcc (cons (car ejemplos) PSET))
(define NSETcc (cons (car ejemplos) NSET))
(define SPECS (delete-duplicates (egs-generate-specs (car HSET) (car ejemplos) NSET)))
(define NEWSET2 (eval-SPECS SPECS (car HSET) PSETcc NSETcc '()))

;(nuevo-conceptoNB metadatos)

;(score-CL (car HSET) PSETcc NSETcc)
;(list-ref SPECS 1)
;(score-CL (list-ref SPECS 1) PSETcc NSETcc)
;(list-ref SPECS 1)
;(score-TC (cons 1 '((soleado) (5 31) (23 80) (*) (bajo) (no))) PSETcc NSETcc)
;(match-TC '(4 (soleado)(*)(10 40)(si)) '(soleado 30 40 si))
;(match-TC '(3 (soleado)(*)(10 40)(si)) '(soleado 30 40 si))
;(match-TC '(2 (soleado)(*)(10 (40))(si)) '(soleado 30 40 si))
;(match-TC '(4 (soleado)(*)(10 40)(si)) '(soleado 30 25 no))
;(match-TC '(3 (soleado)(*)(10 40)(si)) '(30 soleado 25 si))
;
;(TCi '(4 (soleado)(*)(10 40)(si)) '(soleado 30 40 si))
;(TCi '(3 (soleado)(*)(10 40)(si)) '(soleado 30 40 si))
;(TCi '(2 (soleado)(*)(10 (40))(si)) '(soleado 30 40 si))
;(TCi '(4 (soleado)(*)(10 40)(si)) '(soleado 30 25 no))
;(TCi '(3 (soleado)(*)(10 40)(si)) '(30 soleado 25 si))
;(car (cdr ejemplos))
;(cons (length H) H)
;(especializaciones-TC (cons 3 H) metadatos (car (cdr ejemplos)))
(define 1ejemplo (car (cdr ejemplos)))
(define HUU (nuevo-conceptoUU metadatos 1))
(define HUU+ (list (car HUU) (drop-right (cadr HUU) 1)))
(set! HSET (cons '((*) (69 420) (*) (*) (*) (*)) HSET))
;HSET
;(generate-newset HSET NSET '() metadatos)
1ejemplo

(define concNB (nuevo-conceptoNB metadatos))
;concNB
(INB concNB 1ejemplo)
(define conNBfull (NB ejemplos))
(probabilidades '+ conNBfull (drop-right 1ejemplo 1))

;(IBi ejemplos '(soleado 25 35 si medio no))
;(match-IB ejemplos '(soleado 25 35 si medio no))
;HUU
;HUU+

;(calcular-vector-gradiente HUU+ (cdr megaejemplos) (build-list (length (cadr HUU+)) (lambda (x) 0.0)))

;(PRM HUU megaejemplos)
;(PCP megaejemplos)
;(LMS megaejemplos)
;(cross-validation PCP LUUi lepiota 4)
;(cross-validation LMS LUUi lepiota 3)
;(match-LUU (list metadatos '(0 1 1 0 0 0 -105)) (drop-right 1ejemplo 1))
;(match-LUU (list metadatos '(0 1 1 0 0 0 -106)) (drop-right 1ejemplo 1))

;(LUUi (list metadatos '(0 1 1 0 0 0 -105)) (drop-right 1ejemplo 1))
;(LUUi (list metadatos '(0 1 1 0 0 0 -106)) (drop-right 1ejemplo 1))

;(create-full-counter '(1 2 3 3 3 2 1 1 1 1) '() 0)
;(define contadores (list (cons 1 5) (cons 1 4) (cons 1 3) (cons 1 2)))
;(contador>? (list-ref contadores 1) (list-ref contadores 0))


;(traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)
;(traducir '(perspectiva numerico) '15)
;(nuevo-conceptoUU metadatos 3)
;(car (cdr ejemplos))
;metadatos
;(traducir-ejemplo-sin-clase metadatos (drop-right (car (cdr ejemplos)) 1))
;lepiota
;ionosphere
(trace HGS0)
;(HGS ionosphere)
;PSET
;NSET
;HSET
;(length (generate-exclusive-newset HSET NSET '() (car megaejemplos)))
;(car (cdr (eval-hset (egs-generate-specs (car HSET) (car megaejemplos) NSET) PSET NSET '() '())))
(define HSET+ (car (cdr (eval-hset (generate-exclusive-newset HSET NSET '() (car megaejemplos)) PSET NSET '() '()))))
(define NEWSET (generate-exclusive-newset HSET+ NSET '() (car megaejemplos)))
;HSET
;HSET+
;NEWSET
;(eval-hset NEWSET PSET NSET '() '())
;(define esencia (IA1 ejemplos))
;(define ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) (list-tail ejemplos 1)))
;(define extension (map (lambda(x) (A1i esencia x)) ejemplos-sin-clase))
;megaejemplos


;(especializaciones-atributo-nominal '(() ((24) (25)) (80) (no) (medio) (no)) 0 (car ejemplos))
;(generalizaciones-atributo-nominal '(() ((24) (25)) (80) (no) (medio) (no)) 0 (car ejemplos))
;(generalizaciones-CL '((*) (24) (80) (no) (medio) (no)) (car ejemplos) '(nublado 25 89 no medio no +))

;(especializaciones-CL '((*) ((24) 30) (80) (no) (medio) (no)) (car ejemplos) '(nublado 24 89 no medio no -))



;(num-concept? '(1 +inf.0))
;(pasa-test? '((1) +inf.0) 1)
;(car (cdr ejemplos))
;(pasa-test? '((24) 25) 25)
;(generalizaciones-atributo-numerico '((nublado) (0 60) (80) (no) (medio) (no)) 1 '(nublado 83 83 no medio no +))
;(especializaciones-atributo-numerico '((nublado) (25) (80) (no) (medio) (no)) 1 '(nublado 25 80 no medio no -))
;(especializaciones-atributo-numerico '((nublado) (0 60) (80) (no) (medio) (no)) 1 '(nublado 25 80 no medio no -))
;(especializaciones-atributo-numerico '((nublado) ((0) (60)) (80) (no) (medio) (no)) 1 '(nublado 25 80 no medio no -))
;(especializaciones-atributo-numerico '((nublado) (*) (80) (no) (medio) (no)) 1 '(nublado 25 80 no medio no -))

;(define separados (separar 0.666 megaejemplos))
;(define training (cons (list-ref separados 0) (list-ref separados 1)))
;(define test (list-ref separados 2))
;ejemplos2
;training
;test
;(reemplaza-elemento (cdr ejemplos) '(cacafuti) 2)
;(concepto-CL-mas-general (car ejemplos))
;(concepto-CL-mas-especifico (car ejemplos))
;
;(test-CL>= '(*) '())
;(test-CL>= '(*) '(*))
;(test-CL>= '() '(*))
;(test-CL>= '(hola) '(quetal))
;(test-CL>= '(1) '(2))
;(test-CL>= '(1 2) '(2))
;(test-CL>= '(1) '(2 3))

;(cmp-concepto-CL '((lluvioso)(2 10)) '((soleado)(20)))
;(cmp-concepto-CL '((lluvioso)(2 15)) '((soleado)(10 20)))
;(cmp-concepto-CL '((lluvioso)(23)) '((soleado)(10 25)))

;(define stratest '((1 -)(1 -)(1 +)(1 +)(1 +)(1 +)(1 +)(1 +)))
;(define ogratio (ratio-clases '(+ -) stratest))
;ogratio
;
;(define elementos '())
;(do ((x 1000 (- x 1)))
;  ((= x 0) elementos)
;  (set! elementos
;        (cons (obtener-al-azar-straty '((a -)(a -)(a +)(a +)) ogratio '(+ -)) elementos)))
;;
;elementos
;(map (lambda (x)
;       (cons x (count (lambda(y) (listas-iguales? x y)) elementos)))
;     '((a -)(a +)))
;

;(map (lambda (x) (get-type x)) (list-ref ejemplos 1))
;(CLi '((nubladgo) ((24) (25)) (80) (no) (medio) (no)) '(nublado 25 80 no medio no))
;(match-CL '((nublado) ((25) (26)) (80) (no) (medio) (no)) '(nublado 25 80 no medio no))
;(match-CL '((nublado) (25) (80) (no) (medio) (no)) '(nublado 25 80 no medio no))
;(match-CL '((*) (25) (80) (no) (medio) (no)) '(nublado 25 80 no medio no))
;(listas-iguales? '(a -) '(a -))
;(define megafold (stratify 10 megaejemplos))
;(stratified-cross-validation A0 A0i megaejemplos 3)
;(cross-validation A0 A0i megaejemplos 2)
;(leave-one-out A0 A0i megaejemplos)
;(= (cross-validation A0 A0i megaejemplos 2) (stratified-cross-validation A0 A0i megaejemplos 2))
;megaejemplos
;(cdr megafold)
;(unir-folds (car megafold) (cdr megafold) '())
;(holdout A0 A0i training test)
;(leave-one-out A0 A0i ejemplos)
;(define concepto '((+ . 0) (- . 0)))
;(IIA1 (car ejemplos) concepto (car (cdr ejemplos2)))
