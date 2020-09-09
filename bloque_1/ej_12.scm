#lang racket
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
  (if (list? x)
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
  (if (not (or (num-concept? (car concepto)) (symbol? (car (car concepto)))))
      #f
      (if (null? (list-tail concepto 1))
          #t
          (concepto-bien-formado? (list-tail concepto 1)))))

(define (validar-concepto concepto ejemplo-sin-clase)
  (if (not (concepto-bien-formado? concepto))
      #f
      (if (not (eqv? (get-type (car concepto)) (get-type (car ejemplo-sin-clase))))
          #f
          (if (null? (list-tail concepto 1))
              #t
              (validar-concepto (list-tail concepto 1) (list-tail ejemplo-sin-clase 1))))))
          

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
                        (if (and (<= ini valor-ejemplo) (>= fin valor-ejemplo))
                            #t
                            #f)
                        '()))))
        (= ini valor-ejemplo))))

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
      '(1)
      (let* ((x (car test)))
        (if (eqv? x '*)
            '(3)
            (if (symbol? x)
                '(2)
                (if (num-concept? test)
                    (list 2 (length test))    ;TODO en el futuro habra que calcular un rango, o eso sospecho
                    '(-1)))))))

(define (test-CL>= t1 t2)
  (let* ((grado1 (grado-generalidad? t1))
         (grado2 (grado-generalidad? t2)))
         (if (num-concept? t1)
             (>= (+ (car grado1) (list-ref grado1 1)) (+ (car grado2) (list-ref grado2 1)))
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
  (let ((a-insertar (if (symbol? (car elementos)) (list (car elementos)) (car elementos))))
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
        concepto-CL
        (if (not (pasa-test? test valor))
            (reemplaza-elemento concepto-CL (generaliza-rango test valor) indice)
            concepto-CL))))

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
                    (gean-rec concepto-CL indice (list (list (car test) valor) (list valor (car (cdr test)))))))))))
                
                

(define megaejemplos (mezclar ejemplos ejemplos2))
;(define esencia (IA1 ejemplos))
;(define ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) (list-tail ejemplos 1)))
;(define extension (map (lambda(x) (A1i esencia x)) ejemplos-sin-clase))
;megaejemplos


;(especializaciones-atributo-nominal '(() ((24) (25)) (80) (no) (medio) (no)) 0 (car ejemplos))
(generalizaciones-atributo-nominal '(() ((24) (25)) (80) (no) (medio) (no)) 0 (car ejemplos))


;(num-concept? '(1 +inf.0))
;(pasa-test? '((1) +inf.0) 1)
(car (cdr ejemplos))
(pasa-test? '((24) 25) 25)
;(generalizaciones-atributo-numerico '((nublado) (0 60) (80) (no) (medio) (no)) 1 '(nublado 25 80 no medio no +))
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

;ejemplos

;(separar 0.33 ejemplos)

;(atributo 'clase ejemplos)



;(ratio-clases '(+ -) ejemplos)

;(stratify 6 ejemplos)

;(folds 3 ejemplos)
