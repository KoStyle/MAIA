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
          (append (list (generalizaciones-atributo-numerico concepto-CL indice ejemplo)) (gene-CL-rec concepto-CL metadatos ejemplo (+ 1 indice)))
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
             (NEWSET (egs-valid-specs (egs-generate-specs H metadata NSET) CSET)))
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
(define (especializacion-umbral concepto-TC)
  (let* ((len (length (cdr concepto-TC)))
         (umb (car concepto-TC))
         (newumb (if (< umb len) (+ 1 umb) umb)))
    (if (= newumb len)
        '()
        (list (cons newumb (cdr concepto-TC))))))

(define (espe-TC-rec concepto-TC metadatos ejemplo indice)
  (if (= (- (length ejemplo) 1) indice)
      '()
      (if (= -1 indice)
          (append (especializacion-umbral concepto-TC) (map (lambda (x) (cons (car concepto-TC) x)) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice))))
          (if (number? (list-ref ejemplo indice))
              (append (especializaciones-atributo-numerico (cdr concepto-TC) indice ejemplo) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice)))
              (append (especializaciones-atributo-nominal (cdr concepto-TC) indice metadatos) (espe-TC-rec concepto-TC metadatos ejemplo (+ 1 indice)))))))

(define (especializaciones-TC concepto-TC metadatos ejemplo)
  (espe-TC-rec concepto-TC metadatos ejemplo -1))

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
  (cons metadatos (nuevo-conceptoUU-rec metadatos init)))

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
;(score-CL (car HSET) PSETcc NSETcc)
;(list-ref SPECS 1)
;(score-CL (list-ref SPECS 1) PSETcc NSETcc)
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
;(especializaciones-TC (cons 4 H) metadatos (car (cdr ejemplos)))

;(traducir '(perspectiva (soleado nublado lluvioso)) 'lluvioso)
;(traducir '(perspectiva numerico) '15)
(nuevo-conceptoUU metadatos 3)

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
