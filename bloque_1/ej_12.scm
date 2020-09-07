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
  (append ejemplo-sin-clase (obtener-al-azar concepto)))




(define esencia (IA1 ejemplos))
(define ejemplos-sin-clase (map (lambda(x) (drop-right x 1)) (list-tail ejemplos2 1)))
(define extension (map (lambda(x) (A1i esencia x)) ejemplos-sin-clase))
esencia
ejemplos2
extension

;(define concepto '((+ . 0) (- . 0)))
;(IIA1 (car ejemplos) concepto (car (cdr ejemplos2)))

;ejemplos

;(separar 0.33 ejemplos)

;(atributo 'clase ejemplos)



;(ratio-clases '(+ -) ejemplos)

;(stratify 6 ejemplos)

;(folds 3 ejemplos)
