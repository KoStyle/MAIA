;;Plantilla b2-e30-e33.scm
;;Autor: Manuel Konomi Pilkati

;;Ejercicio 30
(he-tardado 40 'b2-e30)

;Esta funcion recibe un concepto-UU y un set de ejemplos.
;Recorre recursivamente la lista de ejemplos, y en cada ejemplo, extrae su clase, calcula la prediccion del concepto, y calcula el signo del incremento de los pesos.
;Si no ha habido match, aplica a todos los pesos del vector la actualizacion de W = W + (V* S * n) donde V es el valor del ejemplo para ese peso y S es el signo calculado.
;Por ultimo, n es el factor de aprendizaje, para el que usamos 0.4
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

;;Ejercicio 31
(he-tardado 25 'b2-e31)

;Devuelve true si todos los ejemlos del set son clasificados correctamente por H
(define (all-classified? H ejemplos)
  (if (null? ejemplos)
      #t
      (let* ((I (car ejemplos))
             (C (list-ref I (- (length I) 1)))
             (P (list-ref (LUUi H (drop-right I 1)) (- (length I) 1))))
        (if (not (eqv? C P))
            #f
            (all-classified? H (cdr ejemplos))))))

(define COUNT 1000) ;Este contador controla cuantos ciclos haremos con PCP y con LMS
(define COUNT2 0) ;Este contador es el que utilizan internamente PCP y LMS. Cuando arrancan lo actualizan al valor de COUNT

;Esta funcion comprueba si H clasifica bien todos los ejemplos, si no lo hace, decrementa COUNT2, ejecuta PRM actualizando H y entra en recursion.
(define (PCP-loop H ejemplos-sc)
  (if (<= COUNT2 0)
      H
      (let* ((bien-clasificado (all-classified? H ejemplos-sc)))
        (if bien-clasificado
            H
            (let ()
              (set! COUNT2 (- COUNT2 1))
              (PCP-loop (PRM H ejemplos-sc) ejemplos-sc))))))

;Esta funcion llama a PCP-loop tras generar un concepto general, y separar los metadatos de los ejemplos. Tambien reestablece el valor de COUNT2
(define (PCP ejemplos)
  (let* ((metadatos (car ejemplos))
         (ejemplos-sc (cdr ejemplos))
         (H (nuevo-conceptoUU metadatos 1)))
    (set! COUNT2 COUNT)
    (PCP-loop H ejemplos-sc)))

;;Ejercicio 32
(he-tardado 90 'b2-e32)
;;Se ha eliminado este segundo (define COUNT) que choca con el de arriba

;Similar a la funcion match-UU, pero la salida no es un booleano, si no que se suma todo el vector (llegará sin umbral) y se utiliza en
;la funcion sigmoide, para tener una transicion continua entre el 1 y el 0. Se divide la suma entre 4 para que la zona de transicion entre el 1 y el 2 sea mas amplia
;en el eje x
(define (predict-LUU conceptoUU ejemplo-sin-clase)
  (let* ((eje-traducido (traducir-ejemplo-sin-clase (car conceptoUU) ejemplo-sin-clase))
         (multiplic (multiply-lists (car (cdr conceptoUU)) eje-traducido))
         (suma (sum-list multiplic)))
    (/ 1 (+ 1 (exp (/ (- suma) 4)))))) ;Funcion sigmoide. Temos que hacer Umbral (siempre 0.5, en el interpretre) menos el resto. Al sumar todo de golpe nosotros hacemos el resto-umbra, deahi el cambio de signo

;Esta funcion calcula la suma de errores cuadrados (Sum Square Error) que hay para cada clase de un ejemplo I frente a la prediccion de la clase de H.
(define (calcular-SSE-LUU H ISET SSEacumulado)
  (if (null? ISET)
      SSEacumulado
      (let* ((I (car ISET))
             (C (list-ref I (- (length I) 1)))
             (O (if (eqv? C '+) 1 0))
             (P (predict-LUU H (drop-right I 1)))
             (SSE+ (+ SSEacumulado (expt (- O P) 2))))
        (calcular-SSE-LUU H (cdr ISET) SSE+))))

;Esta funcion calculula el valor de gradiente para cada atributo de HSET en base al error (segun la formula e= P*(1-P)*(O-P) donde P es la prediccion para I y O es la clase para I)
; calculado. Se acumulan los valores de gradiente de todos los ejemplos I y finalmente se devuelven
(define (calcular-vector-gradiente H ISET grad)
  (if (null? ISET)
      grad
      (let* ((I (car ISET)) 
             (I-sc (drop-right I 1)) ;ejemplo sin clase
             (I-sc-trad (traducir-ejemplo-sin-clase (car H) I-sc)) ;ejemplo traducido
             (C (list-ref I (- (length I) 1))) ;literal de la clase
             (O (if (eqv? C '+) 1 0)) ;observed class (1 para positivo 0 negativo)
             (P (predict-LUU H I-sc)) ;predicted class(salida de una sigmoide entre 1 y 0)
             (ita 0.99)   ;learning factor
             (e (* P (- 1 P) (- O P))) ;error
             (grad-parcial (map (lambda (x) (* ita e x)) I-sc-trad)) ;(n*e*v) donde n es el learning factor, e es el error y v el valor de I en esa posicion
             (nueva-grad (map (lambda (x y) (+ x y)) grad-parcial grad))) 
        (calcular-vector-gradiente H (cdr ISET) nueva-grad))))


;El bucle principal de LMS. Por cada ciclo, calcula el MSE (mean square error) que es el SSE del H actual dividido entre la cantidad de ejemplos.
;Si el mse es mejor que el error minimo (puesto de forma estatica a 0.01), se devuelve H, si no, se itera de nuevo.
;Si no se cumple el minimo, se calcula el vector de gradiente para todos los pesos, se le suma el vector de incremento de pesos de la iteracion anterior (si existe)
;multiplicado por alfa (la constante de momento, establecida a 0.2). Cone esto obtenemos el nuevo vector de incremento, se añade a los pesos de H y se itera.
(define (LMS-loop H ISET vec-delta-old)
  (if (<= COUNT2 0)
      H
      (let* ((min-err 0.01)  ;En terminos de MSE
             (MSE (/ (calcular-SSE-LUU H ISET 0) (length ISET)))
             (alfa 0.2))
        (if (< MSE min-err)
            H
            (let* ((GRAD (calcular-vector-gradiente H ISET (build-list (length (cadr H)) (lambda (x) 0.0))))
                   (vec-delta-new (map (lambda (x y) (+ x (* alfa y))) GRAD vec-delta-old))  ;Se calcula el nuevo vector del ta segun la gradiente y la inercia
                   (H+ (list (car H) (map (lambda (x y) (+ x y)) (cadr H) vec-delta-new))))  ;Se actualiza la hipotesis con el nuevo vector delta
              (set! COUNT2 (- COUNT2 1))
              (LMS-loop H+ ISET vec-delta-new))))))
        

;Desde aqui separamos el set en metadatos y ejemplos. Se calcula el H mas generico y se le elimina el umbral (H+) y se llama a LMS-loop.
;Al resultado de LMS-loop se le añade el umbral 0.5 (lo metemos como -0.5 porque en la estructura de datos se guarda en negativo, para que funcione el interprete)
(define (LMS ejemplos)
  (let* ((metadatos (car ejemplos))
         (ejemplos-sc (cdr ejemplos))
         (H (nuevo-conceptoUU metadatos 0.2))
         (H+ (list (car H) (drop-right (cadr H) 1))) ;eliminamos el umbral, está definido de forma constante en la sigmoide
         (vec-delta-anterior (build-list (length (cadr H+)) (lambda (x) 0.0))))
    (set! COUNT2 COUNT)
    (let ((Hnew (LMS-loop H+ ejemplos-sc vec-delta-anterior)))
      (list (car Hnew) (append (cadr Hnew) '(-0.5))))))
;;Ejercicio 33
(he-tardado <minutos> 'b2-e33)
;;Tras realizar el cross-validation con los sets de ionosphere y lepiota, estos son lso resultados obtenidos (aproximados
; Ionosphere: tanto LMS como PCP se encuentran en el rango del 83-87% de exito en clasificación, sin mucha diferencia entre ejecuciones.
; Lepiota: En este casi, PCP alcanca de forma bastante constante porcentajes de acierto en el rango de los 90-94%, mientras que LMS:
;          Tiene problemas para clasificar si se usan los valores que se usaban incialmente (n=0.99 y alfa=0.2). Jugando con estos dos parametros, se consigue clasificar
;          En el rango de los 74-78, pero con menos constancia de lo que lo hace PCP o LMC con ionosphere.
;          Esto puede deberse a la naturaleza de los datos de lepiota. Todos los atributos son nominales, y las entradas son numeros enteros en el rango de 0 al 5 o al 6, mucho
;          Mas grande que Ionosphere. Esto, combinado con un factor de aprendizaje y un alfa alto, puede causar que el algoritmo actualice los pesos con valores demasiado grande
;          Perjudicando la capacidad de exploración del espacio de busqueda.



