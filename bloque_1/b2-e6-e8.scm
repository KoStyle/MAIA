;;Plantilla b2-e6-e8.scm
;;Autor: Manuel Konomi Pilkati
;necesita de los ficheros anteriores para funciones como num-concept?

;;Ejercicio 6
(he-tardado 20 'b2-e6)

;Esta funcion revuelve una lista de dos elementos en el que se contiene el nivel de generalidad, con el siguiente formato:
; (X Y) donde X es el nivel principal de generalidad, cuanto mas alto mas general el test
; e Y es el nivel secundario de generalidad, necesrio para distinguir test numericos que desriben rangos de valores (e.g. (3 (9))) y test concretos (e.g. (4))
(define (grado-generalidad? test)
  (if (eqv? test '())
      '(1 0)
      (let* ((x (car test)))
        (if (eqv? x '*)
            '(3 0)
            (if (symbol? x)
                '(2 0)
                (if (num-concept? test)
                    (list 2 (length test))   
                    '(-1)))))))
;Esta funcion determina si el test t1 tiene mayor o igual generalidad que t2, para esto se utiliza la funcion anterior que calcula los
;grados de generalidad de cada test.
(define (test-CL>= t1 t2)
  (let* ((grado1 (grado-generalidad? t1))
         (grado2 (grado-generalidad? t2)))
         (if (num-concept? t1)
             (if (not (= (car grado1) (car grado2)))
                 (>= (car grado1) (car grado2))
                 (>= (list-ref grado1 1) (list-ref grado2 1)))
             (>= (car grado1) (car grado2)))))
;;Ejercicio 7
(he-tardado 15 'b2-e7)

; Se utiliza la funcion anterior test-CL>= sobre todos los test de dos conceptos c1 y c2 para determinar cual de los dos es mas general.
(define (concepto-CL>= c1 c2)
  (if (not (test-CL>= (car c1) (car c2)))
      #f
      (if (null? (list-tail c1 1))
          #t
          (concepto-CL>= (list-tail c1 1) (list-tail c2 1)))))

;;Ejercicio 8
(he-tardado 15 'b2-e8)

;Esta funcion devuelve 1 si c1 es estrictamente mas general que c2, 0 si son igual de generales, y -1 si c2 es estrictamente mas general que c1
(define (cmp-concepto-CL c1 c2)
  (if (and (concepto-CL>= c1 c2) (not (concepto-CL>= c2 c1)))
      1
      (if (and (concepto-CL>= c1 c2) (concepto-CL>= c2 c1))
          0
          -1)))


