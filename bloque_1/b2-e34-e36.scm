;;Plantilla b2-e34-e36.scm
;;Autor: Manuel Konomi

;;Ejercicio 34
(he-tardado 15 'b2-e34)

;Estraido directamente del enunciado del ejercicio
(define (IB ejemplos) ejemplos)

;Esta funcion mide la distancia entre dos valores, sean del tipo que sean. Si ambos son numeros, los resta, si son simbolos, 0 si son iguales, 1 si no lo son
(define (distancia-valor valor1 valor2)
  (if (and (number? valor1) (number? valor2))
      (- valor1 valor2)
      (if (eqv? valor1 valor2)
          0
          1)))

;Parte recursiva del calculo de distancia, recibe dos ejemplos sin clase y va calculando su diferencia al cuadrado en cada paso.
;Devuelve la suma de todas esas diferencias al cuadrado.
(define (distancia-rec ej1 ej2)
  (if (null? ej1)
      0
      (+ (expt (distancia-valor (car ej1) (car ej2)) 2) (distancia-rec (cdr ej1) (cdr ej2)))))

;Se asegura de que los dos ejemplos tengan la misma longitud y esten sin clase, llama a la funcion recursiva, y devuelve la raiz cuadrada de su resultado
(define (distancia ejemplo-sin-clase ejemplo)
  (let* ((ejsc ejemplo-sin-clase)
         (ej2 (if (< (length ejsc) (length ejemplo)) (drop-right ejemplo 1) ejemplo)))
    (sqrt (distancia-rec ejsc ej2))))

;;Ejercicio 35
(he-tardado 20 'b2-e35)

;Parte recursiva del interprete, en cada ciclo compara la distancia del proximo vecino del concepto con el vecino que hasta ahora tiene la distancia mas pequeña.
;Si el nuevo vecino esta mas cerca que el plusmarquista, se pasa a la siguiente iteración con el nuevo vecino. De lo contrario se mantiene el antiguo. Si el vecino que recibimos
; es '() (en la primera ejecucion, por ejemplo) entonces tanto el vecino actual como el maybe se convierten en el proximo vecino del concepto.
;Finalmente, cuando se terminan los ejemplos del concepto, se devuelve el ejemplo sin clase con la clase de vecino-hasta-ahora.
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

;Funcion principal, segun concepto-IB tenga cabecera o no, llama a la funcion recursiva dandole el concepto tal cual o sin cabecera.
(define (IBi concepto-IB ejemplo-sin-clase)
  (if (list? (car (car concepto-IB)))
      (IBi-rec (cdr concepto-IB) ejemplo-sin-clase '())  ;quitamos la cabecera si la tiene
      (IBi-rec concepto-IB ejemplo-sin-clase '())))


;;Ejercicio 36
(he-tardado 40 'b2-e36)

;;No se especifica mucho de este ejercicio, y no se ha encontrado nada en la literatura del capitulo 4.1. Hasta ahora, las funcones match-XX indican si un ejemplo sin clase
; cumple con el concepto. Con esta infomación se desarrollaba entonces un interprete XXi que, en caso de que hubiera match asignaba una clase positiva al ejemplo y lo devolvia,
;o le asignaba una clase negativa y lo devolvia. En este caso ya tenemos un interprete, de modo que el match no hace falta para interpretar. Ya que no se especifica otra cosa,
;Nuestro match comprueba que clase se le ha asignado al ejemplo. Si se le a aplicado una clase negativa, devuelve false, si se le ha aplicado una clase positiva, devuelve true.
;En otras palabras, antes haciamos match-XX -> XXi, y para este caso hacemos XXi -> match-XX
(define (match-IB concepto-IB ejemplo-sin-clase)
  (let ((resultado (IBi concepto-IB ejemplo-sin-clase)))
    (if (eqv? (list-ref resultado (- (length resultado) 1)) '-)
        #f
        #t)))




