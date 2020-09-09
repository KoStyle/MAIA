;;Plantilla b1-e15-e19.scm
;;Autor: Manuel Konomi Pilkati

;;Ejercicio 15
(he-tardado 4 'b1-e15)
;;Existen errores, todos los casos positivos se han etiquetado como negativos ya que la mayoria originalmente eran negativos (12 positivos 14 negativos). A0 le
; asigna a cualquier ejemplo que evalue la clase mayoritaria del set de entrenamiento.


;;Ejercicio 16
(he-tardado 2 'b1-e16)
;;dado que solo acierta los casos que eran originalmente negativos (14) la precision es de 14/26, es decir, aproximadamente 53,85%

;;Ejercicio 17
(he-tardado 8 'b1-e17)
;;La precision obtenida de evaluar los 5 nuevos casos a partir de lo aprendido con los casos originales es del 40%,
; los 5 casos estan compuestos de tres positivos y dos negativos, pero como lo que aprendemos es que todo se evalua a negativo (la mayoria en los ejemplos
; originales, evaluamos todo a negativo. No obstante, es casualidad, ya que no se utiliza información de los casos que se estan evaluando para decidir su clase. 

;;Ejercicio 18
(he-tardado 5 'b1-e18)
;;<comentarios>
(define (A1 ejemplos)
 (ratio-clases '(+ -) (list-tail ejemplos 1)))
(define (A1i concepto ejemplo-sin-clase)
  (append ejemplo-sin-clase (cons (obtener-al-azar concepto) '())))

;;Ejercicio 19
(he-tardado 2 'b1-e19)
;;El algoritmo A1/A1i puede ser mas preciso que el A0/A0i con el set de entrenamiento y el set de test que tenemos, pero esto
; es a fin de cuentas aleatorio, en unas ejecuciones he obtenido aciertos del 20% y en otras aciertos del 60%. Ocurre lo mismo, no tenemos en cuenta
; la información en el ejemplo evaluado, solo usamos las estadisticas del grupo de entrenamiento para asignar un valor al azar.
