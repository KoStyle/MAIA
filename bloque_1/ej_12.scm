#lang racket
(require "ej_6.scm")
(require "ej_9_inicio.scm")





(define ejemplos (leer-ejemplos "C:\\Users\\konom\\Desktop\\IA_metodos_aprendizaje\\bloque_1\\ejemplos.scm"))

(define eliminar_elemento
  (lambda (lista item)
    (if (null? lista)
        '()
        (if (eqv? (list-ref lista 0) item)
            (list-tail lista 1)
            (cons (list-ref lista 0) (eliminar_elemento (list-tail lista 1) item))))))



;devuelve un dato al azar y lo elimina de la lista original
;TODO hacer una funcion recursiva dentro de obtener_al_azar_etc y cambiar esta ultima a "separar"
(define obtener_al_azar_sin_reemplazamiento
  (lambda (perc x)
    (if (null? x)
        '()
        (let ((elemento (obtener-al-azar x)))
          (set! x (eliminar_elemento x elemento))
          elemento))))

ejemplos
;(obtener-al-azar (car (list-tail ejemplos 1)))
(obtener_al_azar_sin_reemplazamiento (car (list-tail ejemplos 1)))
ejemplos
