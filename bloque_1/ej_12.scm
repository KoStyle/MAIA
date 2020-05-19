#lang racket
(require "ej_6.scm")
(require "ej_9_inicio.scm")





(define ejemplos (leer-ejemplos "C:\\Users\\konom\\Desktop\\IA_metodos_aprendizaje\\bloque_1\\ejemplos.scm"))

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
;TODO hacer una funcion recursiva dentro de obtener_al_azar_etc y cambiar esta ultima a "separar"
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

(define separar 
  (lambda (perc ejemp)
    (if (or (not (real? perc)) (not (list? ejemp)))
        '()
        (let ((tam (convertir_porcentaje perc (length (list-tail ejemp 1)))))  ;Calculamos el tamaño de la lista a generar
          (cons (list-ref ejemp 0) (separar_rec tam '() (list-tail ejemp 1))))))) ; devolvemos una lista con los  metadatos y dos listas de ejemplos, una con el porcentaje aproximado y otra con las sobras

;ejemplos

;(separar 0.33 ejemplos)
