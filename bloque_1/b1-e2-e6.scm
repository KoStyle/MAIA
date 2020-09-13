;;Copyright (C) 2008  Manuel Konomi Pilkati

;;This program is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require srfi/1)

;;Ejercicio 2
(he-tardado 90 'b1-e2)
;;se comprueba que se recibe una lista, se incrementa en 1 cada posicion de forma recursiva
(define (siguiente lista)
 (if (list? lista)
      (if (not (null? (list-tail lista 1)))
          (cons (+ (list-ref lista 0) 1) (siguiente (list-tail lista 1)))
          (cons (+ (list-ref lista 0) 1) '()))))

;;Ejercicio 3
(he-tardado 40 'b1-e3)
;;se comprueba que los dos argumentos son listas de la misma longitud.
; si es asi, se suman los elementos uno a uno recursivamente
(define (sumas lista1 lista2)
 (let ((x lista1) (y lista2))
    (if (and (list? x) (list? y) (= (length x) (length y)))
        (if (not (null? (list-tail x 1)))
            (cons (+ (list-ref x 0) (list-ref y 0)) (sumas (list-tail x 1) (list-tail y 1)))
            (cons (+ (list-ref x 0) (list-ref y 0)) '())))))

;;Ejercicio 4
(he-tardado 10 'b1-e4)
;;Definicion basica del factorial recursivo. Valores negativos no devuelven resultado.
(define (factorial numero)
  (if (>= numero 0)
      (if (= numero 0)
          1 
          (* numero (factorial (- numero 1))))))

;;Ejercicio 5
(he-tardado 100 'b1-e5)
;;Se han necesitado dos funciones adicionales. prob total calcula la suma de las probabilidades
; de todos los cons de la lista.
; encuentra_posicion busca en la lista de cons y para cuando la variable acumulador
; supera o iguala la variable theshhold. Acumulador va sumando la probabilidad de cada elemento
; y theshhold es un numero aleatorio calculado en obtener-al-azar0 tal que 0<= threshold <= prob_total
; Es un mecanismo de seleccion por ruleta basado en la probabilidad de cada elemento de la lista.
(define prob_total
  (lambda (x)
    (if (null? (list-tail x 1))
        (cdr (list-ref x 0))
        (+ (cdr (list-ref x 0)) (prob_total (list-tail x 1))))))


(define encuentra_posicion
  (lambda (lista threshold acumulado)
    (set! acumulado (+ acumulado (cdr (list-ref lista 0))))
    (if (>= acumulado threshold)
        (car (list-ref lista 0))
        (encuentra_posicion (list-tail lista 1) threshold acumulado))))

;;IMPORTANTE!!!!!! random no es estandar de R5RS, funciona si se utiliza #lang racket. 
(define (obtener-al-azar0 lista)
 (encuentra_posicion lista (* (random) (prob_total lista)) 0))

;;Ejercicio 6
(he-tardado 20 'b1-e6)
;;Se crea una funcion adiciona "convertir-a-cons" que recibe una lista sencilla de elementos y les asocia a todos;
; un cdr 1 creando una lista de parejas de forma recursiva
(define convertir-a-cons
  (lambda (x)
    (if (null? (list-tail x 1))
        (cons (cons (list-ref x 0) 1) '())
        (cons (cons (list-ref x 0) 1) (convertir-a-cons (list-tail x 1))))))

;Se comprueba si el primer elemento es un par, si no lo es se procesa con convertir-a-cons,
; si no se deja como estaba. Finalmente se llama a obtener-al-azar0
(define (obtener-al-azar lista)
 (let ((x lista))
    (if (not (pair? (list-ref x 0)))
      (set! x (convertir-a-cons x))
      (if (not (number? (cdr (list-ref x 0))))
          (set! x (convertir-a-cons x))
          '()))
    (obtener-al-azar0 x)))
