;;Plantilla b1-e8-e11
;;AUTOR:
;;Copyright (C) 2008  <NOMBRE DEL ALUMNO>

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

;;Ejercicio 8
;;Se lee el fichero siguiendo las instrucciones dadas por el enunciado.
(he-tardado 35 'b1-e8)
(define (leer-ejemplos archivo)
  (let ((x archivo))
    (call-with-input-file x
      (lambda (i)
        (let* ((a (read i)))
          a)))))

;;Ejercicio 9
;;Se recorre la lista de ejemplos original hasta llegar al final recursivamente y se vuelve a montar con el ejemplo al final.
(he-tardado 100 'b1-e9)
(define (anadir-ejemplo ejemplos ejemplo)
  (if (or (null? ejemplos) (not (list? ejemplo)))
      '()
      (if (null? (list-tail ejemplos 1))
          (cons (list-ref ejemplos 0) (cons ejemplo '()))
          (cons (list-ref ejemplos 0) (anadir-ejemplo (list-tail ejemplos 1) ejemplo)))))

;;Ejercicio 10
;;Se han creado dos funciones adicionales. busca-atributo localiza en la primera parte de los ejemplos
; la posicion en la que se encuentra el simbolo proporcionado por argumento
; La segunda funcion utiliza esa posicion y el cdr de la lista de ejemplos para recuperar el valor de esa posicion en
; todos los ejemplos.
(he-tardado 85 'b1-e10)

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
;;Por que debemos escribir 'perspectiva y no simplemente perspectiva.
;;Si no se usa la comilla simple, se entiende como el identificador de una variable en lugar de como un simbolo literal.

;;Ejercicio 11
;;Se asume que las dos listas se reciben con la misma estructura (las dos tienen cabecera con la especificacion de los datos)
(he-tardado 15 'b1-e11)

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
