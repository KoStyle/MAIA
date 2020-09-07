;;Plantilla b1-e12-e14.scm
;;AUTOR:
;;Copyright (C) 2009  Manuel Konomi Pilkati

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

;;Ejercicio 12
;;<Comentarios al ejercicio 12>
(he-tardado 45 'b1-e12)

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
          (cons (list-ref ejemp 0) (separar_rec tam '() (list-tail ejemp 1))))))) ; devolvemos una lista con los  metadatos y dos listas de ejemplos, una con el porcentaje aproximado y otra con las sobras)

;;Ejercicio 13
;;<Comentarios al ejercicio 13>
(he-tardado 120 'b1-e13)

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

;;Ejercicio 14
;;Para este ejercicio he necesitado muchas funciones auxiliares. En esencia, es una copia de las funciones que utiliza folds, pero con un nuevo obtener-al-azar que le
; Asigna valores personalizados a los ejemplos segun su clase. Esta funcion es necesaria para mantener el ratio con el que se extraen los elementos segun va disminuyendo
; una clase u otra.
; El valor para obtener-al-azar que se le asigna a cada ejemplo, es (#+_original/#+_actual) para los casos positivos y (#-_original/#-_actual) para los negativos.
; De esta forma se mantiene la proporcion original en la medida de lo posible. Segun disminuye una clase, mas probable se hace que se elija un elemento de la misma.
; Por eso mismo se necesita una funcion que extraiga la cantidad de una clase u otra en cualquier momento.
; En resumidas cuentas, ejecutamos un fold que calcula el ratio original (cantidad + y cantidad -) y lo va pasando como argumento hacia abajo.
; Llegados al punto de obtener-al-azar, la funcion original se encapsula en obtener-al-azar-stra que le asigna los pesos a cada elemento segun se ha explicado. A partir de ahi
; se reutiliza el codigo ya implementado
(he-tardado 180 'b1-e14)

;Funcion para contar un simbolo en una lista simple (count no es R5RS, esta funcion lo suple)
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

;Esta funcion devuelve los ratios de cada tipo de clase definido en la lista "clases" para una lista de ejemplos (sin cabecera) llamada "lista"
(define (ratio-clases clases lista)
  (map (lambda (x)
         (cons x (cuenta-simbolo x (recupera-atributo-ejemplos (- (length (car lista)) 1) lista)))) ;recuperamos los valores de la ultima posicion de todos los ejemplos (recibimos solo ejemplos)
       clases))

;Esta funcion calcula el peso que hay que asignar a un ejemplo dado el ratio original y el actual
(define (calcular-ratio-ejemplo ejemplo ogratio newratio)
  (if (and (list? ejemplo) (list? ogratio) (list? newratio))
      (let ((clase (list-ref ejemplo (- (length ejemplo) 1)))) ;extraemos la clase del ejemplo (sea el simbolo que sea, existe en los ratios)
        (if (eqv? clase (car (car ogratio)))  ;si es igual al simbolo de la priemra clase, el valor devuelto es cantidad original de instancias de esa clase / cantidad actual
            (/ (cdr (car ogratio)) (cdr (car newratio))) ;De esta forma, al asignar el resultado a obten-valor, se mantiene el ratio de casos obtenidos.
            (/ (cdr (list-ref ogratio 1)) (cdr (list-ref newratio 1)))))
      '()))

;Esta funcion convierte una lista de ejemplos en cons de (ejemplo . peso) para usarse en obtener-al-azar segun el ratio original y el actual
(define convertir-a-cons-straty
  (lambda (x ogratio newratio)
    (if (null? (list-tail x 1))
        (cons (cons (list-ref x 0) (calcular-ratio-ejemplo (list-ref x 0) ogratio newratio)) '())
        (cons (cons (list-ref x 0) (calcular-ratio-ejemplo (list-ref x 0) ogratio newratio)) (convertir-a-cons-straty (list-tail x 1) ogratio newratio)))))

;esta funcion encapsula la obtencion al azar asignando pesos de forma dinamica a la lista que le llegue
(define (obtener-al-azar-straty lista ogratio simbolos)
  (if (and (list? lista) (list? ogratio) (list? simbolos))
      (let* ((ratio-actual (ratio-clases simbolos lista))
             (conlista (convertir-a-cons-straty lista ogratio ratio-actual)))
        (obtener-al-azar conlista))
      '()))

;Esta funcion (basada en las de separar) crea el fold usando obtener-al-azar-straty
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

;Funcion que estratifica recursivamente la lista de ejemplos (sin cabecera)
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

;Calculamos los parametros necesarios y eliminamos la cabecera antes de llamar a stratify-rec
(define (stratify n ejemplos)
  (let ((num n) (eje ejemplos))
    (if (or (not (integer? num)) (< num 1) (not (list? eje)))
        '()
        (let ((n_extra_lists (modulo (length (list-tail eje 1)) num)) ; Calculamos el resto de dividir por el tamaño de fold (para distribuir los sobrantes en las listas)
              (len_fold (floor (/ (length (list-tail eje 1)) num)))   ; Calculamos la longitud de los folds y cuantos de ellos tendran un elemento extra
              (ogratio (ratio-clases '(+ -) (list-tail eje 1))))      ; Calculamos el ratio inicial de clases
          (cons (list-ref eje 0) (stratify-rec (list-tail eje 1) num n_extra_lists len_fold ogratio '(+ -)))))))
