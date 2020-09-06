(define leer-ejemplos
  (lambda (x)
    (call-with-input-file x
     (lambda (i)
       (let* ((a (read i)))
         a)))))

(define ejemplos (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos.scm"))
(define ejemplos2 (leer-ejemplos "C:\\Users\\konom\\OneDrive\\Escritorio\\MAIA\\bloque_1\\ejemplos.scm"))

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



(mezclar ejemplos ejemplos2)



