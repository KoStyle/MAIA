#lang racket
(define leer-ejemplos
  (lambda (x)
    (if (not (string? x))
        '()
        (read (open-input-file x)))))
       

(leer-ejemplos "C:\Users\konom\Desktop\IA_metodos_aprendizaje\bloque_1\ejemplos.scm")