(define (A0 ejemplos)
  (let*
      (;;Asignacion de VARIABLES locales.
       ;;==================================
       (atributos (car ejemplos))
       (casos (cdr ejemplos))
       (indice-clase ;el  ́ındice de ’clase en la lista atributos.
        (list-index (lambda(x) (eq? x ’clase)) (map first atributos)))
       (clases-posibles
        (second ;s ́olo interesa el conjunto de valores.
         (list-ref atributos indice-clase)))
       ;;variable que mantiene la cuenta de las apariciones de cada clase.
       (clases-contabilizadas
        ;;Como primer paso, se inicializa a 0 la cuenta de cada clase.
        (map (lambda(clase) (cons clase 0)) clases-posibles))
       (concepto ());variable sin asignaci ́on, de momento
       ;;Asignaci ́on de FUNCIONES locales.
       ;;==================================
       ;;funci ́on que admite como par ́ametro un ejemplo, el cual utiliza
       ;;para actualizar la contabilizaci ́on de clases.
       (actualizar-contabilizacion
        (lambda(ejemplo)
          (let ((clase-del-ejemplo (list-ref ejemplo indice-clase)))
            (set! clases-contabilizadas
                  (map (lambda(x)
                         (if (eq? (car x) clase-del-ejemplo)
                             ;then
                             (cons (car x) (+ (cdr x) 1))
                             ;else
                             x))
                       clases-contabilizadas
                       )))))
       );fin de las asignaciones let*
    ;;Ahora, por cada ejemplo de entrenamiento,
    ;; se actualiza la contabilizaci ́on de clases.
    (for-each actualizar-contabilizacion ejemplos)
    ;;Finalmente se escoge la clase que m ́as veces ha aparecido:
    ;;;primero, se obtiene el n ́umero m ́aximo;
    (set! concepto (apply max (map cdr clases-contabilizadas)))
    ;;;segundo, se obtiene la clase con ese n ́umero m ́aximo
    ;;; (almacenado temporalmente en la variable concepto).
    (set! concepto
          (first (find (lambda(x) (= (cdr x) concepto)) clases-contabilizadas)))
    ;;Y por  ́ultimo se devuelve el concepto inducido por el algoritmo.
    concepto
    ))


(define (A0i concepto ejemplo-sin-clase)
  (append ejemplo-sin-clase (list concepto)))
