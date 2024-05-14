; Le pasas el mapeo del tablero y te devuelve la función de evaluación
; ?color indica con que color esta jugando la IA para saber si tiene que maximizar o minimizar
; esta funcion por ahora no tiene en cuenta que el jugador es +1
(deffunction eval-function ($?tablero)
  (bind ?valor 0)
  (foreach ?casilla $?tablero 
    (bind ?valor (+ ?valor ?casilla))
  )
  (return ?valor)
)

; hecho con reglas
(defrule minmax
  ?start <- (start minmax)
  ?depth <- (depth ?depth)
  ?tablero <- ( tablero (depth ?d) )
  (test (eq ?d ?depth))

  =>
  (printout t ?tablero)
  (retract ?start)
)

; hecho con funciones
(deffunction value (?depth ?player $?tablero)
  ; comprobamos que sea la depth que queremos o que sea un tablero terminado
  (if (or (eq ?depth 0) (win)) then (return eval-function($?tablero)) )

  ; si le toca al jugador max
  (max ?depth ?player $?tablero)

  ; si le toca al jugador min
  (min ?depth ?player $?tablero)
)

; (deffunction minValue (?depth ?player $?tablero)
;   (bind ?v -999999999)
; 
;   (do-for-all-facts 
; 
;   )
; )
