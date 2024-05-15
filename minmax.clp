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

; Tenemos una cantidad X de facts TABLERO en nuestra base de datos de hechos. Sobre esos hechos tenemos que aplicar el algoritmo que nos diga que tablero (mov)
; es el más adecuado según nuestra función de evaluación
;
; 
;(Tablero (ID x)(Padre Y)(Heuristico Z)(Mapeo ...)(Prof u)(Movs 1 2 1 4 3 4)(Alpha )(Beta )(Min )(Max )
;(Tablero (ID x)(Padre Y)(Heuristico Z)(Mapeo ...)(Prof u)(Movs 1 2 1 4 3 4)(Alpha )(Beta )(Min )(Max )
;(Tablero (ID x)(Padre Y)(Heuristico Z)(Mapeo ...)(Prof u)(Movs 1 2 1 4 3 4)(Alpha )(Beta )(Min )(Max )
; ...
; ...
;(Tablero (ID x)(Padre Y)(Heuristico Z)(Mapeo ...)(Prof u)(Movs 1 2 1 4 3 4)(Alpha )(Beta )(Min )(Max )
;(Tablero (ID x)(Padre Y)(Heuristico Z)(Mapeo ...)(Prof u)(Movs 1 2 1 4 3 4)(Alpha )(Beta )(Min )(Max )
;
;
; El algoritmo empieza desde la base por lo que tendremos que empezar por lo hechos cuyo depth sea igual al depth que tenemos establecido como fact inicial

(defrule minmax
  ?start <- (start minmax)
  ?depth <- (depth ?depth)
  ?tablero <- ( tablero (depth ?d) )
  (test (eq ?d ?depth))

  =>
  (printout t ?tablero)
  (retract ?start)
)
