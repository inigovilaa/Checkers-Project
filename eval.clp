; +1 blanco, +2 dama blanca
; -1 negro, -2 dama negra
; Le pasas el mapeo del tablero y te devuelve la función de evaluación
; ?color indica con que color esta jugando la IA para saber si tiene que maximizar o minimizar
; esta funcion por ahora no tiene en cuenta que el jugador es +1
(deffunction eval-function ($?tablero ?color)
  (bind ?valor 0)
  (foreach ?casilla $?tablero 
    (bind ?valor (+ ?valor ?casilla))
  )
  (return ?valor)
)

(deffunction eval-function2 (?color ?tam $?tablero)
  ; ?i es una variable que lleva el numero de iteracion
  (bind ?i 1)
  (bind ?result 0)
  ; numero de fichas de cada jugador
  (bind ?mine 0)
  (bind ?opp 0)

  (switch (es-ganador ?color $?tablero)
    ; perdedor
    (case 1 then (bind ?result (- ?result 1000000)))

    ; tablas
    (case 2 then (bind ?result (- ?result 1000)))

    ; ganar
    (case 3 then (bind ?result (+ ?result 1000000)))
  )

  (foreach ?casilla $?tablero
    ; hay una ficha de mi color
    (if (mismo-color ?color ?casilla) then 
      (
        (bind ?mine (+ ?mine +1))

        ; es un peon de mi color
        (if (es-mi-peon ?color ?casilla) then (bind ?result (+ ?result 5)) )

        ; es una dama de mi color
        (if (es-mi-dama ?color ?casilla) then (bind ?result (+ ?result 10)) )

        ; ahora comprobamos que si puedo comer
        (if (neq (length$ (seguirComiendo ?i $?tablero)) 0) then (bind ?result (+ ?result 6)))
      )
    
    else 
      ; la ficha no es de nuestro color
      (if (not (mismo-color ?color ?casilla)) then
        (
          (bind ?opp (+ ?opp +1))
          ; comprobamos si me pueden comer
          (if (neq (length$ (seguirComiendo ?i $?tablero)) 0) then (bind ?result (- ?result 3)))
        ))
    )
    (bind ?i (+ ?i 1))
  )
  (return (+ ?result (* (- ?mine ?opp) 1000) ))
)

; recibe una casilla y devuele un +1, -1 o 0 dependiendo del color que sea
(deffunction que-color-es (?casilla)
  (if (or (eq ?casilla +1) (eq ?ficha +2)) then (return +1))
  (if (or (eq ?casilla -1) (eq ?ficha -2)) then (return -1))
  (return 0)
)

; ?color puede ser {+1, -1} y comprueba que tenga el mismo color que la casilla
; devuelve TRUE or FALSE
(deffunction mismo-color (?color ?casilla)
  (eq ?color (que-color-es ?casilla))
)

; devuelve un booleano indicando si en la casilla hay una dama de mi color
(deffunction es-mi-dama (?color ?casilla)
  (return (and (mismo-color ?color ?casilla) (or (eq ?casilla +2) (eq ?casilla -2))))
)

; devuelve un booleano indicando si se puede comer
(deffunction es-mi-peon (?color ?casilla)
  (return (and (mismo-color ?color ?casilla) (or (eq ?casilla +1) (eq ?casilla -1))))
)

(defrule eval-function-apply
  ?tablero <- (Tablero (ID ?)(Padre ?)(Heuristico 0)(Mapeo $?tablero)(Prof ?)(Movs ?)(Min ?)(Max ?) ) 
  (colorReal ?color)
  (tam ?tam)
=>
  (bind ?heur (eval-function2 ?color ?tam $?tablero))
  (modify ?fn (heuristico ?heur))
)