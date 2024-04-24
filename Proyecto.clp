(deftemplate tablero
  (slot ID (default 0))
  (slot padre (default 0))
  (slot heuristico (default 0.0))
  (multislot mapeo)
  (slot profundidad (default 0))
  (multislot movs)
  (slot Min (default 0))
  (slot Max (default 0))

)

;(Tablero (ID x)(Padre Y)(Heuristico Z)(Mapeo ...)(Prof u)(Movs 1 2 1 4 3 4)(Alpha )(Beta )(Min )(Max )

(deffunction generarLineas (?x)
  (printout t crlf)
  (printout t "      |")
  (loop-for-count ?x
    (printout t "-----|")
  )
  (printout t crlf)
)

(deffunction generarLineas2 (?x)
  (printout t " ")
  (loop-for-count ?x
    (printout t "     |")
  )
  (printout t "     |")
  (printout t crlf)
)

;Imprimir el estado actual del mapeo
(deffunction imprimir-mapeo ($?mapeo)
    (bind ?tamanoFila (integer (sqrt (length$ $?mapeo))))
    (printout ?tamanoFila)
    (printout t crlf)
    (printout t  crlf)
    (loop-for-count (?i 0 ?tamanoFila) do
      (if (= ?i 0) then
      (printout t "       ")
      else
      (printout t "  "?i "   "))
    )
    (generarLineas ?tamanoFila)
    (loop-for-count (?fila 1 ?tamanoFila ) do
      (generarLineas2 ?tamanoFila)
      (printout t "   " ?fila "  |" )
      (loop-for-count (?columna 1 ?tamanoFila) do
            (bind ?contenido (nth$  (+ (* ?tamanoFila (- ?fila 1)) ?columna) $?mapeo))
			(if (or (eq ?contenido 1)(eq ?contenido -1)(eq ?contenido 2)(eq ?contenido -2)) then
                (if (eq ?contenido 1) then
                    (printout t  "  o  |")
                )
				(if (eq ?contenido -1) then
                    (printout t  "  x  |")
                )
                (if (eq ?contenido 2) then
                    (printout t  " oOo |")
                )
				(if (eq ?contenido -2) then
                    (printout t  " xXx |")
                )
			else
				(printout t "     |")
			)
      )
      (generarLineas ?tamanoFila)
    )
)

(defrule pedir-tamano-profundidad
    (declare (salience 1000))
=>
    (printout t "Inserta el tamaño del tablero: 5x5 6x6 8x8" crlf)
    (bind ?tamString (readline))
    

    (assert (tam (string-to-field(sub-string 1 1 ?tamString) ) ) ) 

    (printout t "Inserta la profundidad a la que desarrollar los árboles:" crlf)
    (bind ?prof (read))
    (assert (profundidad ?prof))

    ;(assert idActualTab 0)

)


(deffunction posibles-movs (?fila ?columna)
    (bind ?tablero (find-fact ((?f tablero)) (= ?f:ID 1)))
    (bind ?mapeo ?tablero:mapeo)
    (bind ?tamanoFila (sqrt(length$ ?mapeo)))
    (bind ?posicion (+ (* ?tamanoFila (- ?fila 1)) ?columna))
    (bind ?contenido (nth$ ?posicion $?mapeo))
    (bind $?movimientos (create$))

    (if (eq ?contenido 0) then
      (printout t "Posición vacía, elija otra posicion" crlf)
    else(
      if (eq ?contenido -1) then ;ficha negra
        if(eq(nth$ (+ ?posicion ?tamanoFila) ?mapeo) 0) then ;se puede hacer movimiento hacia abajo

        else(
          if(and(eq(nth$ (+ ?posicion 1) ?mapeo) 0)(eq(mod ?posicion ?tamanoFila)0)) then ;mov a a la dch
        ;en esta situacion si se puede hacer el movimiento a la derecga
        )

    )







    )
)

(defrule iniciar-tablero
  (declare (salience 999))
  (tam ?x)
  ;?a <- (idActualTab ?y)
=>
;poner de momento solo una fila en la penultima
  (bind $?mapa (create$))
  (loop-for-count (?i 1 (* ?x ?x))
    (if (or (<= ?i ?x);primera fila
            (and (> ?i (* ?x 2)) ;de 2 fila palante 
                 (<= ?i (-(* ?x ?x)(* 2 ?x))));de penultima pa tras
            (> ?i (- (* ?x ?x) ?x));de penultima pa lante
        )then 
      (bind $?mapa (create$ $?mapa 0))
    else
      (if (and (> ?i ?x)
               (<= ?i (* ?x 2))
          ) then
        (bind $?mapa (create$ $?mapa -1))
      )
      (if (and (> ?i (-(* ?x ?x)(* 2 ?x))) 
               (<= ?i (-(* ?x ?x)?x))
          ) then
        (bind $?mapa (create$ $?mapa 1))
      )
    )
  )

  (assert (tablero
            (ID 1)
            (padre 0)
            (heuristico 0.0)
            (mapeo $?mapa)
            (profundidad 0)
            (movs )
            (Min 0)
            (Max 0)
          )
  )

  (imprimir-mapeo $?mapa)
)

