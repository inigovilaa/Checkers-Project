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
    (printout t crlf)
    (printout t  crlf)
    (loop-for-count (?i 0 ?*tamanoFila*) do
      (if (= ?i 0) then
      (printout t "       ")
      else
      (printout t "  "?i "   "))
    )
    (generarLineas ?*tamanoFila*)
    (loop-for-count (?fila 1 ?*tamanoFila* ) do
      (generarLineas2 ?*tamanoFila*)
      (printout t "   " ?fila "  |" )
      (loop-for-count (?columna 1 ?*tamanoFila*) do
            (bind ?contenido (nth$  (+ (* ?*tamanoFila* (- ?fila 1)) ?columna) $?mapeo))
			(if (or (eq ?contenido ficha_blanca)(eq ?contenido ficha_negra)(eq ?contenido dama_blanca)(eq ?contenido dama_negra)) then
                (if (eq ?contenido ficha_blanca) then
                    (printout t  "  o  |")
                )
				(if (eq ?contenido ficha_negra) then
                    (printout t  "  x  |")
                )
                (if (eq ?contenido dama_blanca) then
                    (printout t  " oOo |")
                )
				(if (eq ?contenido dama_negra) then
                    (printout t  " xXx |")
                )
			else
				(printout t "     |")
			)
      )
      (generarLineas ?*tamanoFila*)
    )
)

(defrule pedir-tamano-profundidad

=>
    (declare (salience 1000))
    (printout t "Inserta el tamaño del tablero: 5x5 6x6 8x8" crlf)
    (bind ?tamString (readline))
    
    (assert (bind ?tam (sub-string 1 1 (?tamString)))) 

    (printout t "Inserta la profundidad a la que desarrollar los árboles:" crlf)
    (bind ?prof (read))
    (assert ?prof)
)

(defrule iniciar-tablero
  (tam ?x)
=>
;poner de momento solo una fila en la anteultima
)

