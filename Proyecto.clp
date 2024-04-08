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
(deffunction JUEGO::imprimir-mapeo ($?mapeo)
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