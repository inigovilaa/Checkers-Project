(deftemplate tablero
  (slot ID (default 0))
  (slot padre (default 0))
  (slot heuristico (default 0.0))
  (multislot mapeo)
  (slot profundidad (default 0))
  (multislot movs)
  (slot Min (default 0))
  (slot Max (default 0))
  (slot turno)
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

;(deffunction posibles-movs (?fila ?columna)
;    (bind ?tablero (find-fact ((?f tablero)) (= ?f:ID 1)))
;    (bind ?mapeo ?tablero:mapeo)
;    (bind ?tamanoFila (sqrt(length$ ?mapeo)))
;    (bind ?posicion (+ (* ?tamanoFila (- ?fila 1)) ?columna))
;    (bind ?contenido (nth$ ?posicion $?mapeo))
;    (bind $?movimientos (create$))
;
;    (if (eq ?contenido 0) then
;      (printout t "Posición vacía, elija otra posicion" crlf)
;    else(
;      if (eq ?contenido -1) then ;ficha negra
;        if(eq(nth$ (+ ?posicion ?tamanoFila) ?mapeo) 0) then ;se puede hacer movimiento hacia abajo
;
;        else(
;          if(and(eq(nth$ (+ ?posicion 1) ?mapeo) 0)(eq(mod ?posicion ?tamanoFila)0)) then ;mov a a la dch
;        ;en esta situacion si se puede hacer el movimiento a la derecga
;        )
;
;    )
;    )
;    ;mover abajo
;    ;mover 
;)




(defrule pedir-movimiento
  (declare (salience 800))
  (idActual ?x)
  (tablero (ID ?x))
  
=>

  (printout t "Inserta la coordenada de la ficha que quieras mover" crlf)
  (bind ?lineaOrigen (readline))

  (bind ?origen (string-to-field(sub-string 1 1 ?lineaOrigen)) (string-to-field(sub-string 3 3 ?lineaOrigen)))
  (printout t "Inserta la coordenada a la que quieras mover la ficha" crlf)

  (bind ?lineaDestino (readline))
  (bind ?destinoReal (string-to-field(sub-string 1 1 ?lineaDestino)) (string-to-field(sub-string 3 3 ?lineaDestino)))
  



)


;mov izq
(defrule mov-izquierda
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo ?mapeo))
  (origen ?filaOrigen ?colOrigen)

=>

  (bind ?tamanoFila (sqrt(length$ $?mapeo)))
  (bind ?posOrigen (+ (* ?tamanoFila (- ?filaOrigen 1)) ?colOrigen))
  (bind ?posDestino (- ?posOrigen 1))
  (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
  (bind ?fichaDestino (nth$ ?posDestino $?mapeo))

  (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) ) then
    (if (eq ?fichaDestino 0) then
      (bind ?newId (+ ?ID 1))
      (bind ?newPadre ?ID)
      (bind ?auxMap (replace$ ?mapeo ?posDestino ?posDestino ?fichaOrigen))
      (bind ?newMap (replace$ ?auxMap ?posOrigen ?posOrigen 0))
       
      (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
      
    else
      
      (bind ?saltoComer (nth$ (- ?posDestino 2) $?mapeo)) ;esta variable si es 0 significa q se puede comer porq hay hueco
      (if (and (neq (mod (- ?posOrigen 1) ?tamanoFila) 1) (eq ?saltoComer 0)) then
        (bind ?newId (+ ?ID (+ -1 2))) ;;;;;;;;;;;;;;;;QUITAR ESTO, ES CHORRA
      )
      
    )
  )
)

;pasar a las funciones por parametro el tablero y el color de la ficha
;mov der

;mov izq
;mov arriba
;mov abajo

;mov dama der
;mov dama izq
;mov dama arriba
;mov dama abajo

;meter un hecho por cada jugador el cual tenga un boolean que indique si le toda o no y con qué color juega

(defrule pedir-tamano-profundidad
    (declare (salience 1000))
=>
    (printout t "Inserta el tamaño del tablero: 5x5 6x6 8x8" crlf)
    (bind ?tamString (readline))
    

    (assert (tam (string-to-field(sub-string 1 1 ?tamString) ) ) ) 

    (printout t "Inserta la profundidad a la que desarrollar los árboles:" crlf)
    (bind ?prof (read))
    (assert (profundidad ?prof))

    (printout t "Elige el color que quieres ser 1(blanco) 0(negro):" crlf)
    (bind ?color (read))
    (assert (colorReal ?color))

    (if (eq colorReal 1) then
      (assert turno 1)
    else
      (assert turno 0)
    )

)

(defrule iniciar-tablero
  (declare (salience 999))
  (tam ?x)
  ;?tabnum <- (idActual ?y)
  (colorReal ?y)
  ?a <-(turno ?z)
=>
;poner de momento solo una fila en la penultima
  (bind $?mapa (create$))
  (if (eq ?y 1) then
    (loop-for-count (?i 1 (* ?x ?x)) ;e.g 5*5
      (if (or (<= ?i ?x);primera fila (1..5)
              (and (> ?i (* ?x 2)) ;de 2 fila palante (11..)
                  (<= ?i (-(* ?x ?x)(* 2 ?x))));de penultima pa tras(..15)
              (> ?i (- (* ?x ?x) ?x));de penultima pa lante (21..25)
          )then 
        (bind $?mapa (create$ $?mapa 0))
      else
        (if (and (> ?i ?x) ;segunda fila (6..10)
                (<= ?i (* ?x 2))
            ) then
          (bind $?mapa (create$ $?mapa -1))
        )
        (if (and (> ?i (-(* ?x ?x)(* 2 ?x))) ;(16..20)
                (<= ?i (-(* ?x ?x)?x))
            ) then
          (bind $?mapa (create$ $?mapa 1))
        )
      )
    )
  else
        (loop-for-count (?i 1 (* ?x ?x)) ;e.g 5*5
      (if (or (<= ?i ?x);primera fila (1..5)
              (and (> ?i (* ?x 2)) ;de 2 fila palante (11..)
                  (<= ?i (-(* ?x ?x)(* 2 ?x))));de penultima pa tras(..15)
              (> ?i (- (* ?x ?x) ?x));de penultima pa lante (21..25)
          )then 
        (bind $?mapa (create$ $?mapa 0))
      else
        (if (and (> ?i ?x) ;segunda fila (6..10)
                (<= ?i (* ?x 2))
            ) then
          (bind $?mapa (create$ $?mapa 1))
        )
        (if (and (> ?i (-(* ?x ?x)(* 2 ?x))) ;(16..20)
                (<= ?i (-(* ?x ?x)?x))
            ) then
          (bind $?mapa (create$ $?mapa -1))
        )
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
            (turno ?z)
          )
  )

  (imprimir-mapeo $?mapa)
  ;(modify ?tabNum (idActual (+ 1 ?y)))
  (assert (idActual 0))
  (retract ?a)
)



