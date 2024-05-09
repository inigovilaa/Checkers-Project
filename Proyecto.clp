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

(deffunction manhattanDistance (?origX ?origY ?destX ?destY)
  (bind ?distance (+ (abs (- ?origX ?destX)) (abs (- ?origY ?destY))))
  (return ?distance)
)

(defrule pedir-movimiento
  (tam ?tamanoFila)
  (idActual ?ID)
  (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo)(turno ?i))
  ?a <- (turno ?tur)
  (test (eq ?tur (* -1 ?i)))
=>
  (printout t "Inserta la coordenada de la ficha que quieras mover:" crlf)
  (bind ?lineaOrigen (explode$(readline)))

  (bind ?filaOrigen (nth$ 1 ?lineaOrigen))
  (bind ?colOrigen (nth$ 2 ?lineaOrigen))

  (bind ?posOrigen (integer (+ (* ?tamanoFila (- ?filaOrigen 1)) ?colOrigen)))
  (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))

  
  (printout t "Inserta la coordenada a cordenada los movimientos de la ficha:" crlf)
  (bind ?lineaDestino (explode$ (readline)))
  (bind ?long (length$ ?lineaDestino))

  (bind ?newId (+ ?ID 1))
  (bind ?newPadre ?ID)

  (if (eq ?long 2) then ;it only has one jump/step
    (bind ?filaDestino (nth$ 1 ?lineaDestino))
    (bind ?colDestino (nth$ 2 ?lineaDestino))
    (bind ?posDestino (integer (+ (* ?tamanoFila (- ?filaDestino 1)) ?colDestino)))
    (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
    
    ;generacion damas
    (if (and (or (<= ?posDestino ?tamanoFila) (> ?posDestino (- (* ?tamanoFila ?tamanoFila) ?tamanoFila))) (eq ?fichaOrigen ?tur)) then ; (eq ?fichaOrigen ?tur) solo entra cuando es un peon del mismo color, si es una dama da igual porque no se va a cambiar
      (if (eq ?fichaOrigen 1) then
        (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino 2))
      else
        (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino -2))
      )
      (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
    else
      (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
      (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
    )

    (if (eq (manhattanDistance ?filaOrigen ?colOrigen ?filaDestino ?colDestino) 2) then ;1 comido
      (if (eq ?posDestino (- ?posOrigen 2)) then ;comido + izq
        (bind $?newMap (replace$ $?newMap (- ?posOrigen 1) (- ?posOrigen 1) 0))
      else 
        (if (eq ?posDestino (+ ?posOrigen 2)) then ;comido + derecha
          (bind $?newMap (replace$ $?newMap (+ ?posOrigen 1) (+ ?posOrigen 1) 0))
        else
          (if (eq ?posDestino (- ?posOrigen (* 2 ?tamanoFila))) then ;comido + salto hacia arriba
            (bind $?newMap (replace$ $?newMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0))
          else
            (bind $?newMap (replace$ $?newMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0))
          )
        )
      )
    )
    
  else ;mas de un salto
    (bind $?newMap $?mapeo)
    (bind ?i 2)
    (while (<= ?i ?long)
      (bind ?filaDestino (nth$ (- ?i 1) ?lineaDestino))
      (bind ?colDestino (nth$ ?i ?lineaDestino))
      (bind ?posDestino (integer (+ (* ?tamanoFila (- ?filaDestino 1)) ?colDestino)))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      
      ;generacion damas
      (if (and (or (<= ?posDestino ?tamanoFila) (> ?posDestino (- (* ?tamanoFila ?tamanoFila) ?tamanoFila))) (eq ?fichaOrigen ?tur)) then ; (eq ?fichaOrigen ?tur) solo entra cuando es un peon del mismo color, si es una dama da igual porque no se va a cambiar
        (if (eq ?fichaOrigen 1) then
          (bind $?newMap (replace$ $?newMap ?posDestino ?posDestino 2))
        else
          (bind $?newMap (replace$ $?newMap ?posDestino ?posDestino -2))
        )
        (bind $?newMap (replace$ $?newMap ?posOrigen ?posOrigen 0))
      else
        (bind $?newMap (replace$ $?newMap ?posDestino ?posDestino ?fichaOrigen))
        (bind $?newMap (replace$ $?newMap ?posOrigen ?posOrigen 0))
      )
      
      (if (eq ?posDestino (- ?posOrigen 2)) then ;comido + izq
        (bind $?newMap (replace$ $?newMap (- ?posOrigen 1) (- ?posOrigen 1) 0))
      else 
        (if (eq ?posDestino (+ ?posOrigen 2)) then ;comido + derecha
          (bind $?newMap (replace$ $?newMap (+ ?posOrigen 1) (+ ?posOrigen 1) 0))
        else
          (if (eq ?posDestino (- ?posOrigen (* 2 ?tamanoFila))) then ;comido + salto hacia arriba
            (bind $?newMap (replace$ $?newMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0))
          else
            (bind $?newMap (replace$ $?newMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0))
          )
        )
      )

      (bind ?filaOrigen ?filaDestino)
      (bind ?colOrigen ?colDestino)
      (bind ?posOrigen ?posDestino)
      
      (bind ?i (+ ?i 2))
    )
  )
  
  (retract ?a)

  (if (eq ?i 1) then
    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ?lineaDestino) (Min 0) (Max 0) (turno -1)))
    (assert (turno 1))

  else
    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ?lineaDestino) (Min 0) (Max 0) (turno 1)))
    (assert (turno -1))
  )
  (assert (idActual (+ ?ID 1)))
  (imprimir-mapeo $?newMap)
  (printout t "Movimientos: " ?lineaOrigen " " ?lineaDestino crlf)
)


;mov izq
(defrule mov-izquierda
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo))
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
      (bind ?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
      (bind ?newMap (replace$ ?auxMap ?posOrigen ?posOrigen 0))
       
      (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)));movs?
      
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

    (printout t "Elige el color que quieres ser 1(blanco) -1(negro):" crlf)
    (bind ?color (read))
    (assert (colorReal ?color))
    (assert (turno 1))

    (if (eq ?color 1) then
      (assert (colorIA -1))
    else
      (assert (colorIA 1))
    )

)

(defrule iniciar-tablero
  (declare (salience 999))
  (tam ?x)
  (profundidad ?p)
  (colorReal ?y)
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
  ;hay que editar
  (assert (tablero
            (ID 0)
            (padre 0)
            (heuristico 0.0)
            (mapeo $?mapa)
            (profundidad 0)
            (movs )
            (Min 0)
            (Max 0)
            (turno -1)
          )
  )

  (imprimir-mapeo $?mapa)
  ;(modify ?tabNum (idActual (+ 1 ?y)))
  (assert (idActual 0))
  (assert (turno 1))

)



