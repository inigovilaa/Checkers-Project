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

;(load imprimir.clp)
;(load minmax.clp)

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

(deffunction seguirComiendo(?posOrigen $?mapeo)
  (bind ?tamanoFila (integer(sqrt(length$ $?mapeo))))
  (bind ?posOrigen ?posOrigen)
  (bind ?filaOrigen (+ (div(- ?posOrigen 1) ?tamanoFila) 1))
  (bind ?colOrigen (+ (mod(- ?posOrigen 1) ?tamanoFila) 1))
  (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))

  ;return $?(reina? X Y newmapeo  reina? X Y newmapeo    reina? X Y newmapeo    reina? X Y newmapeo)
              ;(posible abajo)    ;(posible arriba)         ;(posible der)            ;(posible izq)
  (bind $?nuevomapa (create$))

  ;ficha normal
  (if (or (eq ?fichaOrigen 1) (eq ?fichaOrigen -1)) then 
    ;IZQUIERDA
    (bind ?posDestino (- ?posOrigen 1));es el contiguo, no al que salta
    (bind ?fichaDestino (nth$ ?posDestino $?mapeo))  
    (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)) (neq ?fichaDestino 0) ) then ;no es posicion borde el contiguo, no es de mi color y no es vacio
      (if (and (neq (mod (- ?posOrigen 1) ?tamanoFila) 1) (eq (nth$ (- ?posDestino 1) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0    
        (bind $?auxMap $?mapeo)
        (bind $?auxMap (replace$ $?auxMap (- ?posDestino 1) (- ?posDestino 1) ?fichaOrigen))
        (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
        (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

        (bind ?filaDestino ?filaOrigen)
        (bind ?colDestino (- ?colOrigen 2)) 

        (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
      )
    ) 

    (printout t "normalIZQ" crlf)

    ;DERECHA
    (bind ?posDestino (+ ?posOrigen 1));es el directamente contiguo
    (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
    (if (and (neq (mod ?posOrigen ?tamanoFila) 0) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)) (neq ?fichaDestino 0)) then  ;no es posicion borde el contiguo, no es de mi color y no es vacio
      (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0  
        (bind $?auxMap $?mapeo)
        (bind $?auxMap (replace$ $?auxMap (+ ?posDestino 1) (+ ?posDestino 1) ?fichaOrigen))
        (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0)) 
        (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

        (bind ?filaDestino ?filaOrigen)
        (bind ?colDestino (+ ?colOrigen 2)) 

        (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
      )
    )

    (printout t "normalDER" crlf)

    (if (eq ?fichaOrigen -1) then ;negra solo abajo -> sumar fila
      ;ABAJO
      (bind ?filaDestino (+ ?filaOrigen 1)) ;fila contigua
      (bind ?colDestino ?colOrigen)   
      (bind ?posDestino (+ ?posOrigen ?tamanoFila));pos contigua
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      (if (and (neq ?filaOrigen ?tamanoFila) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)) (neq ?fichaDestino 0)) then ;no es posicion borde el contiguo, no es de mi color y no es vacio
        (if (and (neq ?filaDestino ?tamanoFila) (eq (nth$ (+ ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0 
          (bind $?auxMap $?mapeo)
          (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
          (bind $?auxMap (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0))

          (if (> (+ ?posDestino ?tamanoFila) (- (* ?tamanoFila ?tamanoFila) ?tamanoFila)) then ;damaaaaaa
            (bind $?auxMap (replace$ $?auxMap (+ ?posDestino ?tamanoFila) (+ ?posDestino ?tamanoFila) -2))
            (bind $?nuevomapa (create$ $?nuevomapa 1 (+ ?filaOrigen 2) ?colDestino $?auxMap))
          else
            (bind $?auxMap (replace$ $?auxMap (+ ?posDestino ?tamanoFila) (+ ?posDestino ?tamanoFila) ?fichaOrigen))
            (bind $?nuevomapa (create$ $?nuevomapa 0 (+ ?filaOrigen 2) ?colDestino $?auxMap))

          )
        )
      )
      (printout t "normalABAJO" crlf)

    else
      ;ARRIBA
      (bind ?filaDestino (- ?filaOrigen 1))
      (bind ?colDestino ?colOrigen)
      (bind ?posDestino (- ?posOrigen ?tamanoFila))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      ;(printout t "normalABAJO 1" crlf)

      (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)) (neq ?fichaDestino 0)) then
        (if (and (neq ?filaDestino 1) (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0)) then
          ;(printout t "normalABAJO 1" crlf)

          (bind $?auxMap $?mapeo)
          (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
          (bind $?auxMap (replace$ $?auxMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0))
          
          (if (<= (- ?posDestino ?tamanoFila) ?tamanoFila) then
            (bind $?auxMap (replace$ $?auxMap (- ?posDestino ?tamanoFila) (- ?posDestino ?tamanoFila) 2))
            (bind $?nuevomapa (create$ $?nuevomapa 1 (- ?filaOrigen 2) ?colDestino $?auxMap))
          else
            (bind $?auxMap (replace$ $?auxMap (- ?posDestino ?tamanoFila) (- ?posDestino ?tamanoFila) ?fichaOrigen))
            (bind $?nuevomapa (create$ $?nuevomapa 0 (- ?filaOrigen 2) ?colDestino $?auxMap))
          )
        )
      )
      (printout t "normalUP" crlf)
    ) 


  ;DAMAAAAAAAAAAAA_________________________________________________________________________________
  else
    (if (or (eq ?fichaOrigen 2) (eq ?fichaOrigen -2)) then
      ;a cualquier lado de uno en uno hasta que el destino sea una ficha del color contrario y entonces hacer como con las fichas normales y devolver el resultado
      ;IZQUIERDA
      (bind ?posDestino (- ?posOrigen 1))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      
      (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;no es posicion borde el origen, no es de mi color
        (if (neq ?fichaDestino 0) then ;y no es vacio -> es contrario
          (if (and (neq (mod (- ?posOrigen 1) ?tamanoFila) 1) (eq (nth$ (- ?posDestino 1) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0    
            (bind $?auxMap $?mapeo)
            (bind $?auxMap (replace$ $?auxMap (- ?posDestino 1) (- ?posDestino 1) ?fichaOrigen))
            (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

            (bind ?filaDestino ?filaOrigen)
            (bind ?colDestino (- ?colOrigen 2)) 

            (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
          )
        else ;es vacio
          (while (and (eq ?fichaDestino 0) (neq (mod ?posDestino ?tamanoFila) 1));hacia la izquierda hasta que nos topemos con alguna ficha o borde
            (bind ?posDestino (- ?posDestino 1))
            (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
          )
          (if (neq (mod ?posDestino ?tamanoFila) 1) then ;no es borde
            (if (and (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2)))  then ;es contraria, si no no hago nada
              (if  (eq (nth$ (- ?posDestino 1) $?mapeo) 0) then ;despues que la posicion a la q se salta es 0    
                (bind $?auxMap $?mapeo)
                (bind $?auxMap (replace$ $?auxMap (- ?posDestino 1) (- ?posDestino 1) ?fichaOrigen))
                (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
                (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

                (bind ?filaDestino ?filaOrigen)
                (bind ?colDestino (- ?colOrigen (- ?posOrigen ?posDestino))) 

                (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
              )
            )
          )
        )
      )
      (printout t "damaIZQ" crlf)

      ;DERECHA
      (bind ?posDestino (+ ?posOrigen 1))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      
      (if (and (neq (mod ?posOrigen ?tamanoFila) 0) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then  ;no es posicion borde, el origen no es de mi color
        (if (neq ?fichaDestino 0) then ;y no es vacio -> es contrario
          (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0  
            (bind $?auxMap $?mapeo)
            (bind $?auxMap (replace$ $?auxMap (+ ?posDestino 1) (+ ?posDestino 1) ?fichaOrigen))
            (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0)) 
            (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

            (bind ?filaDestino ?filaOrigen)
            (bind ?colDestino (+ ?colOrigen 2)) 

            (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
          )
        else ;es vacio
          (while (and (eq ?fichaDestino 0) (neq (mod ?posDestino ?tamanoFila) 0));hacia la derecha hasta que nos topemos con alguna ficha o borde
            (bind ?posDestino (+ ?posDestino 1))
            (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
          )
          (if (neq (mod ?posDestino ?tamanoFila) 0) then ;no es borde
            (if (and (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;es contraria, si no no hago nada
              (if (eq (nth$ (+ ?posDestino 1) $?mapeo) 0) then ;despues que la posicion a la q se salta es 0    
                (bind $?auxMap $?mapeo)
                (bind $?auxMap (replace$ $?auxMap (+ ?posDestino 1) (+ ?posDestino 1) ?fichaOrigen))
                (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
                (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

                (bind ?filaDestino ?filaOrigen)
                (bind ?colDestino (+ ?colOrigen (- ?posDestino ?posOrigen))) 

                (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
              )
            )
          )
        )
      )
      (printout t "damaDER" crlf)

      ;ABAJO
      (bind ?filaDestino (+ ?filaOrigen 1)) ;fila contigua
      (bind ?colDestino ?colOrigen)   
      (bind ?posDestino (+ ?posOrigen ?tamanoFila));pos contigua
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))

      (if (and (neq ?filaOrigen ?tamanoFila) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;no es posicion borde el contiguo, no es de mi color y no es vacio
        (if (neq ?fichaDestino 0) then ; es contraria
          (if (and (neq ?filaDestino ?tamanoFila) (eq (nth$ (+ ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0 
            (printout t "damaABAJO1.1" crlf)
            (bind $?auxMap $?mapeo)
            (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (bind $?auxMap (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0))

            (bind $?auxMap (replace$ $?auxMap (+ ?posDestino ?tamanoFila) (+ ?posDestino ?tamanoFila) ?fichaOrigen))
            (bind $?nuevomapa (create$ $?nuevomapa 0 (+ ?filaOrigen 2) ?colDestino $?auxMap))
            (printout t "damaABAJO1.2" crlf)

          )
        else ;es vacio
          (bind ?filaDestino (+ ?filaOrigen (div (- ?posDestino ?posOrigen) ?tamanoFila)))
          (bind ?posDestino ?posOrigen)
          (while (and (eq ?fichaDestino 0) (neq ?posDestino ?tamanoFila));hacia la abajo hasta que nos topemos con alguna ficha o borde
            (bind ?posDestino (+ ?posDestino ?tamanoFila));pos contigua
            (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
          )
          (if (neq ?posDestino ?tamanoFila) then ;no es borde
            (if (and (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;es contraria, si no no hago nada
              (if (eq (nth$ (+ ?posDestino ?tamanoFila) $?mapeo) 0) then ;despues que la posicion a la q se salta es 0  
                (bind $?auxMap $?mapeo)
                (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
                (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))

                (bind ?filaDestino (+ ?filaOrigen (div (- ?posDestino ?posOrigen) ?tamanoFila)) )

                (bind $?auxMap (replace$ $?auxMap (+ ?posDestino ?tamanoFila) (+ ?posDestino ?tamanoFila) ?fichaOrigen))
                (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
              )
            )
          )
        )
      )
      (printout t "damaABAJO" crlf)
      (printout t ?nuevomapa crlf)
      ;ARRIBA
      (bind ?filaDestino (- ?filaOrigen 1))
      (bind ?colDestino ?colOrigen)
      (bind ?posDestino (- ?posOrigen ?tamanoFila))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            (printout t "damaARRIBA1" crlf)

      (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;no es posicion borde el contiguo, no es de mi color 
        (if (neq ?fichaDestino 0) then  ;no es vacio -> es contraria
          (if (and (neq ?filaDestino 1) (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0 
      (printout t "damaARRIBA2" crlf)

            (bind $?auxMap $?mapeo)
            (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (bind $?auxMap (replace$ $?auxMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0))

            (bind $?auxMap (replace$ $?auxMap (- ?posDestino ?tamanoFila) (- ?posDestino ?tamanoFila) ?fichaOrigen))
            (bind $?nuevomapa (create$ $?nuevomapa 0 (- ?filaOrigen 2) ?colDestino $?auxMap))
                (printout t "damaARRIBA3" crlf)

          )
        else 
          (printout t "elseARRIBA" crlf)
          (bind ?filaDestino (- ?filaOrigen (div (- ?posOrigen ?posDestino) ?tamanoFila)) )
          (bind ?posDestino ?posOrigen)
          (while (and (eq ?fichaDestino 0)(neq ?filaDestino 1));hacia la abajo hasta que nos topemos con alguna ficha o borde
            (bind ?posDestino (- ?posDestino ?tamanoFila));pos contigua
            (printout t ?posDestino crlf)
            (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            (printout t ?fichaDestino crlf)
            (bind ?filaDestino (- ?filaOrigen (div (- ?posOrigen ?posDestino) ?tamanoFila)) )
          )
          (printout t "whileOUTARRIBA" crlf)

          (if (neq ?filaDestino 1) then ;no es borde
            (if (and (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;es contraria, si no no hago nada
              (if (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0) then ;despues que la posicion a la q se salta es 0  
                (bind $?auxMap $?mapeo)
                (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
                (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))
          (printout t "elseIF" crlf)

                ;(bind ?filaDestino (- ?filaOrigen (div (- ?posOrigen ?posDestino) ?tamanoFila)) )

                (bind $?auxMap (replace$ $?auxMap (- ?posDestino ?tamanoFila) (- ?posDestino ?tamanoFila) ?fichaOrigen))
                (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
              )
            )
          )
        )
      )
      (printout t "damaARRIBA" crlf)
    )
  )
  (return $?nuevomapa)
)

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
    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (create$ ?lineaOrigen ?lineaDestino)) (Min 0) (Max 0) (turno -1)))
    (assert (turno 1))

  else
    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (create$ ?lineaOrigen ?lineaDestino)) (Min 0) (Max 0) (turno 1)))
    (assert (turno -1))
  )
  (assert (idActual (+ ?ID 1)))
  (imprimir-mapeo $?newMap)
  (printout t "Movimientos: " ?lineaOrigen " " ?lineaDestino crlf)
)


;mov izq


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
    (assert (profundidad (integer(* ?prof 2))))

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
  (colorReal ?color)

  =>

  ;poner de momento solo una fila en la penultima
  (bind $?mapa (create$))
  (if (eq ?color 1) then
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
