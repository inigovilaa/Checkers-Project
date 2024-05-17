(deftemplate tablero
  (slot ID (default 0))
  (slot padre (default 0))
  (slot heuristico (default 0))
  (multislot mapeo)
  (slot profundidad (default 0))
  (multislot movs)
  (slot Min (default 0))
  (slot Max (default 0))
  (slot turno (default 0))
  (slot esHoja (default 0))
)

;(load imprimir.clp)
;(load minmax.clp)

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

    ;(printout t "normalIZQ" crlf)

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

    ;(printout t "normalDER" crlf)

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
      ;(printout t "normalABAJO" crlf)

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
      ;(printout t "normalUP" crlf)
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
      ;(printout t "damaIZQ" crlf)

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
      ;(printout t "damaDER" crlf)

      ;ABAJO
      (bind ?filaDestino (+ ?filaOrigen 1)) ;fila contigua
      (bind ?colDestino ?colOrigen)   
      (bind ?posDestino (+ ?posOrigen ?tamanoFila));pos contigua
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))

      (if (and (neq ?filaOrigen ?tamanoFila) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;no es posicion borde el contiguo, no es de mi color y no es vacio
        (if (neq ?fichaDestino 0) then ; es contraria
          (if (and (neq ?filaDestino ?tamanoFila) (eq (nth$ (+ ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0 
            ;(printout t "damaABAJO1.1" crlf)
            (bind $?auxMap $?mapeo)
            (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (bind $?auxMap (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0))

            (bind $?auxMap (replace$ $?auxMap (+ ?posDestino ?tamanoFila) (+ ?posDestino ?tamanoFila) ?fichaOrigen))
            (bind $?nuevomapa (create$ $?nuevomapa 0 (+ ?filaOrigen 2) ?colDestino $?auxMap))
            ;(printout t "damaABAJO1.2" crlf)

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
      ;(printout t "damaABAJO" crlf)
      ;(printout t ?nuevomapa crlf)
      ;ARRIBA
      (bind ?filaDestino (- ?filaOrigen 1))
      (bind ?colDestino ?colOrigen)
      (bind ?posDestino (- ?posOrigen ?tamanoFila))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      ;(printout t "damaARRIBA1" crlf)

      (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;no es posicion borde el contiguo, no es de mi color 
        (if (neq ?fichaDestino 0) then  ;no es vacio -> es contraria
          (if (and (neq ?filaDestino 1) (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0 
            ;(printout t "damaARRIBA2" crlf)
            (bind $?auxMap $?mapeo)
            (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (bind $?auxMap (replace$ $?auxMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0))

            (bind $?auxMap (replace$ $?auxMap (- ?posDestino ?tamanoFila) (- ?posDestino ?tamanoFila) ?fichaOrigen))
            (bind $?nuevomapa (create$ $?nuevomapa 0 (- ?filaOrigen 2) ?colDestino $?auxMap))
            ;(printout t "damaARRIBA3" crlf)

          )
        else 
          ;(printout t "elseARRIBA" crlf)
          (bind ?filaDestino (- ?filaOrigen (div (- ?posOrigen ?posDestino) ?tamanoFila)) )
          (bind ?posDestino ?posOrigen)
          (while (and (eq ?fichaDestino 0)(neq ?filaDestino 1));hacia la abajo hasta que nos topemos con alguna ficha o borde
            (bind ?posDestino (- ?posDestino ?tamanoFila));pos contigua
            (printout t ?posDestino crlf)
            (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            (printout t ?fichaDestino crlf)
            (bind ?filaDestino (- ?filaOrigen (div (- ?posOrigen ?posDestino) ?tamanoFila)) )
          )
          ;(printout t "whileOUTARRIBA" crlf)

          (if (neq ?filaDestino 1) then ;no es borde
            (if (and (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2))) then ;es contraria, si no no hago nada
              (if (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0) then ;despues que la posicion a la q se salta es 0  
                (bind $?auxMap $?mapeo)
                (bind $?auxMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
                (bind $?auxMap (replace$ $?auxMap ?posDestino ?posDestino 0))
                ;(printout t "elseIF" crlf)

                ;(bind ?filaDestino (- ?filaOrigen (div (- ?posOrigen ?posDestino) ?tamanoFila)) )

                (bind $?auxMap (replace$ $?auxMap (- ?posDestino ?tamanoFila) (- ?posDestino ?tamanoFila) ?fichaOrigen))
                (bind $?nuevomapa (create$ $?nuevomapa 0 ?filaDestino ?colDestino $?auxMap))
              )
            )
          )
        )
      )
      ;(printout t "damaARRIBA" crlf)
    )
  )
  (return $?nuevomapa)
)

(deffunction manhattanDistance (?origX ?origY ?destX ?destY)
  (bind ?distance (+ (abs (- ?origX ?destX)) (abs (- ?origY ?destY))))
  (return ?distance)
)

(deffunction es-ganador (?profundidad ?color $?mapeo)
  (bind ?blancas 0)
  (bind ?negras 0)
  (bind ?damaBlanca 0)
  (bind ?damaNegras 0)
  (bind ?tamanoFila (integer(sqrt(length$ $?mapeo))))

  (foreach ?ficha $?mapeo
    (if (or (eq ?ficha 1)(eq ?ficha 2)) then
      (bind ?blancas (+ ?blancas 1))
      
      (if (eq ?ficha 2) then
        (bind ?damaBlanca (+ ?damaBlanca 1))
      )
    else
      (if (neq ?ficha 0) then
        (bind ?negras (+ ?negras 1))

        (if (eq ?ficha -2) then
          (bind ?damaNegras (+ ?damaNegras 1))
        )
      )
    )
  )

  (if (or (>= ?negras 2)(>= ?blancas 2)) then
    (return 4)
  )
  
  (if (or (and (eq ?color 1) (eq ?negras 0)) (and (eq ?color -1) (eq ?blancas 0))) then 
    (return 3)
  else
    (if (or (and (eq ?color -1) (eq ?negras 0)) (and (eq ?color 1) (eq ?blancas 0))) then
      (return 1)
    )
  )

  (if (and (eq ?blancas 1) (eq ?negras 1)) then
      (bind ?posNegra (member$ -1 $?mapeo))
      (bind ?filaNegra (+(div(- ?posNegra 1) ?tamanoFila)1))
      (bind ?colNegra (+(mod(- ?posNegra 1) ?tamanoFila) 1))
        
      (bind ?posBlanca (member$ 1 $?mapeo))
      (bind ?filaBlanca (+(div(- ?posBlanca 1) ?tamanoFila)1))
      (bind ?colBlanca  (+(mod(- ?posBlanca 1) ?tamanoFila) 1))

    (if (eq (mod ?profundidad 2 ) 0) then ;minimizar -> el siguiente turno es nuestro
      (if (and (eq ?damaBlanca 0) (eq ?damaNegras 0)) then ;no hay damas
        (if (eq ?color 1) then ;somos blancas
          (if (and (eq (- ?filaNegra ?filaBlanca) 1) (eq ?colNegra ?colBlanca)) then ;negra abajo de blanca misma col
            (return 2)
          else 
            (if (or (eq (- ?filaBlanca ?filaNegra) 1)(eq ?colNegra ?colBlanca)) then ;blanca abajo de negra misma col
              (if (neq ?filaNegra 1) then ;en la siguiente ganamos
                (return 3)
              )
            )
          )

          (if (and (eq (- ?colNegra ?colBlanca) 1) (eq ?filaNegra ?filaBlanca)) then ;negra derecha de blanca y misma fila
            (if (neq (mod ?colNegra ?tamanoFila) 0) then
             (return 3)
            )
          else 
            (if (neq (mod ?colNegra ?tamanoFila) 1) then ;blanca derecha de negra
              (return 3)
            )
          )

        else ;somos negras
          (if (and (eq (- ?filaNegra ?filaBlanca) 1) (eq ?colNegra ?colBlanca)) then ;negra abajo de blanca misma col
            (return 2)
          else 
            (if (or (eq (- ?filaBlanca ?filaNegra) 1)(eq ?colNegra ?colBlanca)) then ;blanca abajo de negra misma col
              (if (neq ?filaBlanca ?tamanoFila) then ;en la siguiente ganamos
                (return 3)
              )
            )
          )

          (if (and (eq (- ?colNegra ?colBlanca) 1) (eq ?filaNegra ?filaBlanca)) then ;negra derecha de blanca y misma fila
            (if (neq (mod ?colBlanca ?tamanoFila) 1) then
             (return 3)
            )
          else 
            (if (neq (mod ?colNegra ?tamanoFila) 0) then ;blanca derecha de negra
              (return 3)
            )
          )
        )
      else  ; hay damas
        (if (eq ?color 1) then ;somos blancas
          (if (and (eq ?damaBlanca 1)(eq ?damaNegras 0)) then ;una dama blanca
            (if (and (eq ?filaBlanca ?filaNegra) (neq (mod ?colNegra ?tamanoFila) 0) (neq (mod ?colNegra ?tamanoFila) 1)) then
              (return 3)
            else
              (if (and (eq ?colBlanca ?colNegra) (neq ?filaNegra 1) (neq ?filaNegra ?tamanoFila)) then
                (return 3)
              )
            )
          else 
            (if (and (eq ?damaBlanca 0)(eq ?damaNegras 1)) then ;una dama negra
              (if (and (eq ?colBlanca ?colNegra) (eq (- ?filaBlanca ?filaNegra) 1) (neq ?filaNegra 1)) then
                (return 3)
              else
                (if (and (eq ?filaBlanca ?filaNegra) (or (eq (- ?colNegra ?colBlanca) 1)(eq (- ?colBlanca ?colNegra) 1)) (neq (mod ?colNegra ?tamanoFila) 0) (neq (mod ?colNegra ?tamanoFila) 1)) then
                  (return 3)
                )
              )
            else
              (if (and (eq ?damaBlanca 1)(eq ?damaNegras 1)) then ;una dama de cada
                (if (and (eq ?colBlanca ?colNegra) (neq (mod ?colNegra ?tamanoFila) 0) (neq (mod ?colNegra ?tamanoFila) 1)) then
                  (return 3)
                else
                  (if (and (eq ?filaBlanca ?filaNegra) (neq ?filaNegra 1) (neq ?filaNegra ?tamanoFila)) then
                    (return 3)
                  )
                )
              )
            )
          )
        else ;somos negras
          (if (and (eq ?damaBlanca 0)(eq ?damaNegras 1)) then ;una dama negra
            (if (and (eq ?colBlanca ?colNegra) (neq ?filaBlanca 1) (neq ?filaBlanca ?tamanoFila)) then
              (return 3)
            else
              (if (and (eq ?filaBlanca ?filaNegra) (neq (mod ?colBlanca ?tamanoFila) 0) (neq (mod ?colBlanca ?tamanoFila) 1)) then
                (return 3)
              )
            )
          else 
            (if (and (eq ?damaBlanca 1)(eq ?damaNegras 0)) then ;una dama blanca
              (if (and (eq ?colBlanca ?colNegra) (eq (- ?filaBlanca ?filaNegra) 1) (neq ?filaBlanca ?tamanoFila)) then
                (return 3)
              else
                (if (and (eq ?filaBlanca ?filaNegra) (or (eq (- ?colNegra ?colBlanca) 1)(eq (- ?colBlanca ?colNegra) 1)) (neq (mod ?colBlanca ?tamanoFila) 0) (neq (mod ?colBlanca ?tamanoFila) 1)) then
                  (return 3)
                )
              )
            else
              (if (and (eq ?damaBlanca 1)(eq ?damaNegras 1)) then ;una dama de cada
                (if (and (eq ?colBlanca ?colNegra)(neq ?filaBlanca 1) (neq ?filaBlanca ?tamanoFila) ) then
                  (return 3)
                else
                  (if (and (eq ?filaBlanca ?filaNegra)(neq (mod ?colBlanca ?tamanoFila) 0) (neq (mod ?colBlanca ?tamanoFila) 1)) then
                    (return 3)
                  )
                )
              )
            )
          )
        )
      )
    else ;el siguiente es de ellos
      (if (and (eq ?damaBlanca 0) (eq ?damaNegras 0)) then ;no hay damas
        (if (eq ?color 1) then ;somos blancas
          (if (and (eq (- ?filaNegra ?filaBlanca) 1) (eq ?colNegra ?colBlanca)) then ;negra abajo de blanca misma col
            (return 2)
          else 
            (if (or (eq (- ?filaBlanca ?filaNegra) 1)(eq ?colNegra ?colBlanca)) then ;blanca abajo de negra misma col
              (if (neq ?filaBlanca ?tamanoFila) then ;en la siguiente perdemos
                (return 1)
              )
            )
          )

          (if (and (eq (- ?colNegra ?colBlanca) 1) (eq ?filaNegra ?filaBlanca)) then ;negra derecha de blanca y misma fila
            (if (neq (mod ?colBlanca ?tamanoFila) 1) then
             (return 1)
            )
          else 
            (if (neq (mod ?colBlanca ?tamanoFila) 0) then ;blanca derecha de negra
              (return 1)
            )
          )

        else ;somos negras
          (if (and (eq (- ?filaNegra ?filaBlanca) 1) (eq ?colNegra ?colBlanca)) then ;negra abajo de blanca misma col
            (return 2)
          else 
            (if (or (eq (- ?filaBlanca ?filaNegra) 1)(eq ?colNegra ?colBlanca)) then ;blanca abajo de negra misma col
              (if (neq ?filaNegra 0) then ;en la siguiente perdemos
                (return 1)
              )
            )
          )

          (if (and (eq (- ?colNegra ?colBlanca) 1) (eq ?filaNegra ?filaBlanca)) then ;negra derecha de blanca y misma fila
            (if (neq (mod ?colNegra ?tamanoFila) 0) then
             (return 1)
            )
          else 
            (if (neq (mod ?colNegra ?tamanoFila) 1) then ;blanca derecha de negra
              (return 1)
            )
          )
        )

      else  ; hay damas
        (if (eq ?color 1) then ;somos blancas
          (if (and (eq ?damaBlanca 0)(eq ?damaNegras 1)) then ;una dama negra
            (if (and (eq ?filaBlanca ?filaNegra) (neq (mod ?colBlanca ?tamanoFila) 0) (neq (mod ?colBlanca ?tamanoFila) 1)) then
              (return 1)
            else
              (if (and (eq ?colBlanca ?colNegra) (neq ?filaBlanca 1) (neq ?filaBlanca ?tamanoFila)) then
                (return 1)
              )
            )
          else 
            (if (and (eq ?damaBlanca 1)(eq ?damaNegras 0)) then ;una dama blanca
              (if (and (eq ?colBlanca ?colNegra) (eq (- ?filaBlanca ?filaNegra) 1) (neq ?filaBlanca ?tamanoFila)) then
                (return 1)
              else
                (if (and (eq ?filaBlanca ?filaNegra) (or (eq (- ?colNegra ?colBlanca) 1)(eq (- ?colBlanca ?colNegra) 1)) (neq (mod ?colBlanca ?tamanoFila) 0) (neq (mod ?colBlanca ?tamanoFila) 1)) then
                  (return 1)
                )
              )
            else
              (if (and (eq ?damaBlanca 1)(eq ?damaNegras 1)) then ;una dama de cada
                (if (and (eq ?filaBlanca ?filaNegra) (neq (mod ?colBlanca ?tamanoFila) 0) (neq (mod ?colBlanca ?tamanoFila) 1)) then
                  (return 1)
                else
                  (if (and (eq ?colBlanca ?colNegra) (neq ?filaBlanca 1) (neq ?filaBlanca ?tamanoFila)) then
                    (return 1)
                  )
                )
              )
            )
          )
        else ;somos negras
          (if (and (eq ?damaBlanca 1)(eq ?damaNegras 0)) then ;una dama blanca
            (if (and (eq ?colBlanca ?colNegra) (neq ?filaNegra 1) (neq ?filaNegra ?tamanoFila)) then
              (return 1)
            else
              (if (and (eq ?filaBlanca ?filaNegra) (neq (mod ?colNegra ?tamanoFila) 0) (neq (mod ?colNegra ?tamanoFila) 1)) then
                (return 1)
              )
            )
          else 
            (if (and (eq ?damaBlanca 0)(eq ?damaNegras 1)) then ;una dama negra
              (if (and (eq ?colBlanca ?colNegra) (eq (- ?filaBlanca ?filaNegra) 1) (neq ?filaNegra 1)) then
                (return 1)
              else
                (if (and (eq ?filaBlanca ?filaNegra) (or (eq (- ?colNegra ?colBlanca) 1)(eq (- ?colBlanca ?colNegra) 1)) (neq (mod ?colNegra ?tamanoFila) 0) (neq (mod ?colNegra ?tamanoFila) 1)) then
                  (return 1)
                )
              )
            else
              (if (and (eq ?damaBlanca 1)(eq ?damaNegras 1)) then ;una dama de cada
                (if (and (eq ?colBlanca ?colNegra)(neq ?filaNegra 1) (neq ?filaNegra ?tamanoFila) ) then
                  (return 1)
                else
                  (if (and (eq ?filaBlanca ?filaNegra)(neq (mod ?colNegra ?tamanoFila) 0) (neq (mod ?colNegra ?tamanoFila) 1)) then
                    (return 1)
                  )
                )
              )
            )
          )
        )

      )    
    )
    (return 2)
  )
)



(defrule pedir-movimiento
  (tam ?tamanoFila)
  ?b <- (idActual ?ID)
  (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo)(turno ?i))
  (colorReal ?tur)
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
  (retract ?b)

  (if (eq ?i 1) then

    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?newMap) (profundidad 0) (movs (create$ ?lineaOrigen ?lineaDestino)) (Min 0) (Max 0) (turno -1)))
    (assert (turno 1))

  else
    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?newMap) (profundidad 0) (movs (create$ ?lineaOrigen ?lineaDestino)) (Min 0) (Max 0) (turno 1)))
    (assert (turno -1))
  )
  (assert (idActual (+ ?ID 1)))
  (imprimir-mapeo $?newMap)
  (printout t "Movimientos: " ?lineaOrigen " " ?lineaDestino crlf)
)




;pasar a las funciones por parametro el tablero y el color de la ficha
;mov izq
(defrule mov-izquierda
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo)(movs $?movsPa))
  (colorIA ?IA)
  ?contTabs <- (tabsCont ?numTab)

=>
  (bind ?tamanoFila (integer (sqrt (length$ $?mapeo))))  
  (bind ?i 0)
  (bind ?pararMovDama 0)
  (bind ?reinaHaComido 0)
  (bind ?hacerAssert 1)
  (bind ?seguirComiendo 1) ; NOSE SI HACE FALTA AQUI INICIALIZARLO
  
  (bind ?numSigTab (+ ?numTab1 1));;;

  (foreach ?ficha $?mapeo 
    (bind ?i (+ ?i 1))
    
    (if( and (neq ?ficha -2) (neq ?ficha 2)) then  ;2 y -2 son las reinas blancas y negras
      
      (bind ?posOrigen ?i)
      (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1)) ;calculo de la fila origen
      (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1)) ;calculo de la columna origen
      (bind ?posDestino (- ?posOrigen 1)) ; aunq 
      (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo)) ; ficha origen para saber si es -1, 1, 2 o -2 la ficha
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))    

      (bind $?newMov  (create$ ?filaOrigen ?colOrigen))

      (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2))) then ;comprobacion posicion borde
  
        (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
          (bind ?newId ?numSigTab)
          (bind ?newPadre ?ID)
          (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
          (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
          (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?newMap) (profundidad 0) (movs (create$ $?movs ?filaOrigen (- ?colOrigen 1))) (Min 0) (Max 0) (turno ?IA)))
          
          (bind ?numSigTab (+ ?numTab1 1))

        else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria
          (if (and (neq (mod (- ?posOrigen 1) ?tamanoFila) 1) (eq (nth$ (- ?posDestino 1) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0
            (bind ?newId ?numSigTab)
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo (- ?posOrigen 2) (- ?posOrigen 2) ?fichaOrigen))
            (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen 1) (- ?posOrigen 1) 0))
            (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
            (bind $?newMov (create$ $?newMov ?filaOrigen (- ?colOrigen 2)))
            (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
            (bind ?numSigTab (+ ?numTab1 1))

            (if (eq (length$ $?comerMas) 0) ;el peon esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
              (bind ?pararMovDama 1)
            else
              (bind ?j 1)
              (?tamTablero (* ?tamanoFila ?tamanoFila))
              (while (and (<= ?j (length$ $?comerMas)))
                (bind ?reina (nth$ ?j $?comerMas))
                (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                (bind $?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                (bind ?j (+ ?j ?tamTablero))
                (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) 
                (bind $?newMov (create$ $?newMov ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                
                (if (eq ?reina 1) then ;(or (eq ?destinoFila 1) (eq ?destinoFila ?tamanoFila)) then
                  (bind ?reinaCreada 1)
                else
                  (bind ?z 1)
                  (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)) (eq ?reinaCreada 0))
                    (bind ?hacerAssert 0)
                    (bind ?reina (nth$ ?z $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                    (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                    (bind $?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                    (bind ?z (+ ?z ?tamTablero))
                    (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?mapeoComer) (profundidad 0) (movs (create$ $?newMov ?destinoFila ?destinoCol)) (Min 0) (Max 0) (turno ?IA)))
                  ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                )
                (if (eq ?hacerAssert 1) then
                  (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?mapeoComer) (profundidad 0) (movs (create$ $?newMov ?destinoFila ?destinoCol)) (Min 0) (Max 0) (turno ?IA)))
                )
                ;(bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
              )         
            ) 
          )  
        )
      )
    else ;aqui se entra cuando la ficha es una dama CUANDO ES UNA DAMA DAMA DAMA DAMA
      (bind $?movs (create$ ?filaOrigen ?colOrigen))

      (while (eq ?pararMovDama 0) ;si la var es 0 significa q se puede seguir moviendo la dama hacia la derecha 
        (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (div ?fichaOrigen 2)) ) then ;comprobacion posicion borde
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId ?numSigTab)
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))

            (bind ?numSigTab (+ ?numTab1 1))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?newMap) (profundidad 0) (movs (create$ $?movs ?filaOrigen (- ?colOrigen 1))) (Min 0) (Max 0) (turno ?IA)))
        
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId ?numSigTab)
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (- ?posOrigen 2) (- ?posOrigen 2) ?fichaOrigen))
              (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen 1) (- ?posOrigen 1) 0))
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
              (bind ?numSigTab (+ ?numTab1 1))  

              (bind $?newMov (create$ ?filaOrigen (- ?colOrigen 2)))

              (bind ?reinaHaComido 1)
              ;(assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?newMap) (profundidad 0) (movs (bind $?movs (create$ $?movs ?filaOrigen (- ?colOrigen 2)))) (Min 0) (Max 0) (turno ?IA)))
              
              (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
              (if (eq (length$ $?comerMas) 0) ;la reina esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                (bind ?pararMovDama 1)
              else
                (bind ?j 1)
                (?tamTablero (* ?tamanoFila ?tamanoFila))
                (while (and (<= ?j (length$ $?comerMas)) (eq ?stop 0))
                  (bind ?reina (nth$ ?j $?comerMas))
                  (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                  (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                  (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                  (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                  (bind ?j (+ ?j ?tamTablero))
                  (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                  (bind $?newMov (create$ $?newMov ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                  (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  (bind ?z 1)
                  (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)))
                    (bind ?hacerAssert 0)
                    (bind ?reina (nth$ ?z $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                    (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                    (bind ?z (+ ?z ?tamTablero))
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?mapeoComer) (profundidad 0) (movs (create$ $?movs $?newMov ?destinoFila ?destinoCol)) (Min 0) (Max 0) (turno ?IA)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?mapeoComer) (profundidad 0) (movs (create$ $?movs $?newMov? ?destinoFila ?destinoCol)) (Min 0) (Max 0) (turno ?IA)))
                  ) 
                  ;(bind ?stop 1)
                )
              ) 
            )
          )

          (bind ?posOrigen (- ?posOrigen 1))
          (bind ?filaOrigen (+ (div(- ?posOrigen 1) ?tamanoFila)1))
          (bind ?colOrigen (+ (mod(- ?posOrigen 1) ?tamanoFila) 1))
          (bind ?posDestino (- ?posOrigen 1))
          (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
          (bind ?fichaDestino (nth$ ?posDestino $?mapeo))  

        else ;este else indica q la reina esta en una posicion borde
          (if (eq ?reinaHaComido 0) then ;en este caso si esta en borde y no ha comido ya no puede seguir movviendose
            (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while
          else
            (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
              (if (eq (length$ $?comerMas) 0) ;la reina esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                (bind ?pararMovDama 1)
              else
                (bind ?j 1)
                (?tamTablero (* ?tamanoFila ?tamanoFila))
                (while (and (<= ?j (length$ $?comerMas)) (eq ?stop 0))
                  (bind ?reina (nth$ ?j $?comerMas))
                  (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                  (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                  (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                  (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                  (bind ?j (+ ?j ?tamTablero))
                  (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                  (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                  (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  (bind ?z 1)
                  (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)))
                    (bind ?hacerAssert 0)
                    (bind ?reina (nth$ ?z $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                    (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                    (bind ?z (+ ?z ?tamTablero))
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?mapeoComer) (profundidad 0) (movs (create$ $?movs $?newMov ?destinoFila ?destinoCol)) (Min 0) (Max 0) (turno ?IA)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0) (mapeo $?mapeoComer) (profundidad 0) (movs (create$ $?movs $?newMov? ?destinoFila ?destinoCol)) (Min 0) (Max 0) (turno ?IA)))
                  ) 
                  ;(bind ?stop 1)
                )
              )  
          )
        )  
      )
    )
  )
)          ; SOBRA CREO (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)



  

;mov der
(defrule mov-derecha
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo)(movs $?movs))
  (colorIA ?IA)

=>
  (bind ?tamanoFila (integer(sqrt(length$ $?mapeo))))  (bind ?i 0)
  (bind ?pararMovDama 0)
  (bind ?reinaHaComido)
  (bind ?hacerAssert 1)
  (foreach ?ficha $?mapeo ;se iteran todas las fichas del mapeo HAY Q PONER Q SOLO SE ITEREN LAS DEL TURNO Q TOCA??? (con lo de la profundidad y tal)
    (bind ?i (+ ?i 1))
  
    (if(and(neq ?ficha -2) (neq ?ficha 2)) then  ;2 y -2 son las reinas blancas y negras
  
      (bind ?posOrigen ?i)
      (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
      (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
      (bind ?posDestino (+ ?posOrigen 1))
      (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))

      (if (and (neq (mod ?posOrigen ?tamanoFila) 0) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2))) then 
        (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
          (bind ?newId (+ ?ID 1))
          (bind ?newPadre ?ID)
          (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
          (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
          (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ ?filaOrigen (+ ?colOrigen 1)))) (Min 0) (Max 0)))
        
        else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
          (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen 2) (+ ?posOrigen 2) ?fichaOrigen))
            (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen 1) (+ ?posOrigen 1) 0))
            (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
            (bind $?newMov (bind ?movs (create$ ?filaOrigen (+ ?colOrigen 2))))
            (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
              (if (eq (length$ $?comerMas) 0) ;el peon esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                (bind ?pararMovDama 1)
              else
                (bind ?j 1)
                (?tamTablero (* ?tamanoFila ?tamanoFila))
                (while (and (<= ?j (length$ $?comerMas)))
                  (bind ?reina (nth$ ?j $?comerMas))
                  (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                  (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                  (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                  (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                  (bind ?j (+ ?j ?tamTablero))
                  (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                  (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) 
                  (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                  (if (or(eq ?destinoFila 1) (eq ?destinoFila ?tamanoFila)) then
                    (bind ?reinaCreada 1)
                  else
                    (bind ?z 1)
                      (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)) (eq ?reinaCreada 0))
                    ;nose
                        (bind ?hacerAssert 0)
                        (bind ?reina (nth$ ?z $?comerMas))
                        (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                        (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                        (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                        (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                        (bind ?z (+ ?z ?tamTablero))
                        (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                      ) 
                      (if (eq ?hacerAssert 1) then
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ) 
                  )
                    ;(bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
                )         
              ;(assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))     
              ) 
               )  
            )
       )
    
    else                                  ;este else es para cuando la ficha q toca es una reina                                       ;hay q hacer un while se puede seguir moviendo la reina...
      
      (while (eq ?pararMovDama 0) ;si la var es 0 significa q se puede seguir moviendo la dama hacia la derecha 
        (if (and (neq (mod ?posOrigen ?tamanoFila) 0) (neq ?fichaOrigen ?fichaDestino) ) then 
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ ?filaOrigen (+ ?colOrigen 1)))) (Min 0) (Max 0)))
        
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen 2) (+ ?posOrigen 2) ?fichaOrigen))
              (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen 1) (+ ?posOrigen 1) 0))
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
              (bind ?reinaHaComido 1)
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ ?filaOrigen (+ ?colOrigen 2)))) (Min 0) (Max 0)))
            )
          )
              (bind ?posOrigen (+ ?posOrigen 1))
              (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
              (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
              (bind ?posDestino (+ ?posOrigen 1))
              (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
              (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            else ;este else indica q la reina esta en una posicion borde
              (if (eq ?reinaHaComido 0) then ;en este caso si esta en borde y no ha comido ya no puede seguir movviendose
                (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while
              else
                (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
                (if (eq (length$ $?comerMas) 0) ;la reina esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                  (bind ?pararMovDama 1)
                else
                  (bind ?j 1)
                  (?tamTablero (* ?tamanoFila ?tamanoFila))
                  (while (and (<= ?j (length$ $?comerMas)) (eq ?stop 0))
                    (bind ?reina (nth$ ?j $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                    (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                    (bind ?j (+ ?j ?tamTablero))
                    (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                    (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                    (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                    (bind ?z 1)
                    (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)))
                      (bind ?hacerAssert 0)
                      (bind ?reina (nth$ ?z $?comerMas))
                      (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                      (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                      (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                      (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                      (bind ?z (+ ?z ?tamTablero))
                      (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                  ) 
                  ;(bind ?stop 1)
                )
                  ; SOBRA CREO (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
              )
            )
        )
      )
    )
  )
)


;mov abajo

(defrule mov-abajo
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo ?mapeo)(movs $?movs))
  (colorIA ?IA)
=>
; HAY QUE MOVEER EL IF?????? REVUSAR EL IF LO DE IA -1
  ;(if (eq ?IA -1) then ;solo se genera movimiento hacia abajo para las fichas negras (y para las damas)
    (bind ?tamanoFila (integer(sqrt(length$ $?mapeo))))    
    (bind ?i 0)
    (bind ?pararMovDama 0)
    (bind ?seguirComiendo 1)
    (bind ?reinaCreada 1)
    (bind ?hacerAssert 1)
    
    (foreach ?ficha $?mapeo
      (bind ?i (+ ?i 1))
      (if (eq ?ficha -1) then  ;si la ficha es negra normal
        (bind ?posOrigen ?i)
        (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
        (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
        (bind ?filaDestino (+ ?filaOrigen 1))
        (bind ?colDestino ?colOrigen)    
        (bind ?posDestino (+ ?posOrigen ?tamanoFila))
        (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
        (bind ?fichaDestino (nth$ ?posDestino $?mapeo))

        (if (and (neq ?filaOrigen ?tamanoFila) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)))  then ;comprobacion posicion borde
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId (+ ?ID 1))
            (bind $?newPadre ?ID)

            (if (eq ?filaDestino ?tamanoFila) then ;va a crearse una reina
              (bind ?reinaCreada 1)
              (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino -2)) ;se crea una reina negra, por eso el -2
              (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (+ ?filaOrigen 1) ?colOrigen))) (Min 0) (Max 0)))
            else
              (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen)) ;simplemente se mueve sin crear reina
              (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (+ ?filaOrigen 1) ?colOrigen))) (Min 0) (Max 0)))
            )
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq ?filaDestino (?tamanoFila) (eq (nth$ (+ ?posDestino ?tamanoFila) $?mapeo) 0))) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
            
              (if (eq ?filaOrigen (- ?tamanoFila 2)) then ;si entra en este if se va a crear una reina
                (bind ?reinaCreada 1)
                (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen (* ?tamanoFila 2)) (+ ?posOrigen (* ?tamanoFila 2)) -2)) ; posicion a la q salta (se pone una reina negra)
                (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
                (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien
                (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (+ ?filaOrigen 2) ?colOrigen))) (Min 0) (Max 0)))
            
              else
                (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen (* ?tamanoFila 2)) (+ ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
                (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
                (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien
                (bind $?newMov (bind ?movs (create$ (+ ?filaOrigen 2) ?colOrigen)))
                (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
                (if (eq (length$ $?comerMas) 0) ;el peon esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                  (bind ?pararMovDama 1)
                else
                  (bind ?j 1)
                  (?tamTablero (* ?tamanoFila ?tamanoFila))
                  (while (and (<= ?j (length$ $?comerMas)))
                    (bind ?reina (nth$ ?j $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                    (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                    (bind ?j (+ ?j ?tamTablero))
                    (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                    (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) 
                    (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                    (if (or(eq ?destinoFila 1) (eq ?destinoFila ?tamanoFila)) then
                      (bind ?reinaCreada 1)
                    else
                      (bind ?z 1)
                        (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)) (eq ?reinaCreada 0))
                    ;nose
                          (bind ?hacerAssert 0)
                          (bind ?reina (nth$ ?z $?comerMas))
                          (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                          (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                          (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                          (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                          (bind ?z (+ ?z ?tamTablero))
                          (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                          (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                        ) 
                      (if (eq ?hacerAssert 1) then
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ) 
                  )
                    ;(bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
                )         
              ) 
            )  
          )
      )
  else ;por aqui cuando la ficha es una dama

    (if(or(eq ?ficha 2) (eq ?ficha -2)) then ;la ficha es una dama
      (while (and (eq ?pararMovDama 0) (eq ?seguirComiendo 1));si la var es 0 significa q se puede seguir moviendo la dama hacia abajo 
        (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) ) then ;comprobacion posicion borde
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (+ ?filaOrigen 1) ?colOrigen))) (Min 0) (Max 0)))
            
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen (* ?tamanoFila 2)) (+ ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
              (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien   
              (bind ?reinaHaComido 1)
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (+ ?filaOrigen 2) ?colOrigen))) (Min 0) (Max 0)))
            )
          )
              (bind ?posOrigen (+ ?posOrigen ?tamanoFila))
              (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
              (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
              (bind ?posDestino (+ ?posOrigen ?tamanoFila))
              (bind ?filaDestino (+ ?filaOrigen 1))
              (bind ?colDestino ?colOrigen)  
              (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
              (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            else ;este else indica q la reina esta en una posicion borde
          (if (eq ?reinaHaComido 0) then ;en este caso si esta en borde y no ha comido ya no puede seguir movviendose
            (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while
          else
            (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
              (if (eq (length$ $?comerMas) 0) ;la reina esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                (bind ?pararMovDama 1)
              else
                (bind ?j 1)
                (?tamTablero (* ?tamanoFila ?tamanoFila))
                (while (and (<= ?j (length$ $?comerMas)) (eq ?stop 0))
                  (bind ?reina (nth$ ?j $?comerMas))
                  (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                  (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                  (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                  (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                  (bind ?j (+ ?j ?tamTablero))
                  (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                  (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                  (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  (bind ?z 1)
                  (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)))
                    (bind ?hacerAssert 0)
                    (bind ?reina (nth$ ?z $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                    (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                    (bind ?z (+ ?z ?tamTablero))
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                  ) 
                  ;(bind ?stop 1)
                )
              )  
          )
        )  
      )
    )
  )
)          ; SOBRA CREO (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
    ))



;mov arriba
(defrule mov-arriba
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo ?mapeo)(movs $?movs))
  (origen ?filaOrigen ?colOrigen)
  (colorIA ?IA)

=>
;falta el loop que vaya cogiendo cada ficha y dentro del loop comprobacion de si la ficha es del color q toca
  ;(if (eq ?IA -1) then ;solo se genera movimiento hacia abajo para las fichas blancas
    (bind ?tamanoFila (integer(sqrt(length$ $?mapeo))))    
    (bind ?i 0)
    (bind ?pararMovDama 0)
    (bind ?reinaHaComido 0)
    (bind ?hacerAssert 1)
    (bind ?seguirComiendo 1)

  (foreach ?ficha $?mapeo
    (bind ?i (+ ?i 1))
    (if (eq ?ficha -1) then  ;si la ficha es blanca normal    
      (bind ?posOrigen ?i)
      (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
      (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
      (bind ?filaDestino (- ?filaOrigen 1))
      (bind ?colDestino ?colOrigen)
      (bind ?posDestino (+ ?posOrigen ?tamanoFila))
      (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
      
      (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2))) then
        (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
          (bind ?newId (+ ?ID 1))
          (bind $?newPadre ?ID)

          (if  (eq ?filaDestino 1) then ;se crea una reina blanca
            (bind ?reinaCreada 1)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino 2))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (- ?filaOrigen 1) ?colOrigen))) (Min 0) (Max 0)))
          else
                (bind $?auxMap (replace$ $?mapeo (- ?posOrigen (* ?tamanoFila 2)) (- ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
                (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
                (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien
                (bind $?newMov (bind ?movs (create$ (- ?filaOrigen 2) ?colOrigen)))
                (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
                (if (eq (length$ $?comerMas) 0) ;el peon esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                  (bind ?pararMovDama 1)
                else
                  (bind ?j 1)
                  (?tamTablero (* ?tamanoFila ?tamanoFila))
                  (while (and (<= ?j (length$ $?comerMas)))
                    (bind ?reina (nth$ ?j $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                    (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                    (bind ?j (+ ?j ?tamTablero))
                    (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                    (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) 
                    (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                    (if (or(eq ?destinoFila 1) (eq ?destinoFila ?tamanoFila)) then
                      (bind ?reinaCreada 1)
                    else
                      (bind ?z 1)
                        (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)) (eq ?reinaCreada 0))
                    ;nose
                          (bind ?hacerAssert 0)
                          (bind ?reina (nth$ ?z $?comerMas))
                          (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                          (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                          (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                          (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                          (bind ?z (+ ?z ?tamTablero))
                          (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                          (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                        ) 
                      (if (eq ?hacerAssert 1) then
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ) 
                  )
                    ;(bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
                )         
              ) 
            )  
          )
      )
    else
      

    (if(or(eq ?ficha 2) (eq ?ficha -2)) then ;la ficha es una dama
      (while (and (eq ?pararMovDama 0) (eq ?seguirComiendo 1));si la var es 0 significa q se puede seguir moviendo la dama hacia abajo 
        (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino)) then          
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco  
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (- ?filaOrigen 1) ?colOrigen))) (Min 0) (Max 0)))
            
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen (* ?tamanoFila 2)) (+ ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
              (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien   
              (bind ?reinaHaComido 1)
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs (create$ (- ?filaOrigen 2) ?colOrigen))) (Min 0) (Max 0)))
            )
          )
              (bind ?posOrigen (+ ?posOrigen ?tamanoFila))
              (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
              (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
              (bind ?posDestino (+ ?posOrigen ?tamanoFila))
              (bind ?filaDestino (+ ?filaOrigen 1))
              (bind ?colDestino ?colOrigen)  
              (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
              (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            else ;este else indica q la reina esta en una posicion borde
          (if (eq ?reinaHaComido 0) then ;en este caso si esta en borde y no ha comido ya no puede seguir movviendose
            (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while
          else
            (bind $?comerMas seguirComiendo(?posOrigen $?newMap)) ;interpretamos lo que devuelve esta llamada para saber si se puede seguir comiendo 
              (if (eq (length$ $?comerMas) 0) ;la reina esta en una posicion borde y no puede seguir comiendo asiq ponemos ?pararMov a 1 para salir del while
                (bind ?pararMovDama 1)
              else
                (bind ?j 1)
                (?tamTablero (* ?tamanoFila ?tamanoFila))
                (while (and (<= ?j (length$ $?comerMas)) (eq ?stop 0))
                  (bind ?reina (nth$ ?j $?comerMas))
                  (bind ?destinoFila (nth$ (+ ?j 1) $?comerMas))
                  (bind ?destinoCol (nth$ (+ ?j 2) $?comerMas))
                  (bind ?j (+ ?j 3)) ;aqui en la primera iteracion la j vale 4
                  (bind ?mapeoComer (subseq$ $?comerMas ?j (+ ?j (- ?tamTablero 1))))
                  (bind ?j (+ ?j ?tamTablero))
                  (bind ?posOrigen (+ (* ?tamanoFila (- ?destinoFila 1)) ?destinoCol))
                  (bind ?newMov (create$ ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
                  (bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  (bind ?z 1)
                  (while (and (neq (length$ $?auxComerMas) 0) (<= ?z (length$ $?comerMas)))
                    (bind ?hacerAssert 0)
                    (bind ?reina (nth$ ?z $?comerMas))
                    (bind ?destinoFila (nth$ (+ ?z 1) $?comerMas))
                    (bind ?destinoCol (nth$ (+ ?z 2) $?comerMas))
                    (bind ?z (+ ?z 3)) ;aqui en la primera iteracion la j vale 4
                    (bind ?mapeoComer (subseq$ $?comerMas ?z (+ ?z (- ?tamTablero 1))))
                    (bind ?z (+ ?z ?tamTablero))
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov (create$ ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                  ) 
                  ;(bind ?stop 1)
                )
              )  
          )
        )  
      )
    )
  )
)          ; SOBRA CREO (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
    )


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
            (jugador -1)
          )
  )

  (imprimir-mapeo $?mapa)
  ;(modify ?tabNum (idActual (+ 1 ?y)))
  (assert (idActual 0))
  (assert (turno 1))
  (assert (tabsCont 0))

)
