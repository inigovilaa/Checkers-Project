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
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo)(movs $?movs))
  ;(origen ?filaOrigen ?colOrigen)
    (colorIA ?IA)

=>
  (bind ?tamanoFila (sqrt (length$ $?mapeo)))
  (bind ?i 0)
  (bind ?pararMovDama 0)
  (bind ?reinaHaComido 0)
  (bind ?hacerAssert 1)
  (foreach ?ficha $?mapeo 
    
    (bind ?i (+ ?i 1))
    
    (if( and (neq ?ficha -2) (neq ?ficha 2)) then  ;2 y -2 son las reinas blancas y negras
      
      (bind ?posOrigen ?i)
      (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1)) ;calculo de la fila origen
      (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1)) ;calculo de la columna origen
      (bind ?posDestino (- ?posOrigen 1)) ; aunq 
      (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo)) ; ficha origen para saber si es -1, 1, 2 o -2 la ficha
      (bind ?fichaDestino (nth$ ?posDestino $?mapeo))    
    
    (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2))) then ;comprobacion posicion borde
  
        (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
          (bind ?newId (+ ?ID 1))
          (bind ?newPadre ?ID)
          (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
          (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
          (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs ($create ?filaOrigen (- ?colOrigen 1)))) (Min 0) (Max 0)))
        
        else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria
          (if (and (neq (mod (- ?posOrigen 1) ?tamanoFila) 1) (eq (nth$ (- ?posDestino 1) $?mapeo) 0)) then ;se comprueba primero que la posicion de al lado no es borde y despues que la posicion a la q se salta es 0
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo (- ?posOrigen 2) (- ?posOrigen 2) ?fichaOrigen))
            (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen 1) (- ?posOrigen 1) 0))
            (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
            (bind $?newMov (bind ?movs ($create ?filaOrigen (- ?colOrigen 2))))
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
                  (bind ?newMov ($create ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
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
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                      ) 
                      (if (eq ?hacerAssert 1) then
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ) 
                  )
                    ;(bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
                )         
              ;(assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))     
              ) 
               )  
            )
       )
  
    else ;aqui se entra cuando la ficha es una dama CUANDO ES UNA DAMA DAMA DAMA DAMA
      
      (while (eq ?pararMovDama 0) ;si la var es 0 significa q se puede seguir moviendo la dama hacia la derecha 
        (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) ) then ;comprobacion posicion borde
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs ($create ?filaOrigen (- ?colOrigen 1)))) (Min 0) (Max 0)))
        
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (- ?posOrigen 2) (- ?posOrigen 2) ?fichaOrigen))
              (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen 1) (- ?posOrigen 1) 0))
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
              (bind ?reinaHaComido 1)
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs ($create ?filaOrigen (- ?colOrigen 2)))) (Min 0) (Max 0)))
            )
          )
          (bind ?posOrigen (- ?posOrigen 1))
          (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
          (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
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
                  (bind ?newMov ($create ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
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
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
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

  

;mov der
(defrule mov-derecha
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo $?mapeo)(movs $?movs))
  (colorIA ?IA)

=>
  (bind ?tamanoFila (sqrt(length$ $?mapeo)))
  (bind ?i 0)
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

      (if (and (neq (mod ?posOrigen ?tamanoFila) 0) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)) then 
        (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
          (bind ?newId (+ ?ID 1))
          (bind ?newPadre ?ID)
          (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
          (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
          (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs ($create ?filaOrigen (+ ?colOrigen 1)))) (Min 0) (Max 0)))
        
        else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
          (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen 2) (+ ?posOrigen 2) ?fichaOrigen))
            (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen 1) (+ ?posOrigen 1) 0))
            (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
            (bind $?newMov (bind ?movs ($create ?filaOrigen (+ ?colOrigen 2))))
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
                  (bind ?newMov ($create ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
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
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                      ) 
                      (if (eq ?hacerAssert 1) then
                        (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                      ) 
                  )
                )         
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
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs ($create ?filaOrigen (+ ?colOrigen 1)))) (Min 0) (Max 0)))
        
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen 2) (+ ?posOrigen 2) ?fichaOrigen))
              (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen 1) (+ ?posOrigen 1) 0))
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0))
              (bind ?reinaHaComido 1)
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (bind ?movs ($create ?filaOrigen (+ ?colOrigen 2)))) (Min 0) (Max 0)))
    
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
                  (bind ?newMov ($create ?destinoFila ?destinoCol)) ; SE CONCATENA EL NUEVO MOVIMIENTO DEVUELTO POR LA LLAMADA SEGUIR COMIENDO
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
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                    ;(bind $?auxComerMas seguirComiendo(?posOrigen $?mapeoComer)) ;REVISAR POSORIGEN, PORQ HA CAMBIADO NO? 
                  )  
                  (if (eq ?hacerAssert 1) then
                    (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?mapeoComer) (profundidad 0) (movs (bind ?newMov ($create ?destinoFila ?destinoCol))) (Min 0) (Max 0)))
                  ) 
                  ;(bind ?stop 1)
                )
                  ; SOBRA CREO (bind ?pararMovDama 1)   ;se pone pararMovDama a 1 para salir del while principal (se pasa por aqui al salir del while de j...)
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
    (bind ?tamanoFila (sqrt(length$ $?mapeo)))
    (bind ?i 0)
    (bind ?pararMovDama 0)
    (bind ?seguirComiendo 1)
    (bind ?reinaCreada 1)
    
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

        (if (and (neq ?filaOrigen ?tamanoFila) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2))  then ;comprobacion posicion borde
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId (+ ?ID 1))
            (bind $?newPadre ?ID)

            (if (eq ?filaDestino ?tamanoFila) then ;va a crearse una reina
              (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino -2)) ;se crea una reina negra, por eso el -2
              (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
            else
              (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen)) ;simplemente se mueve sin crear reina
              (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
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
                (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
            
              else
                (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen (* ?tamanoFila 2)) (+ ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
                (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
                (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien
                (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
              ) 
            ) 
          )    
        )
  else 
    (if(or(eq ?ficha 2) (eq ?ficha -2)) then ;la ficha es una dama
      (while (and (eq ?pararMovDama 0) (eq ?seguirComiendo 1));si la var es 0 significa q se puede seguir moviendo la dama hacia la derecha 
        (if (and (neq (mod ?posOrigen ?tamanoFila) 1) (neq ?fichaOrigen ?fichaDestino) ) then ;comprobacion posicion borde
          (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
            
          else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq (mod (+ ?posOrigen 1) ?tamanoFila) 0) (eq (nth$ (+ ?posDestino 1) $?mapeo) 0)) then ;se comprueba que 
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo (+ ?posOrigen (* ?tamanoFila 2)) (+ ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
              (bind $?auxMap2 (replace$ $?auxMap (+ ?posOrigen ?tamanoFila) (+ ?posOrigen ?tamanoFila) 0)) ; posicion que come se pone a 0
              (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien   
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
        
              (bind ?posOrigen (+ ?posOrigen ?tamanoFila))
              (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
              (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
              (bind ?posDestino (+ ?posOrigen ?tamanoFila))
              (bind ?filaDestino (+ ?filaOrigen 1))
              (bind ?colDestino ?colOrigen)  
              (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
              (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
            )
          )
    else
      (bind ?pararMovDama 1)     
          )
        )    
      )  
    )
  ))
)


;mov arriba
(defrule mov-arriba
  (idActual ?ID)
  ?tab <- (tablero (ID ?ID)(padre ?padre)(mapeo ?mapeo)(movs $?movs))
  (origen ?filaOrigen ?colOrigen)
  (colorIA ?IA)

=>
;falta el loop que vaya cogiendo cada ficha y dentro del loop comprobacion de si la ficha es del color q toca
  ;(if (eq ?IA -1) then ;solo se genera movimiento hacia abajo para las fichas blancas
    (bind ?tamanoFila (sqrt(length$ $?mapeo)))
    (bind ?i 0)
    (bind ?pararMovDama 0)
    (bind ?seguirComiendo 1)

  (foreach ?ficha $?mapeo
    (bind ?i (+ ?i 1))
    (bind ?posOrigen ?i)
    (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
    (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
    (bind ?filaDestino (- ?filaOrigen 1))
    (bind ?colDestino ?colOrigen)
    (bind ?posDestino (+ ?posOrigen ?tamanoFila))
    (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
    (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
    
    (if (eq ?ficha -1) then  ;si la ficha es blanca normal    
      (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino) (neq ?fichaDestino (* ?fichaOrigen 2)) then
        (if (eq ?fichaDestino 0) then ;significa q la ficha de al lado hay hueco
          (bind ?newId (+ ?ID 1))
          (bind $?newPadre ?ID)

          (if  (eq ?filaDestino 1) then ;se crea una reina blanca
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino 2))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
          else
            (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
            (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
          )
        else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
          (if (and (neq ?filaDestino 1) (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba que        
            (bind ?newId (+ ?ID 1))
            (bind ?newPadre ?ID)
            (if eq ?filaOrigen (- ?tamanoFila 2) then ;si entra en este if se va a crear una reina blanca
              (bind ?reinaCreada 1)
              (bind $?auxMap (replace$ $?mapeo (- ?posOrigen (* ?tamanoFila 2)) (- ?posOrigen (* ?tamanoFila 2)) 2)) ; posicion a la q salta se crea una reina blanca
            else
              (bind $?auxMap (replace$ $?mapeo (- ?posOrigen (* ?tamanoFila 2)) (- ?posOrigen (* ?tamanoFila 2)) ?fichaOrigen)) ; posicion a la q salta
            )
            (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0)) ; posicion que come
            (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba
            (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs (create$ ?filaOrigen ?colOrigen)) (Min 0) (Max 0)))
          )     
        )
      )
    else
      (if(or(eq ?ficha 2) (eq ?ficha -2)) then ; si la ficha es dama
        (while (and (eq ?pararMovDama 0) (eq ?seguirComiendo 1)) ;si la var es 0 pararMovDama significa q se puede seguir moviendo la dama, si seguir comiendo es 0 es q no se puede seguir comiendo
          (if (and (neq ?filaOrigen 1) (neq ?fichaOrigen ?fichaDestino)) then
            (if (eq ?fichaDestino 0) then ;
              (bind ?newId (+ ?ID 1))
              (bind ?newPadre ?ID)
              (bind $?auxMap (replace$ $?mapeo ?posDestino ?posDestino ?fichaOrigen))
              (bind $?newMap (replace$ $?auxMap ?posOrigen ?posOrigen 0))  
              (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
              
            else ;caso en el que en la casilla a la q se quiere mover hay una ficha contraria    
            (if (and (neq ?filaDestino 1) (eq (nth$ (- ?posDestino ?tamanoFila) $?mapeo) 0)) then ;se comprueba que        
                (bind ?newId (+ ?ID 1))
                (bind ?newPadre ?ID)
                (bind $?auxMap (replace$ $?mapeo (- ?posOrigen (* ?tamanoFila 2)) (- ?posOrigen (* ?tamanoFila 2)) 2)) ; posicion a la q salta se crea una reina blanca
                (bind $?auxMap2 (replace$ $?auxMap (- ?posOrigen ?tamanoFila) (- ?posOrigen ?tamanoFila) 0)) ; posicion que come
                (bind $?newMap (replace$ $?auxMap2 ?posOrigen ?posOrigen 0)) ; posicion en la q estaba se pone a 0 tambien  
                (assert (tablero (ID ?newId) (padre ?newPadre) (heuristico 0.0) (mapeo $?newMap) (profundidad 0) (movs ) (Min 0) (Max 0)))
            )
            )
          (bind ?posOrigen (+ ?posOrigen ?tamanoFila))
          (bind ?filaOrigen (+(div(- ?posOrigen 1) ?tamanoFila)1))
          (bind ?colOrigen (+(mod(- ?posOrigen 1) ?tamanoFila) 1))
          (bind ?filaDestino (- ?filaOrigen 1))
          (bind ?colDestino ?colOrigen)
          (bind ?posDestino (+ ?posOrigen ?tamanoFila))
          (bind ?fichaOrigen (nth$ ?posOrigen $?mapeo))
          (bind ?fichaDestino (nth$ ?posDestino $?mapeo))
          )   
      ;else ;HAY QUE AÑADIR QUE SOLO PONGA LA SITUACION EN SITUACIONN BORDE si no se puede seguir comiendo, porq si se puede seguir comiendo hacia otra direccion
      ;da igual que este en el borde
;si no ha comido y esta en el borde se para
      (if )
          (bind ?pararMovDama 1)     
        )
      ) 
    )   )
  ) ; cierra el foreach
)  


;pasar a las funciones por parametro el tablero y el color de la ficha


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
      (assert (turno 1))
    else
      (assert (turno 0))
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
            (esHoja 0)
          )
  )

  (imprimir-mapeo $?mapa)
  ;(modify ?tabNum (idActual (+ 1 ?y)))
  (assert (idActual 0))
  (retract ?a)
)


