; +1 blanco, +2 dama blanca
; -1 negro, -2 dama negra

; recibe una casilla y devuele un +1, -1 o 0 dependiendo del color que sea
(deffunction que-color-es (?casilla)
  (if (or (eq ?casilla +1) (eq ?casilla +2)) then (return +1))
  (if (or (eq ?casilla -1) (eq ?casilla -2)) then (return -1))
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

; Le pasas el mapeo del tablero y te devuelve la función de evaluación
; ?color indica con que color esta jugando la IA para saber si tiene que maximizar o minimizar
; esta funcion por ahora no tiene en cuenta que el jugador es +1
(deffunction eval-function (?color $?tablero)
  (bind ?valor 0)
  (foreach ?casilla $?tablero 
    (bind ?valor (+ ?valor ?casilla))
  )
  (return ?valor)
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
        (bind ?mine (+ ?mine 1))

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
        (bind ?opp (+ ?opp +1))
        ; comprobamos si me pueden comer
        (if (neq (length$ (seguirComiendo ?i $?tablero)) 0) then (bind ?result (- ?result 3)))
      )
    )
    (bind ?i (+ ?i 1))
  )
  (return (+ ?result (* (- ?mine ?opp) 1000) )
)

