; AVISO A NAVEGANTES: las condiciones de ejecucion no las he comprobado y es posible que no sean las adecuadas. CAMBIAR

; Recibe una depth y dos heuristicos (padre e hijo).
; devuelve un booleano que indica si hay que modificar el padre
(deffunction hay-que-modificar (?depth ?padre ?hijo)
  ; par => minimizar
  (if (eq (mod ?depth 2) 0) then
    (return (> ?padre ?hijo))

  ; impar => maximizar
  else 
    (return (< ?padre ?hijo))
  )

)

; calcula todos los heuristicos de las hojas
(deffunction calcular-heuristicos ()
  (do-for-all-facts ((?var tablero)) (eq ?var:esHoja 1)
    (bind ?heur (eval-function2 ?color ?tam ?var:mapeo))
    (modify ?var (esHoja 1) (heuristico ?heur))
  )
)

(deffunction anadir-si-no-esta (?elemento $?lista)
  ; Añadir
  (if (eq (member$ ?elemento $?lista) FALSE) then (
    (bind $?lista (create$ $?lista ?elemento))
  ))
  (return $?lista)
)

; REGLAS

(defrule heuristico
  ?d <- (depth ?depth)
=>
  (calcular-heuristicos)
  (assert (start minmax))
)

; no se puede meter en una lista los hechos. Los vamos a iterar con el do-for-all-facts
(defrule minmax
  ?d <- (depth ?depth)
  (color ?color)
  (tam ?tam)
  (not (stop minmax))
=>
  (while (neq ?depth 1) do
    ; los recorremos uno por uno y miramos su padre
    (do-for-all-facts ((?hijo tablero)) (eq ?hijo:depth)
      (do-for-fact ((?padre tablero)) (eq ?padre:ID ?hijo:padre)
        (if (hay-que-modificar ?depth ?padre:heuristico ?hijo:heuristico) then (
          (modify ?padre (heuristico ?hijo:heuristico))
        )) 
      )
      (retract ?hijo)
    )

    ; actualizar el depth
    (bind ?depth (- ?depth 1))
  )

  ; sacamos el max de la lista de hechos, ese va a ser nuestro movimiento
  ; sacamos el movimiento del hecho que tenga mayor minmax
  (do-for-fact ((?hijo tablero))
    (bind $?mejormovimiento ?hijo:movs)
    (bind ?mejorheuristico ?hijo:heur)
    (retract ?hijo)
  )

  (do-for-all-facts ((?hijo tablero))
    (if (< ?mejorheuristico ?hijo:heuristico) (
      (bind $?mejormovimiento ?hijo:movs)
      (bind ?mejorheuristico ?hijo:heur)
    ))
  )

  ; una vez ya tenemos el mejor movimiento lo insertamos como hecho
  (assert (mejor-movimiento $?mejormovimiento))
)