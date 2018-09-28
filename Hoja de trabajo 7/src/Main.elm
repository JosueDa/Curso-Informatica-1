module Hoja6 exposing (..)

type Natural = Suc Natural | Cero

resta : Natural -> Natural -> Natural
resta x y = case (x,y) of 
    (x_, Cero) -> x
    (Cero, y_) -> Cero
    (Suc x_, Suc y_) -> resta x_ y_

suma : Natural -> Natural -> Natural
suma x y = case (x, y) of 
    (x_, Cero) -> x_
    (Cero, y_) -> y_
    (Suc x_, Suc y_) ->  Suc (Suc (suma x_ y_))

multiplicacion : Natural -> Natural -> Natural
multiplicacion x y = case (x, y) of
    (x_, Cero) -> Cero
    (Cero, y_) -> Cero
    (Suc x_, y_) -> suma y_ (multiplicacion x_ y_)


division : Natural -> Natural -> (Natural, Natural)
division x y = count (x) (y) (Cero)
count x y z = 
    if x == (Suc(Cero)) && y == (Suc (Cero)) then ((Suc(z), Cero))
    else if resta x y == Cero then (z, y)
    else count (resta x y) (y) (Suc (z))

type GExpresion x = Valor x 
    | Suma (GExpresion x) (GExpresion x)
    | Multiplicacion (GExpresion x) (GExpresion x)

type alias Expresion = GExpresion Int

type Estado = Final Int 
    | Parcial (List Char)
