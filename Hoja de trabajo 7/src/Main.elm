module Hoja7 exposing (..)

suma : Int -> Int -> Int
suma x y = x + y

multiplicacion : Int -> Int -> Int
multiplicacion x y = x * y

type Expresion = Valor Int
    | Suma Expresion Expresion 
    | Multiplicacion Expresion Expresion  
    

reducir : ((Int -> Int -> Int), (Int-> Int -> Int)) -> Expresion -> Int
reducir (adicion, multi) exp = case exp of
    Valor x -> x
    Suma op1 op2 -> adicion (reducir (adicion, multi) op1)  (reducir (adicion, multi) op2)
    Multiplicacion op1 op2 -> multi (reducir (adicion, multi) op1)  (reducir (adicion, multi) op2)
    