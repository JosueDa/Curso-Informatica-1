module Main exposing (..)

suma: Int -> Int
suma x = x + x

duplicar: Int -> Int
duplicar z = z * z

zip : List a -> List b -> List (a,b)
zip bss css =
    case (bss,css) of 
        ([],_) -> []
        (_,[]) -> []
        (a::bs, b::cs) -> (a,b)::zip bs cs

type Natural = Suc Natural | Cero
enteroANatural : Int -> Natural
enteroANatural i = if i == 0 then Cero else Suc (enteroANatural (i-1))

sumar n1 n2 = case (n1,n2) of 
    (Cero, n2_) -> n2_
    (n1_, Cero) -> n1_
    (Suc n1_, n2_) -> Suc (sumar n1_ n2_ )

type Figura = Circulo Float
    | Cuadrado Float
    | Triangulo Float

area : Figura -> Float
area figura = case figura of 
    Circulo r -> Basics.pi * r * r
    Cuadrado l -> l*l
    Triangulo l1 l2 -> l1 * (sqrt ((l1/2)*(l1/2) + l2*l2))

type MiList a = Cons a (MiList  a) | Nil
    crearLista : [a] -> MiList a
    crearLista l = case l of
        [] -> Nil
        x::xs -> Cons x (crearLista xs)