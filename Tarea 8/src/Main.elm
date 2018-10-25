module Main exposing (..)

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith z x1 y1 = case (x1, y1) of 
    ([], y1_) -> []
    (x1_, []) -> []
    (x::xs, y::ys) -> z x y :: zipWith z xs ys

groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy x list = (sii x list, noo x list)

sii a list = case list of 
    [] -> [] 
    (x::xss) -> if a x then sii a xss 
                else x:: sii a xss

noo a list = case list of 
    [] -> []
    (b::bss) -> if a b then b:: noo a bss 
                else sii a bss

bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind xs z = case xs of 
    Nothing -> Nothing 
    Just a -> z a