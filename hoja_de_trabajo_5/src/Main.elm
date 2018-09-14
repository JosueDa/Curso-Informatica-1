module Main exposing (..)

esPrimo: Int -> Bool
esPrimo n =  primoN 2 n 
primoN  cont n =
    if n == 1 
    then False
    else if n == 2 
    then True 
    else if modBy   cont  n  == 0 
    then False 
    else if cont == n-1
    then True
    else primoN (cont + 1) n

fibonacci : Int-> Int
fibonacci n = 
    if n == 0 then 0
    else if n == 1 then 1
    else if n > 1 then fibonacci (n-1) + fibonacci (n-2) else 0

primos : Int -> List Int
primos n =
    if 1 > n then []
    else if esPrimo n == False then primos (n-1)
    else n :: primos (n-1) 

nprimos: Int -> List Int
nprimos n  = incluye (n,2) 
incluye (n,x) = 
    if n == 0 then [] 
    else if esPrimo x == False then incluye (n, x + 1)
    else x:: incluye (n - 1, x + 1)
