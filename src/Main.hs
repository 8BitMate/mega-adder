{-# LANGUAGE RankNTypes #-}

module Main where

newtype Adder = Adder { unAdder :: forall b. Int -> (Int -> b) -> b }

inc :: Adder
inc = Adder $ \n fn -> fn (n + 1)

addTo :: Int -> Adder
addTo n = Adder $ \n' fn -> fn (n' + n)

infixl 1 >>>

(>>>) :: Adder -> Adder -> Adder
a1 >>> a2 = Adder $ \n fn ->
    let fx x = unAdder a2 x fn
    in  unAdder a1 n fx

runAdder :: Int -> Adder -> Int
runAdder n a = unAdder a n id

megaAdd :: Int -> Int -> Int
megaAdd n1 n2 = runAdder 0 (addTo n1 >>> addTo n2)
{-
Following is a calculation of how the expression (addTo n1 >>> addTo n2)
gets reduced to something that makes more sense

addTo n1 >>> addTo n2 = Adder \n fn ->
    let fx x = unAdder (addTo n2) x fn
    in  unAdder (addTo n1) n fx

unAdder (addTo n2) x fn = (\n' fn' -> fn' (n' + n2)) x fn
= fn (x + n2)
fx x = fn (x + n2)

unAdder (addTo n1) n fx = (\n' fn' -> fn' (n' + n1)) n fx
= fx (n + n1) = fn ((n + n1) + n2)

addTo n1 >>> addTo n2 = Adder $ \n fn -> fn ((n + n1) + n2)
-}

megaInc :: Int -> Int
megaInc n = runAdder n inc

main :: IO ()
main = do print $ megaAdd 2 3
          print $ megaInc 9
