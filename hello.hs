import Data.Bits (FiniteBits(finiteBitSize))
--Comentario é com dois hífens seguidos

hello :: IO ()
hello = putStrLn "Hello World!"

-- Nome da função junto dos parâmetros
dobro :: Num a => a -> a
dobro x = x*2

{-
bhaskara a b c = 
    if delta < 0
        then [] 
        else if delta == 0 then [x'] else [x', x'']
-}


-- Delta
delta :: Floating a => a -> a -> a -> a
delta a b c = b**2 - 4*a*c

-- Bhaskara
bhaskara :: (Ord a, Floating a) => a -> a -> a -> [a]
bhaskara a b c
    | d < 0 = []
    | d == 0 = [x']
    | otherwise = [x', x'']
    where
        d = delta a b c
        x' = (-b + sqrt d) / (2*a)
        x'' = (-b - sqrt d) / (2*a)

-- Fib Recursivo
fib  :: Int ->  Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- Fib Iterativo
fib2 :: Int -> Int
fib2 0 = 0
fib2 1 = 1
fib2 n = fib' 0 1 2
    where
        fib' n2 n1 i =
            if i == n
                then n2 + n1 
                else fib' n1 (n2+n1) (i+1)

-- Lista Infinita (não está pronto)
fibSeq :: [Integer]
fibSeq = 0 : 1 : zipWith (+) fibSeq (tail fibSeq)