module Main where

type Flow a = [a]
type Stream a = [Maybe a]

flow :: a -> Stream a -> Flow a
flow x (Nothing : ys) = x : flow x ys
flow x (Just y  : ys) = y : flow y ys
flow x []             = []

pre :: Stream a -> Stream a
pre ys = find ys
 where
  find ~(my : ys) = Nothing : case my of
                                Nothing -> find ys
                                Just y  -> found y ys
  
  found y ~(my : ys) = (case my of
                          Nothing -> Nothing
                          Just y' -> Just y) : found (case my of
                                                        Nothing -> y
                                                        Just y' -> y') ys

(!+) :: Stream Int -> Flow Int -> Stream Int
(Just x  : xs) !+ ~(y : ys) = Just (x+y) : (xs !+ ys)
(Nothing : xs) !+ ~(y : ys) = Nothing : (xs !+ ys)
[]             !+ _         = []

summy :: Stream Int -> (Stream Int, Flow Int, Stream Int)
summy inp =
 let a = pre s
     b = flow 0 a
     s = inp !+ b
  in (a,b,s)

main =
  do printStream inp
     printStream a
     printFlow   b
     printStream s
 where
  inp = [ if t `mod` 3 == 0 then Just (t `div` 3) else Nothing | t <- [1..] ]
  (a,b,s) = summy inp

printStream = putStrLn . take 100 . concat . map show4
 where
  show4 Nothing = "  - "
  show4 (Just i) = (++" ") . make 3 . show $ i

  make k s
    | length s <= k = replicate (k-length s) ' ' ++ s
    | length s == k = s
    | otherwise     = "?" ++ reverse (take (k-1) (reverse s))

printFlow = printStream . map Just

