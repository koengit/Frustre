module Main where

import qualified Lustre as L
import Lustre hiding ( pre, pre0, start )

--------------------------------------------------------------------------------

type Flow a   = Signal a
type Stream a = (Signal Bool, Signal a)
-- two possibilities:
-- 1. only when Signal Bool is true, does Signal a make sense
-- 2. Signal a always carries the value from the last time Signal Bool was true
--    (this still needs special care in the beginning)
--    (maybe easier to turn this into flows)
-- (for now, choose 1.)

--------------------------------------------------------------------------------

(~~>) :: Val a => a -> Stream a -> Flow a
x ~~> (act, s) = y
 where
  y = ifThenElse act s (val x --> L.pre y)

pre :: Stream a -> Stream a
pre (act, s) = (started' /\ act, s')
 where
  started' = false --> L.pre (act \/ started')
  s'       =           L.pre (ifThenElse act s s')

pre0 :: Val a => a -> Stream a -> Stream a
pre0 x (act, s) = (act, s')
 where
  s' = val x --> L.pre (ifThenElse act s s')

start :: Stream Bool
start = (L.start, val True)

--------------------------------------------------------------------------------

(!+) :: Stream Int -> Flow Int -> Stream Int
(act, s) !+ a = (act, s + a)

ifStream :: Stream Bool -> Flow a -> Flow a -> Stream a
ifStream (act, c) a b = (act, ifThenElse c a b)

merge :: (Signal a->Signal a->Signal a) -> Stream a -> Stream a -> Stream a
merge f (act1,s1) (act2,s2) = (act1 \/ act2, s)
 where
  s = ifThenElse act1 (ifThenElse act2 (f s1 s2) s1) s2

mergeL :: Stream a -> Stream a -> Stream a
mergeL (act1,s1) (act2,s2) = (act1 \/ act2, s)
 where
  s = ifThenElse act1 s1 s2

{-
later1 :: Flow Time -> Stream a -> Stream a


when :: (Signal a->Signal Bool) -> Flow a -> Stream a

time :: Flow Time

rawTemp :: Stream Double

temp = 20 ~~> rawTemp

when (even) time
-}

--------------------------------------------------------------------------------

later1 :: Flow Int -> Stream a -> Stream a
later1 t (set,inp) = (get,mem)
 where
  get   = timer set t
  ready = get \/ (true --> L.pre (nt set /\ ready))
  mem   = L.pre (ifThenElse (set /\ ready) inp mem)

ticking :: Flow Int -> Stream Bool
ticking t = (act, val True)
 where
  act = true --> timer act t

--------------------------------------------------------------------------------

stream :: Stream a -> [Expr]
stream (act,a) = [unSignal act, unSignal a]

count :: Stream a -> Stream Int
count (act,inp) = s
 where
  s = (act,0) !+ (1 ~~> pre (s!+1))

summy :: Stream Int -> Stream Int
summy inp = s
 where
  s = inp !+ (0 ~~> pre s)

counter :: Stream Int -> Stream Int -> Stream Int
counter down up = result
 where
  change = merge (+) (down !+ val (-2)) (merge (+) up
           (ifStream (ticking (1000)) (val 1) (val 1)))
  result = change !+ (0 ~~> pre result)

main =
  do writeFile "LUS_step.c" $ unlines $
       compile
            ["a_on", "a", "b_on", "b"]
            (let a = (var "a_on", var "a")
                 b = (var "b_on", var "b")
                 r = counter a b
              in stream a ++ stream b ++ stream r
            )
            (\[a_on, a, b_on, b, r_on, r] ->
              [ "printf(\"%d: a(%d:%d) b(%d:%d) r(%d:%d)\\n\", now, "
                ++ a_on ++ ", " ++ a ++ ", "
                ++ b_on ++ ", " ++ b ++ ", "
                ++ r_on ++ ", " ++ r
                ++ ");"
              ]
            )

