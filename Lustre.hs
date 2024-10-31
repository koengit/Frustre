module Lustre where

import qualified Data.Map as M
import qualified Data.Set as S
import Ref
import Data.List
import Data.Function( on )
import Debug.Trace( trace ) 

--------------------------------------------------------------------------------

type Var   = String
type Short = String
type Name  = String

data E e = E
  { name  :: String
  , args  :: [e]
  , args' :: [e]
  , state :: Var -> ([String],[String])
  , code  :: Var -> [Short] -> [Short] -> Either Short [String]
  , post  :: Var -> [Short] -> [Short] -> [String]
  }

expr :: E e
expr = E "" [] [] (\_ -> ([],[])) (\_ _ _ -> Right []) (\_ _ _ -> [])

app :: E a -> [b] -> [b] -> E b
app e as as' = E (name e) as as' (state e) (code e) (post e)

instance Functor E where
  fmap f e = app e (map f (args e)) (map f (args' e))

instance Show e => Show (E e) where
  show p = name p -- ++ arg "()" (args p) ++ arg "[]" (args' p)
   where
    arg [a,b] [] = ""
    arg [a,b] as = [a] ++ concat (intersperse "," (map show as)) ++ [b]

what p = (name p, args p, args' p)

instance Eq e => Eq (E e) where
  p == q = what p == what q

instance Ord e => Ord (E e) where
  p `compare` q = what p `compare` what q

op2 :: Name -> e -> e -> E e
op2 op a b =
  expr{ name = op, args = [a,b], code =
    \x [a,b] _ ->
       Right [ "int " ++ x ++ " = " ++ a ++ " " ++ op ++ " " ++ b ++ ";" ]
  }

addE   = op2 "+"
andE   = op2 "&&"
orE    = op2 "||"
notE a = expr{ name = "not", args = [a], code = \_ [a] _ -> Left ("!" ++ a) }

ifE c a b =
  expr{ name = "if", args = [c,a,b], code = 
    \x ~(c:a:b:_) _ ->
      Right [ "int " ++ x ++ " = " ++ c ++ " ? " ++ a ++ " : " ++ b ++ ";" ]
  }

intE n = expr{ name = show n, code = \_ _ _ -> Left (show n) }
varE v = expr{ name = v, code = \_ _ _ -> Left v }
startE = expr{ name = "start", code = \_ _ _ -> Left "s->start" }

preE b =
  expr{ name  = "pre"
      , args' = [b]
      , state = \x -> (["int pre" ++ x ++ ";"],[{- "s->pre" ++ x ++ " = -666;" -}])
      , code  = \x _ _ -> Left ("s->pre" ++ x)
      , post  = \x _ [b] -> ["s->pre" ++ x ++ " = " ++ b ++ ";"]
      }

timerE a t =
  expr{ name  = "timer"
      , args' = [a,t]
      , state = \x -> (
                  [ "TIME tim" ++ x ++ ";"
                  ],
                  [ "s->tim" ++ x ++ " = NO_TIME;"
                  ]
                )
      , code  = \x _ _ -> Right
                  [ "TIME " ++ x ++ " = s->tim" ++ x ++ " != NO_TIME"
                                 ++ " && s->tim" ++ x ++ " <= now"
                                 ++ " && (s->tim" ++ x ++ " = NO_TIME, 1);" 
                  ]
      , post  = \x _ [a,t] ->
                  [ "if (s->tim" ++ x ++ " == NO_TIME && " ++ a ++ ")"
                  , "  s->tim" ++ x ++ " = now + " ++ t ++ ";"
                  , "if (s->tim" ++ x ++ " != NO_TIME"
                                 ++ " && (wake == NO_TIME || wake <= s->tim" ++ x ++ "))"
                  , "  wake = s->tim" ++ x ++ ";"
                  ]
      }

newtype Expr = Expr (Ref (E Expr))
 deriving ( Eq, Ord, Show )

newtype Signal a = Signal{ unSignal :: Expr }

--------------------------------------------------------------------------------

class Val a where
  val :: a -> Signal a

instance Val Int where
  val n = Signal (Expr (ref (intE n)))

instance Val Bool where
  val b = if b then true else false

--------------------------------------------------------------------------------

instance Num Expr where
  fromInteger n = Expr (ref (intE (fromInteger n)))
  a + b = Expr (ref (addE a b))

instance Num a => Num (Signal a) where
  fromInteger n = Signal (fromInteger n)
  Signal a + Signal b = Signal (a + b)

false, true :: Signal Bool
false = Signal (Expr (ref (intE 0)))
true  = Signal (Expr (ref (intE 1)))

nt :: Signal Bool -> Signal Bool
nt (Signal a) = Signal (Expr (ref (notE a)))

(/\) :: Signal Bool -> Signal Bool -> Signal Bool
Signal a /\ Signal b = Signal (Expr (ref (andE a b)))

(\/) :: Signal Bool -> Signal Bool -> Signal Bool
Signal a \/ Signal b = Signal (Expr (ref (orE a b)))

ifThenElse :: Signal Bool -> Signal a -> Signal a -> Signal a
ifThenElse (Signal c) (Signal a) (Signal b) = Signal (Expr (ref (ifE c a b)))

start :: Signal Bool
start = Signal (Expr (ref startE))

pre :: Signal a -> Signal a
pre (Signal b) = Signal (Expr (ref (preE b)))

(-->) :: Signal a -> Signal a -> Signal a
s0 --> s = ifThenElse start s0 s

pre0 :: Signal a -> Signal a -> Signal a
pre0 s0 s = s0 --> pre s

var :: String -> Signal a
var v = Signal (Expr (ref (varE v)))

timer :: Signal Bool -> Signal Int -> Signal Bool
timer (Signal a) (Signal t) = Signal (Expr (ref (timerE a t)))

--------------------------------------------------------------------------------

defs :: [Expr] -> ([(Var, E Var)], [Var])
defs es = (ds, vs)
 where
  (_, _, ds, vs) = defList 0 M.empty es
 
  def i mp (Expr r) =
    case M.lookup r mp of
      Just v  -> (i, mp, [], v)
      Nothing -> (i2, mp2, ds1 ++ [(x, app e vs1 vs2)] ++ ds2, x)
       where
        e = deref r
        x = "_x" ++ show i
        
        (i1,mp1,ds1,vs1) = i `seq` defList (i+1) mp (args e)
        (i2,mp2,ds2,vs2) = defList i1 (M.insert r x mp1) (args' e)

  defList i mp []     = (i,mp,[],[])
  defList i mp (e:es) = (i2,mp2,ds1++ds2,v:vs)
   where
    (i1,mp1,ds1,v)  = def i mp e
    (i2,mp2,ds2,vs) = defList i1 mp1 es

fun :: Ord a => M.Map a a -> a -> a
fun mp x = case M.lookup x mp of
             Nothing -> x
             Just y  -> y

optimize :: ([(Var, E Var)], [Var]) -> ([(Var, E Var)], [Var])
optimize (ds, vs) =
  ( [ (x, fmap rep e)
    | (x,e) <- ds
    , x == rep x
    ]
  , map rep vs
  )
 where
  rep = fun mp
  
  qs = [ x | (x, e) <- ds, not (null (args' e)) ]

  mp = iter [sort qs]
  
  iter eqs = go (M.fromList [ (r,q) | q:rs <- eqs, r <- rs ]) M.empty ds
   where
    go mpv mpe ((x, e) : ds) =
      case M.lookup e' mpe of
        Just y  -> go (M.insert x y mpv) mpe ds
        Nothing -> go mpv (M.insert e' x mpe) ds
     where
      e' = fmap (fun mpv) e
   
    go mpv mpe []
      | eqs' == eqs = mpv
      | otherwise   = iter eqs'
     where
      eqs' = sort
           . map (sort . map snd)
           . groupBy ((==) `on` fst)
           . sort
           $ [ (map (fun mpv) (args' e), x) | (x, e) <- ds, not (null (args' e)) ]

compile :: [Var] -> [Expr] -> ([Var] -> [String]) -> [String]
compile inps outs k =
     [ "typedef struct {"
     , "  int start;"
     ]
  ++ [ "  " ++ l
     | (x, e) <- ds
     , l <- fst (state e x)
     ]
  ++ [ "} LUS_state;"
     , ""
     , "void LUS_init( LUS_state *s ) {"
     , "  s->start = 1;"
     ]
  ++ [ "  " ++ l
     | (x, e) <- ds
     , l <- snd (state e x)
     ]
  ++ [ "}"
     , ""
     , "static inline TIME LUS_step( LUS_state *s"
     , "             , TIME now"
     ]
  ++ [ "             , int " ++ inp
     | inp <- inps
     ]
  ++ [ "             ) {"
     , "  TIME wake = NO_TIME;"
     , ""
     , "  /* INTERNAL SIGNALS */"
     ]
  ++ [ "  " ++ l
     | (x, e) <- ds
     , Right c <- [code e x (map val (args e)) (map val (args' e))]
     , l <- c 
     ]
  ++ [ ""
     , "  /* PREPARING FOR NEXT STATE */"
     ]
  ++ [ "  s->start = 0;" ]
  ++ [ "  " ++ l
     | (x, e) <- ds
     , l <- post e x (map val (args e)) (map val (args' e))
     ]
  ++ [ ""
     , "  /* OUTPUTS */"
     ]
  ++ [ "  " ++ s | s <- k (map val vs) ]
  ++ [ ""
     , "  return wake;"
     , "}"
     ]
 where
  (ds,vs) = optimize (defs outs)
  mp = M.fromList [ (x,a)
                  | (x,e)  <- ds
                  , Left a <- [code e x (map val (args e)) (map val (args' e))]
                  ]
  val x = case M.lookup x mp of
            Nothing -> x
            Just a  -> a

