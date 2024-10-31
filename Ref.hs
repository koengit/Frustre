module Ref( Ref(..), ref ) where

import Data.Unique
import System.IO.Unsafe( unsafePerformIO )

data Ref a = Ref{ tag :: Unique, deref :: a }

instance Eq (Ref a) where
  Ref p _ == Ref q _ = p == q

instance Ord (Ref a) where
  Ref p _ `compare` Ref q _ = p `compare` q

instance Show a => Show (Ref a) where
  show (Ref _ a) = "{" ++ show a ++ "}"

{-# NOINLINE ref #-}
ref :: a -> Ref a
ref x = unsafePerformIO (do p <- newUnique; return (Ref p x))

