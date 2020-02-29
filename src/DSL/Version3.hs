{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.Version3 where

import Prelude hiding ((>>))
import qualified Control.Lens as Lens
import Control.Monad.Indexed ((>>>=))
import Control.Monad.Indexed.State (IxState(..), imodify)
import Data.Kind (Constraint)
import Data.List.NonEmpty as NonEmpty (NonEmpty, fromList)

data X_Abs a b cs = X_Abs
  { _aField  :: a
  , _bField  :: b
  , _csField :: cs
  }
Lens.makeLenses ''X_Abs

type A = Int
type B = String
type C = Double

type X_Empty = X_Abs () () ()
type X       = X_Abs A B (NonEmpty C)

x_empty :: X_Empty
x_empty = X_Abs () () ()

(>>) :: IxState i j a -> IxState j k b -> IxState i k b
(>>) m = (>>>=) m . const

type family ExactlyOnceConstraints field a a' b b' cs cs' :: Constraint where
  ExactlyOnceConstraints A a a' b b' cs cs' = (a ~ (), b ~ b', cs ~ cs')
  ExactlyOnceConstraints B a a' b b' cs cs' = (a ~ a', b ~ (), cs ~ cs')
  ExactlyOnceConstraints (NonEmpty C) a a' b b' cs cs' = (a ~ a', b ~ b', cs ~ ())

set :: ExactlyOnceConstraints y a a' b b' cs cs'
    => Lens.ASetter (X_Abs a b cs) (X_Abs a' b' cs') x y
    -> y -> IxState(X_Abs a b cs) (X_Abs a' b' cs') ()
set field = imodify . Lens.set field

type MkX a = IxState X_Empty X a

mkX :: MkX a -> X
mkX = snd . flip runIxState x_empty

x :: X
x = mkX $ do
  set aField 42
  set bField "Hello"
  set csField (NonEmpty.fromList [1.0, 2.0])
