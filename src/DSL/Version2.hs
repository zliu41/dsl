{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.Version2 where

import qualified Control.Lens as Lens
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

type family ExactlyOnceConstraints field a a' b b' cs cs' :: Constraint where
  ExactlyOnceConstraints A a a' b b' cs cs' = (a ~ (), b ~ b', cs ~ cs')
  ExactlyOnceConstraints B a a' b b' cs cs' = (a ~ a', b ~ (), cs ~ cs')
  ExactlyOnceConstraints (NonEmpty C) a a' b b' cs cs' = (a ~ a', b ~ b', cs ~ ())

set :: ExactlyOnceConstraints y a a' b b' cs cs'
    => Lens.ASetter (X_Abs a b cs) (X_Abs a' b' cs') x y
    -> y -> X_Abs a b cs -> X_Abs a' b' cs'
set = Lens.set

x :: X
x = set aField 42
  . set bField "Hello"
  . set csField (NonEmpty.fromList [1.0, 2.0])
  $ x_empty

{- Alternatively:

set_aField :: a -> X_Abs () b cs -> X_Abs a b cs
set_aField = Lens.set aField

set_bField :: b -> X_Abs a () cs -> X_Abs a b cs
set_bField = Lens.set bField

set_csField :: cs -> X_Abs a b () -> X_Abs a b cs
set_csField = Lens.set csField

x :: X
x = set_aField 42
  . set_bField "Hello"
  . set_csField (NonEmpty.fromList [1.0, 2.0])
  $ x_empty

-}
