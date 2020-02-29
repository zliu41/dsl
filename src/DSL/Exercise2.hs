{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DSL.Exercise2 where

import Prelude hiding ((>>), (>>=))
import qualified Control.Lens as Lens
import Control.Monad.Indexed (IxApplicative, IxFunctor, IxMonad, IxPointed, (>>>=))
import Control.Monad.Indexed.State (IxMonadState, IxStateT(..), imodify)
import Control.Monad.Indexed.Trans ( ilift )
import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import GHC.Exts (IsList(..))
import GHC.TypeLits (KnownSymbol, Nat, Symbol, symbolVal, type(+), type(<=))

type TLabel  = [Symbol]

data X_Abs a b cs (xss :: [TLabel]) = X_Abs
  { _aField  :: a
  , _bField  :: b
  , _csField :: cs
  }
Lens.makeLenses ''X_Abs

type A = Int
type B = String
type C = Double

type Component = String
type Label = [Component]
type C_Labeled = (Label, C)

type X_Empty = X_Abs () () [C_Labeled] '[]
type X' xss  = X_Abs A B (NonEmpty C_Labeled) xss
data X       = forall xss. X (X' xss)

x_empty :: X_Empty
x_empty = X_Abs () () []

(>>=) :: IxMonadState m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

(>>) :: IxMonadState m => m i j a -> m j k b -> m i k b
(>>) m = (>>>=) m . const

newtype X_M (xs :: TLabel) i j a = X_M
  { runX_M :: IxStateT (Reader Label) i j a }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadState)

type family ExactlyOnceConstraints field a a' b b' cs cs' :: Constraint where
  ExactlyOnceConstraints A a a' b b' cs cs' = (a ~ (), b ~ b', cs ~ cs')
  ExactlyOnceConstraints B a a' b b' cs cs' = (a ~ a', b ~ (), cs ~ cs')

set :: ExactlyOnceConstraints y a a' b b' cs cs'
    => Lens.ASetter (X_Abs a b cs xss) (X_Abs a' b' cs' xss) x y
    -> y -> X_M xs (X_Abs a b cs xss) (X_Abs a' b' cs' xss) ()
set field = imodify . Lens.set field

type MkX xss a = X_M '[] X_Empty (X' xss) a

mkX :: MkX xss a -> X
mkX = X . snd . flip runReader [] . flip runIxStateT x_empty . runX_M

new_c :: (IsList cs, Item cs ~ C_Labeled, AtMostOnce xs xss)
      => C
      -> X_M xs (X_Abs a b cs xss) (X_Abs a b (NonEmpty C_Labeled) (ConsNE xs xss)) ()
new_c c = X_M $ do
  labels <- ilift ask
  let labeled = (labels, c)
  imodify $ Lens.over csField ((labeled :|) . toList)

label :: forall xs y ys a b cs cs' xss xss'.
         (xs ~ Reverse (y ': Reverse ys), KnownSymbol y)
      => X_M xs
             (X_Abs a b cs xss)
             (X_Abs a b cs' xss')
             ()
      -> X_M ys
            (X_Abs a b cs xss)
            (X_Abs a b cs' xss')
            ()
label = ilocal (symbolVal (Proxy @y) :)

ilocal :: (Label -> Label) -> X_M xs i j a -> X_M ys i j a
ilocal f m = X_M . IxStateT $ local f . runIxStateT (runX_M m)

type Reverse xs = ReverseHelper xs '[]

type family ReverseHelper xs acc where
  ReverseHelper '[] acc = acc
  ReverseHelper (x ': xs) acc = ReverseHelper xs (x ': acc)

type family ConsNE (xs :: TLabel) (xss :: [TLabel]) :: [TLabel] where
  ConsNE '[] xss = xss
  ConsNE xs xss = xs ': xss

type family Tail xs where
  Tail '[] = '[]
  Tail (x ': xs) = xs

type family Count x xs :: Nat where
  Count x '[] = 0
  Count x (x ': xs) = 1 + Count x xs
  Count x (y ': xs) = Count x xs

type family AtMostOnce x xs :: Constraint where
  AtMostOnce x xs = Count x xs <= 1

x :: X
x = mkX $ do
  set aField 42
  set bField "Hello"
  new_c 1.0
  label @'["label-foo"] $ do
    new_c 2.0
    label @'["label-foo", "label-bar"] $ do
      new_c 3.0
      label @'["label-foo", "label-bar", "label-baz"] $ do
        new_c 4.0
      new_c 5.0
    new_c 6.0
    -- new_c 7.0
  new_c 8.0
