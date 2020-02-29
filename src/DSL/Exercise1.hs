{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DSL.Exercise1 where

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
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

type TLabel  = [Symbol]

data X_Abs a b cs = X_Abs
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

type X_Empty = X_Abs () () [C_Labeled]
type X       = X_Abs A B (NonEmpty C_Labeled)

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
    => Lens.ASetter (X_Abs a b cs) (X_Abs a' b' cs') x y
    -> y -> X_M xs (X_Abs a b cs) (X_Abs a' b' cs') ()
set field = imodify . Lens.set field

type MkX a = X_M '[] X_Empty X a

mkX :: MkX a -> X
mkX = snd . flip runReader [] . flip runIxStateT x_empty .runX_M

new_c :: (IsList cs, Item cs ~ C_Labeled)
      => C
      -> X_M n (X_Abs a b cs) (X_Abs a b (NonEmpty C_Labeled)) ()
new_c c = X_M $ do
  labels <- ilift ask
  let labeled = (labels, c)
  imodify $ Lens.over csField ((labeled :|) . toList)

label :: forall xs i j y ys. (xs ~ Reverse (y ': Reverse ys), KnownSymbol y)
      => X_M xs i j () -> X_M ys i j ()
label = ilocal (symbolVal (Proxy @y) :)

ilocal :: (Label -> Label) -> X_M xs i j a -> X_M ys i j a
ilocal f m = X_M . IxStateT $ local f . runIxStateT (runX_M m)

type Reverse xs = ReverseHelper xs '[]

type family ReverseHelper xs acc where
  ReverseHelper '[] acc = acc
  ReverseHelper (x ': xs) acc = ReverseHelper xs (x ': acc)

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
    new_c 7.0
  new_c 8.0
