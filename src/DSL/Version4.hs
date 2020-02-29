{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DSL.Version4 where

import Prelude hiding ((>>), (>>=))
import qualified Control.Lens as Lens
import Control.Monad.Indexed ((>>>=))
import Control.Monad.Indexed.State (IxMonadState, IxStateT(..), imodify)
import Control.Monad.Indexed.Trans (ilift)
import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts (IsList(..))

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

type X_M i j a = IxStateT (Reader Label) i j a

type family ExactlyOnceConstraints field a a' b b' cs cs' :: Constraint where
  ExactlyOnceConstraints A a a' b b' cs cs' = (a ~ (), b ~ b', cs ~ cs')
  ExactlyOnceConstraints B a a' b b' cs cs' = (a ~ a', b ~ (), cs ~ cs')

set :: ExactlyOnceConstraints y a a' b b' cs cs'
    => Lens.ASetter (X_Abs a b cs) (X_Abs a' b' cs') x y
    -> y -> X_M (X_Abs a b cs) (X_Abs a' b' cs') ()
set field = imodify . Lens.set field

type MkX a = X_M X_Empty X a

mkX :: MkX a -> X
mkX = snd . flip runReader [] . flip runIxStateT x_empty

new_c :: (IsList cs, Item cs ~ C_Labeled)
      => C
      -> X_M (X_Abs a b cs) (X_Abs a b (NonEmpty C_Labeled)) ()
new_c c = do
  labels <- ilift ask
  let labeled = (labels, c)
  imodify $ Lens.over csField ((labeled :|) . toList)

label :: Component -> X_M i j () -> X_M i j ()
label = ilocal . (:)

ilocal :: (Label -> Label) -> X_M i j a -> X_M i j a
ilocal f m = IxStateT $ local f . runIxStateT m

x :: X
x = mkX $ do
  set aField 42
  set bField "Hello"
  new_c 1.0        -- 1.0 has no label
  label "label-foo" $ do
    new_c 2.0      -- 2.0 is labeled ["label-foo"]
    label "label-bar" $ do
      new_c 3.0    -- 3.0 is labeled ["label-foo", "label-bar"]
      label "label-baz" $ do
        new_c 4.0  -- 4.0 is labeled ["label-foo", "label-bar", "label-baz"]
      new_c 5.0    -- 5.0 is labeled ["label-foo", "label-bar"]
    new_c 6.0      -- 6.0 is labeled ["label-foo"]
    new_c 7.0      -- 7.0 is labeled ["label-foo"]
  new_c 8.0        -- 8.0 has no label
