{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DSL.Exercise5 where

import Prelude hiding ((>>=), (>>))
import qualified Control.Lens as Lens
import Control.Monad.Indexed.State (imodify)
import Control.Monad.Indexed.Trans (ilift)
import Control.Monad.Trans.Reader (ask)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts (IsList(..))
import GHC.TypeLits (Nat, type(+))

import DSL.Version6 hiding (new_c)
import qualified DSL.Version6 as V6

type Cs (n :: Nat) (xss :: [TLabel]) =
  forall xs a b cs yss. (IsList cs, Item cs ~ C_Labeled) =>
  X_M n
      xs
      (X_Abs a b cs yss)
      (X_Abs a b (NonEmpty C_Labeled) (ConcatNE (ReverseAppendAll (Reverse xss) xs) yss))
      ()

type NoLabel = ('[] :: TLabel)

new_c :: (IsList cs, Item cs ~ C_Labeled)
      => C
      -> X_M n xs (X_Abs a b cs xss) (X_Abs a b (NonEmpty C_Labeled) (ConsNE xs xss)) ()
new_c c = X_M $ do
  labels <- ilift ask
  let labeled = (labels, c)
  imodify $ Lens.over csField ((labeled :|) . toList)

cs :: forall n. Cs n [ NoLabel
                     , '["label-bar"]
                     , '["label-bar", "label-baz"]
                     , '["label-bar"]
                     , NoLabel
                     ]
cs = do
  new_c 2.0
  label @n @"label-bar" $ do
    new_c 3.0
    label @(n+1) @"label-baz" $ do
      new_c 4.0
    new_c 5.0
  new_c 6.0

type family ConcatNE (xss :: [TLabel]) (yss :: [TLabel]) :: [TLabel] where
  ConcatNE '[] yss = yss
  ConcatNE (xs ': xss) yss = ConsNE xs (ConcatNE xss yss)

type family ReverseAppendAll (xss :: [TLabel]) (ys :: TLabel) :: [TLabel] where
  ReverseAppendAll '[] _ = '[]
  ReverseAppendAll (xs ': xss) ys = (Reverse xs ++ ys) ': ReverseAppendAll xss ys

type Reverse xs = ReverseHelper xs '[]

type family ReverseHelper xs acc where
  ReverseHelper '[] acc = acc
  ReverseHelper (x ': xs) acc = ReverseHelper xs (x ': acc)

type family (++) xs ys where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

x :: X
x = mkX $ do
  set aField 42
  set bField "Hello"
  V6.new_c 1.0
  label @0 @"label-foo" $ do
    cs
    -- V6.new_c 7.0
  new_c 8.0
