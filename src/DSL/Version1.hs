module DSL.Version1 where

import Data.List.NonEmpty as NonEmpty (NonEmpty, fromList)

data X = X
  { aField  :: Int
  , bField  :: String
  , csField :: NonEmpty C
  }

type A = Int
type B = String
type C = Double

x :: X
x = X
  { aField  = 42
  , bField  = "Hello"
  , csField = NonEmpty.fromList [1.0, 2.0]
  }
