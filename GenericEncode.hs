{-# Language DefaultSignatures #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveGeneric #-}
import Debug.Trace
import GHC.Generics

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Generic

-- Empty datatypes: V1
-- Constructors without fields: U1
-- Individual fields of constructors: K1
-- Meta information: M1

-- encode :: Generic a => a -> [Bool]

class Encode' f where
  encode' :: f p -> [Bool]

instance Encode' V1 where
  encode' x = undefined

instance Encode' U1 where
  encode' U1 = []

instance (Encode' f, Encode' g) => Encode' (f :+: g) where
  encode' (L1 x) = False : encode' x
  encode' (R1 x) = True  : encode' x

instance (Encode' f, Encode' g) => Encode' (f :*: g) where
  encode' (x :*: y) = encode' x ++ encode' y

instance (Encode c) => Encode' (K1 i c) where
  encode' (K1 x) = encode x

instance (Encode' f) => Encode' (M1 i t f) where
  encode' (M1 x) = encode' x

class Encode a where
  encode :: a -> [Bool]
  default encode :: (Generic a, Encode' (Rep a)) => a -> [Bool]
  encode x = encode' (from x)

instance (Encode a) => Encode (Tree a)
instance Encode Bool where
  encode True = True:[]
  encode False = False:[]

