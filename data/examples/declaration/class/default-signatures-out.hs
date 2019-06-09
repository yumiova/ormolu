{-# LANGUAGE DefaultSignatures #-}
class Foo a where
  foo :: a -> String
  default foo :: Show a => a -> String
  foo = show

class Bar a where
  bar
    :: String
    -> String
    -> a
  default bar
    :: ( Read a
       , Semigroup a
       )
    => a
    -> a
    -> a
  bar
    a
    b = read a <> read b
