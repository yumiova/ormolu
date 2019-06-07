{-# LANGUAGE MultiParamTypeClasses #-}
class Foo a b where
  foo :: a -> b

class Bar a b c d where
  bar
    :: a
    -> b
    -> c
    -> d

class Baz where
  baz :: Int

class BarBaz a b c d e f where
  barbaz
    :: a -> f
  bazbar
    :: e
    -> f
