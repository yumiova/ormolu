{-# LANGUAGE FunctionalDependencies #-}
class Foo a b | a -> b

class Bar a b | a -> b, b -> a where
  bar :: a

class Baz a b c d
      | a b -> c d
      , b c -> a d
      , a c -> b d
      , a c d -> b
      , a b d -> a b c d where
  baz :: a -> b
