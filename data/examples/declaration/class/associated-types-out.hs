{-# LANGUAGE TypeFamilies #-}
class Foo a where
  type FooBar a

class Bar a where
  type BarBar a
  type BarBaz a

class Baz a where
  type BazBar a b c
  type BazBaz b a c
