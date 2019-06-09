{-# LANGUAGE TypeFamilies #-}
class Foo a where
  data FooBar a

class Bar a where
  data BarBar a
  data BarBaz a

class Baz a where
  data BazBar a b c
  data BazBaz b a c
