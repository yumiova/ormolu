{-# LANGUAGE TypeFamilies #-}

class Foo a where type FooBar a = Int

class Bar a where
  type BarBar a
    = BarBaz a
  type BarBaz
         a = BarBar a

class Baz a where
  type BazBar
         a

  type BazBar a =
         Bar a
