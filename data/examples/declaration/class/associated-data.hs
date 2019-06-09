{-# LANGUAGE TypeFamilies #-}

class Foo a where data FooBar a

class Bar a
  where
    data BarBar
        a
    data family BarBaz
        a

class Baz a where
    data BazBar
        a
        b
        c

    data family BazBaz
        b
        a
        c
