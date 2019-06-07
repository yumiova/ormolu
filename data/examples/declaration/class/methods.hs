class Foo a where foo :: a

class Bar a where
    bar :: a -> a -> a

class Baz a where
    baz ::
        (a,
         a)
        -> a
        -> a

class BarBaz a where
    barbaz ::
        a -> b
    bazbar :: b -> a
