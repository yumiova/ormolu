class Foo a where
    foo :: a -> a
    foo a = a

class Bar a where
    bar ::
           a
        -> Int
    bar = const 0

class Baz a where
    foobar :: a -> a
    foobar a =
        barbaz (bazbar a)
    barbaz ::
        a -> a
    bazbar ::
        a ->
        a
    barbaz a
        = bazbar
            a
    bazbar a
        = barbaz
            a
