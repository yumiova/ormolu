class Foo a

class Foo a => Bar a

class (Foo a, Bar a)
      => Baz a

class ( Foo a
      , Bar a
      , Baz a
      )
      => BarBar a
