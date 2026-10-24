# oak
A simple functional strong-typed, statically-typed language implemented in OCaml.

#### Starting point
###### Target language:
very simple int/float calculator that only has `+` operators and is strongly typed
###### Parsing dataflow:
string -> Untyped Tree -> Typed Tree -> Interpreted

###### Issues
- currently untyped tree deals ints and floats differently. If I'm seeking a *untyped*-tree, I should search how other languages deal with such issues. First, thing coming to mind is using a dynamic-way to represent literals and check them during the type checker.
- Since, we need ad-hoc polymorphism for int and float arithmetics and since OCaml divides the two(with `+` and `+.`), I have different type constructor for each too (`TermInt` and `TermFloat`). Need to think about how I can have an uniform interface later on with more types, more operators 