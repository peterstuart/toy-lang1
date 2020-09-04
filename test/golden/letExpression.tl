doSomethingElse =
  \a b ->
    let
      x = plus a 1
      y = plus b 2
      z = doSomething x y
    in
      plus (plus x y) z