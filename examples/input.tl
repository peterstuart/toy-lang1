greaterThan = \a b -> not (lessThan a b);

doSomething = \a b ->
  if greaterThan a b
    then 1
    else 2;

doSomethingElse = \a b ->
  let
    x = plus a 1,
    y = plus b 2,
    z = doSomething x y
  in
    plus (plus x y) z;

constantNum = 3;

constantString = "hello"