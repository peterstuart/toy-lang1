const doSomethingElse = ((a) => {return ((b) => {return (() => {const x = plus(a)(1.0);const y = plus(b)(2.0);const z = doSomething(x)(y);return plus(plus(x)(y))(z)})()})});