const greaterThan = ((a) => {return ((b) => {return not(lessThan(a)(b))})});

const doSomething = ((a) => {return ((b) => {return (greaterThan(a)(b)) ? (1.0) : (2.0)})});

const doSomethingElse = ((a) => {return ((b) => {return (() => {const x = plus(a)(1.0);const y = plus(b)(2.0);const z = doSomething(x)(y);return plus(plus(x)(y))(z)})()})});

const constantNum = 3.0;

const constantString = "hello";