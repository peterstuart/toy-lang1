const greaterThan = a => b => not(lessThan(a)(b))

const doSomething = a => b => greaterThan(a)(b) ? 1.0 : 2.0

const doSomethingElse = a => b => (() => {
    const x = plus(a)(1.0)
    const y = plus(b)(2.0)
    const z = doSomething(x)(y)
    return plus(plus(x)(y))(z)
})()

const constantNum = 3.0

const constantString = 'hello'
