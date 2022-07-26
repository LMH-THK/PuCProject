import kotlinx.collections.immutable.PersistentMap
import kotlinx.collections.immutable.persistentHashMapOf

sealed class Expr {
    data class Var(val name: String) : Expr()
    data class Lambda(val binder: String, val tyBinder: MonoType?, val body: Expr) : Expr()
    data class App(val func: Expr, val arg: Expr) : Expr()
    data class If(val condition: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr()
    data class Binary(val left: Expr, val op: Operator, val right: Expr) : Expr()
    data class Let(val recursive: Boolean, val binder: String, val expr: Expr, val body: Expr) : Expr()
    data class Def(val recursive: Boolean, val binder: String, val expr: Expr) : Expr()

    data class IntLiteral(val num: Int) : Expr()
    data class BoolLiteral(val bool: Boolean) : Expr()
    data class StringLiteral(val string: String) : Expr()

    data class AssertTrue(val expr: Expr) : Expr()
    data class AssertFalse(val expr: Expr) : Expr()
    data class AssertEqual(val left: Expr, val right: Expr) : Expr()
    data class AssertNotEqual(val left: Expr, val right: Expr) : Expr()
    data class AssertType(val value: Expr, val type: MonoType) : Expr()
    data class AssertNotType(val value: Expr, val type: MonoType) : Expr()
    data class AssertThrows(val expr: Expr) : Expr()
    data class AssertGreaterThan(val left: Expr, val right: Expr) : Expr()
    data class AssertGreaterEqualThan(val left: Expr, val right: Expr) : Expr()
    data class AssertSmallerThan(val left: Expr, val right: Expr) : Expr()
    data class AssertSmallerEqualThan(val left: Expr, val right: Expr) : Expr()
}

enum class Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equality,
    Concat
}

val passedTests = mutableListOf<Expr>()
val failedTests = mutableListOf<Pair<Expr, String>>()

fun testPassed(expr: Expr): Value.Bool {
    passedTests.add(expr)
    return Value.Bool(true)
}

fun testFailed(expr: Expr, message: String): Value.Bool {
    failedTests.add(Pair(expr, message))
    return Value.Bool(false)
}

typealias Env = PersistentMap<String, Value>

sealed class Value {
    data class Int(val num: kotlin.Int) : Value()
    data class Bool(val bool: Boolean) : Value()
    data class String(val string: kotlin.String) : Value()
    data class Closure(var env: Env, val binder: kotlin.String, val body: Expr) : Value()
}

fun eval(env: Env, expr: Expr): Value {
    return when (expr) {
        is Expr.IntLiteral -> Value.Int(expr.num)
        is Expr.BoolLiteral -> Value.Bool(expr.bool)
        is Expr.StringLiteral -> Value.String(expr.string)
        is Expr.Binary -> {
            val left = eval(env, expr.left)
            val right = eval(env, expr.right)
            return when (expr.op) {
                Operator.Equality -> if (left is Value.Int && right is Value.Int) {
                    Value.Bool(left.num == right.num)
                } else if (left is Value.Bool && right is Value.Bool) {
                    Value.Bool(left.bool == right.bool)
                } else if (left is Value.String && right is Value.String) {
                    Value.Bool(left.string == right.string)
                } else {
                    throw Error("Comparing incompatible values: $left and $right")
                }
                Operator.Concat -> if (left is Value.String && right is Value.String) {
                    Value.String(left.string + right.string)
                } else {
                    throw Error("Can't concatenate non-string values: $left and $right")
                }
                else -> numericBinary(left, right, nameForOp(expr.op)) { x, y -> applyOp(expr.op, x, y) }
            }

        }
        is Expr.If -> {
            val condition = eval(env, expr.condition)
            if (condition !is Value.Bool) {
                throw Exception("Expected a boolean condition, but got $condition")
            }
            return if (condition.bool) {
                eval(env, expr.thenBranch)
            } else {
                eval(env, expr.elseBranch)
            }
        }
        is Expr.Let -> {
            val evaledExpr = eval(env, expr.expr)
            if (expr.recursive && evaledExpr is Value.Closure) {
                evaledExpr.env = evaledExpr.env.put(expr.binder, evaledExpr)
            }
            val extendedEnv = env.put(expr.binder, evaledExpr)
            eval(extendedEnv, expr.body)
        }
        is Expr.Lambda -> Value.Closure(env, expr.binder, expr.body)
        is Expr.Var ->
            when (expr.name) {
                "#firstChar" -> {
                    val s = env["x"]!! as Value.String
                    Value.String(s.string.take(1))
                }
                "#remainingChars" -> {
                    val s = env["x"]!! as Value.String
                    Value.String(s.string.drop(1))
                }
                "#charCode" -> {
                    val s = env["x"]!! as Value.String
                    Value.Int(s.string[0].code)
                }
                "#codeChar" -> {
                    val x = env["x"]!! as Value.Int
                    Value.String(x.num.toChar().toString())
                }
                else -> env.get(expr.name) ?: globalEnv.get(expr.name) ?: throw Exception("Unbound variable ${expr.name}")

            }
        is Expr.App -> {
            val func = eval(env, expr.func)
            if (func !is Value.Closure) {
                throw Exception("$func is not a function")
            } else {
                val arg = eval(env, expr.arg)
                //globalEnv = globalEnv.put(func.binder, arg)
                val newEnv = func.env.put(func.binder, arg)
                eval(newEnv, func.body)
            }
        }
        is Expr.AssertTrue -> {
            try {
                val evaluated = eval(env, expr.expr)

                if (evaluated !is Value.Bool) {
                    return testFailed(expr, "Expected a boolean expression, but got $evaluated")
                }
                return if (evaluated.bool) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "Expected true but got $evaluated")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertFalse -> {
            try {
                val evaluated = eval(env, expr.expr)

                if (evaluated !is Value.Bool) {
                    return testFailed(expr, "Expected a boolean expression, but got $evaluated")
                }
                return if (!evaluated.bool) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "Expected false but got $evaluated")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertEqual -> {
            try {
                val lEval = eval(env, expr.left)
                val rEval = eval(env, expr.right)

                return if (lEval != rEval) {
                    testFailed(expr, "$lEval is not equal to $rEval")
                } else {
                    testPassed(expr)
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertNotEqual -> {
            try {
                val lEval = eval(env, expr.left)
                val rEval = eval(env, expr.right)

                return if (lEval == rEval) {
                    testFailed(expr, "$lEval is equal to $rEval")
                } else {
                    testPassed(expr)
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertType -> {
            try {
                val value = eval(env, expr.value)
                var passed = false
                when (expr.type) {
                    is MonoType.BoolTy -> {
                        if (value is Value.Bool) {
                            passed = true;
                        }
                    }
                    is MonoType.IntTy -> {
                        if (value is Value.Int) {
                            passed = true;
                        }
                    }
                    is MonoType.FunType -> {
                        if (value is Value.Closure) {
                            val ty = infer(initialContext, expr.value)
                            if (ty == expr.type) {
                                passed = true
                            } else {
                                var prettyTy = prettyPoly(generalize(initialContext, applySolution(ty)))
                                if (prettyTy == expr.type.toString()) {
                                    passed = true
                                } else if (prettyTy.contains("forall")) {
                                    val letterToReplace = prettyTy[7]
                                    prettyTy = prettyTy.substring(10, prettyTy.length)
                                    val splittedExpectedType = expr.type.toString().split(" ", ) as MutableList
                                    val splittedPrettyTy = prettyTy.split(" ")
                                    for (i in 0..splittedPrettyTy.size) {
                                        if (splittedExpectedType[i][0] == '(') {
                                            var typeBuilder = splittedExpectedType[i]
                                            for (j in i+1..splittedPrettyTy.size) {
                                                typeBuilder += " ${splittedExpectedType[j]}"
                                                if (splittedExpectedType[j].last() == ')') {
                                                    break
                                                }
                                            }
                                            splittedExpectedType[i] = typeBuilder
                                        }
                                        if (splittedExpectedType[i] != splittedPrettyTy[i] && splittedPrettyTy[i] == "$letterToReplace") {
                                            prettyTy = prettyTy.replace("$letterToReplace", splittedExpectedType[i])
                                            break
                                        }
                                    }
                                    if (prettyTy == expr.type.toString()) {
                                        passed = true
                                    } else if (prettyTy.replace("(", "").replace(")", "") == expr.type.toString().replace("(", "").replace(")", "")) {
                                        passed = true
                                    }
                                }
                            }
                        }
                    }
                    is MonoType.StringTy -> {
                        if (value is Value.String) {
                            passed = true;
                        }
                    }
                    else -> testFailed(expr, "$value is not from Type ${expr.type}")
                }
                if (passed) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "$value is not from Type ${expr.type} but is ${prettyPoly(generalize(initialContext, applySolution(infer(initialContext, expr.value))))}")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertNotType -> {
            val result = eval(env, Expr.AssertType(expr.value, expr.type))
            when (result) {
                is Value.Bool -> {
                    if (result.bool) {
                        passedTests.removeLast()
                        testFailed(expr, "${expr.value} is from type ${expr.type}")
                    } else {
                        failedTests.removeLast()
                        testPassed(expr)
                    }
                }
                else -> throw Exception("Something went wrong!")
            }
        }
        is Expr.AssertThrows -> {
            try {
                val result = eval(env, expr.expr)
                testFailed(expr, "Did not throw a exception")
            } catch (exception: Exception) {
                testPassed(expr)
            } catch (error: Error) {
                testPassed(expr)
            }
        }
        is Expr.AssertGreaterThan -> {
            try {
                val lEval = eval(env, expr.left)
                val rEval = eval(env, expr.right)
                if (lEval !is Value.Int)
                    return testFailed(expr, "$lEval is not a number")
                if (rEval !is Value.Int)
                    return testFailed(expr, "$rEval is not a number")

                return if (lEval.num > rEval.num) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "$lEval is not greater than $rEval")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertGreaterEqualThan -> {
            try {
                val lEval = eval(env, expr.left)
                val rEval = eval(env, expr.right)
                if (lEval !is Value.Int)
                    return testFailed(expr, "$lEval is not a number")
                if (rEval !is Value.Int)
                    return testFailed(expr, "$rEval is not a number")

                return if (lEval.num >= rEval.num) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "$lEval is not greater than $rEval")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertSmallerThan -> {
            try {
                val lEval = eval(env, expr.left)
                val rEval = eval(env, expr.right)
                if (lEval !is Value.Int)
                    return testFailed(expr, "$lEval is not a number")
                if (rEval !is Value.Int)
                    return testFailed(expr, "$rEval is not a number")

                return if (lEval.num < rEval.num) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "$lEval is not greater than $rEval")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.AssertSmallerEqualThan -> {
            try {
                val lEval = eval(env, expr.left)
                val rEval = eval(env, expr.right)
                if (lEval !is Value.Int)
                    return testFailed(expr, "$lEval is not a number")
                if (rEval !is Value.Int)
                    return testFailed(expr, "$rEval is not a number")

                return if (lEval.num <= rEval.num) {
                    testPassed(expr)
                } else {
                    testFailed(expr, "$lEval is not greater than $rEval")
                }
            } catch (exception: Exception) {
                testFailed(expr, exception.message!!)
            }
        }
        is Expr.Def -> {
            val evaledExpr = eval(globalEnv, expr.expr)
            if (expr.recursive && evaledExpr is Value.Closure) {
                globalEnv = globalEnv.put(expr.binder, evaledExpr)
            }
            globalEnv = globalEnv.put(expr.binder, evaledExpr)
            evaledExpr
        }
    }
}

fun applyOp(op: Operator, x: Int, y: Int): Value {
    return when (op) {
        Operator.Add -> Value.Int(x + y)
        Operator.Subtract -> Value.Int(x - y)
        Operator.Multiply -> Value.Int(x * y)
        Operator.Divide -> Value.Int(x / y)
        Operator.Equality -> Value.Bool(x == y)
        else -> throw Error("Can't concat ints")
    }
}

fun nameForOp(op: Operator): String {
    return when (op) {
        Operator.Add -> "add"
        Operator.Subtract -> "subtract"
        Operator.Multiply -> "multiply"
        Operator.Divide -> "divide"
        Operator.Equality -> "compare"
        Operator.Concat -> "concat"
    }
}

fun numericBinary(left: Value, right: Value, operation: String, combine: (Int, Int) -> Value): Value {
    if (left is Value.Int && right is Value.Int) {
        return combine(left.num, right.num)
    } else {
        throw (Exception("Can't $operation non-numbers, $left, $right"))
    }
}

val emptyEnv: Env = persistentHashMapOf()
val initialEnv: Env = persistentHashMapOf(
    "firstChar" to Value.Closure(
        emptyEnv, "x",
        Expr.Var("#firstChar")
    ),
    "remainingChars" to Value.Closure(
        emptyEnv, "x",
        Expr.Var("#remainingChars")
    ),
    "charCode" to Value.Closure(
        emptyEnv, "x",
        Expr.Var("#charCode")
    ),
    "codeChar" to Value.Closure(
        emptyEnv, "x",
        Expr.Var("#codeChar")
    )
)

var globalEnv: Env = persistentHashMapOf()

fun prettyPrintTests(expr: Expr): String {
    return when (expr) {
        is Expr.AssertTrue -> "assertTrue ${prettyPrintTests(expr.expr)};"
        is Expr.AssertFalse -> "assertFalse ${prettyPrintTests(expr.expr)};"
        is Expr.AssertEqual -> "assertEqual ${prettyPrintTests(expr.left)} ${prettyPrintTests(expr.right)};"
        is Expr.AssertNotEqual -> "assertNotEqual ${prettyPrintTests(expr.left)} ${prettyPrintTests(expr.right)};"
        is Expr.AssertType -> "assertType ${prettyPrintTests(expr.value)} ${expr.type};"
        is Expr.AssertNotType -> "assertNotType ${prettyPrintTests(expr.value)} ${expr.type};"
        is Expr.AssertThrows -> "assertThrows ${prettyPrintTests(expr.expr)};"
        is Expr.AssertGreaterThan -> "assertGreaterThan ${prettyPrintTests(expr.left)} ${prettyPrintTests(expr.right)};"
        is Expr.AssertGreaterEqualThan -> "assertGreaterEqualThan ${prettyPrintTests(expr.left)} ${prettyPrintTests(expr.right)};"
        is Expr.AssertSmallerThan -> "assertSmallerThan ${prettyPrintTests(expr.left)} ${prettyPrintTests(expr.right)};"
        is Expr.AssertSmallerEqualThan -> "assertSmallerEqualThan ${prettyPrintTests(expr.left)} ${prettyPrintTests(expr.right)};"

        is Expr.App -> "(${prettyPrintTests(expr.func)} ${prettyPrintTests(expr.arg)})"
        is Expr.Binary -> {
            var string = "(" + prettyPrintTests(expr.left)
            string += when (expr.op) {
                Operator.Add -> "+"
                Operator.Subtract -> "-"
                Operator.Multiply -> "*"
                Operator.Divide -> "/"
                Operator.Equality -> "=="
                Operator.Concat -> "#"
            }
            string += prettyPrintTests(expr.right) + ")"
            string
        }
        is Expr.BoolLiteral -> "${expr.bool}"
        is Expr.If -> "(if ${prettyPrintTests(expr.condition)} then ${prettyPrintTests(expr.thenBranch)} else ${prettyPrintTests(expr.elseBranch)})"
        is Expr.IntLiteral -> "${expr.num}"
        is Expr.Lambda -> {
            if (expr.tyBinder != null)
                "(λ${expr.binder}: ${expr.tyBinder} => ${prettyPrintTests(expr.body)})"
            else
                "(λ${expr.binder} => ${prettyPrintTests(expr.body)})"
        }
        is Expr.StringLiteral -> "\"${expr.string}\""
        is Expr.Var -> expr.name
        is Expr.Let -> ""
        is Expr.Def -> ""
    }
}

fun testInput(input: String) {
    //val expr = Parser(Lexer(input)).parseExpression()
    val expressions = Parser(Lexer(input)).parseAll()

    for (expr in expressions) {
        val ty = infer(initialContext, expr)

        //println("${eval(initialEnv, expr)} : ${prettyPoly(generalize(initialContext, applySolution(ty)))}")
        eval(initialEnv, expr)
        generalize(initialContext, applySolution(ty))
    }

    if (passedTests.isNotEmpty()) {
        println("\n\u001B[32mPassed Tests:\u001B[0m")
        for (test in passedTests) {
            println("\u001B[32m${prettyPrintTests(test)}\u001B[0m\n")
        }
    }
    if (failedTests.isNotEmpty()) {
        println("\n\u001B[31mFailed Tests:\u001B[0m")
        for (test in failedTests) {
            println("\u001B[31mExpression: ${prettyPrintTests(test.first)}\nReason: ${test.second}\u001B[0m\n")
        }
    }
}

fun main() {
    /*testInput("""
      let hello = "Hello" in
      let world = "World" in
      let join = \s1 => \s2 => s1 # " " # s2 in
      let shout = \s => s # "!" in
      let twice = \f => \x => f (f x) in
      twice (twice shout) (join hello world)
    """.trimIndent())*/

    testInput(
        """
            def test = (\x => \y => \z => if x then y else z);
            assertType (test) (Bool -> (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool));
            
            
        """.trimIndent()
    )
}


// assertType (\x => \y => \z => if x then y else z 10) (Bool -> Bool -> (Int -> Bool) -> Bool)
// def rec fib = \x: Int => if (x == 0) then 1 else if (x == 1) then 1 else fib (x-1) + fib (x-2);