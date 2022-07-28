import kotlinx.collections.immutable.persistentHashMapOf

sealed class Token {

    @Override
    override fun toString(): String {
        return this.javaClass.simpleName
    }

    // Keyword
    object IF : Token()
    object THEN : Token()
    object ELSE : Token()
    object LET : Token()
    object REC : Token()
    object IN : Token()
    object DEF : Token()

    object INT : Token()
    object BOOL : Token()
    object STRING : Token()

    // Symbols
    object LPAREN : Token()
    object RPAREN : Token()
    object ARROW : Token()
    object EQ_ARROW : Token()
    object BACKSLASH : Token()
    object EQUALS : Token()
    object COLON : Token()
    object SEMICOLON : Token()

    // Asserts
    object ASSERTTRUE : Token()
    object ASSERTFALSE : Token()
    object ASSERTEQUAL : Token()
    object ASSERTNOTEQUAL : Token()
    object ASSERTTYPE : Token()
    object ASSERTNOTTYPE : Token()
    object ASSERTTHROWS : Token()
    object ASSERTGREATERTHAN : Token()
    object ASSERTGREATEREQUALTHAN : Token()
    object ASSERTSMALLERTHAN : Token()
    object ASSERTSMALLEREQUALTHAN : Token()

    // Literal
    data class BOOL_LIT(val bool: Boolean) : Token()
    data class INT_LIT(val int: Int) : Token()
    data class STRING_LIT(val string: String) : Token()

    data class IDENT(val ident: String) : Token()

    // Operator
    object PLUS : Token()
    object MINUS : Token()
    object MULTIPLY : Token()
    object DIVIDES : Token()
    object DOUBLE_EQUALS : Token()
    object HASH : Token()

    // Control
    object EOF : Token()
}

class PeekableIterator<T>(val iter: Iterator<T>) {
    var lh: T? = null
    fun peek(): T? {
        lh = next()
        return lh
    }

    fun next(): T? {
        lh?.let { lh = null; return it }
        return if (iter.hasNext()) {
            iter.next()
        } else {
            null
        }
    }
}

class Lexer(input: String) {

    private val iter = PeekableIterator(input.iterator())
    var lh: Token? = null

    fun next(): Token {
        chompWhitespace()
        lh?.let { it -> lh = null; return it }
        return when (val c = iter.next()) {
            null -> Token.EOF
            '(' -> Token.LPAREN
            ')' -> Token.RPAREN
            '\\' -> Token.BACKSLASH
            ':' -> Token.COLON
            '+' -> Token.PLUS
            '/' -> Token.DIVIDES
            '*' -> Token.MULTIPLY
            '#' -> Token.HASH
            ';' -> Token.SEMICOLON
            '-' -> if (iter.peek() == '>') {
                iter.next()
                Token.ARROW
            } else {
                Token.MINUS
            }
            '=' -> if (iter.peek() == '>') {
                iter.next()
                Token.EQ_ARROW
            } else if (iter.peek() == '=') {
                iter.next()
                Token.DOUBLE_EQUALS
            } else {
                Token.EQUALS
            }
            '"' -> lexString()
            else -> when {
                c.isJavaIdentifierStart() -> lexIdentifier(c)
                c.isDigit() -> lexInt(c)
                else -> throw Exception("Unexpected $c")
            }
        }
    }

    private fun lexInt(first: Char): Token {
        var res = first.toString()
        while (iter.peek()?.isDigit() == true) {
            res += iter.next()
        }
        return Token.INT_LIT(res.toInt())
    }

    private fun lexIdentifier(first: Char): Token {
        var res = first.toString()
        while (iter.peek()?.isJavaIdentifierPart() == true) {
            res += iter.next()
        }
        return when (res) {
            "if" -> Token.IF
            "then" -> Token.THEN
            "else" -> Token.ELSE
            "let" -> Token.LET
            "rec" -> Token.REC
            "in" -> Token.IN
            "true" -> Token.BOOL_LIT(true)
            "false" -> Token.BOOL_LIT(false)
            "Int" -> Token.INT
            "Bool" -> Token.BOOL
            "String" -> Token.STRING
            "assertTrue" -> Token.ASSERTTRUE
            "assertFalse" -> Token.ASSERTFALSE
            "assertEqual" -> Token.ASSERTEQUAL
            "assertNotEqual" -> Token.ASSERTNOTEQUAL
            "assertType" -> Token.ASSERTTYPE
            "assertNotType" -> Token.ASSERTNOTTYPE
            "assertThrows" -> Token.ASSERTTHROWS
            "assertGreaterThan" -> Token.ASSERTGREATERTHAN
            "assertGreaterEqualThan" -> Token.ASSERTGREATEREQUALTHAN
            "assertSmallerThan" -> Token.ASSERTSMALLERTHAN
            "assertSmallerEqualThan" -> Token.ASSERTSMALLEREQUALTHAN
            "def" -> Token.DEF
            else -> Token.IDENT(res)
        }
    }

    private fun lexString(): Token.STRING_LIT {
        var result = ""
        while (iter.peek() != '"') {
            val next = iter.next() ?: throw Error("Unterminated String Literal")
            result += next
        }
        iter.next()
        return Token.STRING_LIT(result)
    }

    private fun chompWhitespace() {
        while (iter.peek()?.isWhitespace() == true) {
            iter.next()
        }
    }

    public fun lookahead(): Token {
        lh = next()
        return lh ?: Token.EOF
    }
}

class Parser(val lexer: Lexer) {

    fun parseAll(): List<Expr> {
        val expressions = mutableListOf<Expr>()
        var expr = parseExpression()
        while (lexer.lookahead() == Token.SEMICOLON) {
            expect<Token.SEMICOLON>(";")
            if (lexer.lookahead() == Token.EOF)
                break
            expressions.add(expr)
            expr = parseExpression()
        }
        expressions.add(expr);
        return expressions
    }

    fun parseType(): MonoType {
        var ty = parseTypeAtom()
        while (lexer.lookahead() == Token.ARROW) {
            expect<Token.ARROW>("an arrow")
            ty = MonoType.FunType(ty, parseType())
        }
        return ty
    }

    private fun parseTypeAtom(): MonoType {
        return when (val t = lexer.next()) {
            is Token.BOOL -> {
                MonoType.BoolTy
            }
            is Token.INT -> {
                MonoType.IntTy
            }
            is Token.STRING -> {
                MonoType.StringTy
            }
            is Token.LPAREN -> {
                val ty = parseType()
                expect<Token.RPAREN>("a closing paren")
                ty
            }
            else -> throw Error("Expected a type but got: $t")
        }
    }

    fun parseExpression(): Expr {
        return parseBinary(0)
    }

    private fun parseBinary(minBindingPower: Int): Expr {
        var lhs = parseApplication()
        while (true) {
            val op = peekOperator() ?: break
            val (leftBp, rightBp) = bindingPowerForOp(op)
            if (minBindingPower > leftBp) break;
            lexer.next()
            val rhs = parseBinary(rightBp)
            lhs = Expr.Binary(lhs, op, rhs)
        }
        return lhs
    }

    private fun peekOperator(): Operator? {
        return when (lexer.lookahead()) {
            Token.DIVIDES -> Operator.Divide
            Token.DOUBLE_EQUALS -> Operator.Equality
            Token.MINUS -> Operator.Subtract
            Token.MULTIPLY -> Operator.Multiply
            Token.PLUS -> Operator.Add
            Token.HASH -> Operator.Concat
            else -> null
        }
    }

    private fun bindingPowerForOp(op: Operator): Pair<Int, Int> {
        return when (op) {
            Operator.Equality -> 2 to 1
            Operator.Add, Operator.Subtract, Operator.Concat -> 3 to 4
            Operator.Multiply, Operator.Divide -> 5 to 6
        }
    }

    private fun parseApplication(): Expr {
        var expr = parseAtom() ?: throw Exception("Expected an expression")
        while (true) {
            val arg = parseAtom() ?: break
            expr = Expr.App(expr, arg)
        }
        return expr
    }

    private fun parseAssertTrue(): Expr {
        expect<Token.ASSERTTRUE>("assertTrue")
        val expr = parseExpression()
        return Expr.AssertTrue(expr)
    }

    private fun parseAssertFalse(): Expr {
        expect<Token.ASSERTFALSE>("assertFalse")
        val expr = parseExpression()
        return Expr.AssertFalse(expr)
    }

    private fun parseAssertEqual(): Expr {
        expect<Token.ASSERTEQUAL>("assertEqual")
        val left = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.LPAREN -> parseExpression()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        return if (left !is Expr.App) {
            // Links ist eine Application ==> wir müssen noch einen Rechten Parameter parsen
            val right = parseExpression()
            Expr.AssertEqual(left, right)
        } else {
            Expr.AssertEqual(left.func, left.arg)
        }
    }

    private fun parseAssertNotEqual(): Expr {
        expect<Token.ASSERTNOTEQUAL>("assertNotEqual")
        val left = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.LPAREN -> parseExpression()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        return if (left !is Expr.App) {
            val right = parseExpression()
            Expr.AssertNotEqual(left, right)
        } else {
            Expr.AssertNotEqual(left.func, left.arg)
        }
    }

    private fun parseAssertType(): Expr {
        expect<Token.ASSERTTYPE>("assertType")
        val value = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT, is Token.LPAREN -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.BACKSLASH -> parseLambda()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        val type = parseType()
        return Expr.AssertType(value, type)
    }

    private fun parseAssertNotType(): Expr {
        expect<Token.ASSERTNOTTYPE>("assertNotType")
        val value = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT, is Token.LPAREN -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.BACKSLASH -> parseLambda()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        val type = parseType()
        return Expr.AssertNotType(value, type)
    }

    private fun parseAssertThrows(): Expr {
        expect<Token.ASSERTTHROWS>("assertThrows")
        val expr = parseExpression()
        return Expr.AssertThrows(expr)
    }

    private fun parseAssertGreaterThan(): Expr {
        expect<Token.ASSERTGREATERTHAN>("assertGreaterThan")
        val left = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.LPAREN -> parseExpression()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        return if (left !is Expr.App) {
            val right = parseExpression()
            Expr.AssertGreaterThan(left, right)
        } else {
            Expr.AssertGreaterThan(left.func, left.arg)
        }
    }
    private fun parseAssertGreaterEqualThan(): Expr {
        expect<Token.ASSERTGREATEREQUALTHAN>("assertGreaterEqualThan")
        val left = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.LPAREN -> parseExpression()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        return if (left !is Expr.App) {
            val right = parseExpression()
            Expr.AssertGreaterEqualThan(left, right)
        } else {
            Expr.AssertGreaterEqualThan(left.func, left.arg)
        }
    }
    private fun parseAssertSmallerThan(): Expr {
        expect<Token.ASSERTSMALLERTHAN>("assertSmallerThan")
        val left = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.LPAREN -> parseExpression()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        return if (left !is Expr.App) {
            val right = parseExpression()
            Expr.AssertSmallerThan(left, right)
        } else {
            Expr.AssertSmallerThan(left.func, left.arg)
        }
    }
    private fun parseAssertSmallerEqualThan(): Expr {
        expect<Token.ASSERTSMALLEREQUALTHAN>("assertSmallerEqualThan")
        val left = when (lexer.lookahead()) {
            is Token.BOOL_LIT, is Token.INT_LIT, is Token.STRING_LIT -> {
                parseAtom() ?: throw Exception("Expected Atom but got ${lexer.lh}")
            }
            is Token.IDENT -> parseApplication()
            is Token.IF -> parseIf()
            is Token.LPAREN -> parseExpression()
            else -> throw Exception("Expected Atom or Expression but got ${lexer.lh}")
        }
        return if (left !is Expr.App) {
            val right = parseExpression()
            Expr.AssertSmallerEqualThan(left, right)
        } else {
            Expr.AssertSmallerEqualThan(left.func, left.arg)
        }
    }

    private fun parseAtom(): Expr? {
        return when (lexer.lookahead()) {
            is Token.INT_LIT -> parseInt()
            is Token.BOOL_LIT -> parseBool()
            is Token.STRING_LIT -> parseString()
            is Token.BACKSLASH -> parseLambda()
            is Token.LET -> parseLet()
            is Token.IF -> parseIf()
            is Token.IDENT -> parseVar()
            is Token.LPAREN -> {
                expect<Token.LPAREN>("opening paren")
                val inner = parseExpression()
                expect<Token.RPAREN>("closing paren")
                inner
            }
            is Token.ASSERTTRUE -> parseAssertTrue()
            is Token.ASSERTFALSE -> parseAssertFalse()
            is Token.ASSERTEQUAL -> parseAssertEqual()
            is Token.ASSERTNOTEQUAL -> parseAssertNotEqual()
            is Token.ASSERTTYPE -> parseAssertType()
            is Token.ASSERTNOTTYPE -> parseAssertNotType()
            is Token.ASSERTTHROWS -> parseAssertThrows()
            is Token.ASSERTGREATERTHAN -> parseAssertGreaterThan()
            is Token.ASSERTGREATEREQUALTHAN -> parseAssertGreaterEqualThan()
            is Token.ASSERTSMALLERTHAN -> parseAssertSmallerThan()
            is Token.ASSERTSMALLEREQUALTHAN -> parseAssertSmallerEqualThan()
            is Token.DEF -> parseDef()
            else -> null
        }
    }

    private fun parseString(): Expr {
        val t = expect<Token.STRING_LIT>("string")
        return Expr.StringLiteral(t.string)
    }

    private fun parseLet(): Expr {
        expect<Token.LET>("let")
        val recursive = lexer.lookahead() == Token.REC
        if (recursive) {
            expect<Token.REC>("rec")
        }
        val binder = expect<Token.IDENT>("binder").ident
        expect<Token.EQUALS>("equals")
        val expr = parseExpression()
        expect<Token.IN>("in")
        val body = parseExpression()
        return Expr.Let(recursive, binder, expr, body)
    }

    private fun parseDef(): Expr {
        expect<Token.DEF>("def")
        val recursive = lexer.lookahead() == Token.REC
        if (recursive) {
            expect<Token.REC>("rec")
        }
        val binder = expect<Token.IDENT>("binder").ident
        expect<Token.EQUALS>("equals")
        val expr = parseExpression()
        return Expr.Def(recursive, binder, expr)
    }

    private fun parseVar(): Expr.Var {
        val ident = expect<Token.IDENT>("identifier")
        return Expr.Var(ident.ident)
    }

    private fun parseIf(): Expr.If {
        expect<Token.IF>("if")
        val condition = parseExpression()
        expect<Token.THEN>("then")
        val thenBranch = parseExpression()
        expect<Token.ELSE>("else")
        val elseBranch = parseExpression()
        return Expr.If(condition, thenBranch, elseBranch)
    }

    private fun parseLambda(): Expr.Lambda {
        expect<Token.BACKSLASH>("lambda")
        val binder = expect<Token.IDENT>("binder")
        var tyBinder: MonoType? = null
        if (lexer.lookahead() == Token.COLON) {
            expect<Token.COLON>("colon")
            tyBinder = parseType()
        }
        expect<Token.EQ_ARROW>("arrow")
        val body = parseExpression()
        return Expr.Lambda(binder.ident, tyBinder, body)
    }

    private fun parseInt(): Expr.IntLiteral {
        val t = expect<Token.INT_LIT>("integer")
        return Expr.IntLiteral(t.int)
    }

    private fun parseBool(): Expr.BoolLiteral {
        val t = expect<Token.BOOL_LIT>("boolean")
        return Expr.BoolLiteral(t.bool)
    }

    private inline fun <reified T> expect(msg: String): T {
        val tkn = lexer.next()
        return tkn as? T ?: throw Exception("Expected $msg but saw $tkn")
    }
}

fun monoTy(input: String): MonoType {
    return Parser(Lexer(input)).parseType()
}

fun testLex(input: String) {
    val lexer = Lexer(input)
    do {
        println(lexer.next())
    } while (lexer.lookahead() != Token.EOF)
}

fun main() {
    testLex("""assertTrue false;""".trimMargin())
}