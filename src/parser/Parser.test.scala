import munit.FunSuite
import parser._

class ParserTest extends FunSuite {

  test("char parser should correctly parse a specific character") {
    val p = Parser.char('a')

    val result = p.parse("abc", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, 'a')
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 1L)
        assertEquals(remaining, "bc")
      case _ => fail("Expected a successful parse")
    }
  }

  test("char parser should fail on wrong character") {
    val p = Parser.char('a')

    val result = p.parse("bbc", 0)

    assert(result.value.isLeft)
  }

  test("string parser should correctly parse a string") {
    val p = Parser.string("hello")

    val result = p.parse("hello world", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, "hello")
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 5L)
        assertEquals(remaining, " world")
      case _ => fail("Expected a successful parse")
    }
  }

  test("string parser should fail on wrong string") {
    val p = Parser.string("hello")

    val result = p.parse("world", 0)

    result match {
      case ParseResult(Left(error)) =>
        assert(error.message.contains("Expected 'hello'"))
      case _ => fail("Expected a failure")
    }
  }

  test("digit parser should correctly parse a digit") {
    val p = Parser.digit

    val result = p.parse("123", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, 1)
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 1L)
        assertEquals(remaining, "23")
      case _ => fail("Expected a successful parse")
    }
  }

  test("digit parser should fail on non-digit") {
    val p = Parser.digit

    val result = p.parse("abc", 0)

    assert(result.value.isLeft)
  }

  test("many parser should parse zero or more elements") {
    val p = Parser.many(Parser.char('a'))

    val result = p.parse("aaab", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, List('a', 'a', 'a'))
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 3L)
        assertEquals(remaining, "b")
      case _ => fail("Expected a successful parse")
    }
  }

  test("many1 parser should parse one or more elements") {
    val p = Parser.many1(Parser.char('a'))

    val result = p.parse("aaab", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, List('a', 'a', 'a'))
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 3L)
        assertEquals(remaining, "b")
      case _ => fail("Expected a successful parse")
    }
  }

  test("many1 parser should fail on zero elements") {
    val p = Parser.many1(Parser.char('a'))

    val result = p.parse("bcd", 0)

    assert(result.value.isLeft)
  }

  test("optional parser should return None if the parser fails") {
    val p = Parser.optional(Parser.char('a'))

    val result = p.parse("bcd", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, None)
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 0L)
        assertEquals(remaining, "bcd")
      case _ => fail("Expected a successful parse")
    }
  }

  test("optional parser should return Some if the parser succeeds") {
    val p = Parser.optional(Parser.char('a'))

    val result = p.parse("abcd", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, Some('a'))
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 1L)
        assertEquals(remaining, "bcd")
      case _ => fail("Expected a successful parse")
    }
  }

  test("sepBy1 parser should correctly parse elements separated by another parser") {
    val p = Parser.sepBy1(Parser.char('a'), Parser.char(','))

    val result = p.parse("a,a,a", 0)

    result match {
      case ParseResult(Right((tok, remaining))) =>
        assertEquals(tok.value, List('a', 'a', 'a'))
        assertEquals(tok.start, 0L)
        assertEquals(tok.end, 5L)
        assertEquals(remaining, "")
      case _ => fail("Expected a successful parse")
    }
  }

  test("sepBy1 parser should fail if the first element is missing") {
    val p = Parser.sepBy1(Parser.char('a'), Parser.char(','))

    val result = p.parse("b,c", 0)

    assert(result.value.isLeft)
  }

}