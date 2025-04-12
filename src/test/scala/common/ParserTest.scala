import munit.FunSuite
import common.parser._

class ParserTest extends FunSuite:

  test("char parser parses a single character") {
    val input = "abc"
    val result = Parser.char('a').parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, 'a')
        assertEquals(remaining, "bc")
      case Left(error) => fail(s"Unexpected error: $error")
  }

  test("string parser parses a string") {
    val input = "hello world"
    val result = Parser.string("hello").parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, "hello")
        assertEquals(remaining, " world")
      case Left(error) => fail(s"Unexpected error: $error")
  }

  test("digit parser parses a single digit") {
    val input = "123"
    val result = Parser.digit.parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, 1)
        assertEquals(remaining, "23")
      case Left(error) => fail(s"Unexpected error: $error")
  }

  test("many parser parses multiple occurrences") {
    val input = "aaa"
    val result = Parser.many(Parser.char('a')).parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, List('a', 'a', 'a'))
        assertEquals(remaining, "")
      case Left(error) => fail(s"Unexpected error: $error")
  }

  test("optional parser parses optional input") {
    val input = "a"
    val result = Parser.optional(Parser.char('a')).parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, Some('a'))
        assertEquals(remaining, "")
      case Left(error) => fail(s"Unexpected error: $error")
  }

  test("choice parser parses one of the options") {
    val input = "b"
    val result = (Parser.char('a') <|> Parser.char('b')).parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, 'b')
        assertEquals(remaining, "")
      case Left(error) => fail(s"Unexpected error: $error")
  }

  test("eof parser matches end of input") {
    val input = ""
    val result = Parser.eof.parse(input)
    result.value match
      case Right((tok, remaining)) =>
        assertEquals(tok.value, ())
        assertEquals(remaining, "")
      case Left(error) => fail(s"Unexpected error: $error")
  }
end ParserTest
