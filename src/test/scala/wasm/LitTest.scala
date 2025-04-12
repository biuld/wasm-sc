import munit.FunSuite
import common.parser.*
import wasm.wat.parser.*

class LitTest extends FunSuite:

  test("i32Lit should parse 32-bit integers") {
    val input = "  12345"
    val result = i32Lit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, 12345)
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("i64Lit should parse 64-bit integers") {
    val input = "  12345678901234"
    val result = i64Lit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, 12345678901234L)
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("f32Lit should parse 32-bit floats") {
    val input = "  123.45"
    val result = f32Lit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, 123.45f)
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("f64Lit should parse 64-bit doubles") {
    val input = "  123.456789"
    val result = f64Lit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, 123.456789)
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("idLit should parse identifiers starting with $") {
    val input = "  $identifier123"
    val result = idLit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, "identifier123")
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("stringLit should parse string literals") {
    val input = "  \"hello world\""
    val result = stringLit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, "hello world")
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("stringLit should parse strings with escape sequences") {
    val input = "\"hello\\nworld\""
    val result = stringLit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, "hello\nworld")
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}")
    }
  }

  test("stringLit should parse strings with Unicode escape sequences") {
    val input = "\"emoji:\\u{1F600}\"" // 😀
    val result = stringLit.parse(input)
    result.value match {
      case Right((Tok(value, _, _), remaining)) =>
        assertEquals(value, "emoji:😀")
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}")
    }
  }

  test("stringLit should fail on invalid escape sequences") {
    val input = "\"hello\\qworld\""
    val result = stringLit.parse(input)
    result.value match {
      case Left(error) =>
        assert(error.message.contains("Expected"))
      case _ =>
        fail("Expected failure but got success")
    }
  }

  test("stringLit should fail on invalid Unicode escape sequences") {
    val input = "\"hello\\u{110000}\""
    val result = stringLit.parse(input)
    result.value match {
      case Left(error) =>
        assert(error.message.contains("Expected"))
      case _ =>
        fail("Expected failure but got success")
    }
  }

  test("keyword should match specific keywords") {
    val input = "  keyword"
    val result = keyword("keyword").parse(input)
    result.value match {
      case Right((Tok(_, _, _), remaining)) =>
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("lparen should match '('") {
    val input = "  ("
    val result = lparen.parse(input)
    result.value match {
      case Right((Tok(_, _, _), remaining)) =>
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("rparen should match ')'") {
    val input = "  )"
    val result = rparen.parse(input)
    result.value match {
      case Right((Tok(_, _, _), remaining)) =>
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }

  test("comma should match ','") {
    val input = "  ,"
    val result = comma.parse(input)
    result.value match {
      case Right((Tok(_, _, _), remaining)) =>
        assertEquals(remaining, "")
      case Left(error) =>
        fail(s"Failed to parse: ${error.message}, context: ${error.context}")
    }
  }
end LitTest
