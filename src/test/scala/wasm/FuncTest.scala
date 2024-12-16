import common.parser.*
import munit.FunSuite
import wasm.wat.parser.mod.*
import wasm.wat.syntax.instr.*
import wasm.wat.syntax.mod.*
import wasm.wat.syntax.mod.Idx.*
import wasm.wat.syntax.types.*

class FuncTest extends FunSuite:

  test("parse unnamed function") {
    val input = "(func)"
    val result = func.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Func(None, None, Nil, None, Nil, Nil, Nil, Nil), 0, input.length),
        ""
      )
    )
  }

  test("parse exported function") {
    val input = "(func (export \"f\"))"
    val result = func.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Func(None, None, "f" :: Nil, None, Nil, Nil, Nil, Nil),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("parse function with name") {
    val input = "(func $f)"
    val result = func.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Func(Some("f"), None, Nil, None, Nil, Nil, Nil, Nil),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("parse function with name and export") {
    val input = "(func $h (export \"g\"))"
    val result = func.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Func(
            Some("h"),
            None,
            "g" :: Nil,
            None,
            Nil,
            Nil,
            Nil,
            Nil
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("parse function with local variables") {
    val input = "(func (local i32))"
    val result = func.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Func(
            None,
            None,
            Nil,
            None,
            Nil,
            Nil,
            List(Named(None, ValType.Num(NumType.I32))),
            Nil
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("parse function with multiple locals") {
    val input = "(func (local i32 f64 i64))"
    val result = func.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Func(
            None,
            None,
            Nil,
            None,
            Nil,
            Nil,
            Named(None, ValType.Num(NumType.I32)) ::
              Named(None, ValType.Num(NumType.F64)) ::
              Named(None, ValType.Num(NumType.I64)) ::
              Nil,
            Nil
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }
end FuncTest
