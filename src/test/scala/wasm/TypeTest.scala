import munit.FunSuite
import wasm.wat.parser.types.*
import wasm.wat.syntax.types.*
import common.parser.*


class TypeTest extends FunSuite:

  test("numType parses i32 correctly") {
    val input = "i32"
    val result = numType.parse(input)
    assertEquals(result, ParseResult.success(Tok(NumType.I32, 0, 3), ""))
  }

  test("numType parses i64 correctly") {
    val input = "i64"
    val result = numType.parse(input)
    assertEquals(result, ParseResult.success(Tok(NumType.I64, 0, 3), ""))
  }

  test("refType parses funcref correctly") {
    val input = "funcref"
    val result = refType.parse(input)
    assertEquals(result, ParseResult.success(Tok(RefType.FuncRef, 0, 7), ""))
  }

  test("refType parses externref correctly") {
    val input = "externref"
    val result = refType.parse(input)
    assertEquals(result, ParseResult.success(Tok(RefType.ExternRef, 0, 9), ""))
  }

  test("valType parses NumType correctly") {
    val input = "i32"
    val result = valType.parse(input)
    assertEquals(result, ParseResult.success(Tok(ValType.Num(NumType.I32), 0, 3), ""))
  }

  test("valType parses RefType correctly") {
    val input = "funcref"
    val result = valType.parse(input)
    assertEquals(result, ParseResult.success(Tok(ValType.Ref(RefType.FuncRef), 0, 7), ""))
  }

  test("funcType parses func type with params and results") {
    val input = "(func (param i32 i64) (result f32 f64))"
    val result = funcType.parse(input)
    val expected = FuncType(
      params = List(ValType.Num(NumType.I32), ValType.Num(NumType.I64)),
      results = List(ValType.Num(NumType.F32), ValType.Num(NumType.F64))
    )
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("funcType parses func type with no params and results") {
    val input = "(func)"
    val result = funcType.parse(input)
    val expected = FuncType(params = Nil, results = Nil)
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("typeSection parses multiple func types") {
    val input = "(type (func (param i32) (result i64)) (func (param f32) (result f64)))"
    val result = typeSection.parse(input)
    val expected = List(
      FuncType(
        params = List(ValType.Num(NumType.I32)),
        results = List(ValType.Num(NumType.I64))
      ),
      FuncType(
        params = List(ValType.Num(NumType.F32)),
        results = List(ValType.Num(NumType.F64))
      )
    )
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("limits parses correct limits") {
    val input = "0 1"
    val result = limits.parse(input)
    val expected = Limits(0, Some(1))
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("memType parses memory type correctly") {
    val input = "(memory 0 1)"
    val result = memType.parse(input)
    val expected = MemType(Limits(0, Some(1)))
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("tableType parses table type correctly") {
    val input = "(table funcref 0 1)"
    val result = tableType.parse(input)
    val expected = TableType(RefType.FuncRef, Limits(0, Some(1)))
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("globalType parses global type correctly") {
    val input = "(global i32 const)"
    val result = globalType.parse(input)
    val expected = GlobalType(ValType.Num(NumType.I32), Mutability.Const)
    assertEquals(result, ParseResult.success(Tok(expected, 0, input.length), ""))
  }

  test("heapType parses func heap type correctly") {
    val input = "func"
    val result = heapType.parse(input)
    assertEquals(result, ParseResult.success(Tok(HeapType.Func, 0, 4), ""))
  }

  test("heapType parses extern heap type correctly") {
    val input = "extern"
    val result = heapType.parse(input)
    assertEquals(result, ParseResult.success(Tok(HeapType.Extern, 0, 6), ""))
  }
end TypeTest