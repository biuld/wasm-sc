import munit.FunSuite
import common.parser.*
import wasm.wat.parser.instr.ref.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.types.*
import wasm.wat.syntax.mod.Idx.*

class RefInstrTest extends FunSuite:

  test("refNull parses ref.null with func heap type correctly") {
    val input = "ref.null func"
    val result = refNull.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefNull(HeapType.Func), 0, input.length), "")
    )
  }

  test("refNull parses ref.null with extern heap type correctly") {
    val input = "ref.null extern"
    val result = refNull.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefNull(HeapType.Extern), 0, input.length), "")
    )
  }

  test("refIsNull parses ref.is_null correctly") {
    val input = "ref.is_null"
    val result = refIsNull.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefIsNull, 0, input.length), "")
    )
  }

  test("refFunc parses ref.func with numeric index correctly") {
    val input = "ref.func 42"
    val result = refFunc.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefFunc(U32(42)), 0, input.length), "")
    )
  }

  test("refFunc parses ref.func with symbolic index correctly") {
    val input = "ref.func $foo"
    val result = refFunc.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefFunc(Id("foo")), 0, input.length), "")
    )
  }

  test("refInstr parses ref.null with heap type correctly") {
    val input = "ref.null func"
    val result = refInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefNull(HeapType.Func), 0, input.length), "")
    )
  }

  test("refInstr parses ref.is_null correctly") {
    val input = "ref.is_null"
    val result = refInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefIsNull, 0, input.length), "")
    )
  }

  test("refInstr parses ref.func with numeric index correctly") {
    val input = "ref.func 7"
    val result = refInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefFunc(U32(7)), 0, input.length), "")
    )
  }

  test("refInstr parses ref.func with symbolic index correctly") {
    val input = "ref.func $bar"
    val result = refInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(RefFunc(Id("bar")), 0, input.length), "")
    )
  }

  test("refInstr fails on unsupported input") {
    val input = "ref.unknown"
    val result = refInstr.parse(input)
    assert(result.value.isLeft)
  }
end RefInstrTest
