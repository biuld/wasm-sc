import munit.FunSuite
import common.parser.*
import wasm.wat.parser.instr.param.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.types.*

class ParamInstrTest extends FunSuite:

  test("drop parses drop instruction correctly") {
    val input = "drop"
    val result = drop.parse(input)
    assertEquals(result, ParseResult.success(Tok(Drop, 0, input.length), ""))
  }

  test("select parses select instruction without value type correctly") {
    val input = "select"
    val result = select.parse(input)
    assertEquals(result, ParseResult.success(Tok(Select(None), 0, input.length), ""))
  }

  test("select parses select instruction with numeric value type correctly") {
    val input = "select i32"
    val result = select.parse(input)
    assertEquals(result, ParseResult.success(Tok(Select(Some(ValType.Num(NumType.I32))), 0, input.length), ""))
  }

  test("select parses select instruction with reference value type correctly") {
    val input = "select funcref"
    val result = select.parse(input)
    assertEquals(result, ParseResult.success(Tok(Select(Some(ValType.Ref(RefType.FuncRef))), 0, input.length), ""))
  }

  test("paramInstr parses drop instruction correctly") {
    val input = "drop"
    val result = paramInstr.parse(input)
    assertEquals(result, ParseResult.success(Tok(Drop, 0, input.length), ""))
  }

  test("paramInstr parses select instruction without value type correctly") {
    val input = "select"
    val result = paramInstr.parse(input)
    assertEquals(result, ParseResult.success(Tok(Select(None), 0, input.length), ""))
  }

  test("paramInstr parses select instruction with numeric value type correctly") {
    val input = "select i64"
    val result = paramInstr.parse(input)
    assertEquals(result, ParseResult.success(Tok(Select(Some(ValType.Num(NumType.I64))), 0, input.length), ""))
  }

  test("paramInstr parses select instruction with reference value type correctly") {
    val input = "select externref"
    val result = paramInstr.parse(input)
    assertEquals(result, ParseResult.success(Tok(Select(Some(ValType.Ref(RefType.ExternRef))), 0, input.length), ""))
  }

  test("paramInstr fails on unsupported input") {
    val input = "unknown"
    val result = paramInstr.parse(input)
    assert(result.value.isLeft)
  }
end ParamInstrTest