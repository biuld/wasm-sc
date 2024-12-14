import common.parser.*
import munit.FunSuite
import wasm.wat.parser.instr.control.*
import wasm.wat.syntax.instr.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.mod.Idx.*
import wasm.wat.syntax.types.*

class ControlInstrParserTests extends FunSuite {

  test("controlInstr parses block correctly") {
    val input = "block $label (result i32) (i32.const 42) end"
    val result = block.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Block(
            Some("label"),
            Some(ValType.Num(NumType.I32)),
            List(Const(NumType.I32, Val.I32(42)))
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

    test("controlInstr parses block correctly") {
    val input = "block $label i32.const 42 end"
    val result = block.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Block(
            Some("label"),
            None,
            List(Const(NumType.I32, Val.I32(42)))
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("controlInstr parses block correctly") {
    val input = "block $label (i32.const 42) end"
    val result = block.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Block(
            Some("label"),
            None,
            List(Const(NumType.I32, Val.I32(42)))
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("controlInstr parses loop correctly") {
    val input = "loop $loopName (result i64) i64.const 100 end"
    val result = loop.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Loop(
            Some("loopName"),
            Some(ValType.Num(NumType.I64)),
            List(Const(NumType.I64, Val.I64(100)))
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("controlInstr parses if-then-else correctly") {
    val input = "if $ifName (result f32) f32.const 1.23 else f64.const 4.56 end"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          If(
            Some("ifName"),
            Some(ValType.Num(NumType.F32)),
            List(Const(NumType.F32, Val.F32(1.23f))),
            List(Const(NumType.F64, Val.F64(4.56)))
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("controlInstr parses br correctly") {
    val input = "br 1"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Br(U32(1)), 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses br_if correctly") {
    val input = "br_if 2"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(BrIf(U32(2)), 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses br_table correctly") {
    val input = "br_table 1 2 3"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(BrTable(List(U32(1), U32(2), U32(3))), 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses return correctly") {
    val input = "return"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Return, 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses call correctly") {
    val input = "call 3"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Call(U32(3)), 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses call_indirect correctly") {
    val input = "call_indirect 1 2"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(CallIndirect(U32(1), U32(2)), 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses nop correctly") {
    val input = "nop"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Nop, 0, input.length),
        ""
      )
    )
  }

  test("controlInstr parses unreachable correctly") {
    val input = "unreachable"
    val result = controlInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Unreachable, 0, input.length),
        ""
      )
    )
  }
}
