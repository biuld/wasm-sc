import munit.FunSuite
import common.parser.*
import wasm.wat.parser.instr.num.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.types.*
import wasm.wat.syntax.instr.*

class NumInstrTest extends FunSuite {

  // Test for constant instructions
  test("numInstr parses i32.const correctly") {
    val input = "i32.const 42"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Const(NumType.I32, Val.I32(42)), 0, input.length),
        ""
      )
    )
  }

  test("numInstr parses f64.const correctly") {
    val input = "f64.const 3.14"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Const(NumType.F64, Val.F64(3.14)), 0, input.length),
        ""
      )
    )
  }

  // Test for unary instructions
  test("numInstr parses i32.clz correctly") {
    val input = "i32.clz"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(Unary(NumType.I32, UnaryOp.IntOp(IntUnaryOp.Clz)), 0, input.length),
        ""
      )
    )
  }

  test("numInstr parses f32.sqrt correctly") {
    val input = "f32.sqrt"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Unary(NumType.F32, UnaryOp.FloatOp(FloatUnaryOp.Sqrt)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  // Test for binary instructions
  test("numInstr parses i64.add correctly") {
    val input = "i64.add"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Binary(NumType.I64, BinaryOp.IntOp(IntBinaryOp.Add)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("numInstr parses f64.div correctly") {
    val input = "f64.div"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Binary(NumType.F64, BinaryOp.FloatOp(FloatBinaryOp.Div)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  // Test for comparison instructions
  test("numInstr parses i32.eq correctly") {
    val input = "i32.eq"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Comparison(NumType.I32, ComparisonOp.IntOp(IntComparisonOp.Eq)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("numInstr parses f64.ge correctly") {
    val input = "f64.ge"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Comparison(NumType.F64, ComparisonOp.FloatOp(FloatComparisonOp.Ge)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  // Test for conversion instructions
  test("numInstr parses i32.wrap_i64 correctly") {
    val input = "i32.wrap_i64"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Conversion(NumType.I64, NumType.I32, ConversionOp.Wrap, None),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("numInstr parses i64.trunc_f32_s correctly") {
    val input = "i64.trunc_f32_s"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Conversion(NumType.F32, NumType.I64, ConversionOp.Trunc, Some(true)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("numInstr parses f32.convert_i64_u correctly") {
    val input = "f32.convert_i64_u"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Conversion(
            NumType.I64,
            NumType.F32,
            ConversionOp.Convert,
            Some(false)
          ),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("numInstr parses f64.promote_f32 correctly") {
    val input = "f64.promote_f32"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Conversion(NumType.F32, NumType.F64, ConversionOp.Promote, None),
          0,
          input.length
        ),
        ""
      )
    )
  }

  test("numInstr parses i64.extend_i32_s correctly") {
    val input = "i64.extend_i32_s"
    val result = numInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(
          Conversion(NumType.I32, NumType.I64, ConversionOp.Extend, Some(true)),
          0,
          input.length
        ),
        ""
      )
    )
  }

  // Test for invalid input
  test("numInstr fails on unsupported input") {
    val input = "unknown.op"
    val result = numInstr.parse(input)
    assert(result.value.isLeft)
  }
}
