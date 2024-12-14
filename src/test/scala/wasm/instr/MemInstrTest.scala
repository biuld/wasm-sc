import munit.FunSuite
import common.parser.*
import wasm.wat.parser.instr.mem.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.types.NumType.*
import wasm.wat.syntax.mod.Idx.*

import munit.FunSuite
import wasm.wat.parser._
import wasm.wat.syntax._

class MemInstrTest extends FunSuite {

  test("Parse memory.copy") {
    val input = "memory.copy"
    val result = memInstr.parse(input)
    assertEquals(result.value, Right((Tok(MemoryCopy, 0, input.length), "")))
  }

  test("Parse memory.size") {
    val input = "memory.size"
    val result = memInstr.parse(input)
    assertEquals(result.value, Right((Tok(MemorySize, 0, input.length), "")))
  }

  test("Parse memory.fill") {
    val input = "memory.fill"
    val result = memInstr.parse(input)
    assertEquals(result.value, Right((Tok(MemoryFill, 0, input.length), "")))
  }

  test("Parse memory.grow") {
    val input = "memory.grow"
    val result = memInstr.parse(input)
    assertEquals(result.value, Right((Tok(MemoryGrow, 0, input.length), "")))
  }

  test("Parse memory.init with index") {
    val input = "memory.init 5"
    val result = memInstr.parse(input)
    assertEquals(
      result.value,
      Right((Tok(MemoryInit(U32(5)), 0, input.length), ""))
    )
  }

  test("Parse data.drop with index") {
    val input = "data.drop 3"
    val result = memInstr.parse(input)
    assertEquals(
      result.value,
      Right((Tok(DataDrop(U32(3)), 0, input.length), ""))
    )
  }

  test("Parse i32.load") {
    val input = "i32.load 0 4"
    val result = memInstr.parse(input)
    assertEquals(
      result.value,
      Right((Tok(Load(I32, None, None, 0, 4), 0, input.length), ""))
    )
  }

  test("Parse i32.load8_s") {
    val input = "i32.load8_s 0 4"
    val result = memInstr.parse(input)
    assertEquals(
      result.value,
      Right((Tok(Load(I32, Some(true), Some(8), 0, 4), 0, input.length), ""))
    )
  }

  test("Parse i32.store") {
    val input = "i32.store 0 4"
    val result = memInstr.parse(input)
    assertEquals(
      result.value,
      Right((Tok(Store(I32, None, 0, 4), 0, input.length), ""))
    )
  }

  test("Parse i32.store_16") {
    val input = "i32.store_16 0 4"
    val result = memInstr.parse(input)
    assertEquals(
      result.value,
      Right((Tok(Store(I32, Some(16), 0, 4), 0, input.length), ""))
    )
  }
}
