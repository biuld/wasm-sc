import munit.FunSuite
import common.parser.*
import wasm.wat.parser.instr.vari.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.mod.Idx.*

class VarInstrTest extends FunSuite {

  test("localGet parses local.get with numeric index correctly") {
    val input = "local.get 3"
    val result = localGet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalGet(U32(3)), 0, input.length), "")
    )
  }

  test("localGet parses local.get with symbolic index correctly") {
    val input = "local.get $foo"
    val result = localGet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalGet(Id("foo")), 0, input.length), "")
    )
  }

  test("localSet parses local.set with numeric index correctly") {
    val input = "local.set 1"
    val result = localSet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalSet(U32(1)), 0, input.length), "")
    )
  }

  test("localSet parses local.set with symbolic index correctly") {
    val input = "local.set $bar"
    val result = localSet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalSet(Id("bar")), 0, input.length), "")
    )
  }

  test("localTee parses local.tee with numeric index correctly") {
    val input = "local.tee 2"
    val result = localTee.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalTee(U32(2)), 0, input.length), "")
    )
  }

  test("localTee parses local.tee with symbolic index correctly") {
    val input = "local.tee $baz"
    val result = localTee.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalTee(Id("baz")), 0, input.length), "")
    )
  }

  test("globalGet parses global.get with numeric index correctly") {
    val input = "global.get 0"
    val result = globalGet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(GlobalGet(U32(0)), 0, input.length), "")
    )
  }

  test("globalGet parses global.get with symbolic index correctly") {
    val input = "global.get $qux"
    val result = globalGet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(GlobalGet(Id("qux")), 0, input.length), "")
    )
  }

  test("globalSet parses global.set with numeric index correctly") {
    val input = "global.set 4"
    val result = globalSet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(GlobalSet(U32(4)), 0, input.length), "")
    )
  }

  test("globalSet parses global.set with symbolic index correctly") {
    val input = "global.set $xyz"
    val result = globalSet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(GlobalSet(Id("xyz")), 0, input.length), "")
    )
  }

  test("varInstr parses local.get with numeric index correctly") {
    val input = "local.get 5"
    val result = varInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(LocalGet(U32(5)), 0, input.length), "")
    )
  }

  test("varInstr parses global.set with symbolic index correctly") {
    val input = "global.set $abc"
    val result = varInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(GlobalSet(Id("abc")), 0, input.length), "")
    )
  }

  test("varInstr fails on unsupported instruction") {
    val input = "unsupported.instr 3"
    val result = varInstr.parse(input)
    assert(result.value.isLeft)
  }
}
