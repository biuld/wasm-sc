import munit.FunSuite
import common.parser.*
import wasm.wat.parser.instr.table.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.mod.Idx.*

class TableInstrTest extends FunSuite {

  test("tableGet parses table.get with numeric index correctly") {
    val input = "table.get 0"
    val result = tableGet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableGet(U32(0)), 0, input.length), "")
    )
  }

  test("tableGet parses table.get with symbolic index correctly") {
    val input = "table.get $foo"
    val result = tableGet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableGet(Id("foo")), 0, input.length), "")
    )
  }

  test("tableSet parses table.set with numeric index correctly") {
    val input = "table.set 1"
    val result = tableSet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableSet(U32(1)), 0, input.length), "")
    )
  }

  test("tableSet parses table.set with symbolic index correctly") {
    val input = "table.set $bar"
    val result = tableSet.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableSet(Id("bar")), 0, input.length), "")
    )
  }

  test("tableSize parses table.size with numeric index correctly") {
    val input = "table.size 2"
    val result = tableSize.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableSize(U32(2)), 0, input.length), "")
    )
  }

  test("tableSize parses table.size with symbolic index correctly") {
    val input = "table.size $baz"
    val result = tableSize.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableSize(Id("baz")), 0, input.length), "")
    )
  }

  test("tableGrow parses table.grow with numeric index correctly") {
    val input = "table.grow 3"
    val result = tableGrow.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableGrow(U32(3)), 0, input.length), "")
    )
  }

  test("tableGrow parses table.grow with symbolic index correctly") {
    val input = "table.grow $qux"
    val result = tableGrow.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableGrow(Id("qux")), 0, input.length), "")
    )
  }

  test("tableFill parses table.fill with numeric index correctly") {
    val input = "table.fill 4"
    val result = tableFill.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableFill(U32(4)), 0, input.length), "")
    )
  }

  test("tableFill parses table.fill with symbolic index correctly") {
    val input = "table.fill $xyz"
    val result = tableFill.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableFill(Id("xyz")), 0, input.length), "")
    )
  }

  test("tableCopy parses table.copy with numeric indices correctly") {
    val input = "table.copy 5 6"
    val result = tableCopy.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableCopy(U32(5), U32(6)), 0, input.length), "")
    )
  }

  test("tableCopy parses table.copy with symbolic indices correctly") {
    val input = "table.copy $src $dst"
    val result = tableCopy.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(TableCopy(Id("src"), Id("dst")), 0, input.length),
        ""
      )
    )
  }

  test("tableInit parses table.init with numeric indices correctly") {
    val input = "table.init 7 8"
    val result = tableInit.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableInit(U32(7), U32(8)), 0, input.length), "")
    )
  }

  test("tableInit parses table.init with symbolic indices correctly") {
    val input = "table.init $table $elem"
    val result = tableInit.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(TableInit(Id("table"), Id("elem")), 0, input.length),
        ""
      )
    )
  }

  test("elemDrop parses elem.drop with numeric index correctly") {
    val input = "elem.drop 9"
    val result = elemDrop.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(ElemDrop(U32(9)), 0, input.length), "")
    )
  }

  test("elemDrop parses elem.drop with symbolic index correctly") {
    val input = "elem.drop $elem"
    val result = elemDrop.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(ElemDrop(Id("elem")), 0, input.length), "")
    )
  }

  test("tableInstr parses table.get with numeric index correctly") {
    val input = "table.get 10"
    val result = tableInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(Tok(TableGet(U32(10)), 0, input.length), "")
    )
  }

  test("tableInstr parses table.copy with symbolic indices correctly") {
    val input = "table.copy $src $dst"
    val result = tableInstr.parse(input)
    assertEquals(
      result,
      ParseResult.success(
        Tok(TableCopy(Id("src"), Id("dst")), 0, input.length),
        ""
      )
    )
  }

  test("tableInstr fails on unsupported instruction") {
    val input = "unsupported.instr 11"
    val result = tableInstr.parse(input)
    assert(result.value.isLeft)
  }
}
