package wasm.wat.parser.instr.table

import wasm.wat.parser.*
import wasm.wat.parser.instr.idx
import wasm.wat.syntax.*
import wasm.wat.syntax.Instruction.*


def tableGet = for
  _ <- keyword("table.get")
  x <- idx
yield TableGet(x)

def tableSet = for
  _ <- keyword("table.set")
  x <- idx
yield TableSet(x)

def tableSize = for
  _ <- keyword("table.size")
  x <- idx
yield TableSize(x)

def tableGrow = for
  _ <- keyword("table.grow")
  x <- idx
yield TableGrow(x)

def tableFill = for
  _ <- keyword("table.fill")
  x <- idx
yield TableFill(x)

def tableCopy = for
  _ <- keyword("table.copy")
  x <- idx
  y <- idx
yield TableCopy(x, y)

def tableInit = for
  _ <- keyword("table.init")
  x <- idx
  y <- idx
yield TableInit(x, y)

def elemDrop = for
  _ <- keyword("elem.drop")
  x <- idx
yield ElemDrop(x)

def tableInstr =
  tableGet <|>
    tableSet <|>
    tableSize <|>
    tableGrow <|>
    tableFill <|>
    tableCopy <|>
    tableInit <|>
    elemDrop
