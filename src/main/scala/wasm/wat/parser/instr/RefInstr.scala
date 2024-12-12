package wasm.wat.parser.instr.ref

import wasm.wat.parser.*
import wasm.wat.parser.mod.idx
import wasm.wat.parser.types.*
import wasm.wat.syntax.instr.Instruction.*

def refNull = for
  _ <- keyword("ref.null")
  t <- heapType
yield RefNull(t)

def refIsNull = keyword("ref.is_null") `$>` RefIsNull

def refFunc = for
  _ <- keyword("ref.func")
  x <- idx
yield RefFunc(x)

def refInstr = refNull <|> refIsNull <|> refFunc
