package wasm.wat.parser.instr.param

import common.parser.Parser.*
import wasm.wat.parser.*
import wasm.wat.parser.types.*
import wasm.wat.syntax.mod.Idx.*
import wasm.wat.syntax.instr.Instruction.*

def drop = keyword("drop") `$>` Drop

def select = for
  _ <- keyword("select")
  t <- optional(valType)
yield Select(t)


def paramInstr = drop <|> select