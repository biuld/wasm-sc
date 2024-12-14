package wasm.wat.parser.instr

import common.parser.Parser
import common.parser.Parser.many
import wasm.wat.parser.*
import wasm.wat.parser.instr.control.controlInstr
import wasm.wat.parser.instr.mem.memInstr
import wasm.wat.parser.instr.num.numInstr
import wasm.wat.parser.instr.param.paramInstr
import wasm.wat.parser.instr.ref.refInstr
import wasm.wat.parser.instr.table.tableInstr
import wasm.wat.parser.instr.vari.varInstr
import wasm.wat.syntax.mod.Idx.*
import wasm.wat.syntax.instr.Instruction
import common.parser.Parser.optional

def sign = (keyword("u") `$>` false) <|>
  (keyword("s") `$>` true)

def instr: Parser[Instruction] =
  controlInstr <|>
    refInstr <|>
    varInstr <|>
    paramInstr <|>
    tableInstr <|>
    memInstr <|>
    numInstr

def expr: Parser[List[Instruction]] =
  def atom = for i <- instr
  yield i :: Nil

  def list: Parser[List[Instruction]] = for
    _ <- lparen
    xs <- many(expr)
    _ <- rparen
  yield xs.flatten

  atom <|> list
