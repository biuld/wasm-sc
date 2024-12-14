package wasm.wat.parser.instr.control

import common.parser.Parser.*
import wasm.wat.parser.*
import wasm.wat.parser.instr.expr
import wasm.wat.parser.mod.idx
import wasm.wat.parser.types.*
import wasm.wat.syntax.instr.Instruction.*
import wasm.wat.syntax.mod.Idx.*

def blockType = for
  _ <- lparen
  _ <- keyword("result")
  ty <- valType
  _ <- rparen
yield ty

def block = for
  _ <- keyword("block")
  label <- optional(idLit)
  ty <- optional(blockType)
  body <- expr
  _ <- keyword("end")
yield Block(label, ty, body)

def loop = for
  _ <- keyword("loop")
  label <- optional(idLit)
  ty <- optional(blockType)
  body <- expr
  _ <- keyword("end")
yield Loop(label, ty, body)

def ifThen = for
  _ <- keyword("if")
  label <- optional(idLit)
  ty <- optional(blockType)
  thenBr <- expr
  elseBr <- optional:
    for
      _ <- keyword("else")
      xs <- expr
    yield xs
  _ <- keyword("end")
yield If(label, ty, thenBr, elseBr.toList.flatten)

def br = for
  _ <- keyword("br")
  l <- idx
yield Br(l)

def brIf = for
  _ <- keyword("br_if")
  l <- idx
yield BrIf(l)

def brTable = for
  _ <- keyword("br_table")
  ls <- many(idx)
yield BrTable(ls)

def ret = keyword("return") `$>` Return

def call = for
  _ <- keyword("call")
  x <- idx
yield Call(x)

def callIndirect = for
  _ <- keyword("call_indirect")
  x <- idx
  y <- idx
yield CallIndirect(x, y)

def nop = keyword("nop") `$>` Nop

def unreachable = keyword("unreachable") `$>` Unreachable

def controlInstr =
  block <|>
    loop <|>
    ifThen <|>
    unreachable <|>
    nop <|>
    br <|>
    brIf <|>
    brTable <|>
    ret <|>
    call <|>
    callIndirect
