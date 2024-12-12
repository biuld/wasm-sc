package wasm.wat.parser.instr.vari

import wasm.wat.parser.*
import wasm.wat.parser.mod.idx
import wasm.wat.syntax.instr.Instruction.*

def localGet = for
  _ <- keyword("local.get")
  x <- idx
yield LocalGet(x)

def localSet = for
  _ <- keyword("local.set")
  x <- idx
yield LocalSet(x)

def localTee = for
  _ <- keyword("local.tee")
  x <- idx
yield LocalTee(x)

def globalGet = for
  _ <- keyword("global.get")
  x <- idx
yield GlobalGet(x)

def globalSet = for
  _ <- keyword("global.set")
  x <- idx
yield GlobalSet(x)

def varInstr =
  localGet <|>
    localSet <|>
    localTee <|>
    globalGet <|>
    globalSet
