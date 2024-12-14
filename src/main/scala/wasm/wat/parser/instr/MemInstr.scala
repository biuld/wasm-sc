package wasm.wat.parser.instr.mem

import common.parser.Parser.optional
import wasm.wat.parser.i32Lit
import wasm.wat.parser.instr.sign
import wasm.wat.parser.keyword
import wasm.wat.parser.mod.idx
import wasm.wat.parser.types.numType
import wasm.wat.syntax.instr.Instruction.*

def memCopy = keyword("memory.copy") `$>` MemoryCopy

def memSize = keyword("memory.size") `$>` MemorySize

def memFill = keyword("memory.fill") `$>` MemoryFill

def memGrow = keyword("memory.grow") `$>` MemoryGrow

def memInit = for
  _ <- keyword("memory.init")
  x <- idx
yield MemoryInit(x)

def dataDrop = for
  _ <- keyword("data.drop")
  x <- idx
yield DataDrop(x)

def subWith = (keyword("8") `$>` 8) <|>
  (keyword("16") `$>` 16) <|>
  (keyword("32") `$>` 32)

def load = for
  ty <- numType
  _ <- keyword(".load")
  swOpt <- optional:
    for
      w <- subWith
      _ <- keyword("_")
      s <- sign
    yield (w, s)
  (wOpt, sOpt) = swOpt match
    case None         => (None, None)
    case Some((w, s)) => (Some(w), Some(s))
  o <- i32Lit
  a <- i32Lit
yield Load(ty, sOpt, wOpt, o, a)

def store = for
  ty <- numType
  _ <- keyword(".store")
  sOpt <- optional:
    for
      _ <- keyword("_")
      s <- subWith
    yield s
  o <- i32Lit
  a <- i32Lit
yield Store(ty, sOpt, o, a)

def memInstr = memInit <|>
  memCopy <|>
  memFill <|>
  memSize <|>
  memGrow <|>
  dataDrop <|>
  load <|>
  store
