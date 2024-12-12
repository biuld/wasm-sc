package wasm.wat.parser.mod

import common.parser.Parser.many
import wasm.wat.parser.*
import wasm.wat.parser.instr.expr
import wasm.wat.parser.types.*
import wasm.wat.syntax.mod.*
import wasm.wat.syntax.mod.Idx.*
import common.parser.Parser.digitChar
import common.parser.Parser.optional

def idx = idLit.map(Id(_)) <|> i32Lit.map(U32(_))

def importDesc =
  def func = for
    _ <- lparen
    _ <- keyword("func")
    f <- idx
    _ <- rparen
  yield ImportDesc.Func(f)

  def table = for
    _ <- lparen
    _ <- keyword("table")
    t <- tableType
    _ <- rparen
  yield ImportDesc.Table(t)

  def mem = for
    _ <- lparen
    _ <- keyword("memory")
    m <- memType
    _ <- rparen
  yield ImportDesc.Mem(m)

  def global = for
    _ <- lparen
    _ <- keyword("global")
    g <- globalType
    _ <- rparen
  yield ImportDesc.Global(g)

  func <|> table <|> mem <|> global

def exportDesc =
  def func = for
    _ <- lparen
    _ <- keyword("func")
    f <- idx
    _ <- rparen
  yield ExportDesc.Func(f)

  def table = for
    _ <- lparen
    _ <- keyword("table")
    t <- idx
    _ <- rparen
  yield ExportDesc.Table(t)

  def mem = for
    _ <- lparen
    _ <- keyword("memory")
    m <- idx
    _ <- rparen
  yield ExportDesc.Mem(m)

  def global = for
    _ <- lparen
    _ <- keyword("global")
    g <- idx
    _ <- rparen
  yield ExportDesc.Global(g)

  func <|> table <|> mem <|> global

def importP = for
  _ <- lparen
  _ <- keyword("import")
  module <- stringLit
  name <- stringLit
  desc <- importDesc
  _ <- rparen
yield Import(module, name, desc)

def func = for
  _ <- lparen
  _ <- keyword("func")
  t <- idx
  locals <- many(valType)
  body <- many(expr)
  _ <- rparen
yield Func(t, locals, body.flatten)

def table = for
  _ <- lparen
  _ <- keyword("table")
  t <- tableType
  _ <- rparen
yield Table(t)

def mem = for
  _ <- lparen
  _ <- keyword("memory")
  m <- memType
  _ <- rparen
yield Mem(m)

def global = for
  _ <- lparen
  _ <- keyword("global")
  t <- globalType
  init <- expr
  _ <- rparen
yield Global(t, init)

def exportP = for
  _ <- lparen
  _ <- keyword("export")
  name <- stringLit
  desc <- exportDesc
  _ <- rparen
yield Export(name, desc)

def start = for
  _ <- lparen
  _ <- keyword("start")
  funcIdx <- idx
  _ <- rparen
yield Start(funcIdx)

def elem = for
  _ <- lparen
  _ <- keyword("elem")
  tableIdx <- idx
  offset <- expr
  init <- many(idx)
  _ <- rparen
yield Elem(tableIdx, offset, init)

def data = for
  _ <- lparen
  _ <- keyword("data")
  memIdx <- idx
  offset <- expr
  init <- stringLit
  _ <- rparen
yield Data(memIdx, offset, init.toList.map(_.toByte))

def typeP = for
  _ <- lparen
  _ <- keyword("type")
  idOpt <- optional(idLit)
  ty <- funcType
  _ <- rparen
yield (idOpt, ty)

def module = for
  _ <- lparen
  _ <- keyword("module")
  types <- many(typeP)
  imports <- many(importP)
  funcs <- many(func)
  tables <- many(table)
  mems <- many(mem)
  globals <- many(global)
  exports <- many(exportP)
  start <- optional(start)
  elems <- many(elem)
  datas <- many(data)
  _ <- rparen
yield Module(
  types,
  imports,
  funcs,
  tables,
  mems,
  globals,
  exports,
  start,
  elems,
  datas
)
