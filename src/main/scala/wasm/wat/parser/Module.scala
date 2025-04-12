package wasm.wat.parser.mod

import common.parser.Parser.many
import wasm.wat.parser.*
import wasm.wat.parser.instr.expr
import wasm.wat.parser.types.*
import wasm.wat.syntax.mod.*
import wasm.wat.syntax.mod.Idx.*
import common.parser.Parser.digitChar
import common.parser.Parser.optional
import common.parser.Parser
import wasm.wat.syntax.types.ValType
import wasm.wat.syntax.types.FuncType

def idx = idLit.map(Id(_)) <|> i32Lit.map(U32(_))

def importDesc =
  def func = for
    _ <- lparen
    _ <- keyword("func")
    id <- idx
    tu <- typeUse
    _ <- rparen
  yield ImportDesc.Func(id, tu)

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

def imports = for
  _ <- lparen
  _ <- keyword("import")
  module <- stringLit
  name <- stringLit
  desc <- importDesc
  _ <- rparen
yield Import(module, name, desc)

def func =
  val imports: Parser[(String, String)] = for
    _ <- lparen
    _ <- keyword("import")
    mod <- stringLit
    m <- stringLit
    _ <- rparen
  yield (mod, m)

  val exports: Parser[List[String]] = for
    _ <- lparen
    _ <- keyword("export")
    ns <- many(stringLit)
    _ <- rparen
  yield ns

  for
    _ <- lparen
    _ <- keyword("func")
    id <- optional(idLit)
    imOpt <- optional(imports)
    exOpt <- many(exports)
    tuOpt <- optional(typeUse)
    locals <- many(local)
    body <- optional(expr)
    _ <- rparen
  yield Func(
    id,
    imOpt,
    exOpt.flatten,
    tuOpt.getOrElse(EMPTY_TYPE_USE),
    locals.flatten,
    body.toList.flatten
  )

def table = for
  _ <- lparen
  _ <- keyword("table")
  idOpt <- optional(idx)
  t <- tableType
  _ <- rparen
yield Table(idOpt, t)

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

def exports = for
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

def types = for
  _ <- lparen
  _ <- keyword("type")
  idOpt <- optional(idx)
  ty <- funcType
  _ <- rparen
yield Named(idOpt, ty)

def namedValType(keywordStr: String): Parser[List[Named[ValType]]] = for
  _ <- lparen
  _ <- keyword(keywordStr)
  namedTys <- many:
    for
      idOpt <- optional(idx)
      ty <- valType
    yield Named(idOpt, ty)
  _ <- rparen
yield namedTys

def local = namedValType("local")

def params = for
  _ <- lparen
  _ <- keyword("param")
  tys <- many(valType)
  _ <- rparen
yield tys

val results: Parser[List[ValType]] = for
  _ <- lparen
  _ <- keyword("result")
  tys <- many(valType)
  _ <- rparen
yield tys

def typeUse: Parser[TypeUse] =
  def implicits = for
    _ <- lparen
    _ <- keyword("type")
    idOpt <- optional(idx)
    _ <- rparen
  yield TypeUse(idOpt, Nil, Nil)

  def explicits = for
    _ <- lparen
    _ <- keyword("type")
    idOpt <- optional(idx)
    _ <- rparen
    ps <- many(params)
    rs <- many(results)
  yield TypeUse(idOpt, ps.flatten, rs.flatten)

  implicits <|> explicits

def module = for
  _ <- lparen
  _ <- keyword("module")
  types <- many(types)
  imports <- many(imports)
  funcs <- many(func)
  tables <- many(table)
  mems <- many(mem)
  globals <- many(global)
  exports <- many(exports)
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
