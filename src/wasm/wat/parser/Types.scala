package wasm.wat.parser

import parser._
import parser.Parser._
import wasm.wat.syntax._

def valType: Parser[ValType] =
  (keyword("i32") `$>` ValType.Num(NumType.I32)) <|>
    (keyword("i64") `$>` ValType.Num(NumType.I64)) <|>
    (keyword("f32") `$>` ValType.Num(NumType.F32)) <|>
    (keyword("f64") `$>` ValType.Num(NumType.F64)) <|>
    (keyword("funcref") `$>` ValType.Ref(RefType.FuncRef)) <|>
    (keyword("externref") `$>` ValType.Ref(RefType.ExternRef))

def funcType: Parser[FuncType] =
  val params =
    optional(
      for
        _ <- lparen
        _ <- keyword("param")
        types <- many1(valType)
        _ <- rparen
      yield types
    ).map(_.getOrElse(Nil))

  val results =
    optional(
      for
        _ <- lparen
        _ <- keyword("result")
        tys <- many1(valType)
        _ <- rparen
      yield tys
    ).map(_.getOrElse(Nil))

  for
    _ <- lparen
    _ <- keyword("func")
    p <- params
    r <- results
    _ <- rparen
  yield FuncType(p, r)

def typeSection: Parser[List[FuncType]] =
  for
    _ <- lparen
    _ <- keyword("type")
    tys <- many(funcType)
    _ <- rparen
  yield tys

def limits: Parser[Limits] =
  for
    min <- i32Lit
    max <- optional(i32Lit)
  yield Limits(min, max)

def memType: Parser[MemType] =
  for
    _ <- lparen
    _ <- keyword("memory")
    lim <- limits
    _ <- rparen
  yield MemType(lim)

def refType: Parser[RefType] =
  (keyword("funcref") `$>` RefType.FuncRef) <|>
    (keyword("externref") `$>` RefType.ExternRef)

def tableType: Parser[TableType] =
  for
    _ <- lparen
    _ <- keyword("table")
    ref <- refType
    lim <- limits
    _ <- rparen
  yield TableType(ref, lim)

def mutability: Parser[Mutability] =
  (keyword("const") `$>` Mutability.Const) <|>
    (keyword("var") `$>` Mutability.Var)

def globalType: Parser[GlobalType] =
  for
    _ <- lparen
    _ <- keyword("global")
    valType <- valType
    mut <- mutability
    _ <- rparen
  yield GlobalType(valType, mut)
