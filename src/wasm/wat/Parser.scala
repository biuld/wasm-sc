package wasm.wat.parser

import parser._
import parser.Parser._
import wasm.wat.syntax._

def identifier: Parser[String] =
  for
    first <- char('$') <|> letter
    rest <- many(letter <|> digitChar <|> char('_'))
  yield first + rest.mkString

def intLiteral: Parser[Int] = many1(digit).map(_.mkString.toInt)

def floatLiteral: Parser[Double] =
  for
    intPart <- many1(digit)
    _ <- char('.')
    fracPart <- many1(digit)
  yield s"$intPart.$fracPart".toDouble

def lparen: Parser[Unit] = char('(') `$>` ()
def rparen: Parser[Unit] = char(')') `$>` ()
def keyword(k: String): Parser[Unit] = string(k) `$>` ()
def comma: Parser[Unit] = char(',') `$>` ()

def numType: Parser[NumType] =
  (keyword("i32") `$>` NumType.I32) <|>
    (keyword("i64") `$>` NumType.I64) <|>
    (keyword("f32") `$>` NumType.F32) <|>
    (keyword("f64") `$>` NumType.F64)

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
        _ <- string("(param")
        types <- many1(valType)
        _ <- char(')')
      yield types
    ).map(_.getOrElse(Nil))

  val results =
    optional(
      for
        _ <- string("(result")
        types <- many1(valType)
        _ <- char(')')
      yield types
    ).map(_.getOrElse(Nil))

  for
    _ <- string("(func")
    p <- params
    r <- results
    _ <- char(')')
  yield FuncType(p, r)

def typeSection: Parser[List[FuncType]] =
  keyword("(type") >> many(funcType) << rparen
