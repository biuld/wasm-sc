package wasm.wat.parser

import parser._
import parser.Parser._
import wasm.wat.syntax._

def i32Lit: Parser[Int] = skipWhitespace >> many1(digit).map(_.mkString.toInt)
def i64Lit: Parser[Long] = skipWhitespace >> many1(digit).map(_.mkString.toLong)

def f32Lit: Parser[Float] =
  for
    _ <- skipWhitespace
    intPart <- many1(digit)
    _ <- char('.')
    fracPart <- many1(digit)
  yield s"$intPart.$fracPart".toFloat

def f64Lit: Parser[Double] =
  for
    _ <- skipWhitespace
    intPart <- many1(digit)
    _ <- char('.')
    fracPart <- many1(digit)
  yield s"$intPart.$fracPart".toDouble

def idLit: Parser[String] =
  for
    _ <- skipWhitespace
    _ <- char('$')
    id <- many1(char(_.isUnicodeIdentifierPart))
  yield id.mkString

def lparen: Parser[Unit] = char('(') `$>` ()
def rparen: Parser[Unit] = char(')') `$>` ()
def keyword(k: String): Parser[Unit] = token(string(k) `$>` ())
def comma: Parser[Unit] = char(',') `$>` ()
