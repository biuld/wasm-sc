package wasm.wat.parser

import common.parser.*
import common.parser.Parser
import common.parser.Parser.*
import wasm.wat.syntax.*

def i32Lit: Parser[Int] = for
  _ <- skipWhitespace
  digits <- many1(digit)
yield digits.mkString.toInt

def i64Lit: Parser[Long] = for
  _ <- skipWhitespace
  digits <- many1(digit)
yield digits.mkString.toLong

def f32Lit: Parser[Float] =
  for
    _ <- skipWhitespace
    intPart <- many1(digit)
    _ <- char('.')
    fracPart <- many1(digit)
  yield s"${intPart.mkString}.${fracPart.mkString}".toFloat

def f64Lit: Parser[Double] =
  for
    _ <- skipWhitespace
    intPart <- many1(digit)
    _ <- char('.')
    fracPart <- many1(digit)
  yield s"${intPart.mkString}.${fracPart.mkString}".toDouble

def idLit: Parser[String] =
  for
    _ <- skipWhitespace
    _ <- char('$')
    id <- many1(char(_.isUnicodeIdentifierPart))
  yield id.mkString

def keyword(k: String): Parser[Unit] = token(string(k) `$>` ())
def lparen: Parser[Unit] = keyword("(")
def rparen: Parser[Unit] = keyword(")")
def comma: Parser[Unit] = keyword(",")
