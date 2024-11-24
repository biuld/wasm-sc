package parser

import scala.annotation.targetName

case class ParseContext(parserName: String, position: Long, snippet: String)

opaque type ParseError = (String, Option[ParseContext])
object ParseError:
  def apply(message: String, context: Option[ParseContext] = None): ParseError =
    (message, context)

  extension (e: ParseError)
    def message: String = e._1
    def context: Option[ParseContext] = e._2
end ParseError

case class Tok[A](val value: A, val start: Long, val end: Long):
  def map[B](f: A => B): Tok[B] =
    Tok(f(value), start, end)
end Tok

case class ParseResult[A](value: Either[ParseError, (Tok[A], String)]):
  def map[B](f: ((Tok[A], String)) => (Tok[B], String)): ParseResult[B] =
    value match
      case Left(err) => ParseResult(Left(err))
      case Right(result) =>
        val (tok, remaining) = result
        val (newTok, newRemaining) = f(tok, remaining)
        ParseResult.success(newTok, newRemaining)

  def flatMap[B](f: ((Tok[A], String)) => ParseResult[B]): ParseResult[B] =
    value match
      case Left(err)     => ParseResult(Left(err))
      case Right(result) => f(result)
end ParseResult

object ParseResult:
  def success[A](tok: Tok[A], remaining: String): ParseResult[A] =
    ParseResult(Right(tok, remaining))

  def failure[A](s: String, context: Option[ParseContext]): ParseResult[A] =
    ParseResult(Left(ParseError(s, context)))

  def failure[A](err: ParseError): ParseResult[A] =
    ParseResult(Left(err))
end ParseResult

case class Parser[A](f: (String, Long) => ParseResult[A]):
  def parse(input: String, position: Long = 0): ParseResult[A] =
    Parser.skipWhitespace.parse(input, position) match
      case ParseResult(Right((_, remaining))) => f(remaining, position)
      case ParseResult(Left(value))           => ParseResult(Left(value))

  def map[B](ff: A => B): Parser[B] =
    Parser((input, position) =>
      for (tok, remaining) <- parse(input, position)
      yield (tok.map(ff), remaining)
    )

  def flatMap[B](ff: A => Parser[B]): Parser[B] =
    Parser { (input, position) =>
      for
        (Tok(value, start, end), remaining) <- parse(input, position)
        (Tok(newValue, _, newEnd), newRemaining) <- ff(value).parse(
          remaining,
          end
        )
      yield (Tok(newValue, start, newEnd), newRemaining)
    }

  def orElse(other: Parser[A]): Parser[A] =
    Parser((input, position) =>
      parse(input, position) match
        case ParseResult(Left(_)) => other.parse(input, position)
        case success              => success
    )
end Parser

object Parser:
  extension [A](p: Parser[A])
    infix def `>>`[B](other: Parser[B]): Parser[B] = p.flatMap(_ => other)
    infix def `<<`[B](other: Parser[B]): Parser[A] = other.flatMap(_ => p)
    infix def `>>=`[B](ff: A => Parser[B]): Parser[B] = p.flatMap(ff)
    infix def `<|>`(other: Parser[A]): Parser[A] = p.orElse(other)
    infix def `$>`[B](b: B): Parser[B] = p.map(_ => b)

  private def formatError(
      input: String,
      position: Long,
      context: Int = 5
  ): String =
    val start = math.max(0, position.toInt - context)
    val end = math.min(input.length, position.toInt + context)
    val snippet = input.slice(start, end).replace("\n", "\\n")
    s"at position $position near: '$snippet'"

  def char(c: Char): Parser[Char] = char(_ == c)

  def char(p: Char => Boolean): Parser[Char] =
    Parser((input, position) =>
      input.headOption match
        case Some(head) if p(head) =>
          ParseResult.success(Tok(head, position, position + 1), input.tail)
        case Some(_) =>
          ParseResult.failure(
            s"Expected character matching condition",
            Some(ParseContext("char", position, formatError(input, position)))
          )
        case None =>
          ParseResult.failure(
            s"Expected character matching condition",
            Some(ParseContext("char", position, formatError(input, position)))
          )
    )

  def string(s: String): Parser[String] =
    Parser((input, position) =>
      if input.startsWith(s) then
        ParseResult.success(
          Tok(s, position, position + s.length),
          input.drop(s.length)
        )
      else
        ParseResult.failure(
          s"Expected '$s'",
          Some(ParseContext("string", position, formatError(input, position)))
        )
    )

  def digitChar: Parser[Char] = char(_.isDigit)

  def digit: Parser[Int] = digitChar.map(_.asDigit)

  def many[A](p: Parser[A]): Parser[List[A]] =
    Parser((input, position) =>
      var acc = List.empty[A]
      var currentInput = input
      var currentPos = position
      val startPos = position // Record start position for the result
      var keepParsing = true

      while keepParsing do
        p.parse(currentInput, currentPos) match
          case ParseResult(Right((Tok(value, start, end), remaining))) =>
            acc = acc :+ value
            currentInput = remaining
            currentPos = end
          case ParseResult(Left(_)) =>
            keepParsing = false

      ParseResult.success(Tok(acc, startPos, currentPos), currentInput)
    )

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for
      first <- p
      rest <- many(p)
    yield first :: rest

  def optional[A](p: Parser[A]): Parser[Option[A]] =
    Parser((input, position) =>
      p.parse(input, position) match
        case ParseResult(Right((tok, remaining))) =>
          ParseResult.success(
            Tok(Some(tok.value), tok.start, tok.end),
            remaining
          )
        case ParseResult(Left(_)) =>
          ParseResult.success(Tok(None, position, position), input)
    )

  def sepBy[A, S](p: Parser[A], sep: Parser[S]): Parser[List[A]] =
    sepBy1(p, sep) <|> Parser((input, position) =>
      ParseResult.success(Tok(List.empty[A], position, position), input)
    )

  def sepBy1[A, S](p: Parser[A], sep: Parser[S]): Parser[List[A]] =
    for
      first <- p
      rest <- many(sep >> p)
    yield first :: rest

  def whitespace: Parser[Char] = char(_.isWhitespace)

  def letter: Parser[Char] = char(_.isLetter)

  def skipWhitespace: Parser[Unit] = many(whitespace) `$>` ()

  def eof: Parser[Unit] =
    Parser((input, position) =>
      if input.isEmpty then
        ParseResult.success(Tok((), position, position), input)
      else
        ParseResult.failure(
          "Expected end of input",
          Some(ParseContext("eof", position, formatError(input, position)))
        )
    )
end Parser
