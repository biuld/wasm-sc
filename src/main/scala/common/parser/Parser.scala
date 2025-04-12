package common.parser

import scala.annotation.targetName
import scala.util.Try
import scala.util.Failure
import scala.util.Success

case class ParseContext(parserName: String, position: Long, snippet: String)

opaque type ParseError = (String, Option[ParseContext], List[String])
object ParseError:
  def apply(
      message: String,
      context: Option[ParseContext] = None,
      stack: List[String] = List.empty
  ): ParseError =
    (message, context, stack)

  extension (e: ParseError)
    def message: String = e._1
    def context: Option[ParseContext] = e._2
    def stack: List[String] = e._3

    def withStack(parserName: String): ParseError =
      (e._1, e._2, parserName :: e._3)
end ParseError

case class Tok[A](val value: A, val start: Long, val end: Long):
  def map[B](f: A => B): Tok[B] =
    Tok(f(value), start, end)
end Tok

case class ParseResult[A](
    value: Either[ParseError, (Tok[A], String)],
    debugInfo: List[String] = List.empty
):
  def map[B](f: ((Tok[A], String)) => (Tok[B], String)): ParseResult[B] =
    value match
      case Left(err) => ParseResult(Left(err), debugInfo)
      case Right(result) =>
        val (tok, remaining) = result
        val (newTok, newRemaining) = f(tok, remaining)
        ParseResult.success(newTok, newRemaining, debugInfo)

  def flatMap[B](f: ((Tok[A], String)) => ParseResult[B]): ParseResult[B] =
    value match
      case Left(err) => ParseResult(Left(err), debugInfo)
      case Right(result) =>
        val nextResult = f(result)
        nextResult.copy(debugInfo = debugInfo ++ nextResult.debugInfo)
end ParseResult

object ParseResult:
  def formatError(
      input: String,
      position: Long,
      context: Int = 5
  ): String =
    val start = math.max(0, position.toInt - context)
    val end = math.min(input.length, position.toInt + context)
    val snippet = input.slice(start, end).replace("\n", "\\n")
    s"at position $position near: '$snippet'"

  def success[A](
      tok: Tok[A],
      remaining: String,
      debugInfo: List[String] = List.empty
  ): ParseResult[A] =
    ParseResult(Right(tok, remaining), debugInfo)

  def failure[A](
      msg: String,
      input: String,
      position: Long,
      parserName: String = "unnamed parser",
      debugInfo: List[String] = List.empty
  ): ParseResult[A] =
    ParseResult(
      Left(
        ParseError(
          msg,
          Some(
            ParseContext(parserName, position, formatError(input, position))
          ),
          List(parserName)
        )
      ),
      debugInfo
    )
end ParseResult

case class Parser[A](f: (String, Long) => ParseResult[A]):
  def parse(
      input: String,
      position: Long = 0
  ): ParseResult[A] =
    val debug = sys.env
      .get("DEBUG_PARSER")
      .map(_.equalsIgnoreCase("true"))
      .getOrElse(false)
    val result = f(input, position)
    if debug then
      val debugMessage = result.value match
        case Right((Tok(value, start, end), remaining)) =>
          s"Parser succeeded: value='$value', start=$start, end=$end, remaining='$remaining'"
        case Left(error) =>
          s"Parser failed: message='${error.message}', position=$position"
      result.copy(debugInfo = result.debugInfo :+ debugMessage)
    else result

  def map[B](ff: A => B): Parser[B] =
    Parser { (input, position) =>
      val result = parse(input, position)
      result.value match
        case Left(e) => ParseResult(Left(e), result.debugInfo)
        case Right((tok, remaining)) =>
          Try(tok.map(ff)) match
            case Failure(ex) =>
              ParseResult.failure(
                ex.getMessage(),
                input,
                position,
                debugInfo = result.debugInfo
              )
            case Success(v) =>
              ParseResult.success(v, remaining, result.debugInfo)
    }

  def flatMap[B](ff: A => Parser[B]): Parser[B] =
    Parser { (input, position) =>
      val result = parse(input, position)
      result.value match
        case Left(e) => ParseResult(Left(e), result.debugInfo)
        case Right((Tok(value, start, end), remaining)) =>
          Try(ff(value)) match
            case Success(p) =>
              val nextResult = p.parse(remaining, end)
              nextResult.value match
                case Right(
                      (Tok(nextValue, nextStart, nextEnd), nextRemaining)
                    ) =>
                  // Ensure start and end are consistent
                  ParseResult.success(
                    Tok(nextValue, start, nextEnd),
                    nextRemaining,
                    result.debugInfo ++ nextResult.debugInfo
                  )
                case Left(err) =>
                  ParseResult(
                    Left(err),
                    result.debugInfo ++ nextResult.debugInfo
                  )
            case Failure(ex) =>
              ParseResult.failure(
                ex.getMessage(),
                input,
                position,
                debugInfo = result.debugInfo
              )
    }

  def orElse(other: Parser[A]): Parser[A] =
    Parser { (input, position) =>
      val result = parse(input, position)
      result.value match
        case Left(_) =>
          val otherResult = other.parse(input, position)
          otherResult.copy(debugInfo =
            result.debugInfo ++ otherResult.debugInfo
          )
        case success => result
    }

  def withFilter(predicate: A => Boolean): Parser[A] =
    Parser { (input, position) =>
      val result = parse(input, position)
      result.value match
        case Left(e) => ParseResult(Left(e), result.debugInfo)
        case Right((Tok(value, start, end), remaining)) =>
          Try(predicate(value)) match
            case Failure(ex) =>
              ParseResult.failure(
                ex.getMessage(),
                input,
                position,
                debugInfo = result.debugInfo
              )
            case Success(v) =>
              if v then
                ParseResult.success(
                  Tok(value, start, end),
                  remaining,
                  result.debugInfo
                )
              else
                ParseResult.failure(
                  s"Value [$value] did not satisfy the predicate",
                  input,
                  position,
                  debugInfo = result.debugInfo
                )
    }
end Parser

object Parser:
  extension [A](p: Parser[A])
    infix def `>>`[B](other: Parser[B]): Parser[B] = p.flatMap(_ => other)
    infix def `<<`[B](other: Parser[B]): Parser[A] = other.flatMap(_ => p)
    infix def `>>=`[B](ff: A => Parser[B]): Parser[B] = p.flatMap(ff)
    infix def `<|>`(other: Parser[A]): Parser[A] = p.orElse(other)
    infix def `$>`[B](b: B): Parser[B] = p.map(_ => b)

  def char(c: Char): Parser[Char] = char(_ == c)

  def char(p: Char => Boolean): Parser[Char] =
    Parser((input, position) =>
      input.headOption match
        case Some(head) if p(head) =>
          ParseResult.success(Tok(head, position, position + 1), input.tail)
        case Some(_) =>
          ParseResult.failure(
            s"Expected character matching condition",
            input,
            position,
            "char"
          )
        case None =>
          ParseResult.failure(
            s"Expected character matching condition",
            input,
            position,
            "char"
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
          input,
          position,
          "string"
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
          case ParseResult(Right((Tok(value, start, end), remaining)), _) =>
            acc = acc :+ value
            currentInput = remaining
            currentPos = end
          case ParseResult(Left(_), _) =>
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
        case ParseResult(Right((tok, remaining)), _) =>
          ParseResult.success(
            Tok(Some(tok.value), tok.start, tok.end),
            remaining
          )
        case ParseResult(Left(_), _) =>
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

  def token[A](p: Parser[A]): Parser[A] = skipWhitespace >> p

  def choice[A](ps: List[Parser[A]]) = ps.reduce(_ <|> _)

  def eof: Parser[Unit] =
    Parser((input, position) =>
      if input.isEmpty then
        ParseResult.success(Tok((), position, position), input)
      else
        ParseResult.failure(
          "Expected end of input",
          input,
          position,
          "eof"
        )
    )
end Parser
