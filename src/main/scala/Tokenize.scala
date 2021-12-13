package tokenize

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

sealed trait Token {
  def text: String
}

object Token {
  def apply(string: String): Try[Seq[Token]] = string match {
    case OpenBracket.pattern(rest)        => tokenize(rest).map(Seq(OpenBracket) ++ _)
    case CloseBracket.pattern(rest)       => tokenize(rest).map(Seq(CloseBracket) ++ _)
    case Colon.pattern(rest)              => tokenize(rest).map(Seq(Colon) ++ _)
    case Comma.pattern(rest)              => tokenize(rest).map(Seq(Comma) ++ _)
    case Name.pattern(name, rest)         => tokenize(rest).map(Seq(Name(name)) ++ _)
    case Backtick.pattern(rest)           => tokenize(rest).map(Seq(Backtick) ++ _)
    case Dot.pattern(rest)                => tokenize(rest).map(Seq(Dot) ++ _)
    case OpenSquareBracket.pattern(rest)  => tokenize(rest).map(Seq(OpenSquareBracket) ++ _)
    case CloseSquareBracket.pattern(rest) => tokenize(rest).map(Seq(CloseSquareBracket) ++ _)
    case _                                => Failure(new Exception(s"Cannot parse $string"))
  }
}

case object OpenBracket extends Token {
  def pattern: Regex = """\{(.*)""".r
  def text = "{"
}

case object CloseBracket extends Token {
  def pattern: Regex = """\}(.*)""".r
  def text = "}"
}

case object Colon extends Token {
  def pattern: Regex = """:(.*)""".r
  def text = ":"
}

case object Comma extends Token {
  def pattern: Regex = """,(.*)""".r
  def text = ","
}

case class Name(name: String) extends Token {
  def text = name
}

object Name {
  def pattern: Regex = "([A-Za-z0-9]+)(.*)".r
}

case object Backtick extends Token {
  def pattern: Regex = "`(.*)".r
  def text = "`"
}

case object Dot extends Token {
  def pattern: Regex = """\.(.*)""".r
  def text = "."
}

case object OpenSquareBracket extends Token {
  def pattern: Regex = """\[(.*)""".r
  def text = "["
}

case object CloseSquareBracket extends Token {
  def pattern: Regex = """\](.*)""".r
  def text = "]"
}

val whitespacePattern = """^\s*(\S*)(.*)$""".r

def tokenize(string: String): Try[Seq[Token]] = {
  string match {
    case whitespacePattern("", "")   => Success(Seq())
    case whitespacePattern("", rest) => tokenize(rest)
    case whitespacePattern(token, rest) =>
      for {
        thisToken <- Token(token)
        restTokens <- tokenize(rest)
      } yield thisToken ++ restTokens
  }
}
