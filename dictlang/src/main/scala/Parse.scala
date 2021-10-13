import com.sun.tools.javac.parser.Tokens

sealed trait ParseResult[+A <: AST] {
  self =>

  def withFilter(p: Success[A] => Boolean): WithFilter = new WithFilter(p)

  def filter(f: Success[A] => Boolean): ParseResult[A] = flatMap { success =>
    if (f(success)) success else Failure
  }
  def map[B](f: Success[A] => Success[B]): ParseResult[B] = flatMap(f)
  def flatMap[B](f: Success[A] => ParseResult[B]): ParseResult[B]
  def foreach[U](aToU: Success[A] => U): Unit

  class WithFilter(p: Success[A] => Boolean) {
    def map[B](f: Success[A] => Success[B]): ParseResult[B] = self filter p map f
    def flatMap[B](f: Success[A] => ParseResult[B]): ParseResult[B] = self filter p flatMap f
    def foreach[U](f: Success[A] => U): Unit = self filter p foreach f
    def withFilter(q: Success[A] => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  def orElse(other: ParseResult[A]): ParseResult[A]
}

case class Success[+A <: AST](ast: A, rest: Seq[Token]) extends ParseResult[A] {
  override def flatMap[B](f: Success[A] => ParseResult[B]): ParseResult[B] = f(this)
  override def foreach[U](aToU: Success[A] => U): Unit = aToU(this)
  override def orElse(other: ParseResult[A]): ParseResult[A] = this
}

object Success {
  def unapply[A <: AST](arg: Success[A]): Option[(A, Seq[Token])] = Some((arg.ast, arg.rest))
}

case object Failure extends ParseResult[Nothing] {
  override def flatMap[B](f: Success[Nothing] => ParseResult[B]): ParseResult[B] = Failure
  override def foreach[U](aToU: Success[Nothing] => U): Unit = Failure
  override def orElse(other: ParseResult[Nothing]): ParseResult[Nothing] = other
}

sealed trait AST

sealed trait Pattern extends AST

case class Definition(name: String) extends Pattern

object Definition {
  def parse(tokens: Seq[Token]): ParseResult[Definition] = tokens match {
    case Seq(Apostrophe, Name(name), rest @ _*) => Success(Definition(name), rest)
    case _                                      => Failure
  }
}

case class Bind(name: String) extends Pattern

object Bind {
  def parse(tokens: Seq[Token]): ParseResult[Bind] = tokens match {
    case Seq(Backtick, Name(name), rest @ _*) => Success(Bind(name), rest)
    case _                                    => Failure
  }
}

case class Symbol(name: String) extends Pattern

object Symbol {
  def parse(tokens: Seq[Token]): ParseResult[Symbol] = tokens match {
    case Seq(Name(name), rest @ _*) => Success(Symbol(name), rest)
    case _                          => Failure
  }
}

object Dict {
  def parse[K, V](key: Seq[Token] => ParseResult[K], value: Seq[Token] => ParseResult[V])(
      tokens: Seq[Token]
  ): ParseResult[Seq[(K, V)]] = {

    def parsePairs(tokens: Seq[Token]): ParseResult[Seq[(K, V)]] = for {
      Success(k: K, afterKey) <- key(tokens)
      Success(v: V, afterValue) <- afterKey match {
        case Seq(Colon, rest @ _*) => value(rest)
        case _                     => Failure
      }
      result <- afterValue match {
        case Seq(Comma, rest @ _*) =>
          parsePairs(rest) match {
            case Success(morePairs, rest) => Success[Seq[(K, V)]](Seq((k, v)) ++ morePairs, rest)
            case Failure                  => Success[Seq[(K, V)]](Seq((k, v)), rest)
          }
        case _ => Failure
      }
    } yield result

    for {
      Success(pairs, rest) <- tokens match {
        case Seq(OpenBracket, rest @ _*) => parsePairs(rest)
        case _                           => Failure
      }
      result <- rest match {
        case Seq(CloseBracket, rest @ _*) => Success(pairs, rest)
        case _                            => Failure
      }
    } yield result
  }
}

case class PatternDict(dict: Map[Pattern, Pattern]) extends Pattern

object PatternDict {
  def parse(tokens: Seq[Token]): ParseResult[PatternDict] =
    Dict.parse(Pattern.parse, Pattern.parse)(tokens).map { case Success(results, rest) =>
      Success(PatternDict(results.toMap), rest)
    }
}

object Pattern {
  def parse(tokens: Seq[Token]): ParseResult[Pattern] =
    PatternDict.parse(tokens) orElse Definition.parse(tokens) orElse Bind.parse(tokens) orElse Symbol.parse(tokens)
}

sealed trait Expression

object Expression {}
