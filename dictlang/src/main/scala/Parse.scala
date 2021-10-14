sealed trait ParseResult[+A] {
  self =>

  def map[B](f: A => B): ParseResult[B]
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B]

  def orElse[B >: A](other: ParseResult[B]): ParseResult[B]

  def pop[T <: Token](token: T): ParseResult[T]
  def popName(): ParseResult[Name]

  def mapTwo[A, B](a: ParseResult[_] => ParseResult[A], b: ParseResult[_] => ParseResult[B]): ParseResult[(A, B)] = {
    val first = a(this)
    val second = b(first)
    first.flatMap { firstResult =>
      second.map { secondResult =>
        (firstResult, secondResult)
      }
    }
  }
}

case class Success[+A](ast: A, rest: Seq[Token]) extends ParseResult[A] {
  override def map[B](f: A => B): ParseResult[B] = Success[B](f(ast), rest)
  override def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = f(ast)
  override def orElse[B >: A](other: ParseResult[B]): ParseResult[B] = this

  override def pop[T <: Token](token: T): ParseResult[T] = rest match {
    case Seq(`token`, rest @ _*) => Success(token, rest)
    case _                       => Failure
  }

  override def popName(): ParseResult[Name] = rest match {
    case Seq(Name(name), rest @ _*) => Success(Name(name), rest)
    case _                          => Failure
  }
}

object Success {
  def unapply[A <: AST](arg: Success[A]): Option[(A, Seq[Token])] = Some((arg.ast, arg.rest))
}

case object Failure extends ParseResult[Nothing] {
  override def map[B](f: Nothing => B): ParseResult[B] = Failure
  override def flatMap[B](f: Nothing => ParseResult[B]): ParseResult[B] = Failure
  override def orElse[B >: Nothing](other: ParseResult[B]): ParseResult[B] = other

  override def pop[T <: Token](token: T): ParseResult[T] = Failure
  override def popName(): ParseResult[Name] = Failure

}

sealed trait AST

sealed trait Pattern extends AST

case class Definition(name: String) extends Pattern

object Definition {
  def parse(in: ParseResult[_]): ParseResult[Definition] =
    in.mapTwo(_.pop(Apostrophe), _.popName()).map { case (_, Name(name)) => Definition(name) }
}

case class Bind(name: String) extends Pattern

object Bind {
  def parse(in: ParseResult[_]): ParseResult[Bind] =
    in.mapTwo(_.pop(Backtick), _.popName()).map { case (_, Name(name)) => Bind(name) }
}

case class Symbol(name: String) extends Pattern

object Symbol {
  def parse(in: ParseResult[_]): ParseResult[Symbol] = in.popName().map { case Name(name) => Symbol(name) }
}

object Dict {
  def parse[K, V](key: ParseResult[_] => ParseResult[K], value: ParseResult[_] => ParseResult[V])(
      in: ParseResult[_]
  ): ParseResult[Seq[(K, V)]] = {

    def parsePairs(in: ParseResult[_]): ParseResult[Seq[(K, V)]] = {
      val row = in.mapTwo(_.mapTwo(key, _.pop(Colon)), _.mapTwo(value, _.pop(Comma))).map { case ((k, _), (v, _)) =>
        Seq((k, v))
      }
      row.flatMap { r =>
        parsePairs(row) match {
          case Failure            => row
          case Success(ast, rest) => Success(r ++ ast, rest)
        }
      }
    }

    in.mapTwo(_.mapTwo(_.pop(OpenBracket), parsePairs), _.pop(CloseBracket)).map { case ((_, rows), _) => rows }
  }
}

case class PatternDict(dict: Map[Pattern, Pattern]) extends Pattern

object PatternDict {
  def parse(in: ParseResult[_]): ParseResult[PatternDict] =
    Dict.parse(Pattern.parse, Pattern.parse)(in).map { result => PatternDict(result.toMap) }
}

object Pattern {
  def parse(in: ParseResult[_]): ParseResult[Pattern] = {
    val dict: ParseResult[Pattern] = PatternDict.parse(in)
    dict orElse Definition.parse(in) orElse Bind.parse(in) orElse Symbol.parse(in)
  }
}

sealed trait Expression

object Expression {}
