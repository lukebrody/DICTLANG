sealed trait Parsing[+A] {
  def map[B](f: A => B): Parsing[B]
  def flatMap[B](f: A => Parsing[B]): Parsing[B]

  def orElse[B >: A](other: Parsing[B]): Parsing[B]

  def pop[T <: Token](token: T): Parsing[T]
  def popName(): Parsing[Name]

  def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)] = {
    val first = a(this)
    val second = b(first)
    first.flatMap { firstResult =>
      second.map { secondResult =>
        (firstResult, secondResult)
      }
    }
  }
}

case class Success[+A](ast: A, rest: Seq[Token]) extends Parsing[A] {
  override def map[B](f: A => B): Parsing[B] = Success[B](f(ast), rest)
  override def flatMap[B](f: A => Parsing[B]): Parsing[B] = f(ast)
  override def orElse[B >: A](other: Parsing[B]): Parsing[B] = this

  override def pop[T <: Token](token: T): Parsing[T] = rest match {
    case Seq(`token`, rest @ _*) => Success(token, rest)
    case _                       => Failure
  }

  override def popName(): Parsing[Name] = rest match {
    case Seq(Name(name), rest @ _*) => Success(Name(name), rest)
    case _                          => Failure
  }
}

object Success {
  def unapply[A <: AST](arg: Success[A]): Option[(A, Seq[Token])] = Some((arg.ast, arg.rest))
}

case object Failure extends Parsing[Nothing] {
  override def map[B](f: Nothing => B): Parsing[B] = Failure
  override def flatMap[B](f: Nothing => Parsing[B]): Parsing[B] = Failure
  override def orElse[B >: Nothing](other: Parsing[B]): Parsing[B] = other

  override def pop[T <: Token](token: T): Parsing[T] = Failure
  override def popName(): Parsing[Name] = Failure

}

sealed trait AST

sealed trait Pattern extends AST

case class Definition(name: String) extends Pattern

object Definition {
  def parse(in: Parsing[_]): Parsing[Definition] =
    in.mapTwo(_.pop(Apostrophe), _.popName()).map { case (_, Name(name)) => Definition(name) }
}

case class Bind(name: String) extends Pattern

object Bind {
  def parse(in: Parsing[_]): Parsing[Bind] =
    in.mapTwo(_.pop(Backtick), _.popName()).map { case (_, Name(name)) => Bind(name) }
}

case class Symbol(name: String) extends Pattern

object Symbol {
  def parse(in: Parsing[_]): Parsing[Symbol] = in.popName().map { case Name(name) => Symbol(name) }
}

object Dict {
  def parse[K, V](key: Parsing[_] => Parsing[K], value: Parsing[_] => Parsing[V])(
      in: Parsing[_]
  ): Parsing[Seq[(K, V)]] = {

    def parsePairs(in: Parsing[_]): Parsing[Seq[(K, V)]] = {
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
  def parse(in: Parsing[_]): Parsing[PatternDict] =
    Dict.parse(Pattern.parse, Pattern.parse)(in).map { result => PatternDict(result.toMap) }
}

object Pattern {
  def parse(in: Parsing[_]): Parsing[Pattern] = {
    val dict: Parsing[Pattern] = PatternDict.parse(in)
    dict orElse Definition.parse(in) orElse Bind.parse(in) orElse Symbol.parse(in)
  }
}

sealed trait Expression

object Expression {}
