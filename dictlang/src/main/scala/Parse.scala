package parse

import tokenize._

sealed trait Parsing[+A] {
  def map[B](f: A => B): Parsing[B]
  def flatMap[B](f: A => Parsing[B]): Parsing[B]

  def orElse[B >: A](other: Parsing[B]): Parsing[B]

  def pop[T <: Token](token: T): Parsing[T]
  def popName(): Parsing[Name]

  def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)]
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

  override def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)] = {
    val first = a(this)
    first.flatMap { firstResult =>
      b(first).map { secondResult =>
        (firstResult, secondResult)
      }
    }
  }
}

case object Failure extends Parsing[Nothing] {
  override def map[B](f: Nothing => B): Parsing[B] = Failure
  override def flatMap[B](f: Nothing => Parsing[B]): Parsing[B] = Failure
  override def orElse[B >: Nothing](other: Parsing[B]): Parsing[B] = other

  override def pop[T <: Token](token: T): Parsing[T] = Failure
  override def popName(): Parsing[Name] = Failure

  override def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)] = Failure

}

/*

value{
  key{value: match{value: match}}: value{key: value{key: value}}
}

matches move their symbols up their parent
keys move themselves and their children over to the value, and themselves down to the next entries
values export their direct children

key (
  binds,
  exported
)

match

 */

sealed trait Key
sealed trait Match extends Key
sealed trait Value extends Match

case class Definition(name: String) extends Key
case class MatchDict(entries: Seq[Dict.Entry[Value, Match]]) extends Match
case class Bind(name: String) extends Match

case class ValueDict(entries: Seq[Dict.Entry[Key, Value]]) extends Value
case class Symbol(name: String) extends Value
case class DotExpression(left: Value, right: Value) extends Value {
  private def exprs: Seq[Value] = {
    def get(e: Value) = e match {
      case dot: DotExpression => dot.exprs
      case e                  => Seq(e)
    }

    get(left) ++ get(right)
  }

  def leftAssoc: DotExpression = {
    val es = exprs
    es.drop(2).foldLeft(DotExpression(es(0), es(1))) { case (acc, e) => DotExpression(acc, e) }
  }
}

object Value {
  def parse(in: Parsing[_]): Parsing[Value] = {
    DotExpression.parse(in) orElse parseNoDot(in)
  }

  def parseNoDot(in: Parsing[_]): Parsing[Value] = {
    ValueDict.parse(in) orElse Symbol.parse(in)
  }
}

object ValueDict {
  def parse(in: Parsing[_]): Parsing[ValueDict] = {
    Dict.parse[Key, Value](in, Key.parse, Value.parse).map(ValueDict(_))
  }
}

object Key {
  def parse(in: Parsing[_]): Parsing[Key] = {
    MatchDict.parse(in) orElse Definition.parse(in) orElse Bind.parse(in)
  }
}

object MatchDict {
  def parse(in: Parsing[_]): Parsing[MatchDict] = {
    Dict.parse[Value, Match](in, Value.parse, Match.parse).map(MatchDict(_))
  }
}

object Match {
  def parse(in: Parsing[_]): Parsing[Match] = {
    DotExpression.parse(in) orElse MatchDict.parse(in) orElse Bind.parse(in) orElse Symbol.parse(in)
  }
}

object Dict {
  case class Entry[L, R](left: L, right: R)

  def parse[L, R](
      in: Parsing[_],
      parseLeft: Parsing[_] => Parsing[L],
      parseRight: Parsing[_] => Parsing[R]
  ): Parsing[Seq[Entry[L, R]]] = {

    def parsePairs(
        in: Parsing[_]
    ): Parsing[Seq[Entry[L, R]]] = {
      val row =
        in.mapTwo(_.mapTwo(parseLeft, _.pop(Colon)), _.mapTwo(parseRight, _.pop(Comma))).map { case ((l, _), (r, _)) =>
          Seq(Entry(l, r))
        }
      row.flatMap { r =>
        parsePairs(row) match {
          case Failure                            => row
          case success: Success[Seq[Entry[L, R]]] => Success(r ++ success.ast, success.rest)
        }
      }
    }

    in.mapTwo(_.pop(OpenBracket), _.pop(CloseBracket)).map { _ => Seq.empty } orElse in
      .mapTwo(_.mapTwo(_.pop(OpenBracket), parsePairs), _.pop(CloseBracket))
      .map { case ((_, rows), _) =>
        rows
      }
  }
}

object Definition {
  def parse(in: Parsing[_]): Parsing[Definition] =
    in.mapTwo(_.pop(Apostrophe), _.popName()).map { case (_, Name(name)) => Definition(name) }
}

object Bind {
  def parse(in: Parsing[_]): Parsing[Bind] =
    in.mapTwo(_.pop(Backtick), _.popName()).map { case (_, Name(name)) => Bind(name) }
}

object Symbol {
  def parse(in: Parsing[_]): Parsing[Symbol] = in.popName().map { case Name(name) => Symbol(name) }
}

object DotExpression {
  def parse(in: Parsing[_]): Parsing[DotExpression] = {
    in.mapTwo(_.mapTwo(Value.parseNoDot, _.pop(Dot)), Value.parse).map { case ((left, _), right) =>
      DotExpression(left, right).leftAssoc
    }
  }
}
