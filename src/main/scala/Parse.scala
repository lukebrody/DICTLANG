package parse

import tokenize._

sealed trait Parsing[+A] {
  def map[B](f: A => B): Parsing[B]
  def flatMap[B](f: A => Parsing[B]): Parsing[B]

  def orElse[B >: A](other: => Parsing[B]): Parsing[B]

  def pop[T <: Token](token: T): Parsing[T]
  def popName(): Parsing[Name]

  def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[A] => Parsing[B]): Parsing[(A, B)]
}

case class Success[+A](ast: A, rest: Seq[Token]) extends Parsing[A] {
  override def map[B](f: A => B): Parsing[B] = Success[B](f(ast), rest)
  override def flatMap[B](f: A => Parsing[B]): Parsing[B] = f(ast)
  override def orElse[B >: A](other: => Parsing[B]): Parsing[B] = this

  override def pop[T <: Token](token: T): Parsing[T] = rest match {
    case Seq(`token`, rest @ _*) => Success(token, rest)
    case _                       => Failure(s"Expected ${token.text}", rest)
  }

  override def popName(): Parsing[Name] = rest match {
    case Seq(Name(name), rest @ _*) => Success(Name(name), rest)
    case _                          => Failure(s"Expected symbol", rest)
  }

  override def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[A] => Parsing[B]): Parsing[(A, B)] = {
    val first = a(this)
    first.flatMap { firstResult =>
      b(first).map { secondResult =>
        (firstResult, secondResult)
      }
    }
  }
}

case class Failure(error: String, rest: Seq[Token]) extends Parsing[Nothing] {
  override def map[B](f: Nothing => B): Parsing[B] = this
  override def flatMap[B](f: Nothing => Parsing[B]): Parsing[B] = this
  override def orElse[B >: Nothing](other: => Parsing[B]): Parsing[B] = other match {
    case success: Success[B]   => success
    case otherFailure: Failure => if (otherFailure.rest.length < rest.length) otherFailure else this
  }

  override def pop[T <: Token](token: T): Parsing[T] = this
  override def popName(): Parsing[Name] = this

  override def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[A] => Parsing[B]): Parsing[(A, B)] = this
}

sealed trait Match

case class MatchDict(entries: Seq[Dict.Entry[Match, Match]]) extends Match

case class UseSymbol(name: String) extends Value
case class BindSymbol(name: String) extends Match

sealed trait Value extends Match

case class ValueDict(entries: Seq[Dict.Entry[Match, Value]]) extends Value

case class DotExpression(left: Value, right: Value) extends Value

case class SubscriptExpression(left: Value, right: Value) extends Value

object Value {

  private def term(in: Parsing[_]): Parsing[Value] = UseSymbol.parse(in) orElse ValueDict.parse(in)

  private def dotAfter(left: Parsing[Value]): Parsing[Value] = {
    left.flatMap { l =>
      left
        .mapTwo(
          _.mapTwo(_.pop(Dot), term).map { case (_, right) => DotExpression(l, right) },
          dotExpr => dotAfter(dotExpr) orElse subscriptAfter(dotExpr) orElse dotExpr
        )
        .map(_._2)
    }
  }

  private def subscriptAfter(left: Parsing[Value]): Parsing[Value] = {
    left.flatMap { l =>
      left
        .mapTwo(
          _.mapTwo(_.mapTwo(_.pop(OpenSquareBracket), parse), _.pop(CloseSquareBracket)).map { case ((_, right), _) =>
            SubscriptExpression(l, right)
          },
          subscriptExpr => dotAfter(subscriptExpr) orElse subscriptAfter(subscriptExpr) orElse subscriptExpr
        )
        .map(_._2)
    }
  }

  def parse(in: Parsing[_]): Parsing[Value] = {
    dotAfter(term(in)) orElse subscriptAfter(term(in)) orElse term(in)
  }

}

object ValueDict {
  def parse(in: Parsing[_]): Parsing[ValueDict] = {
    Dict.parse[Match, Value](in, Match.parse, Value.parse).map(ValueDict(_))
  }
}

object Match {
  def parse(in: Parsing[_]): Parsing[Match] = {
    MatchDict.parse(in) orElse BindSymbol.parse(in) orElse Value.parse(in)
  }
}

object MatchDict {
  def parse(in: Parsing[_]): Parsing[MatchDict] = {
    Dict.parse[Match, Match](in, Match.parse, Match.parse).map(MatchDict(_))
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
          case _: Failure                         => row
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

object BindSymbol {
  def parse(in: Parsing[_]): Parsing[BindSymbol] =
    in.mapTwo(_.mapTwo(_.pop(Backtick), _.popName()), _.pop(Backtick)).map { case ((_, Name(name)), _) =>
      BindSymbol(name)
    }
}

object UseSymbol {
  def parse(in: Parsing[_]): Parsing[UseSymbol] = in.popName().map { case Name(name) => UseSymbol(name) }
}
