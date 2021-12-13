package parse

import tokenize.*

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

object Dict {
  case class Entry[L, R](left: L, right: R)

  def parse[L, R](
      in: Parsing[_],
      parseLeft: Parsing[_] => Parsing[L],
      parseRight: Parsing[_] => Parsing[R],
      openBracket: Token,
      closeBracket: Token
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

    in.mapTwo(_.pop(openBracket), _.pop(closeBracket)).map { _ => Seq.empty } orElse in
      .mapTwo(_.mapTwo(_.pop(openBracket), parsePairs), _.pop(closeBracket))
      .map { case ((_, rows), _) =>
        rows
      }
  }
}
