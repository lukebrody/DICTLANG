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
  def parse[A](
      in: Parsing[_],
      open: Token,
      close: Token
  )(parseRow: Parsing[_] => Parsing[A]): Parsing[Seq[A]] = {

    def parseRows(
        in: Parsing[_]
    ): Parsing[Seq[A]] = {
      val row = parseRow(in).map(Seq(_))

      val nextRow = row.flatMap { rowValue =>
        row.mapTwo(_.pop(Comma), parseRows).map { case (_, rows) => rowValue ++ rows }
      }

      nextRow orElse row orElse in.map { _ => Seq.empty }
    }

    in.mapTwo(_.mapTwo(_.pop(open), parseRows), _.pop(close)).map { case ((_, value), _) => value }
  }
}
