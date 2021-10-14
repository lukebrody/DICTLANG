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

object Success {
  def unapply[A <: AST](arg: Success[A]): Option[(A, Seq[Token])] = Some((arg.ast, arg.rest))
}

case object Failure extends Parsing[Nothing] {
  override def map[B](f: Nothing => B): Parsing[B] = Failure
  override def flatMap[B](f: Nothing => Parsing[B]): Parsing[B] = Failure
  override def orElse[B >: Nothing](other: Parsing[B]): Parsing[B] = other

  override def pop[T <: Token](token: T): Parsing[T] = Failure
  override def popName(): Parsing[Name] = Failure

  override def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)] = Failure

}

sealed trait AST

sealed trait Expression extends AST

object Expression {
  def parse(in: Parsing[_]): Parsing[Expression] = {
    DotExpression.parse(in) orElse parseNoDot(in)
  }

  def parseNoDot(in: Parsing[_]): Parsing[Expression] = {
    Dict.parse(in) orElse Definition.parse(in) orElse Bind.parse(in) orElse Symbol.parse(
      in
    )
  }
}

case class Dict(dict: Map[Expression, Expression]) extends Expression

object Dict {
  def parse(in: Parsing[_]): Parsing[Dict] = {

    def parsePairs(in: Parsing[_]): Parsing[Seq[(Expression, Expression)]] = {
      val row = in.mapTwo(_.mapTwo(Expression.parse, _.pop(Colon)), _.mapTwo(Expression.parse, _.pop(Comma))).map {
        case ((k, _), (v, _)) =>
          Seq((k, v))
      }
      row.flatMap { r =>
        parsePairs(row) match {
          case Failure                                         => row
          case success: Success[Seq[(Expression, Expression)]] => Success(r ++ success.ast, success.rest)
        }
      }
    }

    in.mapTwo(_.pop(OpenBracket), _.pop(CloseBracket)).map { _ => Dict(Map.empty) } orElse in
      .mapTwo(_.mapTwo(_.pop(OpenBracket), parsePairs), _.pop(CloseBracket))
      .map { case ((_, rows), _) =>
        Dict(rows.toMap)
      }
  }
}

case class Definition(name: String) extends Expression

object Definition {
  def parse(in: Parsing[_]): Parsing[Definition] =
    in.mapTwo(_.pop(Apostrophe), _.popName()).map { case (_, Name(name)) => Definition(name) }
}

case class Bind(name: String) extends Expression

object Bind {
  def parse(in: Parsing[_]): Parsing[Bind] =
    in.mapTwo(_.pop(Backtick), _.popName()).map { case (_, Name(name)) => Bind(name) }
}

case class Symbol(name: String) extends Expression

object Symbol {
  def parse(in: Parsing[_]): Parsing[Symbol] = in.popName().map { case Name(name) => Symbol(name) }
}

case class DotExpression(left: Expression, right: Expression) extends Expression {

  private def exprs: Seq[Expression] = {
    def get(e: Expression) = e match {
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

object DotExpression {
  def parse(in: Parsing[_]): Parsing[DotExpression] = {
    in.mapTwo(_.mapTwo(Expression.parseNoDot, _.pop(Dot)), Expression.parse).map { case ((left, _), right) =>
      DotExpression(left, right).leftAssoc
    }
  }
}
