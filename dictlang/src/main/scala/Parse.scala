package parse

import tokenize._

sealed trait Parsing[+A] {
  def map[B](f: A => B): Parsing[B]
  def flatMap[B](f: A => Parsing[B]): Parsing[B]

  def orElse[B >: A](other: => Parsing[B]): Parsing[B]

  def pop[T <: Token](token: T): Parsing[T]
  def popName(): Parsing[Name]

  def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)]

  def isNot(matchType: Parsing.MatchType): Parsing[A]
  def canBe(matchType: Parsing.MatchType): Boolean
}

object Parsing {
  sealed trait MatchType
}

case class Success[+A](ast: A, rest: Seq[Token], prohibited: Set[Parsing.MatchType]) extends Parsing[A] {
  override def map[B](f: A => B): Parsing[B] = Success[B](f(ast), rest, prohibited)
  override def flatMap[B](f: A => Parsing[B]): Parsing[B] = f(ast)
  override def orElse[B >: A](other: => Parsing[B]): Parsing[B] = this

  override def pop[T <: Token](token: T): Parsing[T] = rest match {
    case Seq(`token`, rest @ _*) => Success(token, rest, Set.empty)
    case _                       => Failure
  }

  override def popName(): Parsing[Name] = rest match {
    case Seq(Name(name), rest @ _*) => Success(Name(name), rest, Set.empty)
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

  override def isNot(matchType: Parsing.MatchType): Parsing[A] = copy(prohibited = prohibited + matchType)
  override def canBe(matchType: Parsing.MatchType): Boolean = !prohibited.contains(matchType)
}

case object Failure extends Parsing[Nothing] {
  override def map[B](f: Nothing => B): Parsing[B] = Failure
  override def flatMap[B](f: Nothing => Parsing[B]): Parsing[B] = Failure
  override def orElse[B >: Nothing](other: => Parsing[B]): Parsing[B] = other

  override def pop[T <: Token](token: T): Parsing[T] = Failure
  override def popName(): Parsing[Name] = Failure

  override def mapTwo[A, B](a: Parsing[_] => Parsing[A], b: Parsing[_] => Parsing[B]): Parsing[(A, B)] = Failure

  override def isNot(matchType: Parsing.MatchType): Parsing[Nothing] = Failure
  override def canBe(matchType: Parsing.MatchType): Boolean = false
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

sealed trait Match

case class MatchDict(entries: Seq[Dict.Entry[Match, Match]]) extends Match

case class UseSymbol(name: String) extends Value
case class BindSymbol(name: String) extends Match

sealed trait Value extends Match

case class ValueDict(entries: Seq[Dict.Entry[Match, Value]]) extends Value

case class DotExpression(left: Value, right: Value) extends Value {

  private trait Expr

  sealed case class ValueExpr(value: Value)
  sealed case class DotExpres

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

case class SubscriptExpression(left: Value, right: Value) extends Value

object Value {
  def parse(in: Parsing[_]): Parsing[Value] = {
    {
      if (in.canBe(DotExpression)) {
        DotExpression.parse(in)
      } else {
        Failure
      }
    } orElse {
      if (in.canBe(SubscriptExpression)) {
        SubscriptExpression.parse(in)
      } else {
        Failure
      }
    } orElse {
      ValueDict.parse(in) orElse UseSymbol.parse(in)
    }
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
          case Failure                            => row
          case success: Success[Seq[Entry[L, R]]] => Success(r ++ success.ast, success.rest, success.prohibited)
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

object DotExpression extends Parsing.MatchType {
  def parse(in: Parsing[_]): Parsing[DotExpression] = {
    in.mapTwo(
      _.mapTwo(a => Value.parse(a.isNot(DotExpression)), _.pop(Dot)),
      Value.parse
    ).map { case ((left, _), right) => DotExpression(left, right) }
  }
}

object SubscriptExpression extends Parsing.MatchType {
  def parse(in: Parsing[_]): Parsing[SubscriptExpression] = {
    in.mapTwo(
      _.mapTwo(a => Value.parse(a.isNot(SubscriptExpression)), _.pop(OpenSquareBracket)),
      _.mapTwo(Value.parse, _.pop(CloseSquareBracket))
    ).map { case ((left, _), (right, _)) => SubscriptExpression(left, right) }
  }
}
