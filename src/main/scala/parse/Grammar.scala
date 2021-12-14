package parse

import tokenize.*

sealed trait Value
object Value {

  private def dotAfter(left: Parsing[Value]): Parsing[DotExpression | SubscriptExpression] = {
    left.flatMap { l =>
      left
        .mapTwo(
          _.mapTwo(_.pop(Dot), Access.parse).map { case (_, right) => DotExpression(l, right) },
          dotExpr => dotAfter(dotExpr) orElse dotExpr
        )
        .map(_._2)
    }
  }

  private def subscriptAfter(left: Parsing[Value]): Parsing[DotExpression | SubscriptExpression] = {
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
    val term = Reference.parse(in) orElse ValueDict.parse(in)
    dotAfter(term) orElse subscriptAfter(term) orElse term
  }
}

case class DotExpression(left: Value, right: Access) extends Value
case class SubscriptExpression(left: Value, right: Value) extends Value

case class Reference(name: String) extends Value
object Reference {
  def parse(in: Parsing[_]): Parsing[Reference] = in.popName().map { case Name(name) => Reference(name) }
}

case class Access(name: String)
object Access {
  def parse(in: Parsing[_]): Parsing[Access] = in.popName().map { case Name(name) => Access(name) }
}

case class ValueDict(entries: Seq[ValueDict.Entry]) extends Value
object ValueDict {

  sealed trait Entry

  case class DefineEntry(define: Define, value: Value) extends Entry
  object DefineEntry {
    def parse(in: Parsing[_]): Parsing[DefineEntry] = in.mapTwo(Define.parse, _.mapTwo(_.pop(Colon), Value.parse)).map {
      case (define, (_, value)) => DefineEntry(define, value)
    }
  }

  case class MatchEntry(pattern: MatchDict, value: Value) extends Entry
  object MatchEntry {
    def parse(in: Parsing[_]): Parsing[MatchEntry] =
      in.mapTwo(MatchDict.parse, _.mapTwo(_.pop(DoubleArrow), Value.parse)).map { case (pattern, (_, value)) =>
        MatchEntry(pattern, value)
      }
  }

  def parse(in: Parsing[_]): Parsing[ValueDict] = {
    Dict
      .parse[Entry](in, OpenBracket, CloseBracket) { rowIn =>
        DefineEntry.parse(rowIn) orElse MatchEntry.parse(rowIn)
      }
      .map(ValueDict(_))
  }
}

case class Define(name: String)
object Define {
  def parse(in: Parsing[_]): Parsing[Define] = in.popName().map { case Name(name) => Define(name) }
}

sealed trait Match
object Match {
  def parse(in: Parsing[_]): Parsing[Match] = {
    MatchDict.parse(in) orElse Bind.parse(in)
  }
}

case class Bind(name: String) extends Match
object Bind {
  def parse(in: Parsing[_]): Parsing[Bind] =
    in.popName().map { case Name(name) =>
      Bind(name)
    }
}

case class MatchDict(entries: Seq[MatchDict.Entry]) extends Match
object MatchDict {

  case class Entry(left: Extract, right: Match)

  def parse(in: Parsing[_]): Parsing[MatchDict] = {
    Dict
      .parse[Entry](in, OpenSquareBracket, CloseSquareBracket) { rowIn =>
        rowIn.mapTwo(Extract.parse, _.mapTwo(_.pop(SingleArrow), Match.parse)).map { case (left, (_, right)) =>
          Entry(left, right)
        }
      }
      .map(MatchDict(_))
  }
}

case class Extract(name: String)
object Extract {
  def parse(in: Parsing[_]): Parsing[Extract] =
    in.popName().map { case Name(name) => Extract(name) }
}
