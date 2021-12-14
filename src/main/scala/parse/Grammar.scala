package parse

import tokenize.*

sealed trait Value
object Value {

  private def term(in: Parsing[_]): Parsing[Value] = UseSymbol.parse(in) orElse ValueDict.parse(in)

  private def dotAfter(left: Parsing[Value]): Parsing[DotExpression | SubscriptExpression] = {
    left.flatMap { l =>
      left
        .mapTwo(
          _.mapTwo(_.pop(Dot), term).map { case (_, right) => DotExpression(l, right) },
          dotExpr => dotAfter(dotExpr) orElse dotExpr
        )
        .map(_._2)
    }
  }

  def parse(in: Parsing[_]): Parsing[Value] = {
    dotAfter(term(in)) orElse term(in)
  }
}

case class DotExpression(left: Value, right: Value) extends Value
case class SubscriptExpression(left: Value, right: Value) extends Value

case class UseSymbol(name: String) extends Value
object UseSymbol {
  def parse(in: Parsing[_]): Parsing[UseSymbol] = in.popName().map { case Name(name) => UseSymbol(name) }
}

case class ValueDict(entries: Seq[Dict.Entry[Match, Value]]) extends Value
object ValueDict {
  def parse(in: Parsing[_]): Parsing[ValueDict] = {
    Dict.parse[Match, Value](in, Match.parse, Value.parse, OpenBracket, CloseBracket, Colon).map(ValueDict(_))
  }
}

sealed trait Key
object Key {
  def parse(in: Parsing[_]): Parsing[Key] = ???
}

case class Define(name: String) extends Key
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

case class MatchDict(entries: Seq[Dict.Entry[Extract, Match]]) extends Match with Key
object MatchDict {
  def parse(in: Parsing[_]): Parsing[MatchDict] = {
    Dict
      .parse[Extract, Match](in, Extract.parse, Match.parse, OpenSquareBracket, CloseSquareBracket, Arrow)
      .map(MatchDict(_))
  }
}

case class Extract(name: String) extends Match
object Extract {
  def parse(in: Parsing[_]): Parsing[Extract] =
    in.popName().map { case Name(name) => Extract(name) }
}
