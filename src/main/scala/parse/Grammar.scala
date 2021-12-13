package parse

import tokenize.*

sealed trait Value extends Match
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

case class DotExpression(left: Value, right: Value) extends Value with Match
case class SubscriptExpression(left: Value, right: Value) extends Value with Match

case class UseSymbol(name: String) extends Value
object UseSymbol {
  def parse(in: Parsing[_]): Parsing[UseSymbol] = in.popName().map { case Name(name) => UseSymbol(name) }
}

case class ValueDict(entries: Seq[Dict.Entry[Match, Value]]) extends Value
object ValueDict {
  def parse(in: Parsing[_]): Parsing[ValueDict] = {
    Dict.parse[Match, Value](in, Match.parse, Value.parse, OpenBracket, CloseBracket).map(ValueDict(_))
  }
}

sealed trait Match
object Match {
  def parse(in: Parsing[_]): Parsing[Match] = {
    MatchDict.parse(in) orElse Bind.parse(in) orElse Value.parse(in)
  }
}

case class Bind(name: String) extends Match
object Bind {
  def parse(in: Parsing[_]): Parsing[Bind] =
    in.mapTwo(_.mapTwo(_.pop(Backtick), _.popName()), _.pop(Backtick)).map { case ((_, Name(name)), _) =>
      Bind(name)
    }
}

case class MatchDict(entries: Seq[Dict.Entry[MatchSymbol, Match]]) extends Match
object MatchDict {
  def parse(in: Parsing[_]): Parsing[MatchDict] = {
    Dict
      .parse[MatchSymbol, Match](in, MatchSymbol.parse, Match.parse, OpenSquareBracket, CloseSquareBracket)
      .map(MatchDict(_))
  }
}

case class MatchSymbol(name: String) extends Match
object MatchSymbol {
  def parse(in: Parsing[_]): Parsing[MatchSymbol] =
    in.popName().map { case Name(name) => MatchSymbol(name) }
}
