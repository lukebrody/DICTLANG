package parse

import tokenize.*

sealed trait Key
object Key {
  def parse(in: Parsing[_]): Parsing[Key] = NewStruct.parse(in) orElse Match.parse(in)
}

case class NewStruct(name: String) extends Key
object NewStruct {
  def parse(in: Parsing[_]): Parsing[NewStruct] =
    in.mapTwo(_.pop(Dot), _.popName()).map { case (_, Name(name)) => NewStruct(name) }
}

sealed trait Match extends Key
object Match {
  def parse(in: Parsing[_]): Parsing[Match] = {
    MatchDict.parse(in) orElse MatchSymbol.parse(in) orElse Value.parseMatch(in) orElse BindSymbol.parse(in)
  }
}

case class MatchDict(entries: Seq[Dict.Entry[Match, Match]]) extends Match
object MatchDict {
  def parse(in: Parsing[_]): Parsing[MatchDict] = {
    Dict.parse[Match, Match](in, Match.parse, Match.parse).map(MatchDict(_))
  }
}

case class MatchSymbol(name: String) extends Match
object MatchSymbol {
  def parse(in: Parsing[_]): Parsing[MatchSymbol] =
    in.mapTwo(_.pop(Backtick), _.popName()).map { case (_, Name(name)) => MatchSymbol(name) }
}

case class BindSymbol(name: String) extends Match
object BindSymbol {
  def parse(in: Parsing[_]): Parsing[BindSymbol] =
    in.popName().map { case Name(name) =>
      BindSymbol(name)
    }
}

sealed trait Value
object Value {

  private def term(in: Parsing[_]): Parsing[Value] = UseSymbol.parse(in) orElse ValueDict.parse(in)

  private def dotAfter(left: Parsing[Value]): Parsing[DotExpression | SubscriptExpression] = {
    left.flatMap { l =>
      left
        .mapTwo(
          _.mapTwo(_.pop(Dot), term).map { case (_, right) => DotExpression(l, right) },
          dotExpr => dotAfter(dotExpr) orElse subscriptAfter(dotExpr) orElse dotExpr
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
    dotAfter(term(in)) orElse subscriptAfter(term(in)) orElse term(in)
  }

  def parseMatch(in: Parsing[_]): Parsing[Match] = dotAfter(term(in)) orElse subscriptAfter(term(in))

}

case class UseSymbol(name: String) extends Value
object UseSymbol {
  def parse(in: Parsing[_]): Parsing[UseSymbol] = in.popName().map { case Name(name) => UseSymbol(name) }
}

case class ValueDict(entries: Seq[Dict.Entry[Key, Value]]) extends Value
object ValueDict {
  def parse(in: Parsing[_]): Parsing[ValueDict] = {
    Dict.parse[Key, Value](in, Key.parse, Value.parse).map(ValueDict(_))
  }
}

case class DotExpression(left: Value, right: Value) extends Value with Match
case class SubscriptExpression(left: Value, right: Value) extends Value with Match
