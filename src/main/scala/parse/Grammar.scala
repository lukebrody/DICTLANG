package parse

class Grammar[A] {

  sealed class AST(info: A)

  sealed trait Value
  case class DotExpression(left: Value, right: Access, info: A) extends AST(info) with Value
  case class SubscriptExpression(left: Value, right: Value, info: A) extends AST(info) with Value

  case class Reference(name: String, info: A) extends AST(info) with Value
  case class Access(name: String, info: A) extends AST(info)

  case class ValueDict(entries: Seq[ValueDict.Entry], info: A) extends AST(info) with Value

  object ValueDict {
    sealed trait Entry
    case class DefineEntry(define: Define, value: Value) extends Entry
    case class MatchEntry(pattern: MatchDict, value: Value) extends Entry
  }

  case class Define(name: String, info: A) extends AST(info)

  sealed trait Match

  case class Bind(name: String, info: A) extends AST(info) with Match

  case class MatchDict(entries: Seq[MatchDict.Entry], info: A) extends AST(info) with Match
  object MatchDict {
    case class Entry(left: Extract, right: Match)
  }

  case class Extract(name: String, info: A) extends AST(info)
}

val grammar = new Grammar[Unit]
