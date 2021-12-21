package interpret

import parse.grammar

sealed trait Value {
  def access(name: String): Option[Value]
  def evaluate(value: Value): Option[Value]
  def reference(name: String): Option[Value]
}

case object Sentinel extends Value {
  override def access(name: String): Option[Value] = None
  override def evaluate(value: Value): Option[Value] = ???
  override def reference(name: String): Option[Value] = None
}

case class ValueDict(parent: Value, entries: Seq[ValueDict.Entry]) extends Value {

  def append(entry: ValueDict.Entry) = this.copy(entries = entries ++ Seq(entry))

  override def access(name: String): Option[Value] = entries.flatMap {
    case ValueDict.Definition(`name`, value) => Some(value)
    case _                                   => None
  }.headOption

  override def evaluate(value: Value): Value = ???

  override def reference(name: String): Option[Value] = access(name) orElse parent.reference(name)
}

object ValueDict {
  sealed trait Entry
  case class Definition(name: String, value: Value) extends Entry
  case class Function(parent: ValueDict, pattern: grammar.MatchDict, ) extends Entry with Value {}

  def empty(parent: Value) = ValueDict(parent, Seq.empty)
}

def interpret(ast: grammar.Value, parent: Value): Value = ast match {
  case grammar.Reference(name, _)                               => parent.reference(name).get
  case grammar.DotExpression(value, grammar.Access(name, _), _) => interpret(value, parent).access(name).get
  case grammar.SubscriptExpression(target, argument, _) =>
    interpret(target, parent).evaluate(interpret(argument, parent))
  case grammar.ValueDict(entries, _) => {
    entries.foldLeft(ValueDict.empty(parent)) {
      case (dict, grammar.ValueDict.DefineEntry(grammar.Define(name, _), value)) =>
        dict.append(ValueDict.Definition(name, interpret(value, dict)))
      case (dict, grammar.ValueDict.MatchEntry(pattern, contents)) => dict.append(ValueDict.Function(dict, ))
    }
  }
}
