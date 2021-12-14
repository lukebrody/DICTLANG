package interpret

import parse.grammar

sealed trait Value {
  def access(name: String): Value
  def evaluate(value: Value): Value
}

case class Symbols(storage: Map[String, Value]) {
  def apply(name: String): Value = storage(name)
}

object Symbols {
  val empty = Symbols(Map.empty)
}

def interpret(ast: grammar.Value, symbols: Symbols): Value = ast match {
  case grammar.Reference(name, _)                               => symbols(name)
  case grammar.DotExpression(value, grammar.Access(name, _), _) => interpret(value, symbols).access(name)
  case grammar.SubscriptExpression(target, argument, _) =>
    interpret(target, symbols).evaluate(interpret(argument, symbols))
}
