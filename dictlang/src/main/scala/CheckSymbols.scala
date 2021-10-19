package checkSymbols

import scala.util.{Failure, Success, Try}

case class Symbols(defintions: Set[String], binds: Set[String]) {
  def hasAny(name: String): Boolean = defintions.contains(name) || binds.contains(name)
  def withBind(name: String): Symbols = Symbols(defintions, binds + name)
  def withDefinition(name: String): Symbols = Symbols(defintions + name, binds)
}

def resolve(ast: parse.AST, symbols: Symbols): Try[Symbols] = {
  ast match {
    case parse.Definition(name) =>
      symbols.hasAny(name) match {
        case true  => Failure(new Exception(s"symbol $name already in use"))
        case false => Success(symbols.withDefinition(name))
      }
    case parse.Bind(name) =>
      symbols.hasAny(name) match {
        case true  => Failure(new Exception(s"symbol $name already in use"))
        case false => Success(symbols.withBind(name))
      }
    case parse.Symbol(name) =>
      symbols.defintions.contains(name) match {
        case true  => Success(symbols)
        case false => Failure(new Exception(s"symbol $name not found"))
      }
    case parse.DotExpression(left, right) => resolve(left, symbols).flatMap(resolve(right, _))
    case parse.Dict(entries) =>
      entries.foldLeft(Try(symbols)) {
        case (Failure(error), _)                              => Failure(error)
        case (Success(symbols), parse.Dict.Entry(key, value)) => ???
      }
  }
}
