package define

import parse.*

import scala.util.Try

//def insertDefinesValue(ast: Value): Value = {
//  ast match {
//    case ValueDict(entries) =>
//      ValueDict(entries.map { case Dict.Entry(key, value) =>
//        Dict.Entry[Match, Value](
//          key match {
//            case UseSymbol(name) => Define(name)
//            case key             => insertDefines(key)
//          },
//          insertDefinesValue(value)
//        )
//      })
//    case DotExpression(left, right)       => DotExpression(insertDefinesValue(left), insertDefinesValue(right))
//    case SubscriptExpression(left, right) => SubscriptExpression(insertDefinesValue(left), insertDefinesValue(right))
//    case UseSymbol(name)                  => UseSymbol(name)
//  }
//}
//
//def insertDefines(ast: Match): Match = {
//  ast match {
//    case value: Value => insertDefinesValue(value)
//    case _: Define    => ???
//    case bind: Bind   => bind
//    case MatchDict(entries) =>
//      MatchDict(entries.map { case Dict.Entry(left, right) =>
//        Dict.Entry[Extract, Match](left, insertDefines(right))
//      })
//    case _: Extract => ???
//  }
//}

//def checkSymbols(ast: Match, symbols: Set[String]): Try[Unit] = {
//  ast match {
//    case class parse.UseSymbol(name)
//  }
//}
