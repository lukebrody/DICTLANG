//package define
//
//sealed trait Match {
//  def symbols: Set[String] = Set.empty
//}
//
//case class MatchDict(entires: Seq[(Match, Match)]) extends Match
//case class BindSymbol(name: String) extends Match
//case class Define
//
//sealed trait Value extends Match
//
//case class UseSymbol(name: String) extends Value
//case class
//
