package parse

import tokenize.*

def parseValue(in: Parsing[_]): Parsing[grammar.Value] = ParsingImplementation.Value.parse(in)

private object ParsingImplementation {

  import grammar._

  object Value {
    private def dotAfter(left: Parsing[Value]): Parsing[DotExpression | SubscriptExpression] = {
      left.flatMap { l =>
        left
          .mapTwo(
            _.mapTwo(_.pop(Dot), AccessParsing.parse).map { case (_, right) => DotExpression(l, right, ()) },
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
              SubscriptExpression(l, right, ())
            },
            subscriptExpr => dotAfter(subscriptExpr) orElse subscriptAfter(subscriptExpr) orElse subscriptExpr
          )
          .map(_._2)
      }
    }

    def parse(in: Parsing[_]): Parsing[Value] = {
      val term = ReferenceParsing.parse(in) orElse ValueDictParsing.parse(in)
      dotAfter(term) orElse subscriptAfter(term) orElse term
    }
  }

  object ReferenceParsing {
    def parse(in: Parsing[_]): Parsing[Reference] = in.popName().map { case Name(name) => Reference(name, ()) }
  }

  object AccessParsing {
    def parse(in: Parsing[_]): Parsing[Access] = in.popName().map { case Name(name) => Access(name, ()) }
  }

  object ValueDictParsing {

    object DefineEntry {
      def parse(in: Parsing[_]): Parsing[ValueDict.DefineEntry] =
        in.mapTwo(DefineParsing.parse, _.mapTwo(_.pop(Colon), Value.parse)).map { case (define, (_, value)) =>
          ValueDict.DefineEntry(define, value)
        }
    }

    object MatchEntry {
      def parse(in: Parsing[_]): Parsing[ValueDict.MatchEntry] =
        in.mapTwo(MatchDictParsing.parse, _.mapTwo(_.pop(DoubleArrow), Value.parse)).map { case (pattern, (_, value)) =>
          ValueDict.MatchEntry(pattern, value)
        }
    }

    def parse(in: Parsing[_]): Parsing[ValueDict] = {
      Dict
        .parse[ValueDict.Entry](in, OpenBracket, CloseBracket) { rowIn =>
          DefineEntry.parse(rowIn) orElse MatchEntry.parse(rowIn)
        }
        .map(ValueDict(_, ()))
    }
  }

  object DefineParsing {
    def parse(in: Parsing[_]): Parsing[Define] = in.popName().map { case Name(name) => Define(name, ()) }
  }

  object Match {
    def parse(in: Parsing[_]): Parsing[Match] = {
      MatchDictParsing.parse(in) orElse BindParsing.parse(in)
    }
  }

  object BindParsing {
    def parse(in: Parsing[_]): Parsing[Bind] =
      in.popName().map { case Name(name) =>
        Bind(name, ())
      }
  }

  object MatchDictParsing {
    def parse(in: Parsing[_]): Parsing[MatchDict] = {
      Dict
        .parse[MatchDict.Entry](in, OpenSquareBracket, CloseSquareBracket) { rowIn =>
          rowIn.mapTwo(ExtractParsing.parse, _.mapTwo(_.pop(SingleArrow), Match.parse)).map { case (left, (_, right)) =>
            MatchDict.Entry(left, right)
          }
        }
        .map(MatchDict(_, ()))
    }
  }

  object ExtractParsing {
    def parse(in: Parsing[_]): Parsing[Extract] =
      in.popName().map { case Name(name) => Extract(name, ()) }
  }
}
