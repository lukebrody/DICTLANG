```
Value = valueTerm (.valueTerm)*
valueTerm = ValueDict | UseSymbol
UseSymbol = name
ValueDict = { (Key: Value,)* }
Key = Define | MatchDict
Define = name

MatchDict = [ (Extract -> Match,)* ]
Match = MatchDict | Bind
Bind = name
Extract = name
```