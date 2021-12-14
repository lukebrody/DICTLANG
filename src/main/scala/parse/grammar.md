```
Value = ValueDict | Reference (.Access | [Value])*
Access = name
Reference = name
ValueDict = { (DefineRow | MatchRow),* }
DefineRow = Define: Value
MatchRow = MatchDict => Value
Define = name

MatchDict = [ (Extract -> Match,)* ]
Match = MatchDict | Bind
Bind = name
Extract = name
```