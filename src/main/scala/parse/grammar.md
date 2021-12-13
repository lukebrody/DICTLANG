`NewStruct = name:` New structural member (highest `match`)

`BindSymbol = name` Bind value to symbol (`match`)

`UseSymbol = name` Use a symbol (`value`)

`` MatchSymbol = `symbol`` Match value of symbol (`match`)

```
Key = StructKey | MatchKey
StructKey = name:
MatchKey  = Match =>
ValueDict = \{ (Key Value,)* \}
valueTerm = ValueDict | UseSymbol
Value = valueTerm (.valueTerm | \[Value\])*

MatchDictKey = name:
MatchDict = \{ (MatchDictKey: Match,)* \}
matchValue = valueTerm (.valueTerm | \[Value\])+
Match = MatchDict | MatchSymbol | matchValue | BindSymbol
```