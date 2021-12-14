```
Value = valueTerm (.valueTerm)*
valueTerm = ValueDict | UseSymbol
UseSymbol = name
ValueDict = { (Match: Value,)* }

Match = Bind | MatchDict | Value
Bind = `name`
MatchDict = [ (MatchSymbol: Match,)* ]
MatchSymbol = name
```

```
[a: [b: `c`], b: c]
{
    a: b,
    b: a
}
```