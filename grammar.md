```
valueDict = { match : value }
valueTerm = valueDict | useSymbol
value = valueTerm (.valueTerm | [value])*

matchDict = { match : match }
match = matchDict | bindSymbol | useSymbol | dotExpr | subscriptExpr
```