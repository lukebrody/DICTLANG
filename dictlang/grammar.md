```
value = { match : value } | useSymbol | dotExpr | subscriptExpr
match = bindSymbol | value | { match : match }
dotExpr = value\-dotExpr.value\-dotExpr\+
subscriptExpr = value-subscriptExpr[value]\+
```

Symbol declarations can never match