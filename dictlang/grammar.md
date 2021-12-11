```
dict = { match : dict } | useSymbol
match = declareSymbol | bindSymbol | useSymbol | { match : match }
dotExpr = dict . dict
```

Symbol declarations can never match