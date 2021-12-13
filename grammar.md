```
valueDict = { match : value }
valueTerm = valueDict | useSymbol
value = valueTerm then .? -> value
dotExpr = valueTerm (. valueTerm)+
subscriptExpr = valueTerm 

matchDict = { match : match }
match = matchDict | bindSymbol | useSymbol | dotExpr | subscriptExpr

V -> V.V | V[V] | S | D

V -> S V' | D V'
V' -> .V V' | [V] V' | 𝜖

a.b.c.d

(A, (B, (C, D)))
(((A, B), C), D)
postorder

/\
A /\
  B /\
    C D
    
  /\
 /\ D
/\ C
A B 

a.b[c].d

/\
a /\
 /\ d
 b c

```

Symbol declarations can never match