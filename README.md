#  DICTLANG

- Dicts are instantiated in-order, atomically (per level), lazily
- Each dictionary is a unique object (destructuring ignores this)
- ' defines a unique new key (cannot conflict with existing key)
- Keys that start with capital letters are exported
- \` binds in pattern matching
- We should be able to do structural typing on everything here
- No shadowing/redifinition/mutability

Builtins
`Macro "` parses a linked list of appropriate symbols
```
'Head: {},
'Next: {},
'Nil: {},
```

## Examples

Functional
```
'Functional: {
    'Acc: {},
    'Fold: {
        {List: Nil, Acc: `a}: a
        {List: {Head: `h, Next: `n}, Acc: `a, Fn: `f}: Fold.{ List: n, Acc: f.{Acc: a, Head: h}, Fn: f }
    },
},
```

Bits, bitstrings
```
{
    '0: {},
    '1: {},
    'A: {},
    'B: {},
    'lo: {},
    'hi: {},
    
    'Macro: {
        `nums: Functional.Fold.{
            List: nums, 
            Acc: Nil,
            Fn: {
                // Can't just pass Head, otherwise type is unbound
                {Acc: `h, Head: 0}: { hi: h, lo: 0 },
                {Acc: `h, Head: 1}: { hi: h, lo: 1 },
            },
        },
    },
    
    _ = Macro "1001" // { hi: { hi: { hi: { hi: Nil, lo: 1 }, lo: 0 }, lo: 0}, lo: 1}
    
    'And: {
        {A: 0, B: 0}: 0,
        {A: 0, B: 1}: 0,
        {A: 1, B: 0}: 0,
        {A: 1, B: 1}: 1,
    },
    
    _ = And.{A: 0, B: 1} // 0
    
    'Xor: {
        {A: 0, B: 0}: 0,
        {A: 0, B: 1}: 1,
        {A: 1, B: 0}: 1,
        {A: 1, B: 1}: 0,
    },
    
    _ = Xor.{A: 0, B: 1} // 1
    
    'Or: {
        {A: 0, B: 0}: 0,
        {A: 0, B: 1}: 1,
        {A: 1, B: 0}: 1,
        {A: 1, B: 1}: 1,
    },
    
    'sum: {},
    'carry: {},
    'halfAdder: {
        `value: { sum: Xor.value, carry: And.value },
    },
    
    'fullAdder: {
        {A: `a, B: `b, carry: `c}: {
            lo: halfAdder.{A: a, B: b},
            hi: halfAdder.{A: low.sum, B: c},
            sum: high.sum,
            carry: Or.{A: low.carry, B: high.carry},
        }
    },
    
    'num: {},
    'add: {
        {A: { hi: `ha, lo: `la }, B: { hi: `hb, lo: `lb }, carry: `c}: {
            lores: fullAdder.{A: la, B: lb, carry: c},
            hires: add.{A: ha, B: hb, carry: lores.carry},
            num: {
                lo: lowres.sum,
                hi: hires.num,
            },
            carry: hires.carry,
        },
        {A: `a, B: Nil}: {num: a, carry: 0},
        {A: Nil, B: `b}: {num: b, carry: 0},
        {A: Nil, B: Nil}: {num: Nil, carry: 0},
    },
    
    'addOne: {
        {hi: Nil, lo: `l}: {hi: {hi: Nil, lo: 1}, lo: l},
        {hi: `h, lo: `l}: {hi: addOne.h, lo: l},
    },
    
    'Add {
        {A: `a, B: `b}: {
            {num: `n, carry: 0}: n,
            {num: `n, carry: 1}: {
        }.add.{a, b, 0},
    },
},
```

