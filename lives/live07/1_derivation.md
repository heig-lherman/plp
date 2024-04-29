Given

```
<expr> ::= <term> { '||' {term} }
<term> ::= <bool> { '&&' <bool> }
<bool> ::= 'True' | 'False'
```

Develop a derivation for the expression `True && False || True`.

```
       <bool> -----    <bool> ------    <bool> ------
              True            False            True
<term> -----------------------------    <term> ------
       True && False                           True
<expr> ----------------------------------------------
       True && False || True
```
