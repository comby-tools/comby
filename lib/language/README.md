# Rule language grammar


```ebnf
grammar ::= "where" sequence

sequence ::= expression ("," expression)*

expression ::= 
| bool
| atom "==" atom 
| atom "!=" atom
| "rewrite" atom "{" qstring "->" atom "}"
| "match" atom "{" ("|" branch )+ "}"

bool ::= "true" | "false"
atom ::= hole | qstring
qstring ::= '"' quoted-string '"'
hole ::= ":[" hole-identifier "]"
branch ::= qstring "->" sequence
```

See the page for [`hole-identifier`](https://comby.dev/docs/syntax-reference) syntax if it matters.
