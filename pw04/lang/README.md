# The Decaf Programming Language

The formal syntax of Decaf is described using Backus-Naur Form (BNF) as follows:

```
<program>           ::= <class>+

<class>             ::= "class" <identifier> "{" <member>* "}"

<member>            ::= <field>
                      | <method>
                      | <constructor>

<field>             ::= <type> <identifier> ("=" <expression>)? ";"

<method>            ::= <type> <identifier> "(" <parameter-list> ")" <block>

<constructor>       ::= <identifier> "(" <parameter-list> ")" <block>

<parameter-list>    ::= (<parameter ("," <parameter>)*)?

<parameter>         ::= <type> <identifier>

<type>              ::= "int" | "void" | <identifier>

<statement>         ::= <block>
                      | <if>
                      | <while>
                      | <return>
                      | <var>
                      | <expression> ";"

<block>             ::= "{" <statement>* "}"

<if>                ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?

<while>             ::= "while" "(" <expression> ")" <statement>

<return>            ::= "return" <expression> ";"

<var>               ::= "var" <identifier> "=" <expression> ";"

<argument-list>     ::= (<expression> ("," <expression>)*)?

<expression>        ::= <identifier>
                      | <number>
                      | <new>
                      | <access>
                      | <call>
                      | <assignment>

<new>               ::= "new" <identifier> "(" <argument-list> ")"

<access>            ::= <identifier> ("." <identifier>)+

<call>              ::= <access> "(" <argument-list> ")"

<assignment>        ::= <access> "=" <expression>

<identifier>        ::= <letter> (<letter> | <digit>)*

<letter>            ::= "a" | "b" | "c" | ... | "z" | "A" | "B" | "C" | ... | "Z"

<digit>             ::= "0" | "1" | "2" | ... | "9"

<number>            ::= <digit>+
```
