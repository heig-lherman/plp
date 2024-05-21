# The Z Programming Language

## Syntax

The formal syntax of Z is described using Backus-Naur Form (BNF) as follows:

```bnf
<program>           ::= <include>* <function>*

<include>           ::= "#include" "<" <header> ">"

<header>            ::= <identifier> ".h"

<function>          ::= <type> <identifier> "(" <parameters> ")" <block>

<parameters>        ::= (<parameter> ("," <parameter>)*)?

<parameter>         ::= <type> <identifier>

<type>              ::= <primitive> "*"*

<primitive>         ::= "int"
                      | "char"
                      | "void"

<statement>         ::= <if>
                      | <while>
                      | <return>
                      | <block>
                      | <empty>
                      | <expr>

<if>                ::= "if" "(" <expression> ")" <statement> ("else" <statement>)?

<while>             ::= "while" "(" <expression> ")" <statement>

<return>            ::= "return" <expression>? ";"

<block>             ::= "{" <var>* <statement>* "}"

<var>               ::= <type> <identifier> ";"

<empty>             ::= ";"

<expr>              ::= <expression> ";"

<expression>        ::= <literal>
                      | <unary>
                      | <binary>
                      | <sizeof>
                      | <address>
                      | <identifier>
                      | <call>
                      | <assignment>
                      | "(" <expression> ")"

<unary>             ::= <unary-op> <expression>

<unary-op>          ::= "+" | "-" | "!" | "*"

<binary>            ::= <expression> <binary-op> <expression>

<binary-op>         ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "!=" | "&&" | "||"

<sizeof>            ::= "sizeof" <sizeof-arg>

<sizeof-arg>        ::= "(" <type> ")" | <expression>

<address>           ::= "&" <identifier>

<call>              ::= <identifier> "(" <arguments> ")"

<arguments>         ::= (<expression> ("," <expression>)*)?

<assignment>        ::= <identifier> "=" <expression>

<identifier>        ::= <letter> (<letter> | <digit>)*

<literal>           ::= <integer> | <character>

<integer>           ::= <digit>+

<letter>            ::= [a-zA-Z_]

<digit>             ::= [0-9]

<character>         ::= "'" . "'"
```

## Standard library

The standard library of the Z programming language consists of the following headers, each containing its respective set of functions:

#### stdlib.h

1. `void abort();`
1. `int abs(int n);`
1. `void* calloc(int num, int size);`
1. `void exit(int status);`
1. `void free(void* ptr);`
1. `void* malloc(int size);`
1. `void* realloc(void* ptr, int size);`

#### stdio.h

1. `int getchar();`
1. `char* gets(char* str);`
1. `int putchar(int c);`
1. `int puts(char * str);`

#### string.h

1. `void* memcpy(void* dest, void* src, int n);`
1. `void* memmove(void* dest, void* src, int n);`
1. `void* memchr(void* s, int c, int n);`
1. `char* strcpy(char* dest, char* src);`
1. `char* strncpy(char* dest, char* src, int n);`
1. `char* strcat(char* dest, char* src);`
1. `char* strncat(char* dest, char* src, int n);`
1. `int strcmp(char* s1, char* s2);`
1. `int strncmp(char* s1, char* s2, int n);`
1. `char* strchr(char* s, int c);`
1. `char* strrchr(char* s, int c);`
1. `char* strstr(char* s1, char* s2);`
1. `int strlen(char* s)},;`

## Intermediate representation

The intermediate representation ZCC uses to represent Z code is described using TypeScript syntax as follows:

```typescript
interface Node {
    loc: Location;
}

interface Program extends Node {
    kind: 'Program';
    includes: Include[];
    functions: Function[];
}

interface Include extends Node {
    kind: 'Include';
    header: Header;
}

interface Header extends Node {
    kind: 'Header';
    name: string;
}

interface Function extends Node {
    kind: 'Function';
    returnType: Type;
    id: Identifier;
    params: Parameter[];
    body: BlockStatement;
}

interface Parameter extends Node {
    kind: 'Parameter';
    type: Type;
    id: Identifier;
}

interface Type extends Node {
    kind: 'Type';
    name: 'int' | 'char' | 'void' | 'pointer'; // 'pointer' means 'int*', 'char**', 'void***', ...
}

interface Variable extends Node {
    kind: 'Variable';
    type: Type;
    id: Identifier;
}

type Statement
    = IfStatement
    | WhileStatement
    | ReturnStatement
    | BlockStatement
    | EmptyStatement
    | ExpressionStatement;

interface IfStatement extends Node {
    kind: 'IfStatement';
    test: Expression;
    consequent: Statement;
    alternate?: Statement; // '?' means optional
}

interface WhileStatement extends Node {
    kind: 'WhileStatement';
    test: Expression;
    body: Statement;
}

interface ReturnStatement extends Node {
    kind: 'ReturnStatement';
    expr?: Expression;
}

interface BlockStatement extends Node {
    kind: 'BlockStatement';
    declarations: Variable[];
    statements: Statement[];
}

interface EmptyStatement extends Node {
    kind: 'EmptyStatement';
}

interface ExpressionStatement extends Node {
    kind: 'ExpressionStatement';
    expr: Expression;
}

type Expression
    = Literal
    | UnaryExpression
    | BinaryExpression
    | SizeofExpression
    | CallExpression
    | AssignmentExpression
    | Identifier

interface Literal extends Node {
    kind: 'Literal';
    value: number | string; // JavaScript doesn't distinguish strings from characters
}

interface UnaryExpression extends Node {
    kind: 'UnaryExpression';
    op: '+' | '-' | '!' | '*' | '&';
    expr: Expression;
}

interface SizeofExpression extends Node {
    kind: 'SizeofExpression';
    arg: Expression | Type;
}

interface BinaryExpression extends Node {
    kind: 'BinaryExpression';
    left: Expression;
    op: '+' | '-' | '*' | '/' | '>' | '<' | '==' | '!=' | '&&' | '||';
    right: Expression;
}

interface CallExpression extends Node {
    kind: 'CallExpression';
    callee: Identifier;
    args: Expression[];
}

interface AssignmentExpression extends Node {
    kind: 'AssignmentExpression';
    left: Identifier;
    right: Expression;
}

interface Identifier extends Node {
    kind: 'Identifier';
    name: string
}

interface Location {
    start: {
        line: number;
        column: number;
    };
    end: {
        line: number;
        column: number;
    }
}
```

In this assignment, you will work with JavaScript objects and use the `kind` property to identify the node type. For example, here is a function extracting the name of a node if it denotes an identifier.

```javascript
function getIdentifierName(node) {
    if (node.kind === 'Identifier') {
        return node.name;
    }
    throw new Error('Not an identifier');
}
```
