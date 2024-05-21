# ZCC

The purpose of this assignment is to provide you with hands-on experience in semantic analysis, with a specific focus on scope analysis and type checking. These two crucial phases play a pivotal role in the compilation process, as they ensure that a program adheres to the defined rules and constraints of the programming language.

## Topic

The topic of the assignment is [Z](lang/README.md), a statically typed imperative programming language. Z is meticulously designed as a subset of the C programming language, sharing both syntax and semantics, including core elements such as scoping and typing rules. This means that any code adhering to the syntax and semantics of Z is inherently valid in C as well.

The Z language, as a subset of C, features the following:

1. **Program Structure**: A Z program consists of optional include statements (`#include`), followed by function declarations.
1. **Include Directive**: The include directive exclusively considers headers from the standard library, exposing standard functions in the global scope.
1. **Function Declaration**: Functions are declared with a return type, an identifier, parameters enclosed in parentheses, and a code block.
1. **Primitive Types**: Z supports primitive types such as `int`, `char`, and `void`. Pointer types are denoted by appending asterisks.
1. **Statements**: The language includes conditional statements, `while` loops, return statements, code blocks (`{}`), and expression statements.
1. **Variable Declarations**: Variables are declared within code blocks using a type and an identifier followed by a semicolon.
1. **Expressions**: Expressions include literals (integers and characters), the `sizeof` operator, address-of operator (`&`), dereference operator (`*`), identifiers, function calls, and assignments.

Essentially, Z is a procedural programming language that relies on functions as its fundamental constructs for code structuring. It provides basic control flow features, permits variable mutation and side effects, and, allows for low-level programming through memory manipulation.

The language is accompanied by a [standard library](lang/README.md#standard-library), also a subset of the C standard library. It incorporates features for explicit memory management, including functions like `malloc` and `free`, as well as operations for input and output on the standard streams. Additionally, it provides an API for manipulating C-like strings.

## Task

The assignment requires implementing a type-checker for Z programs. To this end, you will work on a Node.js command-line application, ZCC. The provided codebase extracts command-line arguments, tokenizes and parses Z source files, and reports any encountered lexical or syntactical errors. All you need to do is implement [type-checking](src/typecheck.mjs) support:

```javascript
// typecheck.mjs

/**
 * Type-check a Z program
 * @param {Object} ast The AST to type-check
 * @param {Reporter} reporter The reporter
 */
function typecheck(ast, reporter) {
  /* ... */
}
```

You must identify and implement the necessary semantic checks to perform on Z code. The number and exact nature of the checks to be carried out is not provided. It is your responsibility to determine potential semantic issues that may arise in Z code and should be detected before program execution. Take inspiration from the C programming language.

For example, type-checking the following Z program:

```cpp
void f(int n) {
  *n;
}
```

should give:

```
file.z:2:3: error: Cannot dereference expression of type 'int'
  *n;
  ^
```

The type-checking process consists of scanning the program's abstract syntax tree for violations of scoping and typing rules. The reference for the Z programming language details the complete [intermediate representation](lang/README.md#intermediate-representation) used by ZCC. Each term of the language is represented as a JavaScript tagged object and includes term-specific properties along with location information.

For instance, this Z code snippet:

```c
return 42;
```

translates to the following intermediate representation:

```javascript
{
  "kind": "ReturnStatement",
  "expr": {
    "kind": "Literal",
    "value": 42,
    "loc": {
      // ...
    }
  },
  "loc": {
    // ...
  }
}
```

This JavaScript object captures the essential information about a return statement:

- `"kind"`: `"ReturnStatement"` indicates that this is a return statement.
- `"expr"` represents the expression being returned.
  - `"kind"`: `"Literal"` specifies that the expression is a literal.
  - `"value"`: `42` provides the value of the literal, which is 42.
  - `"loc"` represents the location information, but it's left as a placeholder here.

The type-checker should collect semantic issues while examining Z code, identifying both clear scoping and typing errors as well as less severe warnings. It doesn't stop at the first problem but continues to find all potential issues. After completing the type-checking process, use the provided [reporting](src/report.mjs) utility to inform of all identified problems, ensuring meaningful messages with relevant locations.

## How-to

Begin by installing Node.js for this project. Node.js allows you to run JavaScript outside of web browsers. Visit https://nodejs.org/, download the LTS version, and follow the installation instructions.

Next, install the project dependencies from the `src` directory in the terminal:

```
% npm install
```

To run the program, use the command `node zcc.mjs`:

```
% node zcc.mjs -h
Usage: zcc [options] <filename>

A type-checker for the Z language

Arguments:
  filename       The source file to type-check

Options:
  --show-tokens  Show the tokens (default: false)
  --show-ast     Show the parsed AST (default: false)
  -h, --help     display help for command
```

Given your experience in Java, C, and C++, JavaScript programming is straightforward as its syntax is inspired by Java. For a quick syntax overview, visit [Learn JavaScript in Y Minutes](https://learnxinyminutes.com/docs/javascript/). Alternatively, for a more formal introduction, consult [MDN's guide on JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide).

> You are strictly prohibited from installing additional npm dependencies; exclusively use the JavaScript standard library, and if necessary, implement any functionalities not provided from scratch.

## Grading

The grading of the assignment will be based on the following criteria:

- **Correctness**: The code should compile without errors and perform the task as per the assignment's requirements. It should handle all edge cases and should not crash under any circumstances.
- **Efficiency**: The code should be optimized to run as efficiently as possible. This includes minimizing the time and space complexity of the code. It should not contain unnecessary computation or redundant code.
- **Readability**: The code should be easy to read and understand. This includes proper indentation, the use of meaningful variable and function names, and the inclusion of comments to explain complex parts of the code.
- **Modularity**: The code should be well-structured and divided into functions and modules, each performing a single task. This makes the code easier to read, understand, and maintain.
- **Style**: The code should demonstrate an idiomatic procedural programming style, employing suitable programming constructs to ensure the code is concise, modular, and easy to reason about.

Each criterion contributes a maximum of one point to the overall grade. The assignment grade is then computed using the following formula:

<p align="center">
$G = 1 + C_x + E_x + R_x + M_x + S_x$
</p>

where $C_x$, $E_x$, $R_x$, $M_x$, and $S_x$ represent the evaluation of your code based on each respective criterion.

## Deliverable

The deliverable for this assignment is a Node.js project, with a main file named `zcc.mjs`. You may structure the program across as many files as necessary. Each file should include a header that identifies the author(s) of the code. The assignment can be completed individually or in pairs. However, it is crucial to ensure that the work submitted is original and not plagiarized. Copying code from the internet or using solutions from other students is strictly prohibited and will be considered a breach of academic integrity.

## Deadline

The deadline for the assignment is **Tuesday, May 21, 2024, at 12:00**. Once this deadline passes, the repository will be locked for grading purposes, and late submissions will not be accepted under any circumstances.
