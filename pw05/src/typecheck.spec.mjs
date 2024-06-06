import test from 'ava';
import { inspect } from 'node:util';
import { typecheck } from './typecheck.mjs';
import { tokenize } from './tokenize.mjs';
import { parse } from './parse.mjs';

/**
 * Utility function to create a mock reporter.
 */
const createMockReporter = () => {
    return {
        errors: [],
        warnings: [],
        error(message, loc) {
            this.errors.push({ message, loc });
        },
        warning(message, loc) {
            this.warnings.push({ message, loc });
        },
    };
};

/**
 * Utility function to convert a code string to an AST.
 * @param {string} code - The code string to convert.
 * @returns {Program} - The AST of the code.
 */
const codeToAst = (code) => {
    const tokens = tokenize(code, createMockReporter());
    return parse(tokens, createMockReporter());
};

/**
 * Assert the presence of an error in the reporter.
 * @param {*} t - AVA test context.
 * @param {*} reporter - The reporter to check.
 * @param {string} message - The error message.
 * @param {number} line - The expected line number.
 * @param {number} column - The expected column number.
 */
const assertError = (t, reporter, message, line, column) => {
    t.true(reporter.errors.length >= 1);
    const found = reporter.errors.some(error => error.message === message && error.loc.start.line === line && error.loc.start.column === column);
    if (!found) {
        t.fail(`Expected error at ${line}:${column} not found: ${message}\nIn: ${inspect(reporter.errors, false, null, true)}`);
    } else {
        t.pass();
    }
};

/**
 * Assert the presence of a warning in the reporter.
 * @param {*} t - AVA test context.
 * @param {*} reporter - The reporter to check.
 * @param {string} message - The warning message.
 * @param {number} line - The expected line number.
 * @param {number} column - The expected column number.
 */
const assertWarning = (t, reporter, message, line, column) => {
    t.true(reporter.warnings.length >= 1);
    const found = reporter.warnings.some(warning => warning.message === message && warning.loc.start.line === line && warning.loc.start.column === column);
    if (!found) {
        t.fail(`Expected warning at ${line}:${column} not found: ${message}\nIn: ${inspect(reporter.warnings, false, null, true)}`);
    } else {
        t.pass();
    }
};

/**
 * Assert that the reporter is clean.
 * @param {*} t AVA test context
 * @param {*} reporter The reporter to check
 */
const assertClean = (t, reporter) => {
    if (reporter.errors.length > 0) {
        t.fail(`Unexpected errors: ${inspect(reporter.errors, false, null, true)}`);
    }

    if (reporter.warnings.length > 0) {
        t.fail(`Unexpected warnings: ${inspect(reporter.warnings, false, null, true)}`);
    }

    t.pass('No errors or warnings found');
};

test('should report shadowed variable warning', t => {
    const code = `
    int main() {
        int x;
        {
            int x;
        }
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Identifier \'x\' shadows a variable in an outer scope,', 5, 17);
    assertWarning(t, reporter, 'previous declaration here.', 3, 13);
});

test('should report redeclared variable', t => {
    const code = `
    int main() {
        int x;
        int x;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Identifier \'x\' is already declared in this scope,', 4, 13);
    assertError(t, reporter, 'previous declaration here.', 3, 13);
});

test('should report unreachable statement', t => {
    const code = `
    int main() {
        return 0;
        'a';
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Unreachable statement.', 4, 9);
});

test('should report implicit type promotion warning (lhs)', t => {
    const code = `
    int main() {
        char c;
        int x;
        c = 'a';
        x = c + 1;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 6, 13);
});

test('should report implicit type promotion warning (rhs)', t => {
    const code = `
    int main() {
        char c;
        int y;
        c = 'a';
        y = 1 + c;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 6, 17);
});

test('should report void value usage warning in unary expression', t => {
    const code = `
    void foo() {}
    int main() {
        int x;
        x = +foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Void value not ignored as it ought to be.', 5, 14);
});

test('should report void value usage warning in lhs of binary expression', t => {
    const code = `
    void foo() {}
    int main() {
        int x;
        x = foo() + 1;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Void value not ignored as it ought to be.', 5, 13);
});

test('should report void value usage warning in rhs of binary expression', t => {
    const code = `
    void foo() {}
    int main() {
        int x;
        x = 1 + foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Void value not ignored as it ought to be.', 5, 17);
});

test('should report void value usage warning in both lhs and rhs of binary expression', t => {
    const code = `
    void foo() {}
    void bar() {}
    int main() {
        int x;
        x = foo() + bar();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Void value not ignored as it ought to be.', 6, 13);
    assertError(t, reporter, 'Void value not ignored as it ought to be.', 6, 21);
});

test('should report void value usage warning in rhs of assignments', t => {
    const code = `
    void foo() {}
    int main() {
        int x;
        x = foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Void value not ignored as it ought to be.', 5, 13);
});

test('should report void variable', t => {
    const code = `
    int main() {
        void x;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Variable \'x\' declared void.', 3, 14);
});

test('should report void function parameter', t => {
    const code = `
    void foo(void x) {}
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Function parameter \'x\' declared void.', 2, 19);
});

test('should report unknown include header file', t => {
    const code = `
    #include <unknown.h>
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Unknown header file: unknown.h', 2, 15);
});

test('should report multiple inclusion of header', t => {
    const code = `
    #include <stdio.h>
    #include <stdio.h>
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Header file \'stdio.h\' is included more than once.', 3, 15);
});

test('should verify the presence of an included function from a header', t => {
    const code = `
    #include <stdlib.h>
    int main() {
        return abs(-1);
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should report multiple definitions of a function', t => {
    const code = `
    int foo() {
        return 0;
    }
    int foo() {
        return 1;
    }
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Function \'foo\' defined more than once.', 5, 9);
});

test('should report non-void function with no return statement', t => {
    const code = `
    int foo() {
    }
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Non-void function \'foo\' does not always return a value.', 2, 5);
});

test('should allow void function with empty return statement', t => {
    const code = `
    void foo() {
        return;
    }
    int main() {
        foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should allow recursion', t => {
    const code = `
    int factorial(int n) {
        if (n < 1) return 1;
        return n * factorial(n - 1);
    }
    int main() {
        return factorial(5);
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should include parameters in function scope and sub scopes', t => {
    const code = `
    int foo(int x) {
        if (x < 1) {
            'a';
        } 
        return x;
    }
    int main() {
        return foo(1);
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should warn for assignment on function parameter', t => {
    const code = `
    int main(int x) {
        x = 1;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Assignment to function parameter \'x\'.', 3, 9);
});

test('should allow variable included in scope and sub scope', t => {
    const code = `
    int main() {
        int x;
        int y;
        x = 1;
        {
            y = x + 2;
        }
        return x;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should report error if condition does not evaluate to int', t => {
    const code = `
    int tst() {
        int* p;
        if (p)
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Condition in if statement must be of type promotable to \'int\'.', 4, 13);
    assertWarning(t, reporter, 'Non-void function \'tst\' does not always return a value.', 2, 5);
});

test('should warn if type protion to int if if statement', t => {
    const code = `
    int main() {
        char c;
        if (c)
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 4, 13);
});

test('should handle constant positive char in if statement', t => {
    const code = `
    int main() {
        if ('a')
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 3, 13);
    assertWarning(t, reporter, 'Condition always evaluates to \'true\'.', 3, 13);
});

test('should handle constant zero char in if statement', t => {
    const code = `
    int main() {
        if ('\0')
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 3, 13);
    assertWarning(t, reporter, 'Condition always evaluates to \'false\'.', 3, 13);
    assertError(t, reporter, 'Unreachable branch.', 4, 13);
});

test('should warn if condition always evaluates to true', t => {
    const code = `
    int main() {
        if (1) {
            return 0;
        }
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'true\'.', 3, 13);
    assertWarning(t, reporter, 'Unreachable statement.', 6, 9);
});

test('should warn if condition always evaluates to false', t => {
    const code = `
    int main() {
        if (0)
            return 0;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'false\'.', 3, 13);
    assertError(t, reporter, 'Unreachable branch.', 4, 13);
});

test('should warn if condition always evaluates to false with unreachable else', t => {
    const code = `
    int main() {
        if (0) {
            return 0;
        } else
            return 1;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'false\'.', 3, 13);
    assertError(t, reporter, 'Unreachable branch.', 3, 16);
    t.is(reporter.errors.length, 1);
    t.is(reporter.warnings.length, 1);
});

test('should handle function already returned should stay returned', t => {
    const code = `
    int main() {
        return 0;
        if (1) {
            return 1;
        }
        return 2;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Unreachable statement.', 4, 9);
    assertWarning(t, reporter, 'Unreachable statement.', 4, 16);
    assertWarning(t, reporter, 'Unreachable statement.', 5, 13);
    assertWarning(t, reporter, 'Unreachable statement.', 7, 9);
});

test('should handle function not pre-returned but returned in constant branch of if', t => {
    const code = `
    int main() {
        if (1) {
            return 1;
        }
        return 2;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'true\'.', 3, 13);
    assertWarning(t, reporter, 'Unreachable statement.', 6, 9);
});

test('should handle function not pre-returned but returned in constant branch of else', t => {
    const code = `
    int main() {
        if (0) {
        } else {
            return 1;
        }
        return 2;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'false\'.', 3, 13);
    assertError(t, reporter, 'Unreachable branch.', 3, 16);
    assertWarning(t, reporter, 'Unreachable statement.', 7, 9);
});

test('should handle function not pre-returned but returned in both if and else', t => {
    const code = `
    int main() {
        if (1) {
            return 1;
        } else {
            return 2;
        }
        return 3;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'true\'.', 3, 13);
    assertWarning(t, reporter, 'Unreachable statement.', 8, 9);
});

test('should handle function not pre-returned and not returned in if so stays not returned', t => {
    const code = `
    int main() {
        int x;
        if (1) {
            x = 0;
        }
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'true\'.', 4, 13);
    t.is(reporter.errors.length, 0);
    t.is(reporter.warnings.length, 1);
});

test('should handle function not pre-returned and not returned in else so stays not returned', t => {
    const code = `
    int main() {
        int x;
        int y;
        if (0) {
            x = 0;
        } else {
            y = 1;
        }
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Condition always evaluates to \'false\'.', 5, 13);
    assertError(t, reporter, 'Unreachable branch.', 5, 16);
    t.is(reporter.errors.length, 1);
    t.is(reporter.warnings.length, 1);
});

test('should report unreachable loop body if condition always evaluates to false', t => {
    const code = `
    int main() {
        int x;
        while (0) {
            x = 0;
        }
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Loop condition always evaluates to \'false\'.', 4, 16);
    assertError(t, reporter, 'Unreachable loop body.', 4, 19);
});

test('should not mark function as returned if return in while body', t => {
    const code = `
    int main() {
        while (0) {
            return 1;
        }
        return 2;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Loop condition always evaluates to \'false\'.', 3, 16);
    assertError(t, reporter, 'Unreachable loop body.', 3, 19);
    t.is(reporter.errors.length, 1);
    t.is(reporter.warnings.length, 1);
});

test('should error if loop condition type not promotable to int', t => {
    const code = `
    int main() {
        int* p;
        while (p)
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Condition in while statement must be of type promotable to \'int\'.', 4, 16);
});

test('should warn if type protion to int if while statement', t => {
    const code = `
    int main() {
        char c;
        while (c)
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 4, 16);
});

test('should handle constant positive char in while statement', t => {
    const code = `
    int main() {
        while ('a')
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 3, 16);
});

test('should handle constant zero char in while statement', t => {
    const code = `
    int main() {
        while ('\0')
            return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 3, 16);
    assertWarning(t, reporter, 'Loop condition always evaluates to \'false\'.', 3, 16);
    assertError(t, reporter, 'Unreachable loop body.', 4, 13);
});

test('should report return type mismatch', t => {
    const code = `
    int foo() {
        return 'a';
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Return type mismatch: expected \'int\', got \'char\'.', 3, 16);
});

test('should report error if void function returns a value', t => {
    const code = `
    void foo() {
        return 1;
    }
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Void function cannot return a value.', 3, 16);
});

test('should report error if non-void function returns but no value', t => {
    const code = `
    int foo() {
        return;
    }
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Non-void function must return a value.', 3, 9);
});

test('should warn for empty statements', t => {
    const code = `
    int main() {
        ;;;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Extraneous semicolon.', 3, 9);
    assertWarning(t, reporter, 'Extraneous semicolon.', 3, 10);
    assertWarning(t, reporter, 'Extraneous semicolon.', 3, 11);
});

test('should forbid unary operations on functions', t => {
    const code = `
    void foo() {}
    int main() {
        +foo;
        -foo;
        !foo;
        *foo;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Cannot perform unary operations on functions.', 4, 9);
    assertError(t, reporter, 'Cannot perform unary operations on functions.', 5, 9);
    assertError(t, reporter, 'Cannot perform unary operations on functions.', 6, 9);
    assertError(t, reporter, 'Cannot perform unary operations on functions.', 7, 9);
});

test('should verify that + operator expects int or char', t => {
    const code = `
    int* foo() {}
    int main() {
        int x;
        x = +foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Unary operator \'+\' expects type \'int\' or \'char\', got \'int*\'.', 5, 14);
});

test('should verify that - operator expects int or char', t => {
    const code = `
    int* foo() {}
    int main() {
        int x;
        x = -foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Unary operator \'-\' expects type \'int\' or \'char\', got \'int*\'.', 5, 14);
});

test('should verify that ! operator expects int', t => {
    const code = `
    char foo() {}
    int main() {
        int x;
        x = !foo();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Cannot invert expression of type \'char\'.', 5, 13);
});

test('should verify that * operator expects pointer', t => {
    const code = `
    int main() {
        int x;
        int y;
        int *p;
        p = &x;
        y = *x;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Cannot dereference expression of type \'int\'', 7, 13);
});

test('should verify that & operator takes everything', t => {
    const code = `
    int main() {
        int x;
        int *p;
        p = &x;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should forbid binary operations on functions', t => {
    const code = `
    void foo() {}
    void bar() {}
    int main() {
        int x;
        x = foo + bar;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Cannot perform binary operations on functions.', 6, 13);
});

test('should warn for char promotion to int in all operations', t => {
    const code = `
    int main() {
        char c;
        int x;
        int y;
        x = c + 1;
        y = 1 + c;
        x = c - 1;
        y = 1 - c;
        x = c * 2;
        y = 2 * c;
        x = c / 2;
        y = 2 / c;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 6, 13);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 7, 17);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 8, 13);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 9, 17);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 10, 13);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 11, 17);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 12, 13);
    assertWarning(t, reporter, 'Implicit type promotion from \'char\' to \'int\'.', 13, 17);
});

test('should handle pointer arithmetic only on + and -', t => {
    const code = `
    int main() {
        int *p;
        int x;
        int *q;
        p = q + x;
        p = q - x;
        p = q * x; // error
        p = q / x; // error
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Type mismatch in binary expression: \'int*\' left-hand expects \'int\' right-hand.', 8, 13);
    assertError(t, reporter, 'Type mismatch in binary expression: \'int*\' left-hand expects \'int\' right-hand.', 9, 13);
});

test('should handle only compatible types (int and pointers)', t => {
    const code = `
    int main() {
        int x;
        int y;
        int *p;
        x = x + y; // valid
        p = p + y; // valid
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});

test('should disallow + - * / if both sides are pointers', t => {
    const code = `
    int main() {
        int *p;
        int *q;
        p = p + q;
        p = p - q;
        p = p * q;
        p = p / q;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Pointers are not allowed for operator \'+\', got \'int*\'.', 5, 13);
    assertError(t, reporter, 'Pointers are not allowed for operator \'-\', got \'int*\'.', 6, 13);
    assertError(t, reporter, 'Pointers are not allowed for operator \'*\', got \'int*\'.', 7, 13);
    assertError(t, reporter, 'Pointers are not allowed for operator \'/\', got \'int*\'.', 8, 13);
});

test('should enforce && and || operators expect int', t => {
    const code = `
    int main() {
        int *x;
        int *p;
        x = x && p; // error
        x = p && x; // error
        x = x || p; // error
        x = p || x; // error
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Binary operator && expects type \'int\', got \'int*\'.', 5, 13);
    assertError(t, reporter, 'Binary operator && expects type \'int\', got \'int*\'.', 6, 13);
    assertError(t, reporter, 'Binary operator || expects type \'int\', got \'int*\'.', 7, 13);
    assertError(t, reporter, 'Binary operator || expects type \'int\', got \'int*\'.', 8, 13);
});

test('should restrict relational operators to handle only the same type (int vs pointer)', t => {
    const code = `
    int main() {
        int x;
        int y;
        int *p;
        int *q;
        if (x < y) {}     // valid
        if (p == q) {}    // valid
        if (x > p) {}     // invalid
        if (p != x) {}    // invalid
        while (x > y) {}  // valid
        while (x == p) {} // invalid
        while (p != x) {} // invalid
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Type mismatch in binary expression: \'int\' left-hand expects \'int*\' right-hand.', 9, 13);
    assertError(t, reporter, 'Type mismatch in binary expression: \'int*\' left-hand expects \'int\' right-hand.', 10, 13);
    assertError(t, reporter, 'Type mismatch in binary expression: \'int\' left-hand expects \'int*\' right-hand.', 12, 16);
    assertError(t, reporter, 'Type mismatch in binary expression: \'int*\' left-hand expects \'int\' right-hand.', 13, 16);
});

test('should prevent sizeof call with function expression', t => {
    const code = `
    void foo() {}
    int main() {
        int x;
        x = sizeof(foo);
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'sizeof cannot be called with function types.', 5, 13);
});

test('should not allow call to object not a function', t => {
    const code = `
    int main() {
        int x;
        x();
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Called object \'x\' is not a function.', 4, 9);
});

test('should not allow call with argument count mismatch', t => {
    const code = `
    void foo(int a) {}
    int main() {
        foo();
        foo(1, 2);
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Argument count mismatch in call to \'foo\', expected 1 arguments.', 4, 9);
    assertError(t, reporter, 'Argument count mismatch in call to \'foo\', expected 1 arguments.', 5, 9);
});

test('should not allow call with argument type mismatch', t => {
    const code = `
    void foo(int a) {}
    int main() {
        char c;
        foo(c);
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Argument type mismatch in call to \'foo\': expected \'int\' at position 0, got \'char\'.', 5, 13);
});

test('should warn for assignment to function parameter', t => {
    const code = `
    void foo(int a) {
        a = 10;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, "Assignment to function parameter 'a'.", 3, 9);
});

test('should warn for incompatible types in assignment', t => {
    const code = `
    int main() {
        int x;
        char c;
        int *p;
        x = c;
        p = &x;
        p = x;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, "Incompatible types in assignment: 'int' cannot be assigned to 'char'.", 6, 9);
    assertError(t, reporter, "Incompatible types in assignment: 'int*' cannot be assigned to 'int'.", 8, 9);
});

test('should report undefined identifiers', t => {
    const code = `
    int main() {
        int x;
        return y;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertError(t, reporter, 'Unknown identifier \'y\'.', 4, 16);
})

test('should report unused variable', t => {
    const code = `
    int main() {
        int x;
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Variable \'x\' declared but not used.', 3, 13);
});

test('should report unused parameter warning', t => {
    const code = `
    int foo(int x) {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Function parameter \'x\' declared but not used.', 2, 17);
});

test('should report unused function warning', t => {
    const code = `
    int foo() {
        return 0;
    }
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Function \'foo\' declared but not used.', 2, 9);
});

test('should report unused library warning', t => {
    const code = `
    #include <stdio.h>
    int main() {
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertWarning(t, reporter, 'Header file \'stdio.h\' included but never used.', 2, 5);
});

test('should work with a complete z script', t => {
    const code = `
    #include <stdio.h>

    int foo(int a, char b) {
        int x;
        int y;
        int *p;
        int *q;
        p = &x;
        q = &y;
        x = a + +b;
        y = a - -b;
        x = a * 2;
        y = +b / 2;

        if (x < y) {
            x = y;
        }

        if (p < q) {
            p = q;
        }

        while (y > 0) {
            y = y - 1;
        }

        x = sizeof(p);
        y = sizeof(*q);

        return x;
    }

    int main() {
        int result;
        char c;
        result = foo(10, 'a');
        if (result != 0) {
            puts(&c);
        }
        return 0;
    }
    `;
    const ast = codeToAst(code);
    const reporter = createMockReporter();

    typecheck(ast, reporter);

    assertClean(t, reporter);
});
