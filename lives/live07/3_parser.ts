/*
 * Grammar:
 * <expr> ::= <var> | '(' <expr> <expr> ')' | 'λ' <var> '.' <expr>
 */
import { inspect } from 'node:util';

export type Expr = Var | App | Abs;

export interface Var {
    tag: 'Var';
    name: string;
}

export interface App {
    tag: 'App';
    callee: Expr;
    argument: Expr;
}

export interface Abs {
    tag: 'Abs';
    param: Var;
    body: Expr;
}

export const parse = (input: string): Expr => {
    let index = 0;
    
    const parseExpr = (): Expr => {
        switch (input[index]) {
            case 'λ': return parseAbs();
            case '(': return parseApp();
            default: return parseVar();
        }
    }

    const parseVar = (): Var => {
        return { tag: 'Var', name: input[index++] };
    }

    const parseApp = (): App => {
        index++; // '('
        const callee = parseExpr();
        index++; // ' '
        const argument = parseExpr();
        index++; // ')'
        return { tag: 'App', callee, argument };
    }

    const parseAbs = (): Abs => {
        index++; // 'λ'
        const param = parseVar();
        index++; // '.'
        const body = parseExpr();
        return { tag: 'Abs', param, body };
    }

    return parseExpr();
}

console.log(inspect(parse('(λx.x z)'), false, null, true));
