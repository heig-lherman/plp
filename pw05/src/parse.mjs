import { EmbeddedActionsParser } from 'chevrotain';
import { Vocabulary } from './tokenize.mjs';

/**
 * The token types to consume
 */
const [
    /* Whitespace */,
    /* Comment */,
    Int,
    Char,
    Void,
    Return,
    If,
    Else,
    While,
    Sizeof,
    Semicolon,
    Comma,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Include,
    Header,
    Plus,
    Minus,
    EqualEqual,
    Equal,
    NotEqual,
    Not,
    Or,
    And,
    Ampersand,
    Asterisk,
    Slash,
    Less,
    Greater,
    Integer,
    Character,
    Identifier
] = Vocabulary;

/**
 * The syntactical grammar of the language
 */
class Parser extends EmbeddedActionsParser {
    constructor() {
        super(Vocabulary);

        const $ = this;

        $.RULE('program', () => {
            const includes = [];
            const functions = [];

            $.MANY(() => includes.push($.SUBRULE($.include)));
            $.MANY1(() => functions.push($.SUBRULE($.function)));

            return { kind: 'Program', includes, functions };
        });

        $.RULE('include', () => {
            const start = $.CONSUME(Include);
            $.CONSUME(Less)
            const header = $.SUBRULE($.header);
            const end = $.CONSUME(Greater);

            return { kind: 'Include', header, start, end };
        });

        $.RULE('header', () => {
            const id = $.SUBRULE($.identifier);
            const suffix = $.CONSUME(Header)

            return { kind: 'Header', id, suffix };
        });

        $.RULE('function', () => {
            const returnType = $.SUBRULE($.type);
            const id = $.SUBRULE($.identifier);
            $.CONSUME(OpenParen);
            const params = $.SUBRULE($.parameters);
            $.CONSUME(CloseParen);
            const body = $.SUBRULE($.block);

            return { kind: 'Function', returnType, id, params, body  };
        });

        $.RULE('parameters', () => {
            const params = [];

            $.MANY_SEP({
                SEP: Comma,
                DEF: () => params.push($.SUBRULE($.parameter))
            });

            return params;
        });

        $.RULE('parameter', () => {
            const type = $.SUBRULE($.type);
            const id = $.SUBRULE($.identifier);

            return { kind: 'Parameter', type, id };
        });

        $.RULE('type', () => {
            const type = $.SUBRULE($.primitive);
            const pointers = [];
            $.MANY(() => pointers.push($.CONSUME(Asterisk)));

            return { kind: 'Type', type, pointers };
        });

        $.RULE('primitive', () => {
            return $.OR([
                { ALT: () => $.CONSUME(Int) },
                { ALT: () => $.CONSUME(Char) },
                { ALT: () => $.CONSUME(Void) }
            ]);
        });

        $.RULE('statement', () => {
            return $.OR([
                { ALT: () => $.SUBRULE($.if) },
                { ALT: () => $.SUBRULE($.while) },
                { ALT: () => $.SUBRULE($.return) },
                { ALT: () => $.SUBRULE($.block) },
                { ALT: () => $.SUBRULE($.empty) },
                { ALT: () => $.SUBRULE($.expr) }
            ]);
        });

        $.RULE('if', () => {
            const start = $.CONSUME(If);
            $.CONSUME(OpenParen);
            const test = $.SUBRULE($.expression);
            $.CONSUME(CloseParen);
            const consequent = $.SUBRULE($.statement);
            const alternate = $.OPTION(() => {
                $.CONSUME(Else);
                return $.SUBRULE2($.statement);
            });

            return { kind: 'IfStatement', test, consequent, alternate, start };
        });

        $.RULE('while', () => {
            const start = $.CONSUME(While);
            $.CONSUME(OpenParen);
            const test = $.SUBRULE($.expression);
            $.CONSUME(CloseParen);
            const body = $.SUBRULE($.statement);

            return { kind: 'WhileStatement', test, body, start };
        });

        $.RULE('return', () => {
            const start = $.CONSUME(Return);
            const expr = $.OPTION(() => $.SUBRULE($.expression));
            const end = $.CONSUME(Semicolon);

            return { kind: 'ReturnStatement', expr, start, end };
        });

        $.RULE('block', () => {
            const declarations = [];
            const statements = [];

            const start = $.CONSUME(OpenBrace);
            $.MANY(() => declarations.push($.SUBRULE($.var)));
            $.MANY1(() => statements.push($.SUBRULE($.statement)));
            const end = $.CONSUME(CloseBrace);

            return { kind: 'BlockStatement', declarations, statements, start, end };
        });

        $.RULE('var', () => {
            const type = $.SUBRULE($.type);
            const id = $.SUBRULE($.identifier);
            const end = $.CONSUME(Semicolon);

            return { kind: 'Variable', type, id, end };
        });

        $.RULE('empty', () => {
            const start = $.CONSUME(Semicolon);

            return { kind: 'EmptyStatement', start };
        });

        $.RULE('expr', () => {
            const expr = $.SUBRULE($.expression);
            const end = $.CONSUME(Semicolon);

            return { kind: 'ExpressionStatement', expr, end };
        });

        $.RULE('expression', () => $.SUBRULE($.assignment));

        $.RULE('assignment', () => {
            const expr = {};
            const left = $.SUBRULE($.logicalOr);
            $.OPTION(() => {
                const op = $.CONSUME(Equal);
                const right = $.SUBRULE($.assignment);

                expr.kind = 'AssignmentExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });

        $.RULE('logicalOr', () => {
            const expr = {};
            const left = $.SUBRULE($.logicalAnd);

            $.MANY(() => {
                const op = $.CONSUME(Or);
                const right = $.SUBRULE1($.logicalAnd);

                expr.kind = 'BinaryExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });

        $.RULE('logicalAnd', () => {
            const expr = {};
            const left = $.SUBRULE($.equality);

            $.MANY(() => {
                const op = $.CONSUME(And);
                const right = $.SUBRULE1($.equality);

                expr.kind = 'BinaryExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });

        $.RULE('equality', () => {
            const expr = {};
            const left = $.SUBRULE($.relational);

            $.OPTION(() => {
                const op = $.OR([
                    { ALT: () => $.CONSUME(EqualEqual) },
                    { ALT: () => $.CONSUME(NotEqual) }
                ]);
                const right = $.SUBRULE1($.relational);

                expr.kind = 'BinaryExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });

        $.RULE('relational', () => {
            const expr = {};
            const left = $.SUBRULE($.additive);

            $.OPTION(() => {
                const op = $.OR([
                    { ALT: () => $.CONSUME(Less) },
                    { ALT: () => $.CONSUME(Greater) }
                ]);
                const right = $.SUBRULE1($.additive);

                expr.kind = 'BinaryExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });


        $.RULE('additive', () => {
            const expr = {};
            const left = $.SUBRULE($.multiplicative);

            $.OPTION(() => {
                const op = $.OR([
                    { ALT: () => $.CONSUME(Plus) },
                    { ALT: () => $.CONSUME(Minus) }
                ]);
                const right = $.SUBRULE1($.multiplicative);

                expr.kind = 'BinaryExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });

        $.RULE('multiplicative', () => {
            const expr = {};
            const left = $.SUBRULE($.unary);

            $.OPTION(() => {
                const op = $.OR([
                    { ALT: () => $.CONSUME(Asterisk) },
                    { ALT: () => $.CONSUME(Slash) }
                ]);
                const right = $.SUBRULE1($.unary);

                expr.kind = 'BinaryExpression';
                expr.left = left;
                expr.op = op;
                expr.right = right;
            });

            return expr.kind ? expr : left;
        });

        $.RULE('unary', () => $.OR([
            { ALT: () => $.SUBRULE($.primary) },
            { ALT: () => {
                const op = $.OR1([
                    { ALT: () => $.CONSUME(Plus) },
                    { ALT: () => $.CONSUME(Minus) },
                    { ALT: () => $.CONSUME(Not) },
                    { ALT: () => $.CONSUME(Asterisk) },
                ]);
                const expr = $.SUBRULE($.unary);

                return { kind: 'UnaryExpression', op, expr };
            }},
            { ALT: () => {
                const op = $.CONSUME(Ampersand);
                const expr = $.SUBRULE($.identifier);

                return { kind: 'UnaryExpression', op, expr };
            }},
            { ALT: () => {
                const op = $.CONSUME(Sizeof);
                const arg = $.OR2([
                    { ALT: () => {
                        $.CONSUME(OpenParen);
                        const type = $.SUBRULE($.type);
                        $.CONSUME(CloseParen);

                        return type;
                    }},
                    { ALT: () => $.SUBRULE1($.unary) }
                ]);

                return { kind: 'SizeofExpression', op, arg };
            }},
        ]));

        $.RULE('primary', () => $.OR([
            { ALT: () => $.SUBRULE($.literal) },
            { ALT: () => {
                $.CONSUME(OpenParen);
                const expr = $.SUBRULE($.expression);
                $.CONSUME(CloseParen);
                return expr;
            }},
            { GATE: () => $.LA(2).tokenType === OpenParen, ALT: () => $.SUBRULE($.call) },
            { ALT: () => $.SUBRULE($.identifier) },
        ]));

        $.RULE('call', () => {
            const callee = $.SUBRULE($.identifier);
            $.CONSUME(OpenParen);
            const args = $.SUBRULE($.arguments);
            const end = $.CONSUME(CloseParen);

            return { kind: 'CallExpression', callee, args, end };
        });

        $.RULE('arguments', () => {
            const args = [];
            $.MANY_SEP({
                SEP: Comma,
                DEF: () => args.push($.SUBRULE($.expression))
            });

            return args;
        });

        $.RULE('literal', () => {
            return $.OR([
                { ALT: () => ({ kind: 'Literal', value: $.SUBRULE($.integer)}) },
                { ALT: () => ({ kind: 'Literal', value: $.SUBRULE($.character)}) }
            ]);
        });

        $.RULE('identifier', () => {
            const id = $.CONSUME(Identifier);

            return { kind: 'Identifier', name: id.image, start: id };
        });

        $.RULE('integer', () => {
            const integer = $.CONSUME(Integer);
            return { kind: 'Integer', value: parseInt(integer.image), start: integer }
        });

        $.RULE('character', () => {
            const character = $.CONSUME(Character);
            return { kind: 'Character', value: character.image.replaceAll(/'/g, ''), start: character };
        });

        this.performSelfAnalysis();
    }
}

/**
 * Transforms the given AST to include location information
 * @param {*} ast the AST to transform
 * @returns the transformed AST
 */
function transform(ast) {
    if (!ast) {
        return null;
    }
    switch (ast.kind) {
        case 'Program': {
            const includes = ast.includes.map(transform);
            const functions = ast.functions.map(transform);

            const loc = { start: { line: 1, column: 1 }, end: { line: 1, column: 2 } };
            let first = includes[0]?.loc || functions[0]?.loc;
            let last  = functions[functions.length - 1]?.loc || includes[includes.length - 1]?.loc;

            if (first) {
                loc.start = { line: first.start.line, column: first.start.column };
                loc.end = { line: last.end.line, column: last.end.column };
            }

            return { kind: 'Program', includes, functions, loc };
        }
        case 'Include': {
            const header = transform(ast.header);

            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.end.endLine, column: ast.end.endColumn };
            const loc = { start, end };

            return { kind: 'Include', header, loc };
        }
        case 'Header': {
            const id = transform(ast.id);
            const suffix = ast.suffix.image;

            const start = { line: id.loc.start.line, column: id.loc.start.column };
            const end = { line: ast.suffix.endLine, column: ast.suffix.endColumn };
            const loc = { start, end };

            return { kind: 'Header', name: id.name + suffix, loc };
        }
        case 'Function': {
            const returnType = transform(ast.returnType);
            const id = transform(ast.id);
            const params = ast.params.map(transform);
            const body = transform(ast.body);

            const start = { line: returnType.loc.start.line, column: returnType.loc.start.column };
            const end = { line: body.loc.end.line, column: body.loc.end.column };
            const loc = { start, end };

            return { kind: 'Function', returnType, id, params, body, loc };
        }
        case 'Parameter': {
            const type = transform(ast.type);
            const id = transform(ast.id);

            const start = { line: type.loc.start.line, column: type.loc.start.column };
            const end = { line: id.loc.end.line, column: id.loc.end.column };
            const loc = { start, end };

            return { kind: 'Parameter', type, id, loc };
        }
        case 'Type': {
            const type = ast.type.image;
            const pointers = ast.pointers.map(ptr => ptr.image).join('');

            const start = { line: ast.type.startLine, column: ast.type.startColumn };
            const end = {
                line: ast.pointers[ast.pointers.length - 1]?.startLine || ast.type.endLine,
                column: ast.pointers[ast.pointers.length - 1]?.startColumn || ast.type.endColumn
            };
            const loc = { start, end };

            return { kind: 'Type', name: type + pointers, loc };
        }
        case 'Variable': {
            const type = transform(ast.type);
            const id = transform(ast.id);

            const start = { line: type.loc.start.line, column: type.loc.start.column };
            const end = { line: ast.end.endLine, column: ast.end.endColumn };
            const loc = { start, end };

            return { kind: 'Variable', type, id, loc };
        }
        case 'IfStatement': {
            const test = transform(ast.test);
            const consequent = transform(ast.consequent);
            const alternate = transform(ast.alternate);

            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = {
                line: alternate?.loc.end.line || consequent.loc.end.line,
                column: alternate?.loc.end.column || consequent.loc.end.column
            };
            const loc = { start, end };

            return { kind: 'IfStatement', test, consequent, alternate, loc };
        }
        case 'WhileStatement': {
            const test = transform(ast.test);
            const body = transform(ast.body);

            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: body.loc.end.line, column: body.loc.end.column };
            const loc = { start, end };

            return { kind: 'WhileStatement', test, body, loc };
        }
        case 'ReturnStatement': {
            const expr = transform(ast.expr);

            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.end.endLine, column: ast.end.endColumn };
            const loc = { start, end };

            return { kind: 'ReturnStatement', expr, loc };
        }
        case 'BlockStatement': {
            const declarations = ast.declarations.map(transform);
            const statements = ast.statements.map(transform);

            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.end.endLine, column: ast.end.endColumn };
            const loc = { start, end };

            return { kind: 'BlockStatement', declarations, statements, loc };
        }
        case 'EmptyStatement': {
            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.start.endLine, column: ast.start.endColumn };

            return { kind: 'EmptyStatement', loc: { start, end } };
        }
        case 'ExpressionStatement': {
            const expr = transform(ast.expr);

            const start = { line: expr.loc.start.line, column: expr.loc.start.column };
            const end = { line: ast.end.endLine, column: ast.end.endColumn };
            const loc = { start, end };

            return { kind: 'ExpressionStatement', expr, loc };
        }
        case 'Literal': {
            const value = transform(ast.value);

            return { kind: 'Literal', value: value.value, loc: value.loc };
        }
        case 'UnaryExpression': {
            const op = ast.op.image;
            const expr = transform(ast.expr);

            const start = { line: ast.op.startLine, column: ast.op.startColumn };
            const end = { line: expr.loc.end.line, column: expr.loc.end.column };
            const loc = { start, end };

            return { kind: 'UnaryExpression', op, expr, loc };
        }
        case 'SizeofExpression': {
            const op = ast.op.image;
            const arg = transform(ast.arg);

            const start = { line: ast.op.startLine, column: ast.op.startColumn };
            const end = { line: arg.loc.end.line, column: arg.loc.end.column };
            const loc = { start, end };

            return { kind: 'SizeofExpression', op, arg, loc };
        }
        case 'BinaryExpression': {
            const left = transform(ast.left);
            const op = ast.op.image;
            const right = transform(ast.right);

            const start = { line: left.loc.start.line, column: left.loc.start.column };
            const end = { line: right.loc.end.line, column: right.loc.end.column };
            const loc = { start, end };

            return { kind: 'BinaryExpression', left, op, right, loc };
        }
        case 'CallExpression': {
            const callee = transform(ast.callee);
            const args = ast.args.map(transform);

            const start = { line: callee.loc.start.line, column: callee.loc.start.column };
            const end = { line: ast.end.endLine, column: ast.end.endColumn };
            const loc = { start, end };

            return { kind: 'CallExpression', callee, args, loc };
        }
        case 'AssignmentExpression': {
            const left = transform(ast.left);
            const right = transform(ast.right);

            const start = { line: left.loc.start.line, column: left.loc.start.column };
            const end = { line: right.loc.end.line, column: right.loc.end.column };
            const loc = { start, end };

            return { kind: 'AssignmentExpression', left, right, loc };
        }
        case 'Identifier': {
            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.start.endLine, column: ast.start.endColumn };
            const loc = { start, end };

            return { kind: 'Identifier', name: ast.name, loc };
        }
        case 'Integer': {
            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.start.endLine, column: ast.start.endColumn };
            const loc = { start, end };

            return { kind: 'Integer', value: ast.value, loc };
        }
        case 'Character': {
            const start = { line: ast.start.startLine, column: ast.start.startColumn };
            const end = { line: ast.start.endLine, column: ast.start.endColumn };
            const loc = { start, end };

            return { kind: 'Character', value: ast.value, loc };
        }
        default:
            throw new Error(`Unknown AST node kind: ${ast.kind}`);
    }
}

/**
 * The parser instance
 */
const parser = new Parser();

/**
 * Parses the given tokens
 * @param {*} tokens the tokens to parse
 * @param {*} reporter the reporter
 * @returns {object} the AST
 */
function parse(tokens, reporter) {
    parser.input = tokens;
    const ast = parser.program();
    if (parser.errors.length) {
        for (const error of parser.errors) {
            const { message, token: { startLine, startColumn, endColumn }} = error;
            const loc = { start: { line: startLine, column: startColumn }, end: { line: startLine, column: endColumn } };
            reporter.error(message, loc);
        }
        process.exit(1);
    }
    return transform(ast);
}

export { parse };
