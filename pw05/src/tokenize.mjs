import { createToken, Lexer } from 'chevrotain';

/* Skippers */
const Whitespace    = createToken({ name: 'Whitespace', label: '<whitespace>', pattern: /\s+/, group: Lexer.SKIPPED });
const Comment       = createToken({ name: 'Comment', label: '<comment>', pattern: /\/\/.*/, group: Lexer.SKIPPED });

/* Identifier */
const Identifier    = createToken({ name: 'Identifier', label: '<identifier>', pattern: /[a-zA-Z_]\w*/ });

/* Types */
const Int           = createToken({ name: 'Int', label: "'int'", pattern: /int/, longer_alt: Identifier });
const Char          = createToken({ name: 'Char', label: "'char'", pattern: /char/, longer_alt: Identifier });
const Void          = createToken({ name: 'Void', label: "'void'", pattern: /void/, longer_alt: Identifier });

/* Keywords */
const Return        = createToken({ name: 'Return', label: "'return'", pattern: /return/, longer_alt: Identifier });
const If            = createToken({ name: 'If', label: "'if'", pattern: /if/, longer_alt: Identifier });
const Else          = createToken({ name: 'Else', label: "'else'", pattern: /else/, longer_alt: Identifier });
const While         = createToken({ name: 'While', label: "'while'", pattern: /while/, longer_alt: Identifier });
const Sizeof        = createToken({ name: 'Sizeof', label: "'sizeof'", pattern: /sizeof/, longer_alt: Identifier });

/* Punctors */
const Semicolon     = createToken({ name: 'Semicolon', label: "';'", pattern: /;/ });
const Comma         = createToken({ name: 'Comma', label: "','", pattern: /,/ });
const OpenParen     = createToken({ name: 'OpenParen', label: "'('", pattern: /\(/ });
const CloseParen    = createToken({ name: 'CloseParen', label: "')'", pattern: /\)/ });
const OpenBrace     = createToken({ name: 'OpenBrace', label: "'{'", pattern: /\{/ });
const CloseBrace    = createToken({ name: 'CloseBrace', label: "'}'", pattern: /\}/ });

/* Preprocessor */
const Include       = createToken({ name: 'Include', label: "'#include'", pattern: /#include/ });
const Header        = createToken({ name: 'Header', label: "'.h'", pattern: /\.h/ });

/* Operators */
const Plus          = createToken({ name: 'Plus', label: "'+'", pattern: /\+/ });
const Minus         = createToken({ name: 'Minus', label: "'-'", pattern: /-/ });
const EqualEqual    = createToken({ name: 'EqualEqual', label: "'=='", pattern: /==/ });
const Equal         = createToken({ name: 'Equal', label: "'='", pattern: /=/ });
const NotEqual      = createToken({ name: 'NotEqual', label: "'!='", pattern: /!=/ });
const Not           = createToken({ name: 'Not', label: "'!'", pattern: /!/ });
const Or            = createToken({ name: 'Or', label: "'||'", pattern: /\|\|/ });
const And           = createToken({ name: 'And', label: "'&&'", pattern: /&&/ });
const Ampersand     = createToken({ name: 'Ampersand', label: "'&'", pattern: /&/ });
const Asterisk      = createToken({ name: 'Asterisk', label: "'*'", pattern: /\*/ });
const Slash         = createToken({ name: 'Slash', label: "'/'", pattern: /\// });
const Less          = createToken({ name: 'Less', label: "'<'", pattern: /</ });
const Greater       = createToken({ name: 'Greater', label: "'>'", pattern: />/ });

/* Literals */
const Integer       = createToken({ name: 'Integer', label: '<integer>', pattern: /0|[1-9]\d*/ });
const Character     = createToken({ name: 'Character', label: '<character>', pattern: /'(\\.|[^\\'])'/ });

/**
 * The lexical grammar of the language
 */
const Vocabulary = [
    Whitespace,
    Comment,
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
    // Identifier must be last
    Identifier
];

/**
 * The lexer instance
 */
const lexer = new Lexer(Vocabulary, { positionTracking: 'full' });

/**
 * Tokenizes the input string
 * @param {string} input The input string
 * @param {*} reporter The reporter
 * @returns {Token[]} The tokens
 */
function tokenize(input, reporter) {
    const result = lexer.tokenize(input);
    if (result.errors.length) {
        for (const error of result.errors) {
            const { message, line, column, length } = error;
            const loc = { start: { line, column }, end: { line, column: column + length } };
            reporter.error(message, loc);
        }
        process.exit(1);
    }
    return result.tokens;
}

export { Vocabulary, tokenize }
