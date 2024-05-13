package decaf.tokenizer;

/**
 * A token.
 */
public class Token {

    /**
     * The different kinds of tokens.
     */
    public static enum TokenKind {
        // Identifiers
        IDENTIFIER,
        // Literals
        NUMBER,
        // Keywords
        VAR,
        CLASS,
        IF,
        ELSE,
        WHILE,
        RETURN,
        NEW,
        // Types
        INT,
        VOID,
        // Punctuation
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,
        COMMA,
        SEMICOLON,
        // Other
        EQUAL,
        DOT,
        EOF
    }

    private final TokenKind kind;
    private final String lexeme;
    private final int offset;

    /**
     * Create a new token.
     * 
     * @param kind The kind of the token.
     * @param lexeme The lexeme of the token.
     * @param offset The offset of the token.
     */
    public Token(TokenKind kind, String lexeme, int offset) {
        this.kind = kind;
        this.lexeme = lexeme;
        this.offset = offset;
    }

    /**
     * Get the kind of the token.
     */
    public TokenKind getKind() {
        return kind;
    }

    /** 
     * Get the lexeme of the token.
     */
    public String getLexeme() {
        return lexeme;
    }

    /**
     * Get the offset of the token.
     */
    public int getOffset() {
        return offset;
    }
}
