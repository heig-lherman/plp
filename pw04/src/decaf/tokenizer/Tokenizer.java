package decaf.tokenizer;

import decaf.reporter.Reporter;
import decaf.tokenizer.Token.TokenKind;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tokenizes a string into tokens.
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Tokenizer {

    private static final Pattern TOKEN_PATTERN = Pattern.compile(
            """
                    (?<Keyword>\\b(var|class|if|else|while|return|new|int|void)\\b)|\
                    (?<Identifier>\\b[a-zA-Z][a-zA-Z0-9]*\\b)|\
                    (?<Number>\\b\\d+\\b)|\
                    (?<Punctuation>[(){};,=.])|\
                    (?<Garbage>\\S+)"""
    );

    private final String input;
    private final Reporter reporter;

    /**
     * Create a new tokenizer.
     *
     * @param input    The input string to tokenize.
     * @param reporter The reporter to report errors to.
     */
    public Tokenizer(String input, Reporter reporter) {
        this.input = input;
        this.reporter = reporter;
    }

    /**
     * Tokenize the input string.
     */
    public List<Token> tokenize() {
        List<Token> tokens = new LinkedList<>();
        Matcher matcher = TOKEN_PATTERN.matcher(input);

        while (matcher.find()) {
            var lexeme = matcher.group();
            var offset = matcher.start();
            if (matcher.group("Keyword") != null) {
                tokens.add(new Token(lexKeyword(lexeme), lexeme, offset));
            } else if (matcher.group("Identifier") != null) {
                tokens.add(new Token(TokenKind.IDENTIFIER, lexeme, offset));
            } else if (matcher.group("Number") != null) {
                tokens.add(new Token(TokenKind.NUMBER, lexeme, offset));
            } else if (matcher.group("Punctuation") != null) {
                tokens.add(new Token(lexPunctuation(lexeme), lexeme, offset));
            } else if (!lexeme.isBlank()) {
                reporter.report("Unknown token: %s".formatted(lexeme), offset);
            }
        }

        tokens.add(new Token(TokenKind.EOF, "", input.length()));
        return tokens;
    }

    private TokenKind lexKeyword(String lexeme) {
        return switch (lexeme) {
            case "var" -> TokenKind.VAR;
            case "class" -> TokenKind.CLASS;
            case "if" -> TokenKind.IF;
            case "else" -> TokenKind.ELSE;
            case "while" -> TokenKind.WHILE;
            case "return" -> TokenKind.RETURN;
            case "new" -> TokenKind.NEW;
            case "int" -> TokenKind.INT;
            case "void" -> TokenKind.VOID;
            // This should never be caught by the regex.
            default -> throw new IllegalStateException("Unexpected token: " + lexeme);
        };
    }

    private TokenKind lexPunctuation(String lexeme) {
        return switch (lexeme) {
            case "(" -> TokenKind.LPAREN;
            case ")" -> TokenKind.RPAREN;
            case "{" -> TokenKind.LBRACE;
            case "}" -> TokenKind.RBRACE;
            case "," -> TokenKind.COMMA;
            case ";" -> TokenKind.SEMICOLON;
            case "=" -> TokenKind.EQUAL;
            case "." -> TokenKind.DOT;
            // This should never be caught by the regex.
            default -> throw new IllegalStateException("Unexpected token: " + lexeme);
        };
    }
}
