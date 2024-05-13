package decaf.parser;

import java.util.ArrayList;
import java.util.List;

import decaf.ast.ClassDeclaration;
import decaf.ast.Program;
import decaf.ast.member.*;
import decaf.ast.statement.*;
import decaf.ast.statement.expression.*;
import decaf.ast.type.TypeDeclaration;
import decaf.reporter.Reporter;
import decaf.tokenizer.Token;

/**
 * Parses tokens into an abstract syntax tree.
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Parser {
    private final List<Token> tokens;
    private final Reporter reporter;
    private int index = 0;

    /**
     * Create a new parser.
     * @param tokens The tokens to parse.
     * @param reporter The reporter to report errors to.
     */
    public Parser(List<Token> tokens, Reporter reporter) {
        this.tokens = tokens;
        this.reporter = reporter;
    }

    /**
     * Parse the tokens into an abstract syntax tree.
     */
    public Program parse() {
        List<ClassDeclaration> classes = new ArrayList<>();
        while (peek().getKind() == Token.TokenKind.CLASS) {
            classes.add(parseClassDeclaration());
        }
        checkAndConsume("at the end of the program", Token.TokenKind.EOF);
        return new Program(classes);
    }

    /**
     * Parse a class declaration.
     * @return The class declaration
     */
    private ClassDeclaration parseClassDeclaration() {
        checkAndConsume("at the beginning of the class", Token.TokenKind.CLASS);
        Identifier identifier = parseIdentifier();
        checkAndConsume("after class name", Token.TokenKind.LBRACE);
        ClassDeclaration classDeclaration = new ClassDeclaration(identifier, parseMembers());
        checkAndConsume("at the end of the class", Token.TokenKind.RBRACE);
        return classDeclaration;
    }

    /**
     * Parse the members of a class.
     * @return The members of the class.
     */
    private List<Member> parseMembers() {
        List<Member> members = new ArrayList<>();
        do {
            members.add(parseMember());
        } while (peekIsType());
        return members;
    }

    /**
     * Parse a member of a class.
     * @return The member.
     */
    private Member parseMember(){
        check("at the beginning of a member", Token.TokenKind.INT, Token.TokenKind.VOID, Token.TokenKind.IDENTIFIER);
        return peekNext().getKind() == Token.TokenKind.LPAREN ? parseConstructor() : parseFieldOrMethod();
    }

    /**
     * Parse a field or a method.
     * @return The field or method.
     */
    private Member parseFieldOrMethod() {
        return peekNext(2).getKind() == Token.TokenKind.LPAREN ? parseMethod() : parseField();
    }

    /**
     * Parse a constructor.
     * @return The constructor.
     */
    private Constructor parseConstructor() {
        Identifier identifier = parseIdentifier();
        checkAndConsume("after constructor name", Token.TokenKind.LPAREN);
        List<Parameter> parameters = parseParameters();
        checkAndConsume("after constructor parameters", Token.TokenKind.RPAREN);
        Block block = parseBlock();
        return new Constructor(identifier, parameters, block);
    }

    /**
     * Parse parameters.
     * @return A list of parameters.
     */
    private List<Parameter> parseParameters() {
        List<Parameter> parameters = new ArrayList<>();
        if (peekIsType()) {
            parameters.add(parseParameter());
            while (peek().getKind() == Token.TokenKind.COMMA) {
                consume();
                parameters.add(parseParameter());
            }
        }
        return parameters;
    }

    /**
     * Parse a parameter.
     * @return The parameter.
     */
    private Parameter parseParameter() {
        return new Parameter(parseType(), parseIdentifier());
    }

    /**
     * Parse a type.
     * @return The type declaration.
     */
    private TypeDeclaration parseType() {
        return new TypeDeclaration(
                checkAndConsume(
                        "for a type declaration",
                        Token.TokenKind.INT, Token.TokenKind.VOID, Token.TokenKind.IDENTIFIER).getLexeme()
        );
    }

    /**
     * Parse a field.
     * @return The field.
     */
    private Field parseField() {
        Field field = new Field(parseType(), parseIdentifier());
        if (peek().getKind() == Token.TokenKind.EQUAL) {
            consume();
            field = field.with(parseExpression());
        }
        checkAndConsume("after field", Token.TokenKind.SEMICOLON);
        return field;
    }

    /**
     * Parse an expression.
     * @return The expression.
     */
    private Expression parseExpression() {
        return switch (peek().getKind()) {
            case IDENTIFIER -> {
                if (peekNext().getKind() != Token.TokenKind.DOT) {
                    yield parseIdentifier();
                } else {
                    yield parseAccessCallOrAssignment();
                }
            }
            case NUMBER -> parseNumber();
            case NEW -> parseNew();
            default -> parseAccessCallOrAssignment();
        };
    }

    /**
     * Parse an identifier.
     * @return The identifier.
     */
    private Identifier parseIdentifier() {
        return new Identifier(checkAndConsume(
                "to identify a variable or method",
                Token.TokenKind.IDENTIFIER).getLexeme());
    }

    /**
     * Parse a number.
     * @return The number.
     */
    private Expression parseNumber() {
        return new NumberExpression(Integer.parseInt(
                checkAndConsume("for a number", Token.TokenKind.NUMBER).getLexeme())
        );
    }

    /**
     * Parse a new expression.
     * @return The new expression.
     */
    private Expression parseNew() {
        checkAndConsume("for a new expression", Token.TokenKind.NEW);
        Identifier identifier = parseIdentifier();
        checkAndConsume("before new expression", Token.TokenKind.LPAREN);
        List<Expression> arguments = parseArguments();
        checkAndConsume("after new expression", Token.TokenKind.RPAREN);
        return new NewExpression(identifier, arguments);
    }

    /**
     * Parse an access, call or assignment.
     * @return The access, call or assignment as an expression.
     */
    private Expression parseAccessCallOrAssignment() {
        check("for an access, call or assignment", Token.TokenKind.IDENTIFIER);
        AccessExpression expression = parseAccessExpression();
        return switch (peek().getKind()) {
            case LPAREN -> {
                checkAndConsume("after call name", Token.TokenKind.LPAREN);
                CallExpression callExpression = new CallExpression(expression, parseArguments());
                checkAndConsume("after call arguments", Token.TokenKind.RPAREN);
                yield callExpression;
            }
            case EQUAL -> {
                consume();
                yield new AssignmentExpression(expression, parseExpression());
            }
            default -> expression;
        };
    }


    /**
     * Parse arguments.
     * @return A list of arguments as expressions.
     */
    private List<Expression> parseArguments() {
        List<Expression> arguments = new ArrayList<>();
        if (peek().getKind() != Token.TokenKind.RPAREN) {
            arguments.add(parseExpression());
            while (peek().getKind() == Token.TokenKind.COMMA) {
                consume();
                arguments.add(parseExpression());
            }
        }
        return arguments;
    }

    /**
     * Parse an access expression.
     * @return The access expression.
     */
    private AccessExpression parseAccessExpression() {
        List<Identifier> identifiers = new ArrayList<>();
        identifiers.add(parseIdentifier());
        do{
            checkAndConsume("after identifier", Token.TokenKind.DOT);
            identifiers.add(parseIdentifier());
        } while (peek().getKind() == Token.TokenKind.DOT);
        return new AccessExpression(identifiers);
    }

    /**
     * Parse a method.
     * @return The method.
     */
    private Method parseMethod() {
        TypeDeclaration returnType = parseType();
        Identifier identifier = parseIdentifier();
        checkAndConsume("after method name", Token.TokenKind.LPAREN);
        List<Parameter> parameters = parseParameters();
        checkAndConsume("after method parameters", Token.TokenKind.RPAREN);
        Block block = parseBlock();
        return new Method(returnType, identifier, parameters, block);
    }

    /**
     * Parse a block.
     * @return The block.
     */
    private Block parseBlock() {
        checkAndConsume("at the beginning of the block", Token.TokenKind.LBRACE);
        List<Statement> statements = parseStatements();
        checkAndConsume("at the end of the block", Token.TokenKind.RBRACE);
        return new Block(statements);
    }

    /**
     * Parse statements.
     * @return A list of statements.
     */
    private List<Statement> parseStatements() {
        List<Statement> statements = new ArrayList<>();
        while (peek().getKind() != Token.TokenKind.RBRACE) {
            statements.add(parseStatement());
        }
        return statements;
    }

    /**
     * Parse a statement.
     * @return The statement.
     */
    private Statement parseStatement() {
        return switch (peek().getKind()) {
            case LBRACE -> parseBlock();
            case IF -> parseIf();
            case WHILE -> parseWhile();
            case RETURN -> parseReturn();
            case VAR -> parseVar();
            default -> {
                Statement accessOrAssignment = parseExpression();
                checkAndConsume("after statement", Token.TokenKind.SEMICOLON);
                yield accessOrAssignment;
            }
        };
    }

    /**
     * Parse an if statement.
     * @return The if statement as a statement.
     */
    private Statement parseIf() {
        checkAndConsume("at the beginning of the if statement", Token.TokenKind.IF);
        checkAndConsume("after if", Token.TokenKind.LPAREN);
        Expression condition = parseExpression();
        checkAndConsume("after if expression", Token.TokenKind.RPAREN);
        Statement thenBlock = parseStatement();
        Statement elseBlock = null;
        if (peek().getKind() == Token.TokenKind.ELSE) {
            consume();
            elseBlock = parseStatement();
        }
        return new IfStatement(condition, thenBlock, elseBlock);
    }

    /**
     * Parse a while statement.
     * @return The while statement as a statement.
     */
    private Statement parseWhile() {
        checkAndConsume("at the beginning of the while statement", Token.TokenKind.WHILE);
        checkAndConsume("after while", Token.TokenKind.LPAREN);
        Expression condition = parseExpression();
        checkAndConsume("after while expression", Token.TokenKind.RPAREN);
        return new WhileStatement(condition, parseStatement());
    }

    /**
     * Parse a return statement.
     * @return The return statement as a statement.
     */
    private Statement parseReturn() {
        checkAndConsume("at the beginning of the return statement", Token.TokenKind.RETURN);
        Expression expression = parseExpression();
        checkAndConsume("after return expression", Token.TokenKind.SEMICOLON);
        return new ReturnStatement(expression);
    }

    /**
     * Parse a variable declaration.
     * @return The variable declaration as a statement.
     */
    private Statement parseVar() {
        checkAndConsume("at the beginning of the variable declaration", Token.TokenKind.VAR);
        Identifier identifier = parseIdentifier();
        Expression expression = null;
        if (peek().getKind() == Token.TokenKind.EQUAL) {
            consume();
            expression = parseExpression();
        }
        checkAndConsume("after variable declaration", Token.TokenKind.SEMICOLON);
        return new VariableDeclaration(identifier, expression);
    }

    /**
     * Check if the next token is one of the given kinds and consume it.
     * @param kind The kinds to check for.
     * @return The consumed token.
     */
    private Token checkAndConsume(String messageAfter, Token.TokenKind... kind) {
        check(messageAfter, kind);
        return consume();
    }

    /**
     * Check if the next token is one of the given kinds.
     * @param messageAfter The message to display after the token expected message.
     * @param kinds The kinds to check for. If none of the kinds match, an error is reported.
     */
    private void check(String messageAfter, Token.TokenKind... kinds) {
        Token token = peek();
        for (Token.TokenKind kind : kinds) {
            if (token.getKind() == kind) {
                return;
            }
        }
        if (kinds.length == 1) {
            reporter.report("Expected " + getDescriptionOfTokenKind(kinds[0]) + " " + messageAfter, token.getOffset());
        } else {
            StringBuilder expected = new StringBuilder();
            for (int i = 0; i < kinds.length - 1; i++) {
                expected.append(kinds[i]).append(", ");
            }
            expected.append("or ").append(getDescriptionOfTokenKind(kinds[kinds.length - 1]));
            reporter.report("Expected " + expected + " after " + messageAfter, token.getOffset());
        }
    }

    /**
     * Get the current token without consuming it.
     * @return The current token.
     */
    private Token peek() {
        return tokens.get(index);
    }

    /**
     * Consume the current token and move to the next one.
     * @return The consumed token.
     */
    private Token consume() {
        return tokens.get(index++);
    }

    /**
     * Peek at the next token without consuming it.
     * @return The next token.
     */
    private Token peekNext() {
        return peekNext(1);
    }

    /**
     * Peek at the token at the given offset without consuming it.
     * @param offset The offset to peek at.
     * @return The token at the given offset.
     */
    private Token peekNext(int offset) {
        return tokens.get(index + offset);
    }

    /**
     * Check if the current token is a possible type.
     * @return Whether the current token is a possible type.
     */
    private boolean peekIsType(){
        return peek().getKind() == Token.TokenKind.INT || peek().getKind() == Token.TokenKind.VOID || peek().getKind() == Token.TokenKind.IDENTIFIER;
    }

    String getDescriptionOfTokenKind(Token.TokenKind kind){
        return switch (kind) {
            case IDENTIFIER -> "identifier";
            case NUMBER -> "number";
            case VAR -> "var";
            case CLASS -> "class";
            case IF -> "if";
            case ELSE -> "else";
            case WHILE -> "while";
            case RETURN -> "return";
            case NEW -> "new";
            case INT -> "int";
            case VOID -> "void";
            case LPAREN -> "(";
            case RPAREN -> ")";
            case LBRACE -> "{";
            case RBRACE -> "}";
            case COMMA -> ",";
            case SEMICOLON -> ";";
            case EQUAL -> "=";
            case DOT -> ".";
            case EOF -> "end of file";
        };
    }
}
