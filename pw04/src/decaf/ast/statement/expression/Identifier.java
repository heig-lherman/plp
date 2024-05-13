package decaf.ast.statement.expression;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Identifier extends Expression {

    protected final String identifier;

    /**
     * Class representing an identifier
     * @param identifier the name of the identifier
     */
    public Identifier(String identifier) {
        this.identifier = identifier;
    }

    /**
     * Get the name of the identifier
     * @return the name of the identifier
     */
    public String getIdentifier() {
        return identifier;
    }
}
