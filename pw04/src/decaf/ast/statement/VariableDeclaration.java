package decaf.ast.statement;

import decaf.ast.statement.expression.Expression;
import decaf.ast.statement.expression.Identifier;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class VariableDeclaration extends Statement {

    protected final Identifier identifier;
    protected final Expression value;

    /**
     * Class representing a variable declaration
     * @param identifier the identifier of the variable
     * @param value the value of the variable
     */
    public VariableDeclaration(Identifier identifier, Expression value) {
        this.identifier = identifier;
        this.value = value;
    }

    /**
     * Get the identifier of the variable
     * @return the identifier of the variable
     */
    public Identifier getIdentifier() {
        return identifier;
    }

    /**
     * Get the value of the variable
     * @return the value of the variable
     */
    public Expression getValue() {
        return value;
    }
}
