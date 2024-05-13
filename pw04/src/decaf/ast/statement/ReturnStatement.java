package decaf.ast.statement;

import decaf.ast.statement.expression.Expression;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class ReturnStatement extends Statement {

    protected final Expression value;

    /**
     * Class representing a return statement
     * @param value the value of the return statement
     */
    public ReturnStatement(Expression value) {
        this.value = value;
    }

    /**
     * Get the value of the return statement
     * @return the value of the return statement
     */
    public Expression getValue() {
        return value;
    }
}
