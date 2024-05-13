package decaf.ast.statement.expression;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class NumberExpression extends Expression {

    protected final int value;

    /**
     * Class representing a number expression
     * @param value the value of the number expression
     */
    public NumberExpression(int value) {
        this.value = value;
    }

    /**
     * Get the value of the number expression
     * @return the value of the number expression
     */
    public int getValue() {
        return value;
    }
}
