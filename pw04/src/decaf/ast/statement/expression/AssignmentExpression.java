package decaf.ast.statement.expression;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class AssignmentExpression extends Expression {

    protected final AccessExpression target;
    protected final Expression value;

    /**
     * Class representing an assignment expression
     * @param target the target of the assignment
     * @param value the value to assign
     */
    public AssignmentExpression(AccessExpression target, Expression value) {
        this.target = target;
        this.value = value;
    }

    /**
     * Get the target of the assignment
     * @return the target of the assignment
     */
    public AccessExpression getTarget() {
        return target;
    }

    /**
     * Get the value to assign
     * @return the value to assign
     */
    public Expression getValue() {
        return value;
    }
}
