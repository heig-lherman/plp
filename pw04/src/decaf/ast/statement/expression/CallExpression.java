package decaf.ast.statement.expression;

import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class CallExpression extends Expression {

    protected final AccessExpression target;
    protected final List<Expression> arguments;

    /**
     * Class representing a call expression
     * @param target the target of the call
     * @param arguments the arguments of the call
     */
    public CallExpression(AccessExpression target, List<Expression> arguments) {
        this.target = target;
        this.arguments = arguments;
    }

    /**
     * Get the target of the call
     * @return the target of the call
     */
    public AccessExpression getTarget() {
        return target;
    }

    /**
     * Get the arguments of the call
     * @return the arguments of the call
     */
    public List<Expression> getArguments() {
        return arguments;
    }
}
