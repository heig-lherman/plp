package decaf.ast.statement.expression;

import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class NewExpression extends Expression {

    protected final Identifier identifier;
    protected final List<Expression> arguments;

    /**
     * Class representing a "new" expression
     * @param identifier the identifier of the "new" expression
     * @param arguments the arguments of the "new" expression
     */
    public NewExpression(Identifier identifier, List<Expression> arguments) {
        this.identifier = identifier;
        this.arguments = arguments;
    }

    /**
     * Get the identifier of the "new" expression
     * @return the identifier of the "new" expression
     */
    public Identifier getIdentifier() {
        return identifier;
    }

    /**
     * Get the arguments of the "new" expression
     * @return the arguments of the "new" expression
     */
    public List<Expression> getArguments() {
        return arguments;
    }
}
