package decaf.ast.statement.expression;

import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class AccessExpression extends Expression {

    protected final List<Identifier> path;

    /**
     * Class representing an access expression
     * @param path the path of the access expression
     */
    public AccessExpression(List<Identifier> path) {
        this.path = path;
    }

    /**
     * Get the path of the access expression
     * @return the path of the access expression
     */
    public List<Identifier> getPath() {
        return path;
    }
}
