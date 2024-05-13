package decaf.ast.statement;

import decaf.ast.statement.expression.Expression;
import java.util.Optional;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class IfStatement extends Statement {

    protected final Expression condition;
    protected final Statement thenBranch;
    protected final Statement elseBranch;

    /**
     * Class representing an if statement
     * @param condition the condition of the if statement
     * @param thenBranch the then branch of the if statement
     * @param elseBranch the else branch of the if statement
     */
    public IfStatement(Expression condition, Statement thenBranch, Statement elseBranch) {
        this.condition = condition;
        this.thenBranch = thenBranch;
        this.elseBranch = elseBranch;
    }

    /**
     * Get the condition of the if statement
     * @return the condition of the if statement
     */
    public Expression getCondition() {
        return condition;
    }

    /**
     * Get the then branch of the if statement
     * @return the then branch of the if statement
     */
    public Statement getThenBranch() {
        return thenBranch;
    }

    /**
     * Get the else branch of the if statement
     * @return the else branch of the if statement
     */
    public Optional<Statement> getElseBranch() {
        return Optional.ofNullable(elseBranch);
    }
}
