package decaf.ast.statement;

import decaf.ast.statement.expression.Expression;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class WhileStatement extends Statement {

    protected final Expression condition;
    protected final Statement branch;

    /**
     * Class representing a while statement
     * @param condition the condition of the while statement
     * @param branch the branch of the while statement
     */
    public WhileStatement(Expression condition, Statement branch) {
        this.condition = condition;
        this.branch = branch;
    }

    /**
     * Get the condition of the while statement
     * @return the condition of the while statement
     */
    public Expression getCondition() {
        return condition;
    }

    /**
     * Get the branch of the while statement
     * @return the branch of the while statement
     */
    public Statement getBranch() {
        return branch;
    }
}
