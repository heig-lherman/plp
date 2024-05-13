package decaf.ast.statement;

import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Block extends Statement {

    protected final List<Statement> statements;

    /**
     * Class representing a block
     * @param statements the statements of the block
     */
    public Block(List<Statement> statements) {
        this.statements = statements;
    }

    /**
     * Get the statements of the block
     * @return the statements of the block
     */
    public List<Statement> getStatements() {
        return statements;
    }
}
