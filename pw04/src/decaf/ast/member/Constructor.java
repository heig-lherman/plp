package decaf.ast.member;

import decaf.ast.statement.Block;
import decaf.ast.statement.expression.Identifier;

import java.util.Collections;
import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Constructor extends Member {

    protected final List<Parameter> parameters;
    protected final Block block;

    /**
     * Class representing a constructor
     * @param identifier the identifier of the constructor
     * @param block the block of the constructor
     */
    public Constructor(Identifier identifier, Block block) {
        this(identifier, Collections.emptyList(), block);
    }

    /**
     * Class representing a constructor
     * @param identifier the identifier of the constructor
     * @param parameters the parameters of the constructor
     * @param block the block of the constructor
     */
    public Constructor(Identifier identifier, List<Parameter> parameters, Block block) {
        super(identifier);
        this.parameters = parameters;
        this.block = block;
    }

    /**
     * Get the parameters of the constructor
     * @return the parameters of the constructor
     */
    public List<Parameter> getParameters() {
        return parameters;
    }

    /**
     * Get the block of the constructor
     * @return the block of the constructor
     */
    public Block getBlock() {
        return block;
    }
}
