package decaf.ast.member;

import decaf.ast.statement.Block;
import decaf.ast.statement.expression.Identifier;
import decaf.ast.type.TypeDeclaration;
import java.util.Collections;
import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Method extends Member {

    protected final TypeDeclaration returnType;
    protected final List<Parameter> parameters;
    protected final Block block;

    /**
     * Class representing a method in a class
     * @param returnType the return type of the method
     * @param identifier the identifier of the method
     * @param block the block of the method
     */
    private Method(TypeDeclaration returnType, Identifier identifier, Block block) {
        this(returnType, identifier, Collections.emptyList(), block);
    }

    /**
     * Class representing a method in a class
     * @param returnType the return type of the method
     * @param identifier the identifier of the method
     * @param parameters the parameters of the method
     * @param block the block of the method
     */
    public Method(TypeDeclaration returnType, Identifier identifier, List<Parameter> parameters, Block block) {
        super(identifier);
        this.returnType = returnType;
        this.parameters = parameters;
        this.block = block;
    }

    /**
     * Get the return type of the method
     * @return the return type of the method
     */
    public TypeDeclaration getReturnType() {
        return returnType;
    }

    /**
     * Get the parameters of the method
     * @return the parameters of the method
     */
    public List<Parameter> getParameters() {
        return parameters;
    }

    /**
     * Get the block of the method
     * @return the block of the method
     */
    public Block getBlock() {
        return block;
    }
}
