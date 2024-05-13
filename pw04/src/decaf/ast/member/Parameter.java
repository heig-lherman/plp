package decaf.ast.member;

import decaf.ast.Node;
import decaf.ast.statement.expression.Identifier;
import decaf.ast.type.TypeDeclaration;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Parameter extends Node {

    protected final TypeDeclaration type;
    protected final Identifier identifier;

    /**
     * Class representing a parameter
     * @param type the type of the parameter
     * @param identifier the identifier of the parameter
     */
    public Parameter(TypeDeclaration type, Identifier identifier) {
        this.type = type;
        this.identifier = identifier;
    }

    /**
     * Get the type of the parameter
     * @return the type of the parameter
     */
    public TypeDeclaration getType() {
        return type;
    }

    /**
     * Get the identifier of the parameter
     * @return the identifier of the parameter
     */
    public Identifier getIdentifier() {
        return identifier;
    }
}
