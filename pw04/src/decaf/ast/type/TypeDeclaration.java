package decaf.ast.type;

import decaf.ast.Node;
import java.util.Objects;

/**
 * Represents a type declaration in the abstract syntax tree.
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class TypeDeclaration extends Node {

    protected final String identifier;

    /**
     * Creates a new type declaration.
     * @param identifier the identifier of the type
     */
    public TypeDeclaration(String identifier) {
        this.identifier = identifier;
    }

    /**
     * Get the identifier of the type.
     * @return the identifier of the type
     */
    public String getIdentifier() {
        return identifier;
    }

    @Override
    public final boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TypeDeclaration type)) return false;
        return Objects.equals(identifier, type.identifier);
    }

    @Override
    public final int hashCode() {
        return Objects.hashCode(identifier);
    }
}
