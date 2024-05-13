package decaf.ast.member;

import decaf.ast.Node;
import decaf.ast.statement.expression.Identifier;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public abstract class Member extends Node {

    protected final Identifier name;

    /**
     * Class representing a member of a class
     * @param name the name of the member
     */
    protected Member(Identifier name) {
        this.name = name;
    }

    /**
     * Get the name of the member
     * @return the name of the member
     */
    public Identifier getName() {
        return name;
    }
}
