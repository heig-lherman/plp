package decaf.ast;

import decaf.ast.member.Member;
import decaf.ast.statement.expression.Identifier;

import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class ClassDeclaration extends Node {

    protected final Identifier name;
    protected final List<Member> members;

    /**
     * Class representing a class declaration
     * @param name the name of the class
     * @param members the members of the class
     */
    public ClassDeclaration(Identifier name, List<Member> members) {
        this.name = name;
        this.members = members;
    }
}
