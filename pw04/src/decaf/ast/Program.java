package decaf.ast;

import java.util.List;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Program extends Node {

    protected final List<ClassDeclaration> classes;

    /**
     * Class representing a program
     * @param classes the classes of the program
     */
    public Program(List<ClassDeclaration> classes) {
        this.classes = classes;
    }
}
