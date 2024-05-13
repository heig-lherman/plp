package decaf.ast.member;

import decaf.ast.statement.expression.Expression;
import decaf.ast.statement.expression.Identifier;
import decaf.ast.type.TypeDeclaration;

/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */
public class Field extends Member {

    protected final TypeDeclaration type;
    protected final Expression initialValue;

    /**
     * Class representing a field in a class
     * @param type the type of the field
     * @param identifier the identifier of the field
     */
    public Field(TypeDeclaration type, Identifier identifier) {
        this(type, identifier, null);
    }

    /**
     * Class representing a field in a class
     * @param type the type of the field
     * @param identifier the identifier of the field
     * @param initialValue the initial value of the field
     */
    public Field(TypeDeclaration type, Identifier identifier, Expression initialValue) {
        super(identifier);
        this.type = type;
        this.initialValue = initialValue;
    }

    /**
     * Get the type of the field
     * @return the type of the field
     */
    public TypeDeclaration getType() {
        return type;
    }

    /**
     * Get the initial value of the field
     * @return the initial value of the field
     */
    public Expression getInitialValue() {
        return initialValue;
    }


    /**
     * Create a new field with the given expression as initial value
     * @param e the expression to use as initial value
     * @return a new field with the given expression as initial value
     */
    public Field with(Expression e) {
        return new Field(type, getName(), e);
    }
}
