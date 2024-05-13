/*
Language spec:

<stmt> ::= <ident> ':=' <expr>
        |  <stmt> ';' <stmt>
        | 'if' <expr> 'then' <stmt> ['else' <stmt>]
        | 'while' <expr> 'do' <stmt>

<expr> ::= <boolean>
        |  <number>
        |  <ident>
        |  ('!'|'-') <expr>
        |  <expr> ('+'|'-'|'*') <expr>

1. Define the abstract syntax tree classes for the language.
2. Define an Env class to store value bindings.
3. Define the evaluate method for expressions.
4. Define the execute method for statements.
*/

import java.util.Optional;
import java.util.HashMap;
import java.util.Map;

abstract class Expr {

    public abstract Value evaluate(Env environment);
}

class Bool extends Expr {
    private final boolean value;

    Bool(boolean value) {
        this.value = value;
    }

    @Override
    public Value evaluate(Env environment) {
        return new BoolValue(value);
    }
}

class Num extends Expr {
    private final int value;

    Num(int value) {
        this.value = value;
    }

    @Override
    public Value evaluate(Env environment) {
        return new NumValue(value);
    }
}

class Ident extends Expr {
    private final String name;

    Ident(String name) {
        this.name = name;
    }

    @Override
    public Value evaluate(Env environment) {
        return environment
            .lookup(name)
            .orElseThrow(() -> new RuntimeException("Undefined identifier: " + name));
    }
}

class Unary extends Expr {
    private final char op;
    private final Expr expr;

    Unary(char op, Expr expr) {
        this.op = op;
        this.expr = expr;
    }

    @Override
    public Value evaluate(Env environment) {
        Value value = expr.evaluate(environment);
        return switch (op) {
            case '!' -> new BoolValue(!value.asBool());
            case '-' -> new NumValue(-value.asInt());
            default -> throw new UnsupportedOperationException("Unsupported unary operator: " + op);
        };
    }
}

class Binary extends Expr {
    private final Expr left;
    private final char op;
    private final Expr right;

    Binary(Expr left, char op, Expr right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }

    @Override
    public Value evaluate(Env environment) {
        Value leftValue = left.evaluate(environment);
        Value rightValue = right.evaluate(environment);
        return switch (op) {
            case '+' -> new NumValue(leftValue.asInt() + rightValue.asInt());
            case '-' -> new NumValue(leftValue.asInt() - rightValue.asInt());
            case '*' -> new NumValue(leftValue.asInt() * rightValue.asInt());
            default -> throw new UnsupportedOperationException("Unsupported binary operator: " + op);
        };
    }
}

abstract class Stmt {

    abstract void execute(Env environment);
}

class Assign extends Stmt {
    private final String name;
    private final Expr expr;

    Assign(String name, Expr expr) {
        this.name = name;
        this.expr = expr;
    }

    @Override
    public void execute(Env environment) {
        Value value = expr.evaluate(environment);
        environment.upsert(name, value);
    }
}

class Seq extends Stmt {
    private final Stmt left;
    private final Stmt right;

    Seq(Stmt left, Stmt right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public void execute(Env environment) {
        left.execute(environment);
        right.execute(environment);
    }
}

class If extends Stmt {
    private final Expr cond;
    private final Stmt then;
    private final Optional<Stmt> els;

    If(Expr cond, Stmt then) {
        this.cond = cond;
        this.then = then;
        this.els = Optional.empty();
    }

    If(Expr cond, Stmt then, Stmt els) {
        this.cond = cond;
        this.then = then;
        this.els = Optional.of(els);
    }

    @Override
    public void execute(Env environment) {
        if (cond.evaluate(environment).asBool()) {
            then.execute(environment);
        } else {
            els.ifPresent(stmt -> stmt.execute(environment));
        }
    }
}

class While extends Stmt {
    private final Expr cond;
    private final Stmt body;

    While(Expr cond, Stmt body) {
        this.cond = cond;
        this.body = body;
    }
    
    @Override
    public void execute(Env environment) {
        while (cond.evaluate(environment).asBool()) {
            body.execute(environment);
        }
    }
}

abstract class Value {
    public int asInt() {
        return switch (this) {
            case BoolValue b -> b.getValue() ? 1 : 0;
            case NumValue n -> n.getValue();
            default -> throw new AssertionError();
        };
    }

    public boolean asBool() {
        return switch (this) {
            case BoolValue b -> b.getValue();
            case NumValue n -> n.getValue() != 0;
            default -> throw new AssertionError();
        };
    }
}

class BoolValue extends Value {
    private final boolean value;

    BoolValue(boolean value) {
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }
}

class NumValue extends Value {
    private final int value;

    NumValue(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}

class Env {
    private final Map<String, Value> delegate = new HashMap<>();

    public void upsert(String name, Value value) {
        delegate.put(name, value);
    }

    public Optional<Value> lookup(String name) {
        return Optional.ofNullable(delegate.get(name));
    }
}
