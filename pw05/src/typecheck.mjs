/**
 * @author Loïc Herman
 * @author Massimo Stefani
 */

import { Reporter } from './report.mjs';
import { stdlib } from './stdlib.mjs';
import { Environment } from './environment.mjs';

/**
 * @param {Typing|LiteralTyping|DirectTyping} type1
 * @param {Typing|LiteralTyping|DirectTyping} type2
 * @returns {boolean}
 */
const areTypesCompatible = (type1, type2) => {
    if (type1.kind === 'function' || type2.kind === 'function') {
        return false;
    }

    if (type1.type === type2.type) {
        return true;
    }

    return isPointer(type1.type) && isPointer(type2.type);
};

/** @param {string} type */
const isPointer = (type) => type.endsWith('*');

/** @param {string} type */
const isInt = (type) => type === 'int';

/** @param {string} type */
const isChar = (type) => type === 'char';

/** @param {string} type */
const isVoid = (type) => type === 'void';

/**
 * @param {Identifier} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const checkIdentifierDefinition = ({ name, loc }, env, reporter) => {
    const localScope = env.hasLocal(name);
    const outerScope = env.hasScope(name);
    if (outerScope && !localScope) {
        reporter.warning(`Identifier '${name}' shadows a ${outerScope.kind} in an outer scope,`, loc);
        reporter.warning('previous declaration here.', outerScope.loc);
    } else if (localScope) {
        reporter.error(`Identifier '${name}' is already declared in this scope,`, loc);
        reporter.error('previous declaration here.', localScope.loc);
    }
};

/**
 * @param {Statement} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const checkUnreachableStatement = ({ loc }, env, reporter) => {
    if (env.getCurrentFunction()?.returned) {
        reporter.warning('Unreachable statement.', loc);
    }
};

/**
 * @param {FunctionTyping|DirectTyping} typing
 * @param {LocationInfo} loc
 * @param {Reporter} reporter
 * @return {boolean}
 */
const checkVoidExpression = ({ kind, type }, loc, reporter) => {
    if (kind === 'function') {
        // Ignore function typings as they are checked separately.
        return true;
    }

    if (isVoid(type)) {
        reporter.error('Void value not ignored as it ought to be.', loc);
        return false;
    }

    return true;
};

/**
 * @param {DirectTyping|LiteralTyping} leftTyping
 * @param {LocationInfo} leftLoc
 * @param {DirectTyping|LiteralTyping} rightTyping
 * @param {LocationInfo} rightLoc
 * @param {Reporter} reporter
 * @return {[string, string]|boolean}
 */
const checkTypePromotion = ({ type: leftType }, leftLoc, { type: rightType }, rightLoc, reporter) => {
    if (isChar(leftType) && isInt(rightType)) {
        reporter.warning('Implicit type promotion from \'char\' to \'int\'.', leftLoc);
        return [rightType, rightType];
    } else if (isChar(rightType) && isInt(leftType)) {
        reporter.warning('Implicit type promotion from \'char\' to \'int\'.', rightLoc);
        return [leftType, leftType];
    }

    return false;
};

/**
 * @param {Typing|DirectTyping|LiteralTyping} [typing]
 * @return {string}
 */
const printTyping = (typing) => {
    if (!typing) {
        return '<unknown>';
    }

    switch (typing.kind) {
        case 'type':
        case 'literal':
        case 'variable':
            return typing.type;
        case 'function':
            return `${typing.returnType}(${typing.params.join(', ')})`;
        default:
            throw new Error(`Unknown typing kind: ${typing.kind}`);
    }
};

/**
 * @param {Program} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitProgram = (node, env, reporter) => {
    node.includes.forEach(include => visitInclude(include, env, reporter));
    node.functions.forEach(func => visitFunction(func, env, reporter));
};

/**
 * @param {Include} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitInclude = ({ header, loc }, env, reporter) => {
    const headerName = visitIncludeHeader(header, env, reporter);
    Object.entries(stdlib[headerName] || []).forEach(
        ([name, { returnType, params }]) => env.defineFunction(
            name,
            loc,
            returnType,
            params,
            headerName,
        ),
    );
};

/**
 * @param {Header} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @return {string|void}
 */
const visitIncludeHeader = ({ name, loc }, env, reporter) => {
    if (!(name in stdlib)) {
        reporter.error(`Unknown header file: ${name}`, loc);
    }

    if (!env.includeHeader(name)) {
        reporter.warning(`Header file '${name}' is included more than once.`, loc);
    }

    return name;
};


/**
 * @param {FunctionNode} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitFunction = (node, env, reporter) => {
    if (env.hasLocal(node.id.name)) {
        reporter.error(`Function '${node.id.name}' defined more than once.`, node.id.loc);
    }

    const { type: returnType } = visitType(node.returnType);
    const localEnv = env.createFunctionScope(node.id.name, node.id.loc, returnType);

    node.params.forEach(param => visitParameter(param, localEnv, reporter));
    visitBlockStatement(node.body, localEnv, reporter);

    if (returnType !== 'void' && !localEnv.getCurrentFunction().returned) {
        reporter.warning(`Non-void function '${node.id.name}' does not always return a value.`, node.loc);
    }
};

/**
 * @param {Parameter} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitParameter = (node, env, reporter) => {
    checkIdentifierDefinition(node.id, env, reporter);
    const { type } = visitType(node.type);

    if (isVoid(type)) {
        reporter.error(`Function parameter '${node.id.name}' declared void.`, node.id.loc);
    }

    env.addFunctionParam(node.id.name, type, node.id.loc);
};

/**
 * @param {Type} node
 * @returns {DirectTyping}
 */
const visitType = ({ name }) => {
    return { kind: 'type', type: name };
};

/**
 * @param {Variable} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitVariable = (node, env, reporter) => {
    checkIdentifierDefinition(node.id, env, reporter);
    const { type } = visitType(node.type);

    if (isVoid(type)) {
        reporter.error(`Variable '${node.id.name}' declared void.`, node.id.loc);
    }

    env.defineVariable(node.id.name, type, node.id.loc);
};

/**
 * @param {IfStatement} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitIfStatement = (node, env, reporter) => {
    checkUnreachableStatement(node, env, reporter);

    const condType = visit(node.test, env, reporter);
    if (!condType) {
        return; // The condition type is unresolvable, but the cause should have been reported already.
    }

    const { type: testType, ...rest } = condType;
    if (isChar(testType)) {
        reporter.warning('Implicit type promotion from \'char\' to \'int\'.', node.test.loc);
    } else if (!isInt(testType)) {
        reporter.error('Condition in if statement must be of type promotable to \'int\'.', node.test.loc);
    }

    const constant = rest.kind === 'literal'
        ? isInt(testType) 
            ? rest.value !== 0
            : isChar(testType) && rest.value !== '\0'
        : undefined;
    if (constant !== undefined) {
        reporter.warning(`Condition always evaluates to '${constant}'.`, node.test.loc);
    }

    if (constant === false) {
        reporter.error('Unreachable branch.', node.consequent.loc);
    }

    const currentFunction = env.getCurrentFunction();
    const isPreReturned = currentFunction.returned;

    visit(node.consequent, env, reporter);

    const isPostConsequentReturned = currentFunction.returned;
    currentFunction.returned = false;

    if (node.alternate) {
        if (constant === true) {
            reporter.error('Unreachable branch.', node.alternate.loc);
        }

        visit(node.alternate, env, reporter);
    }

    const isPostAlternateReturned = !!node.alternate && currentFunction.returned;

    // Make sure that if a return occurred the subsequent code is truly unreachable.
    const bothBranchReturns = isPostConsequentReturned && isPostAlternateReturned;
    const constantConsequentReturns = constant === true && isPostConsequentReturned;
    const constantAlternateReturns = constant === false && isPostAlternateReturned;

    currentFunction.returned = isPreReturned
        || bothBranchReturns
        || constantConsequentReturns
        || constantAlternateReturns;
};

/**
 * @param {WhileStatement} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitWhileStatement = (node, env, reporter) => {
    checkUnreachableStatement(node, env, reporter);

    const condType = visit(node.test, env, reporter);
    if (!condType) {
        return; // The condition type is unresolvable, but the cause should have been reported already.
    }

    const { type: testType, ...rest } = condType;
    if (isChar(testType)) {
        reporter.warning('Implicit type promotion from \'char\' to \'int\'.', node.test.loc);
    } else if (!isInt(testType)) {
        reporter.error('Condition in while statement must be of type promotable to \'int\'.', node.test.loc);
    }

    const constant = rest.kind === 'literal'
        ? isInt(testType) 
            ? rest.value !== 0
            : isChar(testType) && rest.value !== '\0'
        : undefined;
    if (constant === false) {
        reporter.warning('Loop condition always evaluates to \'false\'.', node.test.loc);
        reporter.error('Unreachable loop body.', node.body.loc);
    }

    const currentFunction = env.getCurrentFunction();
    const isPreReturned = currentFunction.returned;

    visit(node.body, env, reporter);

    // we have no guarantee that the loop will run at least once
    currentFunction.returned = isPreReturned;
};

/**
 * @param {ReturnStatement} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitReturnStatement = (node, env, reporter) => {
    checkUnreachableStatement(node, env, reporter);

    const functionScope = env.getCurrentFunction();
    if (!functionScope) {
        // Theoretically, this should not be possible if the AST is well-constructed.
        // The language spec does not allow code to be present outside functions.
        throw new Error('Return statement outside of function.');
    }

    functionScope.returned = true;

    if (node.expr) {
        if (isVoid(functionScope.returnType.type)) {
            reporter.error('Void function cannot return a value.', node.expr.loc);
            return;
        }

        const exprType = visit(node.expr, env, reporter);
        if (!exprType) {
            return; // The expression type is unresolvable, but the cause should have been reported already.
        }

        if (!areTypesCompatible(functionScope.returnType, exprType)) {
            reporter.error(`Return type mismatch: expected '${printTyping(functionScope.returnType)}', got '${printTyping(exprType)}'.`, node.expr.loc);
        }

        return;
    }

    if (!isVoid(functionScope.returnType.type)) {
        reporter.error('Non-void function must return a value.', node.loc);
    }
};

/**
 * @param {BlockStatement} node
 * @param {Environment} env - It is expected that the caller will create a new environment for the block
 * @param {Reporter} reporter
 */
const visitBlockStatement = (node, env, reporter) => {
    checkUnreachableStatement(node, env, reporter);
    node.declarations.forEach(variable => visitVariable(variable, env, reporter));
    node.statements.forEach(statement => visit(statement, env, reporter));
};

/**
 * @param {EmptyStatement} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitEmptyStatement = (node, env, reporter) => {
    checkUnreachableStatement(node, env, reporter);
    reporter.warning('Extraneous semicolon.', node.loc);
};

/**
 * @param {ExpressionStatement} node
 * @param {Environment} env
 * @param {Reporter} reporter
 */
const visitExpressionStatement = (node, env, reporter) => {
    checkUnreachableStatement(node, env, reporter);
    visit(node.expr, env, reporter);
};

/**
 * @param {Literal} node
 * @returns {LiteralTyping}
 */
const visitLiteral = (node) => {
    if (typeof node.value === 'number') {
        return { kind: 'literal', type: 'int', value: node.value };
    } else if (typeof node.value === 'string') {
        return { kind: 'literal', type: 'char', value: node.value };
    }

    throw new Error(`Unknown literal type: ${typeof node.value}`);
};

/**
 * @param {UnaryExpression} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {DirectTyping|void}
 */
const visitUnaryExpression = ({ op, expr, loc }, env, reporter) => {
    const exprType = visit(expr, env, reporter);
    if (!exprType) {
        return; // The type is unresolvable, but the cause should have been reported already.
    }

    if (exprType.kind === 'function') {
        reporter.error('Cannot perform unary operations on functions.', loc);
        return;
    }

    if (!checkVoidExpression(exprType, expr.loc, reporter)) {
        return;
    }

    switch (op) {
        case '+':
        case '-':
            if (!isChar(exprType.type) && !isInt(exprType.type)) {
                reporter.error(`Unary operator '${op}' expects type 'int' or 'char', got '${printTyping(exprType)}'.`, expr.loc);
                return;
            }

            return { kind: 'type', type: 'int' };
        case '!':
            if (!isInt(exprType.type)) {
                reporter.error(`Cannot invert expression of type '${printTyping(exprType)}'.`, loc);
            }

            return { kind: 'type', type: 'int' };
        case '*':
            if (!isPointer(exprType.type)) {
                reporter.error(`Cannot dereference expression of type '${printTyping(exprType)}'`, loc);
            }

            return { kind: 'type', type: exprType.type.slice(0, -1) };
        case '&':
            return { kind: 'type', type: `${exprType}*` };
        default:
            // In theory, this should not be possible if the AST is well-constructed.
            throw new Error(`Unknown unary operator: ${op}`);
    }
};

/**
 * @param {BinaryExpression} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {DirectTyping|void}
 */
const visitBinaryExpression = ({ left, op, right, loc }, env, reporter) => {
    const leftExpr = visit(left, env, reporter);
    const rightExpr = visit(right, env, reporter);
    if (!leftExpr || !rightExpr) {
        return; // At least one type is unresolvable, but the cause should have been reported already.
    }

    const { type: leftType } = leftExpr;
    const { type: rightType } = rightExpr;

    if (leftExpr.kind === 'function' || rightExpr.kind === 'function') {
        reporter.error('Cannot perform binary operations on functions.', loc);
        return;
    }

    const leftTyping = { kind: 'type', type: leftType };
    const rightTyping = { kind: 'type', type: rightType };

    const leftNonVoid = checkVoidExpression(leftTyping, left.loc, reporter);
    const rightNonVoid = checkVoidExpression(rightTyping, right.loc, reporter);
    if (!leftNonVoid || !rightNonVoid) {
        return;
    }

    // Handle type promotion
    const promoted = checkTypePromotion(leftTyping, left.loc, rightTyping, right.loc, reporter);
    if (promoted) {
        const [leftPromoted, rightPromoted] = promoted;
        leftTyping.type = leftPromoted;
        rightTyping.type = rightPromoted;
    }

    // Handle pointer arithmetic
    if (op === '+' || op === '-') {
        if (isPointer(leftTyping.type) && isInt(rightTyping.type)) {
            return { kind: 'type', type: leftTyping.type };
        } else if (isInt(leftTyping.type) && isPointer(rightTyping.type)) {
            return { kind: 'type', type: rightTyping.type };
        }
    }

    if (!areTypesCompatible(leftTyping, rightTyping)) {
        reporter.error(`Type mismatch in binary expression: '${printTyping(leftTyping)}' left-hand expects '${printTyping(rightTyping)}' right-hand.`, loc);
        return;
    }

    switch (op) {
        case '+':
        case '-':
        case '*':
        case '/':
            if (isPointer(leftTyping.type)) {
                reporter.error(`Pointers are not allowed for operator '${op}', got '${printTyping(leftTyping)}'.`, loc);
                return;
            }

            return leftTyping;
        case '>':
        case '<':
        case '==':
        case '!=':
            return { kind: 'type', type: 'int' };
        case '&&':
        case '||':
            if (leftTyping.type !== 'int') {
                reporter.error(`Binary operator ${op} expects type 'int', got '${printTyping(leftTyping)}'.`, loc);
                return;
            }

            return { kind: 'type', type: 'int' };
        default:
            // In theory, this should not be possible if the AST is well-constructed.
            throw new Error(`Unknown binary operator: ${op}`);
    }
};

/**
 * @param {SizeofExpression} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {DirectTyping}
 */
const visitSizeofExpression = ({ arg, loc }, env, reporter) => {
    const type = visit(arg, env, reporter);

    if (type && type.kind === 'function') {
        reporter.error('sizeof cannot be called with function types.', loc);
    }

    // sizeof always returns int, so declare it as such even if the argument is invalid.
    // we might have an unresolvable argument, but the cause should have been reported already.
    return { kind: 'type', type: 'int' };
};

/**
 * @param {CallExpression} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {DirectTyping|void}
 */
const visitCallExpression = ({ callee, args, loc }, env, reporter) => {
    const funcType = visitIdentifier(callee, env, reporter);
    if (!funcType) {
        return; // The function type is unresolvable, but the cause should have been reported already.
    }

    if (funcType.kind !== 'function') {
        reporter.error(`Called object '${callee.name}' is not a function.`, loc);
        return;
    }

    if (funcType.params.length !== args.length) {
        reporter.error(`Argument count mismatch in call to '${callee.name}', expected ${funcType.params.length} arguments.`, loc);
        return funcType.returnType;
    }

    args.forEach((arg, index) => {
        const argType = visit(arg, env, reporter);
        const paramType = funcType.params[index];
        if (!areTypesCompatible(argType, paramType)) {
            reporter.error(
                `Argument type mismatch in call to '${callee.name}': `
                + `expected '${printTyping(paramType)}' at position ${index}, got '${printTyping(argType)}'.`,
                arg.loc,
            );
        }
    });

    return funcType.returnType;
};

/**
 * @param {AssignmentExpression} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {DirectTyping|FunctionTyping|void}
 */
const visitAssignmentExpression = (node, env, reporter) => {
    const leftType = visitIdentifier(node.left, env, reporter);
    if (!leftType) {
        return; // The left type is unresolvable, but the cause should have been reported already.
    }

    if (leftType.source === 'function') {
        reporter.warning(`Assignment to function parameter '${node.left.name}'.`, node.left.loc);
    }

    const rightType = visit(node.right, env, reporter);
    if (!rightType) {
        return; // same.
    }

    if (!checkVoidExpression(rightType, node.right.loc, reporter)) {
        return;
    }

    if (!areTypesCompatible(leftType, rightType)) {
        reporter.error(`Incompatible types in assignment: '${printTyping(leftType)}' cannot be assigned to '${printTyping(rightType)}'.`, node.loc);
    }

    return leftType;
};

/**
 * @param {Identifier} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {VariableTyping|FunctionTyping|void}
 */
const visitIdentifier = ({ name, loc }, env, reporter) => {
    const varType = env.resolve(name);
    if (!varType) {
        reporter.error(`Unknown identifier '${name}'.`, loc);
        return;
    }

    return varType;
};

/**
 * Note: one should prefer to use direct calls when the type of node is known.
 * @param {Node} node
 * @param {Environment} env
 * @param {Reporter} reporter
 * @returns {DirectTyping|FunctionTyping|LiteralTyping|void}
 */
const visit = (node, env, reporter) => {
    switch (node.kind) {
        case 'Program':
            return visitProgram(node, env, reporter);
        case 'Include':
            return visitInclude(node, env, reporter);
        case 'Header':
            return visitIncludeHeader(node, env, reporter);
        case 'Function':
            return visitFunction(node, env, reporter);
        case 'Parameter':
            return visitParameter(node, env, reporter);
        case 'Type':
            return visitType(node);
        case 'Variable':
            return visitVariable(node, env, reporter);
        case 'IfStatement':
            return visitIfStatement(node, env, reporter);
        case 'WhileStatement':
            return visitWhileStatement(node, env, reporter);
        case 'ReturnStatement':
            return visitReturnStatement(node, env, reporter);
        case 'BlockStatement':
            return visitBlockStatement(node, env.createBlockScope(), reporter);
        case 'EmptyStatement':
            return visitEmptyStatement(node, env, reporter);
        case 'ExpressionStatement':
            return visitExpressionStatement(node, env, reporter);
        case 'Literal':
            return visitLiteral(node);
        case 'UnaryExpression':
            return visitUnaryExpression(node, env, reporter);
        case 'BinaryExpression':
            return visitBinaryExpression(node, env, reporter);
        case 'SizeofExpression':
            return visitSizeofExpression(node, env, reporter);
        case 'CallExpression':
            return visitCallExpression(node, env, reporter);
        case 'AssignmentExpression':
            return visitAssignmentExpression(node, env, reporter);
        case 'Identifier':
            return visitIdentifier(node, env, reporter);
        default:
            throw new Error(`Unknown node kind: ${node.kind}`);
    }
};

/**
 * Type-check a Z program
 * @param {Program} ast The AST to type-check
 * @param {Reporter} reporter The reporter
 */
function typecheck(ast, reporter) {
    const env = new Environment();
    visit(ast, env, reporter);
    env.reportUnusedVariables(reporter);
}

export { typecheck };

/**
 * @typedef {Object} Node
 * @property {string} kind
 * @property {LocationInfo} loc
 */

/**
 * @typedef {Node} Program
 * @property {'Program'} kind
 * @property {Include[]} includes
 * @property {FunctionNode[]} functions
 */

/**
 * @typedef {Node} Include
 * @property {'Include'} kind
 * @property {Header} header
 */

/**
 * @typedef {Node} Header
 * @property {'Header'} kind
 * @property {string} name
 */

/**
 * @typedef {Node} FunctionNode
 * @property {'Function'} kind
 * @property {Type} returnType
 * @property {Identifier} id
 * @property {Parameter[]} params
 * @property {BlockStatement} body
 */

/**
 * @typedef {Node} Parameter
 * @property {'Parameter'} kind
 * @property {Type} type
 * @property {Identifier} id
 */

/**
 * @typedef {Node} Type
 * @property {'Type'} kind
 * @property {'int'|'char'|'void'|'pointer'} name
 */

/**
 * @typedef {Node} Variable
 * @property {'Variable'} kind
 * @property {Type} type
 * @property {Identifier} id
 */

/**
 * @typedef {IfStatement|WhileStatement|ReturnStatement|BlockStatement|EmptyStatement|ExpressionStatement} Statement
 */

/**
 * @typedef {Node} IfStatement
 * @property {'IfStatement'} kind
 * @property {Expression} test
 * @property {Statement} consequent
 * @property {Statement} [alternate]
 */

/**
 * @typedef {Node} WhileStatement
 * @property {'WhileStatement'} kind
 * @property {Expression} test
 * @property {Statement} body
 */

/**
 * @typedef {Node} ReturnStatement
 * @property {'ReturnStatement'} kind
 * @property {Expression} [expr]
 */

/**
 * @typedef {Node} BlockStatement
 * @property {'BlockStatement'} kind
 * @property {Variable[]} declarations
 * @property {Statement[]} statements
 */

/**
 * @typedef {Node} EmptyStatement
 * @property {'EmptyStatement'} kind
 */

/**
 * @typedef {Node} ExpressionStatement
 * @property {'ExpressionStatement'} kind
 * @property {Expression} expr
 */

/**
 * @typedef {Literal|UnaryExpression|BinaryExpression|SizeofExpression|CallExpression|AssignmentExpression|Identifier} Expression
 */

/**
 * @typedef {Node} Literal
 * @property {'Literal'} kind
 * @property {number|string} value
 */

/**
 * @typedef {Node} UnaryExpression
 * @property {'UnaryExpression'} kind
 * @property {'+'|'-'|'!'|'*'|'&'} op
 * @property {Expression} expr
 */

/**
 * @typedef {Node} SizeofExpression
 * @property {'SizeofExpression'} kind
 * @property {Expression|Type} arg
 */

/**
 * @typedef {Node} BinaryExpression
 * @property {'BinaryExpression'} kind
 * @property {Expression} left
 * @property {'+'|'-'|'*'|'/'|'>'|'<'|'=='|'!='|'&&'|'||'} op
 * @property {Expression} right
 */

/**
 * @typedef {Node} CallExpression
 * @property {'CallExpression'} kind
 * @property {Identifier} callee
 * @property {Expression[]} args
 */

/**
 * @typedef {Node} AssignmentExpression
 * @property {'AssignmentExpression'} kind
 * @property {Identifier} left
 * @property {Expression} right
 */

/**
 * @typedef {Node} Identifier
 * @property {'Identifier'} kind
 * @property {string} name
 */
