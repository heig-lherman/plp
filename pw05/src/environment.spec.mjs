import test from 'ava';
import { Environment } from './environment.mjs';

const createLocation = () => ({
    start: { line: 1, column: 1 },
    end: { line: 1, column: 10 },
});

test('defineVariable - should define a variable in the current scope', t => {
    const env = new Environment();
    const loc = createLocation();
    const typing = env.defineVariable('x', 'int', loc);

    t.deepEqual(typing, { kind: 'variable', type: 'int', loc, used: false, source: 'var' });
    t.deepEqual(env.hasLocal('x'), typing);
});

test('defineFunction - should define a function in the current scope', t => {
    const env = new Environment();
    const loc = createLocation();
    const typing = env.defineFunction('foo', loc, 'void', []);

    t.deepEqual(typing, {
        kind: 'function',
        returnType: { kind: 'type', type: 'void' },
        params: [],
        loc,
        used: false,
        returned: false,
        library: undefined,
    });
    t.deepEqual(env.hasLocal('foo'), typing);
});

test('includeHeader - should include a header in the current scope', t => {
    const env = new Environment();
    t.true(env.includeHeader('stdio.h'));
    t.false(env.includeHeader('stdio.h')); // Include the same header again should return false
});

test('createFunctionScope - should create a new function scope', t => {
    const env = new Environment();
    const loc = createLocation();
    const funcEnv = env.createFunctionScope('foo', loc, 'void');

    t.not(funcEnv, env);
    t.true(funcEnv instanceof Environment);
    t.deepEqual(env.hasLocal('foo'), {
        kind: 'function',
        returnType: { kind: 'type', type: 'void' },
        params: [],
        loc,
        used: false,
        returned: false,
        library: undefined,
    });
});

test('addFunctionParam - should add a parameter to the current function', t => {
    const env = new Environment();
    const loc = createLocation();
    const funcEnv = env.createFunctionScope('foo', loc, 'void');
    funcEnv.addFunctionParam('param1', 'int', loc);

    const funcTyping = env.hasLocal('foo');
    t.deepEqual(funcTyping.params, [{ kind: 'type', type: 'int' }]);
    t.deepEqual(funcEnv.hasLocal('param1'), { kind: 'variable', type: 'int', loc, used: false, source: 'function' });
});

test('createBlockScope - should create a new block scope', t => {
    const env = new Environment();
    const blockEnv = env.createBlockScope();

    t.not(blockEnv, env);
    t.true(blockEnv instanceof Environment);
    t.is(blockEnv.parent, env);
});

test('hasLocal - should check if a variable or function is defined in the current scope', t => {
    const env = new Environment();
    const loc = createLocation();
    env.defineVariable('x', 'int', loc);
    const blockEnv = env.createBlockScope();

    t.truthy(env.hasLocal('x'));
    t.false(blockEnv.hasLocal('x'))
    t.false(env.hasLocal('y'));
});

test('hasScope - should check if a variable or function is defined in the current or parent scope', t => {
    const env = new Environment();
    const loc = createLocation();
    env.defineVariable('x', 'int', loc);
    const blockEnv = env.createBlockScope();

    t.truthy(blockEnv.hasScope('x'));
    t.false(blockEnv.hasScope('y'));
});

test('getCurrentFunction - should get the current function typing if inside a function scope', t => {
    const env = new Environment();
    const loc = createLocation();
    const funcEnv = env.createFunctionScope('foo', loc, 'void');

    t.deepEqual(funcEnv.getCurrentFunction(), {
        kind: 'function',
        returnType: { kind: 'type', type: 'void' },
        params: [],
        loc,
        used: false,
        returned: false,
        library: undefined,
    });
});

test('resolve - should get the variable or function with the given name', t => {
    const env = new Environment();
    const loc = createLocation();
    env.defineVariable('x', 'int', loc);

    t.deepEqual(env.resolve('x'), { kind: 'variable', type: 'int', loc, used: true, source: 'var' });
    t.is(env.resolve('y'), null);
});

test('reportUnusedVariables - should report unused variables', t => {
    const env = new Environment();
    const loc = createLocation();
    env.defineVariable('x', 'int', loc);

    const reporter = {
        warnings: [],
        warning(message, location) {
            this.warnings.push({ message, location });
        },
    };

    env.reportUnusedVariables(reporter);

    t.deepEqual(reporter.warnings, [
        { message: "Variable 'x' declared but not used.", location: loc }
    ]);
});