/**
 * Represents a scope environment, which keeps track of the variables and functions defined in the current scope.
 * It also has a reference to parent scopes allowing to check for shadowed variables.
 *
 * The environment also offers the possibility to store usage information for variables and functions, allowing the
 * checker to report unused variables and functions.
 *
 * @author Loïc Herman
 * @author Massimo Stefani
 */
export class Environment {

    /** @type {Environment} */
    #parent = null;
    /** @type {Environment[]} */
    #children = [];
    /** @type {Record<string, Typing>} */
    #typings = {};
    /** To keep track of the headers included in the current environment.
     *  @type {string[]} */
    #headers = [];

    /**
     * Create a new environment.
     * @param {Environment} [parent] - The parent environment (for nested scopes).
     */
    constructor(parent = null) {
        if (parent) {
            this.#parent = parent;
            this.#parent.#children.push(this);
        }
    }

    /**
     * Get the parent environment.
     * @return {Environment}
     */
    get parent() {
        return this.#parent;
    }

    /**
     * Define a variable in the current scope.
     * @param {string} name - The name of the variable.
     * @param {string} type - The type of the variable.
     * @param {LocationInfo} loc - The location of the variable declaration.
     * @param {boolean} [used=false] - Whether the variable has been used.
     * @param {'function'|'var'} [source='var'] - The source of the variable. ref. {@link VariableTyping.source}
     * @return {VariableTyping} - The generated typing
     */
    defineVariable(name, type, loc, used = false, source = 'var') {
        const typing = { kind: 'variable', type, loc, used, source };
        this.#typings[name] = typing;
        return typing;
    }

    /**
     * Define a function in the current scope.
     * @param {string} name - The name of the function.
     * @param {LocationInfo} loc - The location of the function declaration.
     * @param {string} returnType - The return type of the function.
     * @param {{name: string, type: string}[]} [params=[]] - The parameters of the function.
     * @param {string} [library] - The library of the function. ref. {@link stdlib}
     * @return {FunctionTyping} - The generated typing
     */
    defineFunction(name, loc, returnType, params = [], library) {
        const typing = {
            kind: 'function',
            returnType: { kind: 'type', type: returnType },
            params: params.map(({type}) => ({ kind: 'type', type })),
            loc,
            used: name === 'main', // The main function is always used
            returned: false,
            library,
        };

        this.#typings[name] = typing;
        return typing;
    }

    /**
     * Include a header file in the current scope.
     * @param {string} header - The header file to include.
     * @return {boolean} - Whether the header was included (false if it already exists).
     */
    includeHeader(header) {
        if (this.#headers.includes(header)) {
            return false;
        }

        this.#headers.push(header);
        return true;
    }

    /**
     * Define a function in the current scope, creating a new local scope.
     * @param {string} name - The name of the function.
     * @param {LocationInfo} loc - The location of the function declaration.
     * @param {string} returnType - The return type of the function.
     * @return {Environment} - The new local environment.
     */
    createFunctionScope(name, loc, returnType) {
        if (this.#parent !== null) {
            throw new Error('Function scopes can only be created from the global scope.');
        }

        this.defineFunction(name, loc, returnType);
        return new Environment(this);
    }

    /**
     * Add a parameter to a function.
     * @param {string} name - The name of the parameter.
     * @param {string} type - The type of the parameter.
     * @param {LocationInfo} loc - The location of the parameter declaration.
     * @return {VariableTyping} - The generated parameter typing.
     */
    addFunctionParam(name, type, loc) {
        const functionName = Object.keys(this.#parent.#typings).pop();
        if (!functionName) {
            throw new Error('No function to add parameter to.');
        }

        const functionTyping = this.#parent.#typings[functionName];
        if (functionTyping.kind !== 'function') {
            throw new Error('Cannot add parameters to a non-function.');
        }

        functionTyping.params.push({ kind: 'type', type });
        this.defineVariable(name, type, loc, false, 'function');
    }

    /**
     * Create a new block scope.
     * @return {Environment} - The new local environment.
     */
    createBlockScope() {
        return new Environment(this);
    }

    /**
     * Check if a variable or function with the given name is defined in the current scope.
     * @param name - The name of the variable or function.
     * @returns {boolean|Typing} - Whether the variable or function is undefined, or the typing if it is.
     */
    hasLocal(name) {
        return name in this.#typings
            ? this.#typings[name]
            : false;
    }

    /**
     * Check if a variable or function with the given name is defined in the current scope or any parent scope.
     * @param name - The name of the variable or function.
     * @returns {boolean|Typing} - Whether the variable or function is defined, or the typing if it is.
     */
    hasScope(name) {
        return name in this.#typings
            ? this.#typings[name]
            : !!this.#parent && this.#parent.hasScope(name);
    }

    /**
     * Get the current function typing, if currently inside a function scope.
     * @returns {FunctionTyping|null} - The typing of the current function, or null if not inside a function scope.
     */
    getCurrentFunction() {
        if (!this.#parent) {
            // We are in the global scope
            return null;
        }

        const root = this.#getRootEnvironment();
        const fn = Object.entries(root.#typings).pop();
        if (!fn || fn[1].kind !== 'function') {
            return null;
        }

        return fn[1];
    }

    /**
     * Get the variable or function with the given name, if it is defined in the current scope or any parent scope.
     * @param {string} name - The name of the variable or function.
     * @return {Typing|null} - The variable or function, or null if it is not found.
     */
    resolve(name) {
        if (name in this.#typings) {
            this.#typings[name].used = true;
            return this.#typings[name];
        }

        return this.#parent && this.#parent.resolve(name);
    }

    /**
     * Report any unused variables in the environment.
     * @param {import('./report.mjs').Reporter} reporter - The reporter to use.
     */
    reportUnusedVariables(reporter) {
        const usedLibraries = {};
        this.#doReportUnusedVariables(reporter, usedLibraries);

        // check for a totally unused library
        for (const [library, { loc, used }] of Object.entries(usedLibraries)) {
            if (!used) {
                reporter.warning(`Header file '${library}' included but never used.`, loc);
            }
        }
    }

    /**
     * @param {import('./report.mjs').Reporter} reporter
     * @param {Record<string, {loc: LocationInfo, used: boolean}>} usedLibraries
     * @internal
     */
    #doReportUnusedVariables(reporter, usedLibraries) {
        for (const [name, typing] of Object.entries(this.#typings)) {
            if (typing.kind === 'function' && typing.library) {
                if (!(typing.library in usedLibraries)) {
                    usedLibraries[typing.library] = { loc: typing.loc, used: false };
                }

                usedLibraries[typing.library].used ||= typing.used;
                continue;
            }

            if (typing.used) {
                continue;
            }

            if (typing.kind === 'function') {
                reporter.warning(`Function '${name}' declared but not used.`, typing.loc);
            } else if (typing.source === 'function') {
                reporter.warning(`Function parameter '${name}' declared but not used.`, typing.loc);
            } else {
                reporter.warning(`Variable '${name}' declared but not used.`, typing.loc);
            }
        }

        for (const child of this.#children) {
            child.#doReportUnusedVariables(reporter, usedLibraries);
        }
    }

    /** @return {Environment} */
    #getRootEnvironment() {
        return this.#parent ? this.#parent.#getRootEnvironment() : this;
    }
}

/**
 * @typedef {Object} LocationInfo
 * @property {Object} start
 * @property {number} start.line
 * @property {number} start.column
 * @property {Object} end
 * @property {number} end.line
 * @property {number} end.column
 */

/** @typedef {VariableTyping|FunctionTyping} Typing */

/**
 * @typedef {Object} VariableTyping
 * @property {'variable'} kind
 * @property {string} type
 * @property {boolean} used
 * @property {'function'|'var'} source
 * @property {LocationInfo} loc
 */

/**
 * @typedef {Object} FunctionTyping
 * @property {'function'} kind
 * @property {DirectTyping} returnType
 * @property {DirectTyping[]} params - The type parameters, in order.
 * @property {boolean} used
 * @property {boolean} returned
 * @property {LocationInfo} loc
 * @property {string} library
 */

// The following extensions of Typing are used in type-checking only.

/**
 * @typedef {Object} DirectTyping
 * @property {'type'} kind
 * @property {string} type
 */

/**
 * @typedef {Object} LiteralTyping
 * @property {'literal'} kind
 * @property {string} type
 * @property {string|number} value
 */
