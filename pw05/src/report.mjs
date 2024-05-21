import chalk from 'chalk';

/**
 * Problem reporter
 */
class Reporter {

    /**
     * Create a reporter
     * @param {string} filename The name of the file being processed
     * @param {string} content The content of the file being processed
     */
    constructor(filename, content) {
        this.filename = filename;
        this.lines = content.split('\n');
    }

    /**
     * Report an error
     * @param {string} message The error message
     * @param {*} loc The location of the error
     */
    error(message, loc) {
        this.#report('error', message, loc);
    }

    /**
     * Report a warning
     * @param {string} message The warning message
     * @param {*} loc The location of the warning
     */
    warning(message, loc) {
        this.#report('warning', message, loc);
    }

    /**
     * Report an information message
     * @param {string} level The level of the information
     * @param {string} message The information message
     * @param {*} loc The location of the information
     */
    #report(level, message, loc) {
        const color = level === 'error' ? chalk.red : chalk.yellow;
        const { start: { line, column } } = loc;
        console.log(color(`${this.filename}:${line}:${column}: ${level}: ${message}`));
        console.log(color(this.lines[line - 1]));
        console.log(' '.repeat(column - 1) + '^');
    }
}

export { Reporter };
