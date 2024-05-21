import fs from 'fs';
import { program } from 'commander';
import { parse } from './parse.mjs';
import { Reporter } from './report.mjs';
import { tokenize } from './tokenize.mjs';
import { typecheck } from './typecheck.mjs';

// Parse command-line arguments
program
  .name('zcc')
  .description('A type-checker for the Z language', { filename: 'The source file to type-check' })
  .arguments('<filename>')
  .option('--show-tokens', 'Show the tokens', false)
  .option('--show-ast', 'Show the parsed AST', false)
  .action((filename) => {
    // Check that the filename ends in '.z'
    if (!filename.endsWith('.z')) {
      console.error(`Unrecognized file extension: ${filename}`);
      process.exit(1);
    }
  });

program.parse(process.argv);

const [filename] = program.args;
const options = program.opts();

// Read the source file
let sourceCode;
try {
  sourceCode = fs.readFileSync(filename, 'utf-8');
} catch (err) {
  console.log(`Failed to read ${filename}: ${err}`);
  process.exit(1);
}

// Create a reporter to print errors and warnings
const reporter = new Reporter(filename, sourceCode);

// Tokenize the source code
const tokens = tokenize(sourceCode, reporter);
if (options.showTokens) {
  console.log(JSON.stringify(tokens, null, 2));
}

// Parse the tokens into an AST
const ast = parse(tokens, reporter);
if (options.showAst) {
  console.log(JSON.stringify(ast, null, 2));
}

// Typecheck the program
typecheck(ast, reporter);
