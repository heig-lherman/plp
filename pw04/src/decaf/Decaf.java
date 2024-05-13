package decaf;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import decaf.parser.Parser;
import decaf.reporter.Reporter;
import decaf.tokenizer.Tokenizer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

/**
 * The Decaf programming language compiler.
 */
public class Decaf {

    private static final Gson gson = new GsonBuilder().setPrettyPrinting().create();

    /**
     * Command line options.
     */
    private static class Options {

        private String filename;
        private boolean showTokens;
        private boolean showAst;

        /**
         * Constructor.
         */
        private Options(String filename, boolean showTokens, boolean showAst) {
            this.filename = filename;
            this.showTokens = showTokens;
            this.showAst = showAst;
        }

        /**
         * Parse the command line options.
         */
        static Options parse(String[] args) {

            /**
             * Check that we have at least one argument.
             */
            if (args.length == 0) {
                usage();
                System.exit(1);
            }

            /**
             * Check that the first argument is a .decaf file.
             */
            var filename = args[0];
            if (!filename.endsWith(".decaf")) {
                System.out.println("Expected a .decaf file");
                System.exit(1);
            }

            /**
             * Parse the rest of the arguments.
             */
            var showTokens = false;
            var showAst = false;
            for (var arg : Arrays.copyOfRange(args, 1, args.length)) {
                switch (arg) {
                    case "--show-tokens":
                        showTokens = true;
                        break;
                    case "--show-ast":
                        showAst = true;
                        break;
                    default:
                        System.out.println("Unknown option: " + arg);
                        usage();
                        System.exit(1);
                }
            }

            return new Options(filename, showTokens, showAst);
        }

        /**
         * Get the filename.
         */
        public String filename() {
            return filename;
        }

        /**
         * Show the tokens.
         */
        public boolean showTokens() {
            return showTokens;
        }

        /**
         * Show the abstract syntax tree.
         */
        public boolean showAst() {
            return showAst;
        }
    }

    /**
     * Print usage information.
     */
    private static void usage() {
        System.out.println("Usage: java Decaf <file> [options]");
        System.out.println("Options:");
        System.out.println("  --show-tokens  -  Show tokens");
        System.out.println("  --show-ast     -  Show AST");
    }

    /**
     * The main entry point.
     */
    public static void main(String[] args) {

        /**
         * Parse the command line options.
         */
        var options = Options.parse(args);
        var filename = options.filename();

        /**
         * Read the input file.
         */
        var input = "";
        try {
            input = Files.readString(Path.of(filename));
        } catch (Exception e) {
            System.out.println("Failed to read file: " + filename);
            System.out.println(e);
            System.exit(1);
        }

        /**
         * Create a reporter for reporting errors.
         */
        var reporter = new Reporter(filename, input);

        /** 
         * Tokenize the input string.
         */
        var tokenizer = new Tokenizer(input, reporter);
        var tokens = tokenizer.tokenize();
        if (options.showTokens()) {
            String json = gson.toJson(tokens);
            System.out.println(json);
        }

        /**
         * Parse the tokens into an abstract syntax tree.
         */
        var parser = new Parser(tokens, reporter);
        var ast = parser.parse();
        if (options.showAst()) {
            String json = gson.toJson(ast);
            System.out.println(json);
        }
    }
}
