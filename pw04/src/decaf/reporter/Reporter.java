package decaf.reporter;

/**
 * A class for reporting errors.
 */
public class Reporter {

    private final String filename;
    private final String fileContents;

    /**
     * Create a new reporter.
     * 
     * @param filename The name of the file being reported on.
     * @param fileContents The contents of the file being reported on.
     */
    public Reporter(String filename, String fileContents) {
        this.filename = filename;
        this.fileContents = fileContents;
    }

    /**
     * Report an error at the given offset.
     *
     * @param message The error message.
     * @param offset  The offset in the file where the error occurred.
     */
    public void report(String message, int offset) {
        var lineAndColumn = getLineAndColumn(fileContents, offset);
        var line = lineAndColumn[0];
        var column = lineAndColumn[1];
        System.out.println(String.format("%s:%d:%d: error: %s", filename, line, column, message));
        System.out.println(fileContents.split("\n")[line - 1]);
        System.out.println(" ".repeat(column - 1) + "^");
        System.exit(1);
    }

    /**
     * Get the line and column number for a given offset.
     *
     * @param content The content of the file.
     * @param offset  The offset in the file.
     * @return An array containing the line and column number.
     */
    private static int[] getLineAndColumn(String content, int offset) {
        var line = 1;
        var column = 1;
        for (var i = 0; i < offset; i++) {
            if (content.charAt(i) == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
        }
        return new int[]{line, column};
    }
}
