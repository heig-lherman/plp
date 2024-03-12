# Word count

The purpose of this assignment is to practice your understanding of fundamental functional programming constructs in Haskell. This encompasses the definition of functions, the utilization of recursion, the manipulation of data structures, and the application of pattern matching.

## Topic

The topic of the assignment is the Unix command-line utility `wc`, which stands for "Word Count". The `wc` utility displays the number of lines, words, and bytes contained in each input file to the standard output.

A line is defined as a string of characters delimited by a newline character. Characters beyond the final newline character are not included in the line count. A word is defined as a string of characters delimited by white space characters.

If more than one input file is specified, a line of cumulative counts for all the files is displayed on a separate line after the output for the last file.

```bash
% printf 'one two\n' > foo
% printf 'three\nfour\n' > bar
% wc foo bar
       1       2       8 foo
       2       2      11 bar
       3       4      19 total
```

The output of the `wc` command is a table with three columns and three rows. Each row corresponds to a file (or a total for all files), and each column corresponds to a specific count: the lines, the words, and the bytes.

## Task

The assignment requires you to complete a partially implemented Haskell program that emulates the Unix command-line utility `wc`. The provided codebase includes the necessary logic to extract command-line arguments, read input files, and display the command output. Your task is to fill in the missing parts of the program:

- Counting lines, words, and bytes in a string.
- Summing a list of counts.
- Finding the maximum number of digits of a number in a list of counts.

Here, "count" refers to a tuple composed of three elements in Haskell. These elements represent, in order, the number of lines, words, and bytes found within a string.

Refrain from making any changes to the existing codebase provided for the assignment. The codebase is designed to provide a structured framework for your work. Any modifications could potentially disrupt this structure and affect the functionality of your program.

While the standard library can be a useful resource, for this assignment, we ask that you do not use it. We encourage you to implement all functionalities from scratch. The goal is to encourage you to think critically and foster the development of your problem-solving skills.

## How-to

Although the introduction to Haskell programs and the compilation process will be covered later in the course, you can compile and execute your code as follows:

```
% ghc wc.hs
[1 of 1] Compiling Main             ( wc.hs, wc.o )
Linking wc ...
% ./wc
usage: wc <file> ...
```

Alternatively, you can test individual functions from your code by loading your source file into the Haskell interpreter and invoking the functions you wish to check:

```
% ghci wc.hs
GHCi, version 9.2.5: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( wc.hs, interpreted )
Ok, one module loaded.
ghci> usage
usage: wc <file> ...
```

To test updated functions without restarting the interpreter, use the `:r` command to reload your source file.

## Grading

The grading of the assignment will be based on the following criteria:

- **Correctness**: The code should compile without errors and perform the task as per the assignment's requirements. It should handle all edge cases and should not crash under any circumstances.
- **Efficiency**: The code should be optimized to run as efficiently as possible. This includes minimizing the time and space complexity of the code. It should not contain unnecessary computation or redundant code.
- **Readability**: The code should be easy to read and understand. This includes proper indentation, the use of meaningful variable and function names, and the inclusion of comments to explain complex parts of the code.
- **Modularity**: The code should be well-structured and divided into functions, each performing a single task. This makes the code easier to read, understand, and maintain.
- **Style**: The code should demonstrate an idiomatic functional programming style, employing suitable programming constructs to ensure the code is concise, modular, and easy to reason about.

Each criterion contributes a maximum of one point to the overall grade. The assignment grade is then computed using the following formula:

<p align="center">
$G = 1 + C_x + E_x + R_x + M_x + S_x$
</p>

where $C_x$, $E_x$, $R_x$, $M_x$, and $S_x$ represent the evaluation of your code based on each respective criterion.

## Deliverable

The deliverable for this assignment is a single Haskell file, named `wc.hs`. This file should include a header that identifies the author(s) of the code. The assignment can be completed individually or in pairs. However, it is crucial to ensure that the work submitted is original and not plagiarized. Copying code from the internet or using solutions from other students is strictly prohibited and will be considered a breach of academic integrity.

## Deadline

The deadline for the assignment is **Tuesday, March 12, 2024, at 12:00**. Once this deadline passes, the repository will be locked for grading purposes, and late submissions will not be accepted under any circumstances.
