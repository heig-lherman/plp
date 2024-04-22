# CSV Toolkit

The purpose of this assignment is to deepen your understanding of input/output (IO) operations and type definitions in the functional programming language, Haskell. This assignment will provide you with practical experience in compiling Haskell code, structuring Haskell programs through modules, defining new types, and manipulating files.

## Topic

The topic of the assignment is comma-separated values (CSV), a widely used, simple file format that stores tabular data (numbers and text) in plain text. Each line of the file is a data record, and each record consists of one or more fields, separated by commas. This format is used to exchange data between disparate applications and is a popular choice due to its simplicity and wide support.

For instance, consider a CSV file that stores information about books. Each line represents a book, and the fields could be the title, author, and publication year. A snippet of such a CSV file might look like this:

```
Title,Author,Year
To Kill a Mockingbird,Harper Lee,1960
The Great Gatsby,F. Scott Fitzgerald,1925
Moby Dick,Herman Melville,1851
```

There are numerous tools and libraries available in various programming languages to manipulate and process CSV files. These tools provide features to read, write, and manipulate CSV data efficiently, handling the parsing and formatting of the data, and dealing with common issues such as handling special characters and different file encodings.

## Task

The task of the assignment involves creating a command-line utility named `csvkit` using Haskell. This utility will be designed to perform a variety of operations on CSV files. For simplicity, we will only consider the comma as the delimiter in the CSV files. Also, we will assume that all fields are plain text, not enclosed in quotation marks.

The operations that the utility should support are as follows:

1. **Filter a CSV according to predicates**: This operation will filter the lines of the CSV file based on specified predicates of the form `column=value`.
2. **Format a CSV as a string**: This operation will format the CSV file as a string, aligning each column according to the longest value within that column.
3. **Join two or more CSVs on a column**: This operation will combine two or more CSV files based on a common column. This is similar to the 'JOIN' operation in SQL.
4. **Merge two or more CSVs**: This operation will merge two or more CSV files into a single one by appending rows from one file to another. It's important to ensure that the CSV files have the same structure.
5. **Sort a CSV by columns**: This operation will sort the CSV file in ascending order, starting with the first column, then the second, and so on.

The `csvkit` utility should be designed to take a CSV file as a command-line argument and parse it into an appropriate structured representation. This representation will then be used to carry out the specified operations. Rather than generating new files, the result of each operation should be printed to the standard output in a formatted fashion.

Additionally, the utility should also be capable of printing a usage message. This message should clearly describe how to use the CSV command-line utility, providing users with guidance on how to perform each of the supported operations:

```
% ./csvkit
Usage: csvkit <operation>
Operations:
  -grep   <file> <column>=<value> ... | Filter a CSV according to predicates.
  -format <file>                      | Format a CSV aligning each column.
  -join   <file> <file>... <column>   | Join a CSV with one or more CSVs on a column.
  -merge  <file> <file>...            | Merge two or more CSVs by appending one after another.
  -sort   <file> <column> ...         | Sort a CSV by columns in ascending order.
```

## How-to

For this task, you have the flexibility to design and implement the CSV toolkit in any way you see fit. You are permitted to utilize the Haskell standard library to meet any requirements you may encounter. However, it's important to remember that you should only use language constructs and features that have been introduced or discussed during our classes. Ultimately, you must comprehend any functionality you utilize, both in terms of its programmatic usage and its internal workings. If any part of your code seems to be beyond the scope of what has been covered in class, be prepared to explain that section in detail.

## Grading

The grading of the assignment will be based on the following criteria:

- **Correctness**: The code should compile without errors and perform the task as per the assignment's requirements. It should handle all edge cases and should not crash under any circumstances.
- **Efficiency**: The code should be optimized to run as efficiently as possible. This includes minimizing the time and space complexity of the code. It should not contain unnecessary computation or redundant code.
- **Readability**: The code should be easy to read and understand. This includes proper indentation, the use of meaningful variable and function names, and the inclusion of comments to explain complex parts of the code.
- **Modularity**: The code should be well-structured and divided into functions and modules, each performing a single task. This makes the code easier to read, understand, and maintain.
- **Style**: The code should demonstrate an idiomatic functional programming style, employing suitable programming constructs to ensure the code is concise, modular, and easy to reason about.

Each criterion contributes a maximum of one point to the overall grade. The assignment grade is then computed using the following formula:

<p align="center">
$G = 1 + C_x + E_x + R_x + M_x + S_x$
</p>

where $C_x$, $E_x$, $R_x$, $M_x$, and $S_x$ represent the evaluation of your code based on each respective criterion.

## Deliverable

The deliverable for this assignment is a Haskell program, named `csvkit.hs`. You may structure the program across as many files as necessary. Each file should include a header that identifies the author(s) of the code. The assignment can be completed individually or in pairs. However, it is crucial to ensure that the work submitted is original and not plagiarized. Copying code from the internet or using solutions from other students is strictly prohibited and will be considered a breach of academic integrity.

## Deadline

The deadline for the assignment is **Tuesday, April 16, 2024, at 12:00**. Once this deadline passes, the repository will be locked for grading purposes, and late submissions will not be accepted under any circumstances.
