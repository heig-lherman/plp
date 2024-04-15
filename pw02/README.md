# Set

The purpose of this assignment is to practice your understanding of higher-order functions in functional programming with Haskell. This includes the creation of lambda expressions, the composition of functions, point-free style programming, and partial application.

## Topic

The topic of the assignment is a functional set data structure. A set is a collection of distinct elements. In this assignment, we will represent sets as functions that map elements to booleans. For example, the set containing the elements 1, 2, and 3 can be represented as the following function:

```haskell
-- The set containing the elements 1, 2, and 3.
s :: Int -> Bool
s 1 = True
s 2 = True
s 3 = True
s _ = False
```

This representation enables the definition of higher-order functions that can perform operations on these sets. This could include operations such as union, intersection, difference, or subset.

## Task

The task of the assignment is developing an API for a functional set data structure using Haskell. You are provided with a code skeleton that you need to complete. The goal is to turn this code into a module, `Set`, that exposes the required features while preserving encapsulation.

Below is a partial list of functions you will need to implement (see [`set.hs`](src/set.hs) for the complete list):

- `empty`: Creates an empty set.
- `singleton`: Creates a set with a single element.
- `insert`: Adds an element to the set.
- `delete`: Removes an element from the set.
- `member`: Checks if an element is in the set.
- `size`: Returns the number of elements in the set.
- `subset`: Checks if a set is a subset of another set.
- `union`: Returns the union of two sets.
- `intersection`: Returns the intersection of two sets.
- `difference`: Returns the difference of two sets.
- `apply`: Applies a function to all elements in the set.
- `select`: Returns a set with elements that satisfy a predicate.
- `reduce`: Reduces the set to a single value by applying a binary function.
- `toList`: Converts the set to a list.
- `fromList`: Converts a list to a set.

Additionally, you need to implement a mechanism allowing Haskell to automatically convert sets into strings when necessary. The expected string representation for sets should be a pair of curly braces with inside all the values separated with commas. For example, an empty set should be represented as `{}`, a singleton set with the element 1 as `{1}`, and a set with the elements 1 and 2 as `{1,2}`.

You may find the `intercalate` function from the `Data.List` module useful when implementing the string representation feature. This function can be used to insert a specific string between each element in a list of strings.

Here's a simple example of how you might use `intercalate`:

```
ghci> import Data.List (intercalate)
ghci> xs = ["Hello", "world!"]
ghci> intercalate ", " xs
"Hello, world!"
```

You are free to use anything that was introduced or discussed in class for implementing the API. You can also use Haskell's extended standard library for lists if this can help you during implementation. However, you should minimize as much as possible converting functional sets into lists. Doing so goes against the whole purpose of the assignment. For some functionalities, there is no choice to do this conversion, but it can be done without for most of them.

Given the fact that we shall represent sets as functions mapping integers to booleans, this representation can induce significant performance overhead when dealing with large sets. To make things simple, we will restrict the possible values of a set within the bound `-1000` to `1000`. No need to implement checks for set values outside of this range.

## How-to

While working on your Haskell module, you don't need to compile it to test it. Instead, you can simply load it into the Haskell interpreter, GHCi. There are two ways to do this:

1. Start GHCi with the filename as a command-line argument.

```bash
% ghci set.hs
```

2. Start GHCi without any arguments, and then load the source file using the `:l` command (which is short for load).

```bash
% ghci
ghci> :l set.hs
```

In both cases, GHCi will load your module and you can then call your functions directly from the GHCi prompt to test them. Remember to reload your module with `:r` every time you make changes to the source file.

## Grading

The grading of the assignment will be based on the following criteria:

- **Correctness**: The code should compile without errors and perform the task as per the assignment's requirements. It should handle all edge cases and should not crash under any circumstances.
- **Efficiency**: The code should be optimized to run as efficiently as possible. This includes minimizing the time and space complexity of the code. It should not contain unnecessary computation or unused code.
- **Readability**: The code should be easy to read and understand. This includes proper indentation, the use of meaningful variable and function names, and the inclusion of comments to explain complex parts of the code.
- **Modularity**: The code should avoid code duplication and reuse implemented functionalities. This makes the code more maintainable, efficient, reliable, and readable.
- **Style**: The code should demonstrate an idiomatic functional programming style, employing suitable programming constructs to ensure the code is concise, modular, and easy to reason about.

Each criterion contributes a maximum of one point to the overall grade. The assignment grade is then computed using the following formula:

<p align="center">
$G = 1 + C_x + E_x + R_x + M_x + S_x$
</p>

where $C_x$, $E_x$, $R_x$, $M_x$, and $S_x$ represent the evaluation of your code based on each respective criterion.

## Deliverable

The deliverable for this assignment is a single Haskell file, named `set.hs`. This file should include a header that identifies the author(s) of the code. The assignment can be completed individually or in pairs. However, it is crucial to ensure that the work submitted is original and not plagiarized. Copying code from the internet or using solutions from other students is strictly prohibited and will be considered a breach of academic integrity.

## Deadline

The deadline for the assignment is **Tuesday, March 26, 2024, at 12:00**. Once this deadline passes, the repository will be locked for grading purposes, and late submissions will not be accepted under any circumstances.
