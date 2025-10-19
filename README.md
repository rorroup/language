# Custom Scripting Language

I wrote this language for mainly 2 reasons: as a learning experience to practice coding, and as a mean to implement a certain level of scripting and parallelism into my own C++ programs.
I tried embedding other scripting languages before, but everytime I ended up using only a tiny portion of them yet lacking crucial features.
Therefore my goal was to make this language as simple as possible thus light and easy to edit.

The result is an interpreted language to allow hot realoading and faking asynchronism.
Its fundamental unit is the **Token** consisting of a tagged union.
Processing is carried out by the standard 3 main components.
The code is first read by the **Lexer** and turned into Tokens.
The **Parser** then rearranges them into a vector in [postfix](https://en.wikipedia.org/wiki/Reverse_Polish_notation) notation through [precedence](https://learn.microsoft.com/en-us/cpp/c-language/precedence-and-order-of-evaluation?view=msvc-170) [climbing](https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing).
Finally the **Interpreter** can execute it in order using a stack.

## Features

1. Interpreted language. Weakly typed. Scripts may be reloaded and are executed at runtime.
2. Scripts are any ASCII text file.
3. C like syntax.
4. Global scope and function local variables scopes.
5. Easy to use and customize.
6. Code execution can be interrupted arbitrarily invoking the AWAIT Token, then be resumed later from the same point.

<details>
<summary>Grammar</summary>
  
```
// Language Grammar.

<array>                 ::= [<<operation>, >*]                                  // Note 1.
<function_definition>   ::= function <name>? (<<name>, >*) { <expression>* }    // Note 1.
<atom>                  ::= <decimal_int> | 0x<hexadecimal_int> | 0b<binary_int> |
                            <floating_point.number> |
                            "<string>" |
                            <array> |
                            <anonymous_function> |
                            <name> |
                            (<operation>)
<index>                 ::= [<operation>]
<call>                  ::= (<<operation>, >*)                                  // Note 1.
<operand>               ::= <unary_operator>* <atom> <<index> | <call>>*
<operation>             ::= <operand> <<binary_operator> <operand>>*
<conditional>           ::= if (<operation>) { <expression>* }
                            <else if (<operation>) { <expression>* }>*
                            <else { <expression>* }>?
<loop>                  ::= < <for (<operation>? ; <operation>? ; <operation>?) { <expression>* }> |
                              <while (<operation>) { <expression>* }> |
                              <do { <expression>* } while (<operation>) ;>
                            > <else { <expression>* }>?
<expression>            ::= <<operation> ;> |                               // Note 2.
                            <conditional> |
                            <loop> |
                            <return <<operation>, >* ;> |                   // Note 1 and 2.
                            <await ;> |
                            <break ;> |
                            <continue ;> |
                            <label "<string>" ;> |
                            <goto <operation> ;>                            // Note 2.

// Note 1: The comma after the last element should be omitted. If there are no elements then there must not be any comma.
// Note 2: If the operation ends in a function_definition then the terminating semicolon may be omitted.
```

</details>

## Usage

Include all the files into your C++ project either by directly pasting them or as a git submodule.
The Language API has been encapsulated into the `Interpreter.h` header which contains the declarations, as well as the definitions subject to conditional compilation.
In order for it to work you must provide the implementation by defining the `LANGUAGE_IMPLEMENTATION` macro in a SINGLE cpp file.
The special subset of functions
```
int_tL LANGUAGE_initialize();
int_tL LANGUAGE_terminate();
int_tL LANGUAGE_reload();
```
is meant to guarantee the Language be left in a particular state.
`LANGUAGE_initialize` should make the symbols necesary to use the Language available thus be called before using it.
`LANGUAGE_terminate` on the other hand should unload all symbols to free the memory and so be called after the Language in not used anymore.
Their implementations can be skipped by not defining the `LANGUAGE_EXAMPLE_LOADING` macro in case the user prefers to write them themselves.

The Language is designed to be embedded into C++ and used in conjunction with it by merely including the library so that you encounter no need to edit its source code.
Instead I strongly suggest you split its implementation like any other. Create a `Language.h` file to include the Language library header
```
#define LANGUAGE_THREAD_PARAMETERS				// Additional Thread_tL members.
#include "pathToThe/Interpreter.h"
```
and add members to the `struct Thread_tL` definition as needed by listing them in the `LANGUAGE_THREAD_PARAMETERS` macro.
Then create a `Language.cpp` file to provide the necessary implementations
```
#define LANGUAGE_IMPLEMENTATION				    // Enable Language Library Implementation.
#define LANGUAGE_EXAMPLE_LOADING			    // Comment to provide 'LANGUAGE_<loading>' functions custom implementation.
//#define LANGUAGE_TEST_PROGRAM					// Uncomment to call library's 'int main()' function and run the example scripts.
#include "Language.h"
```
to complete the embedding.
This way you can include this header into your project with your own additional definitions so that everything remains consistent.
My advice is to disable `LANGUAGE_EXAMPLE_LOADING` and manually paste its contents to use as the starting point for development.
To test everything works you can uncomment the `LANGUAGE_TEST_PROGRAM` macro to run the [example program](language/Interpreter.h#L2024) with the [example srcipts](language/example), beware it will enable the library's internal `int main()` function though.

To utilize the language in your program call `LANGUAGE_initialize();` and `LANGUAGE_terminate();` before and after respectively to perform the necesary configurations.

Scripts may use all currently supported [types and operations](language/Interpreter.h#L116). Pre-defined functions must comply with their [signature](language/Interpreter.h#L1858).

## Development plan

Current Version: beta 1.1.0

> [!CAUTION]
> Backwards compatibility is not guaranteed yet.

As mentioned I am using the language myself, so I will be updating it and fixing bugs as I encounter them whenever I see fit.

I made it lightweight, fast and easy to use to the best of my ability. No bugs were found as of the latest version and every memory leak I could think of is fixed.

### TODO

1. Deprecate the 'function' argument from the Parser methods while keeping the 'labels' structure.
2. Implement increment/decrement operators (++, --).
3. Implement compound assignment operators (+=, -=, *=, etc).

### Known issues

The following currently pose no real problem so will likely stay as they are.

| Issue | Justification |
|-------|---------------|
| Only ASCII characters are supported so anything else will crash the tokenizer. | There is no immediate need to support other character sets. |
| Arbitrary curly braces scopes are not supported aside from delimiting control flow structures and functions. | Only functions affect variables' scope. |
| Control flow structures must always be used with curly braces, even when they only nest a single instruction. | This is mainly to disambiguate nested structures. |
| Logical statements are always fully evaluated. | The user can separate them accordingly. |

### Review

I require further using the language to decide.

1. May make labels identifier instead of string to simplify the parsing at the cost of their flexibility.
2. Check the possibility to return intermediate results upon awaiting a function.
3. Implement ternary operator (?:).
4. Check if there is a better implementation for the language ARRAY other than a std::vector wrapper.
5. Revisit the function unloading mechanism regarding its safety.

## References[^1]

1. [Make YOUR OWN Programming Language](https://youtube.com/playlist?list=PLZQftyCk7_SdoVexSmwy_tBgs7P0b97yD&si=69fO9rY6zqDwD1Zj) by CodePulse
2. [DIY Programming Language #1: The Shunting Yard Algorithm](https://youtu.be/unh6aK8WMwM?si=vLnCIaq3B6qpt7eS) by javidx9
3. [Multiplication Superiority (Compiler Pt.5)](https://youtu.be/6nl5HTGgvnk?si=0rpTdP9fYJhlgJnF) by Pixeled
4. [Reverse Polish Grows on Trees - Computerphile](https://youtu.be/TrfcJCulsF4?si=yyPwzlTKHFQIb4z7) by Computerphile


[^1]: Additional references in the code.
