# Custom Scripting Language

I wrote this language for mainly 2 reasons: as a learning experience to practice coding, and as a mean to implement a certain level of scripting and parallelism into my own C++ programs.
I tried embedding other scripting languages before, but everytime I ended up using only a tiny portion of them yet lacking crucial features.
Therefore my goal was to make this language as simple as possible thus light and easy to customize.

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
5. Easy to use and modify.
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

`#include "Interpreter.h"`, then `LANGUAGE_initialize();` so the language becomes available and `LANGUAGE_terminate();` to clean up after it is no longer used.

It is designed to be embedded into C++ and used in conjunction with it so you probably want to `file_import("your_script_name")` to load some global values and functions, or `file_load("your_script_name")` to grab a function to call whenever needed.
Check the [example program](language/language.cpp) and [example srcipts](language/example).

All supported types and operations are listed in [Interpreter.h](language/Interpreter.h#L116) while pre-defined functions can be found and modified in [Builtin.cpp](language/Builtin.cpp#L133).

## Development plan

Current Version: beta 1.0.0

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
