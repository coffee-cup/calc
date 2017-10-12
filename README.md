# Calc

A simple interpreter for arithmetic expressions. Implemented as

- A parser using Parsec for strings, such as "2 * (3 + 5^2)"
- An evaluator for ASTs of the language

```
stack build
stack exec calc

calc> -(4 * -.2^2) * 100
-16.00
```