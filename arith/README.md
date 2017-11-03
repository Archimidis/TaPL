# Untyped Arithmetic Expressions (arith)

Here's the syntax of the language:
```
t ::=
        true
        false
        0
        succ t
        pred t
        iszero t
        if t then t else t
```

Note that with the current implementation, meaningless expressions such as `succ true` and `if 0 the false else 0` are allowed. You will see no parse errors in the REPL. However, the evaluation will not have any effect on them.

Concerning evaluation, one-step and big-step strategies are implemented. The one-step evaluation is used in the REPL.

## Project

The project is configured with stack. Here are some useful commands:

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Fire up the interpreter.
stack exec arith
```
