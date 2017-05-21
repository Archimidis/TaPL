# Untyped Lambda-Calculus (λ)

The untyped lambda-calculus is implemented here using De Bruijn indexes. The evaluation strategy is call-by-value.

## Operational Semantics

Syntax | Evaluation Rules (call-by-value)
-------|---------------------------------
![Lambda-calculus syntax](./doc/lambda_syntax.png?raw=true "Lambda-calculus syntax") | ![Lambda-Calculus evaluation rules](./doc/lambda_evaluation.png?raw=true "Lambda-Calculus evaluation rules")

## Project

The project is configured with stack. Here are some useful commands:

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Fire up the lambda calculus interpreter.
stack exec untypedlambda
```
