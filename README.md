[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/mvvId2ep)

# Responsibilities

## Riw

- [PR #2](https://github.com/261406-2566-2/a3-typechecking-team/pull/2)
  - apply a2 progress (just copy/paste)
- [PR #3](https://github.com/261406-2566-2/a3-typechecking-team/pull/3)
  - refactor some AST to be a record
  - refactor parser a little bit
  - add type checking for ARITH-INT, ARITH-DOUBLE, ARITH-COERCE-L, ARITH-COERCE-R rule
  - add type checking for function definition
  - support Pair in type checking
- [PR #8](https://github.com/261406-2566-2/a3-typechecking-team/pull/8)
  - add type checking for LET-VAR rule
- [PR #9](https://github.com/261406-2566-2/a3-typechecking-team/pull/9)
  - add type checking for LAMBDA-DECL rule, add test cases, and bug fixes
- [PR #10](https://github.com/261406-2566-2/a3-typechecking-team/pull/10)
  - add type checking for FUNC-COMP rule and add test cases
- [PR #11](https://github.com/261406-2566-2/a3-typechecking-team/pull/11)
  - check for duplicated functions
  - pass `env` to use in function type check
- [PR #14](https://github.com/261406-2566-2/a3-typechecking-team/pull/14)
  - fix type check order bug
- [PR #15](https://github.com/261406-2566-2/a3-typechecking-team/pull/15)
  - add type checking for LET-FUNC rule and add test cases

## Tee

- [PR #4](https://github.com/261406-2566-2/a3-typechecking-team/pull/4)
  - add `init_env`: used in initialize symbol table
- [PR #5](https://github.com/261406-2566-2/a3-typechecking-team/pull/5)
  - add NUMERIC-COMP typechecking rule (number comparison)
  - add COMP typechecking rule (equality comparison)
  - add BOOL typechecking rule (and, or operator)
- [PR #6](https://github.com/261406-2566-2/a3-typechecking-team/pull/6)
  - add LIST-CONS typechecking rule (List construction: appending element in front of list)
  - add LIST-CONCAT typechecking rule (List concatenation)
- [PR #7](https://github.com/261406-2566-2/a3-typechecking-team/pull/7)
  - refactor `env`
  - create `lookup` function for symbol tables
- [PR #13](https://github.com/261406-2566-2/a3-typechecking-team/pull/13)
  - add pattern-matching rules: all except CNSTR-PTRN rule (Constructor pattern)
- [PR #17](https://github.com/261406-2566-2/a3-typechecking-team/pull/17)
  - add testcase

## Gong

- [PR #12](https://github.com/261406-2566-2/a3-typechecking-team/pull/12)
  - add `FUNC-APP-L `: typechecking rule
  - add `FUNC-APP-R `: typechecking rule
- [PR #19](https://github.com/261406-2566-2/a3-typechecking-team/pull/19)
  - add testcase
- Ask for help and knowledge from `Tee` & `Riw`
- In finally gong can understand Ocaml and Functional

# Issues

- `import` not supported yet
- `data` and `type` will not be supported yet since we found out that our parser lacks constructor expression. The refactoring process will take a long time because we mostly reuse some nodes in most grammar rules, making it hard to refactor. `cnstr-ptrn` in pattern rules will also not be supported.
- `FUNC-APP` `L & R` may bug on some Func but gong test เยอะสุดๆ

# Comments
