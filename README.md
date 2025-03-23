# Lite Compiler

Lite Compiler is a simple compiler project aimed at demonstrating basic type checking and parsing functionalities. The goal of this project is to provide a foundational understanding of compiler construction, focusing on type checking and parsing.

> **Note:** This project is still under development and may contain bugs or incomplete features.

## Features

- Type checking for arithmetic operations (integer, double, coercion)
- Type checking for function definitions and applications
- Type checking for let variable and let function rules
- Type checking for lambda declarations
- Type checking for list construction and concatenation
- Type checking for numeric and boolean comparisons
- Pattern matching rules (excluding constructor patterns)

## Known Issues

- `import` statements are not supported yet.
- `data` and `type` declarations are not supported due to parser limitations.
- Potential bugs in function application type checking.

## Tech Stack

- OCaml
- Menhir (parser generator)
- Dune (build system)

## Installation and Prerequisites

1. **Install OCaml:**
   - Follow the instructions on the [OCaml website](https://ocaml.org/docs/install.html) to install OCaml.

2. **Install OPAM (OCaml Package Manager):**
   - Follow the instructions on the [OPAM website](https://opam.ocaml.org/doc/Install.html) to install OPAM.

3. **Install Dune:**
   - Run the following command to install Dune:
     ```sh
     opam install dune
     ```

4. **Install Menhir:**
   - Run the following command to install Menhir:
     ```sh
     opam install menhir
     ```

5. **Clone the Repository:**
   - Clone the project repository to your local machine:
     ```sh
     git clone <repository-url>
     cd lite-compiler
     ```

6. **Build the Project:**
   - Use Dune to build the project:
     ```sh
     dune build
     ```

## Contributors

- **Riw**
  - Implemented various type checking rules and refactored AST and parser.
- **Tee**
  - Added initial environment setup and various type checking rules.
- **Gong**
  - Contributed to function application type checking and test cases.