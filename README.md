# php-rs

A minimalistic PHP interpreter.

## Overview

With this project I'm attempting to create a lightweight PHP interpreter built using Rust.
It's currently in its early stages and is meant for experimental and educational purposes.

## Features

- Basic arithmetic operations
- Conditional operators / boolean expressions

Expect this list to grow 😁

## Usage

Ensure you have Rust and Cargo installed on your system. Clone the repository and navigate to the project root:

```bash
git clone https://github.com/ZakFarmer/php-rs.git
cd php-rs
```

To run the interpreter:

```bash
cargo run
```

To run the testsuite:
```bash
cargo test
```

## Credits
The design of the project was heavily influenced by Thorsten Ball's "Writing an Interpreter in Go" and "Writing a Compiler in Go", of course adapted to Rust.
100% recommend reading both as they're great books if you're interested in language fundamentals.

## Contributing

Although this project isn't intended to be anything consequential and certainly won't ever be used in production, contributions for educational purposes or to address small issues are always welcome.
