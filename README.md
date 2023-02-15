# rulox

[![crates.io](https://img.shields.io/badge/crates.io-fc8d62?style=for-the-badge&labelColor=555555&logo=rust)](https://crates.io/crates/rulox) 
[![github](https://img.shields.io/badge/github-8da0cb?style=for-the-badge&labelColor=555555&logo=github)](https://github.com/Spartan2909/rulox)
[![docs.rs](https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs)](https://docs.rs/rulox/latest) <br>

`rulox` is a lightweight scripting language embedded in Rust. 
It is based on the Lox language from [Crafting Interpreters](https://craftinginterpreters.com/). 

# Examples
```rust
use rulox::*;

fn main() {
    lox! {
        var a = 5;

        print a + 2;
    }

    let b: f64 = a.try_into().unwrap();

    println!("{}", b);
}
```

```rust
use rulox::*;

fn main() {
    lox! {
        for (var i = 5; i > 0; i = i - 1) print i;
    }
}
```

```rust
use rulox::*;

fn main() {
    lox! {
        fun hello(name) {
            print "Hello " + name + "! :)"
        }

        fun add_one(num) {
            return num + 1;
        }
    }

    hello(LoxValue::from("Alice"));

    assert_eq!(add_one(LoxValue::from(3)), LoxValue::from(4));
}
```

```rust
use rulox::*;

fn main() {
    lox! {
        var people = ["Bob", "Alice", "John"];

        for (person in people) {
            print "Hello " + person + "!";
        }
    }
}
```

# Features

- [x] Variable declarations
- [x] Print statements
- [x] Control flow statements
- [x] Loops
- [x] `for ... in ...` loops
- [x] Indefinite loops and `break`
- [x] Functions

# Possible future features

- First-class functions and closures
- Object orientation
- Macros
