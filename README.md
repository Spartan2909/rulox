# rulox

[![crates.io](https://img.shields.io/badge/crates.io-555555?logo=rust&logoColor=fc8d62)](https://crates.io/crates/rulox) 
[![github](https://img.shields.io/badge/github-555555?logo=github&logoColor=8da0cb)](https://github.com/Spartan2909/rulox)
[![docs.rs](https://img.shields.io/badge/docs.rs-555555?logo=docs.rs&logoColor=66c2a5)](https://docs.rs/rulox/latest) <br />

`rulox` is a lightweight scripting language embedded in Rust. It is based on the Lox language from
[Crafting Interpreters](https://craftinginterpreters.com/).

## Basic use

Add `use rulox::prelude::*` at the top level of each module you want to use rulox in, then invoke
the `lox` macro with your Lox code. Note that due to technical limitations, the `lox` macro can
currently only be used in functions that return `Result<_, impl From<LoxError>>`.

## Examples
```rust
use rulox::prelude::*;
use rulox::LoxError;

fn main() -> Result<(), LoxError> {
    lox! {
        var a = 5;

        print a + 2;
    }

    let b: f64 = a.get()?.try_into().unwrap();

    println!("{}", b);

    Ok(())
}
```

```rust
lox! {
    for (var i = 5; i > 0; i = i - 1) print i;
}
```

```rust
lox! {
   fun hello(name) {
        print "Hello " + name + "! :)";
   }

    fun add_one(num) {
        return num + 1;
    }
}

hello.get()?.call([LoxValue::from("Alice")].into());

assert_eq!(add_one.get()?.call([LoxValue::from(3)].into())?, 4);
```

```rust
lox! {
    var people = ["Bob", "Alice", "John"];

    for (person in people) {
        print "Hello " + person + "!";
    }
}
```

```rust
lox! {
    class Person {
        init(name) {
            this.name = name;
        }

        say_hello() {
            print "Hello, my name is " + this.name + "!";
        }
    }

    var jane = Person("Jane");
    jane.say_hello();
}
```

```rust
lox! {
    class Person {
        init(name) {
            this.name = name;
        }

        say_hello() {
            print "Hello, my name is " + this.name + "!";
        }
    }

    class Telepath > Person {
        init(name, power) {
            super(name);
            this.power = power;
        }

        lift(weight) {
            if (this.power < weight) {
                print "It's too heavy!";
            } else if (this.power == weight) {
                print "I can't keep this up for long!";
            } else {
                print "This is child's play.";
            }
        }
    }

    var bob = Person("Bob");
    bob.say_hello();

    print "";

    var alice = Telepath("Alice", 4);
    alice.say_hello();
    alice.lift(1.5);
    alice.lift(4);
    alice.lift(10);
}
```

```rust
lox! {
    var except_ran = false;
    var else_ran = false;
    var finally_ran = false;
    try {
        print "try";
        throw 1;
    } except {
        print "except";
        except_ran = true;
    } else {
        print "else";
        else_ran = true;
    } finally {
        print "finally";
        finally_ran = true;
    }
}
```

## Features

- [x] Variable declarations
- [x] Print statements
- [x] Control flow statements
- [x] Loops
- [x] `for ... in ...` loops
- [x] Indefinite loops and `break`
- [x] Functions as first-class objects
- [x] Object orientation
- [x] Closures
- [x] Error handling
- [x] Async/await
- [x] Hashmaps

## Possible future features

- Macros
