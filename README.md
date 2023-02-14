# rulox

`rulox` is a lightweight scripting language embedded in Rust. 
It is based on the Lox language from [Crafting Interpreters](http://craftinginterpreters.com/). 

# Examples
```rust
use rulox::lox;
use rulox_types::prelude::*;

fn main() {
    lox! {
        var a = 5;

        print a + 2;
    }

    let b: f64 = a.try_into().unwrap();

    println!("{}", b);
}
```

# Features

- [x] Variable declarations
- [x] Print statements
- [ ] Loops
- [ ] Functions
- [ ] Object orientation (possibly)
