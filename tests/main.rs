use rulox::*;

#[test]
fn var_declaration() {
    lox! {
        var value = 5;
    }

    let _ = value;
}

#[test]
fn var_access() {
    lox! {
        var value = 5;
    }

    let _: f64 = value.try_into().unwrap();
}

#[test]
fn lox_print() {
    lox! {
        print 5;
    }
}

#[test]
fn bools() {
    lox! {
        var b = true or false;
    }

    let value: bool = b.try_into().unwrap();

    assert!(value);
}

#[test]
fn arr() {
    lox! {
        var a = [5, "hello", false];

        print a;
    }
}

#[test]
#[allow(unused_variables)]
fn block() {
    lox! {
        var a;

        {
            var b = 5;
            a = 2;
        }

        print a;
    }
}

#[test]
fn if_statement() {
    lox! {
        var a;

        if (true) {
            a = 5;
        } else {
            a = 2;
        }
    }

    assert_eq!(a, LoxValue::Num(5.0))
}

#[test]
fn while_loop() {
    lox! {
        var a = 5;

        while (a > 0) {
            print a;
            a = a - 1;
        }
    }

    assert_eq!(a, 0);
}

#[test]
fn for_loop() {
    lox! {
        var a = 1;

        for (var i = 5; i > 0; i = i - 1) {
            a = a - 1;
        }
    }

    assert_eq!(a, -4);
}

#[test]
fn function() {
    lox! {
        fun hello(name) {
            print "Hello " + name + "! :)";
        }

        fun goodbye(name) {
            print "Goodbye " + name + " :(";
        }

        goodbye("Alice");

        hello("Bob");
    }

    hello(vec![LoxValue::from("Alice")]);
}

#[test]
fn return_test() {
    lox! {
        fun add_one(num) {
            return num + 1;
        }
    }

    assert_eq!(rust_add_one(LoxValue::Num(3.0)), LoxValue::Num(4.0))
}

#[test]
fn index_arr() {
    lox! {
        var list = [true, false];
    }

    let b1: bool = list.index(0).try_into().unwrap();
    let b2: bool = list.index(LoxValue::Num(1.0)).clone().try_into().unwrap();

    assert!(b1);
    assert!(!b2);
}

#[test]
fn for_in() {
    lox! {
        var list = [1, 2, 3];

        for (value in list) {
            print value;
        }
    }
}

#[test]
fn fibonacci() {
    lox! {
        fun fib(n) {
            if (n == 0 or n == 1) return 1;

            return fib(n - 2) + fib(n - 1);
        }
    }

    assert_eq!(rust_fib(LoxValue::Num(5.0)), 8)
}

/*
#[test]
fn closure() {
    lox! {
        fun adder(n) {
            return fun (x) return x + n; ;
        }
    }
}
*/

fn hello(name: String) -> String {
    "Hello ".to_string() + &name
}

#[test]
fn bindgen() {
    lox_bindgen!(fn hello(name) = lox_hello);

    lox! {
        print lox_hello("Alice");
    }
}

#[test]
fn inline_function() {
    lox! {
        var add_one = fun(num) return num + 1; ;
    }

    assert_eq!(add_one(vec![LoxValue::Num(1.5)]), LoxValue::Num(2.5));
}
