use rulox::lox_bindgen;
use rulox::prelude::*;

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

    assert_eq!(value.get(), 5);
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

    let value: bool = b.get().try_into().unwrap();

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

    assert_eq!(a.get(), 5)
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

    assert_eq!(a.get(), 0);
}

#[test]
fn for_loop() {
    lox! {
        var a = 1;

        for (var i = 5; i > 0; i = i - 1) {
            a = a - 1;
        }
    }

    assert_eq!(a.get(), -4);
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

    hello.get().lox_call([LoxValue::from("Alice")].into());
}

#[test]
fn return_test() {
    lox! {
        fun add_one(num) {
            return num + 1;
        }
    }

    assert_eq!(
        add_one.get().lox_call([LoxValue::Num(3.0)].into()),
        LoxValue::Num(4.0)
    )
}

#[test]
fn index_arr() {
    lox! {
        var list = [true, false];
    }

    let b1: bool = list.get().index(0).try_into().unwrap();
    let b2: bool = list.get().index(LoxValue::Num(1.0)).try_into().unwrap();

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

    assert_eq!(fib.get().lox_call([LoxValue::Num(5.0)].into()), 8)
}

#[test]
fn mutual_recursion() {
    lox! {
        fun a(n) {
            if (n > 0) {
                b(n - 1);
            } else {
                print "Ended on 'a'";
            }
        }

        fun b(n) {
            if (n > 0) {
                a(n - 1);
            } else {
                print "Ended on 'b'";
            }
        }
    }

    a.get().lox_call([LoxValue::Num(4.0)].into());
}

#[test]
fn closure() {
    lox! {
        fun adder(n) {
            return fun (x) return x + n; ;
        }
    }
}

fn hello(name: String) -> String {
    "Hello ".to_string() + &name
}

#[test]
fn bindgen() {
    lox_bindgen!(fn hello(name) as lox_hello);

    lox! {
        print lox_hello("Alice");
    }
}

#[test]
fn inline_function() {
    lox! {
        var add_one = fun(num) return num + 1; ;
    }

    assert_eq!(
        add_one.get().lox_call([LoxValue::Num(1.5)].into()),
        LoxValue::Num(2.5)
    );
}

#[test]
fn class() {
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
}

#[test]
fn inheritance() {
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
}
