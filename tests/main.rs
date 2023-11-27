#![allow(clippy::cmp_owned)]
#![allow(clippy::needless_question_mark)]

use std::convert::Infallible;
use std::time::Duration;

use rulox::lox_bindgen;
use rulox::prelude::*;
use rulox::rust_bindgen;
use rulox::LoxArgs;
use rulox::LoxError;
use rulox::LoxObject;
use rulox::LoxResult;

#[test]
fn var_declaration() {
    lox! {
        var value = 5;
    }

    let _ = value;
}

#[test]
fn var_access() -> Result<(), LoxError> {
    lox! {
        var value = 5;
    }

    assert_eq!(value.get()?, 5);

    Ok(())
}

#[test]
fn lox_print() {
    lox! {
        print 5;
    }
}

#[test]
fn bools() -> Result<(), LoxError> {
    lox! {
        var b = true or false;
    }

    let value: bool = b.get()?.try_into().unwrap();

    assert!(value);

    Ok(())
}

#[test]
fn arr() -> Result<(), LoxError> {
    lox! {
        var a = [5, "hello", false];

        print a;
    }

    Ok(())
}

#[test]
#[allow(unused_variables)]
fn block() -> Result<(), LoxError> {
    lox! {
        var a;

        {
            var b = 5;
            a = 2;
        }

        print a;
    }

    Ok(())
}

#[test]
fn if_statement() -> Result<(), LoxError> {
    lox! {
        var a;

        if (true) {
            a = 5;
        } else {
            a = 2;
        }
    }

    assert_eq!(a.get()?, 5);

    Ok(())
}

#[test]
fn while_loop() -> Result<(), LoxError> {
    lox! {
        var a = 5;

        while (a > 0) {
            print a;
            a = a - 1;
        }
    }

    assert_eq!(a.get()?, 0);

    Ok(())
}

#[test]
fn for_loop() -> Result<(), LoxError> {
    lox! {
        var a = 1;

        for (var i = 5; i > 0; i = i - 1) {
            a = a - 1;
        }
    }

    assert_eq!(a.get()?, -4);

    Ok(())
}

#[test]
fn function() -> Result<(), LoxError> {
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

    hello.get()?.call([LoxValue::from("Alice")].into()).unwrap();

    Ok(())
}

#[test]
fn return_test() -> Result<(), LoxError> {
    lox! {
        fun add_one(num) {
            return num + 1;
        }
    }

    assert_eq!(
        add_one.get()?.call([LoxValue::Num(3.0)].into()).unwrap(),
        LoxValue::Num(4.0)
    );

    Ok(())
}

#[test]
fn index_arr() -> Result<(), LoxError> {
    lox! {
        var list = [true, false];
    }

    let b1: bool = list.get()?.index(0.into())?.try_into().unwrap();
    let b2: bool = list.get()?.index(LoxValue::Num(1.0))?.try_into().unwrap();

    assert!(b1);
    assert!(!b2);

    Ok(())
}

#[test]
fn for_in() -> Result<(), LoxError> {
    lox! {
        var list = [1, 2, 3];

        for (value in list) {
            print value;
        }
    }

    Ok(())
}

#[test]
fn fibonacci() -> Result<(), LoxError> {
    lox! {
        fun fib(n) {
            if (n == 0 or n == 1) return 1;

            return fib(n - 2) + fib(n - 1);
        }
    }

    assert_eq!(fib.get()?.call([LoxValue::Num(5.0)].into())?, 8);

    Ok(())
}

#[test]
fn mutual_recursion() -> Result<(), LoxError> {
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

    a.get()?.call([LoxValue::Num(4.0)].into()).unwrap();

    Ok(())
}

#[test]
fn closure() -> Result<(), LoxError> {
    lox! {
        var adder = fun(n) return fun(x) return x + n;;;

        var add_2 = adder(2);
    }

    assert_eq!(add_2.get()?.call([LoxValue::Num(1.0)].into()).unwrap(), 3);

    Ok(())
}

#[test]
fn bindgen() -> Result<(), LoxError> {
    fn hello(name: String) -> String {
        "Hello ".to_string() + &name
    }

    lox_bindgen!(fn hello(name) as lox_hello);

    lox! {
        print lox_hello("Alice");
    }

    Ok(())
}

#[test]
fn inline_function() -> Result<(), LoxError> {
    lox! {
        var add_one = fun(num) return num + 1;;
    }

    assert_eq!(
        add_one.get()?.call([LoxValue::Num(1.5)].into()).unwrap(),
        LoxValue::Num(2.5)
    );

    Ok(())
}

#[test]
fn class() -> Result<(), LoxError> {
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

    Ok(())
}

#[test]
fn inheritance() -> Result<(), LoxError> {
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

    Ok(())
}

#[test]
fn super_call() -> Result<(), LoxError> {
    lox! {
        class A {
            do_thing() {
                print "A";
            }
        }

        class B > A {
            do_thing() {
                print "B";
                super();
            }
        }

        class C > B {}

        var c = C();
        c.do_thing();
    }

    Ok(())
}

#[test]
fn throw() {
    fn throws() -> Result<Infallible, LoxError> {
        lox! {
            throw 3;
        }
    }

    assert!(throws().is_err());
}

#[test]
fn catch() -> Result<(), LoxError> {
    lox! {
        try {
            throw 3;
        } except (e) {
            print e;
        }
    }

    Ok(())
}

#[test]
fn except_guards() -> Result<(), LoxError> {
    lox! {
        var result;
        try {
            throw 3;
        } except (e if e == 2) {
            result = "two";
        } except (e if e == 3) {
            result = "three";
        } except {
            result = "other";
        }
    }

    assert_eq!(result.get()?, "three");

    Ok(())
}

#[test]
fn try_else() -> Result<(), LoxError> {
    lox! {
        var result = 1;
        try {
            result = 2;
        } except {
            result = 3;
        } else {
            result = 4;
        }
    }

    assert_eq!(result.get()?, 4);

    Ok(())
}

#[test]
fn try_finally_error() -> Result<(), LoxError> {
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

    assert_eq!(except_ran.get()?, true);
    assert_eq!(else_ran.get()?, false);
    assert_eq!(finally_ran.get()?, true);

    Ok(())
}

#[test]
fn try_finally_ok() -> Result<(), LoxError> {
    lox! {
        var except_ran = false;
        var else_ran = false;
        var finally_ran = false;
        try {
            print "try";
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

    assert_eq!(except_ran.get()?, false);
    assert_eq!(else_ran.get()?, true);
    assert_eq!(finally_ran.get()?, true);

    Ok(())
}

#[test]
fn sync() {
    fn needs_send_sync<T: Send + Sync>(_x: T) {}

    needs_send_sync(LoxValue::Nil);
}

#[tokio::test]
async fn async_bindgen() -> Result<(), LoxError> {
    async fn do_some_stuff() -> i32 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        3
    }

    lox_bindgen!(async fn do_some_stuff());

    lox! {
        var result = do_some_stuff().await;
    }

    assert_eq!(result.get()?, 3);

    Ok(())
}

#[test]
fn comments() -> Result<(), LoxError> {
    lox! {
        var x = 5; // x coordinate
        // thing to say hello to
        var hello = "world";
    }

    assert_eq!(x.get()?, 5);
    assert_eq!(hello.get()?, "world");

    Ok(())
}

#[test]
fn rust_bindgen() -> Result<(), LoxError> {
    lox! {
        fun hello(name) {
            return "Hello " + name + "!";
        }
    }

    rust_bindgen!(fn hello(name: &str) -> String as rust_hello);

    assert_eq!(rust_hello("John"), Ok("Hello John!".to_string()));

    Ok(())
}

#[tokio::test]
async fn async_function() -> Result<(), LoxError> {
    async fn do_some_stuff() -> i32 {
        tokio::time::sleep(Duration::from_secs(2)).await;
        3
    }

    lox_bindgen!(async fn do_some_stuff() as lox_do_stuff);

    lox! {
        async fun my_function() {
            print lox_do_stuff().await;
        }

        my_function().await;
    }

    Ok(())
}

#[test]
fn primitive_methods() -> Result<(), LoxError> {
    lox! {
        var a = 3.is_num();
        var b = "test".is_str();
    }

    assert_eq!(a.get()?, true);
    assert_eq!(b.get()?, true);

    Ok(())
}

#[test]
fn index() -> Result<(), LoxError> {
    lox! {
        var list = ["a", "b", "c"];
        var a = list[0];
        var b = list[1];
        var c = list[2];
        var d;
        try {
            d = list[3];
        } except {
            d = "failed";
        } else {
            throw "succesfully indexed out of range";
        }
    }

    assert_eq!(a.get()?, "a");
    assert_eq!(b.get()?, "b");
    assert_eq!(c.get()?, "c");
    assert_eq!(d.get()?, "failed");

    Ok(())
}

#[test]
fn index_set() -> Result<(), LoxError> {
    lox! {
        var list = [1, 2, 3, 4];
        list[0] = "hello";
        list[3] = 3;

        var a = list[0];
        var b = list[1];
        var c = list[2];
        var d = list[3];
    }

    assert_eq!(a.get()?, "hello");
    assert_eq!(b.get()?, 2);
    assert_eq!(c.get()?, 3);
    assert_eq!(d.get()?, 3);

    Ok(())
}

#[test]
fn map() -> Result<(), LoxError> {
    lox! {
        var map = {
            "a": 1,
            "b": 2,
        };

        var a = map["a"];
        map["b"] = 3;
        var b = map["b"];
    }

    assert_eq!(a.get()?, 1);
    assert_eq!(b.get()?, 3);

    Ok(())
}

#[test]
fn operator_overloading() -> Result<(), LoxError> {
    lox! {
        class MyNum {
            init(num) {
                this.num = num;
            }

            +(other) {
                return MyNum(this.num + other.num);
            }

            -(other) {
                return MyNum(this.num - other.num);
            }

            *(other) {
                return MyNum(this.num * other.num);
            }

            /(other) {
                return MyNum(this.num / other.num);
            }

            %(other) {
                return MyNum(this.num % other.num);
            }

            -@() {
                return MyNum(-this.num);
            }
        }

        print (MyNum(2) + MyNum(3)).num;
        print (MyNum(2) - MyNum(3)).num;
        print (MyNum(2) * MyNum(3)).num;
        print (MyNum(2) / MyNum(3)).num;
        print (MyNum(2) % MyNum(3)).num;
        print (-MyNum(2)).num;

        class MyFunc {
            init(func) {
                this.func = func;
            }

            ()(arg) {
                this.func(arg);
            }
        }

        fun my_print(arg) {
            print arg;
        }

        MyFunc(my_print)("Hello, world!");

        class MyArr {
            init(arr) {
                this.arr = arr;
            }

            [](index) {
                return this.arr[index + 1];
            }

            []=(index, value) {
                this.arr[index + 1] = value;
            }
        }

        var my_arr = MyArr([1, 2, 3]);
        print my_arr[0];
        my_arr[-1] = 4;
        print my_arr.arr;
    }

    Ok(())
}

#[test]
fn extract_args() -> Result<(), LoxError> {
    #[derive(Debug)]
    struct MyType;

    impl MyType {
        fn do_some_stuff(&self, name: String, age: u8) -> LoxValue {
            assert_eq!(name, "Jane");
            assert_eq!(age, 45);
            LoxValue::Nil
        }
    }

    impl LoxObject for MyType {
        fn type_name() -> String
        where
            Self: Sized,
        {
            "MyType".to_string()
        }

        fn call(&self, args: LoxArgs) -> LoxResult {
            let (name, age) = args.extract()?;
            Ok(self.do_some_stuff(name, age))
        }
    }

    fn new_my_type() -> LoxValue {
        LoxValue::external(MyType)
    }

    lox_bindgen!(fn new_my_type());

    lox! {
        var my_val = new_my_type();

        my_val("Jane", 45);
    }

    Ok(())
}
