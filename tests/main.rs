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
