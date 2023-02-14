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
