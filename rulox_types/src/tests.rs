use super::*;

#[test]
fn display() {
    let value = LoxValue::Arr(vec![
        LoxValue::Bool(true),
        LoxValue::Str("a value".to_string()),
        LoxValue::Num(1.0),
        LoxValue::Num(-2.5),
        LoxValue::Nil,
    ]);

    assert_eq!(value.to_string(), "[true, \"a value\", 1, -2.5, nil]");
}

#[test]
fn add() {
    let num1 = LoxValue::from(1);
    
    let num2: Result<LoxValue, LoxError> = num1 + 2.5;

    assert_eq!(num2.unwrap(), 3.5);
}

#[test]
fn sub() {
    let num1 = LoxValue::from(5);
    
    let num2: Result<LoxValue, LoxError> = num1 - 3;

    assert_eq!(num2.unwrap(), LoxValue::from(2));
}

#[test]
fn mul() {
    let num1 = LoxValue::from(2);
    
    let num2: Result<LoxValue, LoxError> = num1 * 3;

    assert_eq!(num2.unwrap(), 6);
}

#[test]
fn div() {
    let num1 = LoxValue::from(8);
    
    let num2: Result<LoxValue, LoxError> = num1 / 2;

    assert_eq!(num2.unwrap(), LoxValue::from(4));
}
