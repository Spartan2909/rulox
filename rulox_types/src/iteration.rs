use crate::shared::read;
use crate::LoxValue;
use crate::LoxValueType;

use std::vec;

/// An iterator over a `LoxValue::Array` or a `LoxValue::String`.
#[non_exhaustive]
pub enum IntoIter {
    #[doc(hidden)] // Not public API.
    Array(vec::IntoIter<LoxValue>),
    #[doc(hidden)] // Not public API.
    String(vec::IntoIter<u8>),
}

const CONT_MASK: u8 = 0b0011_1111;

const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// ## Panics
/// Panics if `bytes` does not produce a valid UTF-8 string.
fn next_code_point<I: Iterator<Item = u8>>(bytes: &mut I) -> Option<u32> {
    // Decode UTF-8
    let x = bytes.next()?;
    if x < 128 {
        return Some(x as u32);
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    let init = utf8_first_byte(x, 2);
    let y = bytes.next().unwrap();
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        let z = bytes.next().unwrap();
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            let w = bytes.next().unwrap();
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    Some(ch)
}

impl Iterator for IntoIter {
    type Item = LoxValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            IntoIter::Array(iter) => iter.next(),
            IntoIter::String(bytes) => Some(LoxValue::Str(
                char::from_u32(next_code_point(bytes)?)
                    .unwrap()
                    .to_string()
                    .into(),
            )),
        }
    }
}

impl IntoIterator for LoxValue {
    type Item = Self;
    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Arr(arr) => IntoIter::Array(read(&arr).clone().into_iter()),
            Self::Str(string) => IntoIter::String(string.to_string().into_bytes().into_iter()),
            _ => panic!("cannot convert {} into iterator", LoxValueType::from(self)),
        }
    }
}
