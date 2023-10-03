use super::Shared;

use std::cmp::Ordering;

impl<T: PartialEq> PartialEq for Shared<T> {
    fn eq(&self, other: &Self) -> bool {
        self.read().eq(&other.read())
    }
}

impl<T: Eq> Eq for Shared<T> {}

impl<T: PartialOrd> PartialOrd for Shared<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.read().partial_cmp(&other.read())
    }
}

impl<T: Ord> Ord for Shared<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.read().cmp(&other.read())
    }
}

impl<T> From<T> for Shared<T> {
    fn from(value: T) -> Self {
        Shared::new(value)
    }
}
