#![feature(generators)]
#![feature(generator_trait)]
#![feature(conservative_impl_trait)]

#[macro_use]
extern crate futures;

use futures::{join, map, Future};
use futures::pin::{StackPinned, Pin};

fn main() {
    let a = async! {
        println!("in future a");
        7
    };
    let b = async! {
        println!("in future b");
        3
    };
    let mut s = StackPinned::new(join(map(a, |r| r + 1), b));
    Pin::from(&mut s).schedule(&mut |(a, b)| println!("{} {}", a, b));
}