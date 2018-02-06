#![feature(generators)]
#![feature(generator_trait)]
#![feature(conservative_impl_trait)]

#[macro_use]
extern crate futures;

use futures::{join, map, freshen, Future};

fn main() {
    let a = async! {
        println!("in future a");
        7
    };
    let b = async! {
        println!("in future b");
        3
    };
    join(map(a, |r| r + 1), b).schedule(&mut |(a, b)| println!("{} {}", a, b));
}