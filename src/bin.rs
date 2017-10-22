#![feature(generators)]
#![feature(generator_trait)]

#[macro_use]
extern crate futures;

//use futures::{join, map, Future};

fn main() {
    /*let a = async! {
        println!("in future a");
        7
    };
    let b = async! {
        println!("in future b");
        3
    };
    unsafe { join(map(a, |r| r + 1), b).schedule(&mut |(a, b)| println!("{} {}", a, b)) };*/
}