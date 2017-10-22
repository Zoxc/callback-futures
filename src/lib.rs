#![feature(conservative_impl_trait)]
#![feature(generators)]
#![feature(immovable_types)]
#![feature(generator_trait)]
#![feature(fn_traits)]

use std::marker::{PhantomData, Move};
pub use std::ops::Generator;
pub use std::ops::GeneratorState as State;
use std::cell::Cell;

pub trait Future: ?Move {
    type Return;

    unsafe fn schedule(&mut self, callback: &mut FnMut(Self::Return));
}

impl<'a, T: ?Move + Future> Future for &'a mut T {
    type Return = T::Return;

    unsafe fn schedule(&mut self, callback: &mut FnMut(Self::Return)) {
        (*self).schedule(callback)
    }
}

pub struct AsFuture<T: ?Move>(pub T);

pub struct DelayAfterYield {
    operation: *mut FnMut(),
}

#[derive(Clone, Copy)]
pub struct GeneratorResume {
    generator: *mut (),
    resume: fn(*mut ()) -> State<DelayAfterYield, ()>,
}

impl GeneratorResume {
    fn call(&self) {
        with_callback(*self, || {
            process_generator_result((self.resume)(self.generator));
        });
    }
}

fn process_generator_result<R>(r: State<DelayAfterYield, R>) {
    match r {
        State::Complete(_) => (),
        State::Yielded(ref delay) => {
            unsafe { (*delay.operation)() }
        }
    }
}

thread_local! {
    pub static GENERATOR_RETURN: Cell<Option<*mut FnMut()>> = Cell::new(None);
    pub static GENERATOR_RESUME: Cell<Option<GeneratorResume>> = Cell::new(None);
}

fn with_callback<F: FnOnce()>(callback: GeneratorResume, f: F) {
    let old_callback = Cell::new(Some(callback));
    GENERATOR_RESUME.with(|c| c.swap(&old_callback));
    f();
    GENERATOR_RESUME.with(|c| c.swap(&old_callback));
}

impl<T: Generator<Return = PhantomData<R>, Yield = DelayAfterYield> + ?Move, R> Future for AsFuture<T> where T::Return: Move {
    type Return = R;

    unsafe fn schedule(&mut self, callback: &mut FnMut(Self::Return)) {
        with_callback(GeneratorResume {
                generator: self as *mut _ as *mut (),
                resume: std::mem::transmute(<T as Generator>::resume as fn(&mut T) -> State<DelayAfterYield, PhantomData<R>>),
        }, || {
            GENERATOR_RETURN.with(|r| {
                r.set(Some(callback as *mut FnMut(Self::Return) as *mut FnMut()));
            });
            process_generator_result(self.0.resume());
        });
    }
}

pub fn to_phanthom_data_and_callback<T>(_: &T, callback: *mut FnMut()) -> (PhantomData<T>, *mut FnMut(T)) {
    (PhantomData, callback as *mut FnMut(T))
}

#[macro_export]
macro_rules! async {
    ($($b:tt)*) => ({
        $crate::AsFuture(static move || {
            let return_callback = $crate::GENERATOR_RETURN.with(|c| c.get().unwrap());
            let mut inner = static move || {
                // Force a generator by using `yield`
                if false { unsafe { yield ::std::mem::uninitialized() } };
                $($b)*
            };
            let result = loop {
                match $crate::Generator::resume(&mut inner) {
                    $crate::State::Complete(r) => break r,
                    $crate::State::Yielded(y) => yield y,
                }
            };
            let (phanthom_result, return_callback) = $crate::to_phanthom_data_and_callback(&result, return_callback);
            let return_callback = &mut *return_callback;
            return_callback(result);
            phanthom_result
        })
    })
}

pub fn callback() -> GeneratorResume {
    GENERATOR_RESUME.with(|c| c.get().unwrap())
}

#[macro_export]
macro_rules! delay {
    ($($b:tt)*) => ({
        let delay = &mut || {
            $($b)*
        };
        yield DelayAfterYield { operation: std::mem::transmute(delay as *mut FnMut()) };
    })
}

#[macro_export]
macro_rules! await {
    ($e:expr) => ({
        let mut result = None;
        {
            let resume_callback = $crate::callback();
            let callback = &mut |r| {
                result = Some(r);
                resume_callback.call();
            };
            let mut future = $e;
            delay! {
                $crate::Future::schedule(&mut future, callback);
            }
        }
        result.unwrap()
    })
}

pub fn map<A: ?Move, F, U>(future: A, f: F) -> impl Future<Return = U> 
where
    A: Future,
    F: FnOnce(A::Return) -> U,
{
    async! {
        println!("in map");
        f(await!(future))
    }
}

/// Returns the result of the first future to finish
pub fn race<A: ?Move, B: ?Move, R>(mut a: A, mut b: B) -> impl Future<Return = R>
where
    A: Future<Return = R>,
    B: Future<Return = R>,
{
    async! {
        let mut result = None;
        let resume = callback();

        {
            let complete = &mut |r| {
                if result.is_some() {
                    return;
                }
                result = Some(r);
                resume.call();
            };

            delay!{
                // FIXME: Should be unsafe to pass `complete` twice
                a.schedule(complete);
                b.schedule(complete);
            }
        }

        return result.unwrap();
    }
}

/// Waits for two futures to complete
pub fn join<A: ?Move, B: ?Move, RA, RB>(mut a: A, mut b: B) -> impl Future<Return = (RA, RB)>
where
    A: Future<Return = RA>,
    B: Future<Return = RB>,
{
    async! {
        let mut ra = None;
        let mut rb = None;
        let count = Cell::new(0);

        let resume = callback();

        {
            let ca = &mut |r| {
                ra = Some(r);
                count.set(count.get() + 1);
                if count.get() == 2 {
                    resume.call();
                }
            };

            let cb = &mut |r| {
                rb = Some(r);
                count.set(count.get() + 1);
                if count.get() == 2 {
                    resume.call();
                }
            };

            delay!{
                a.schedule(ca);
                b.schedule(cb);
            }
        }

        return (ra.unwrap(), rb.unwrap());
    }
}
