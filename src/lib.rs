#![feature(conservative_impl_trait)]
#![feature(generators)]
#![feature(immovable_types)]
#![feature(generator_trait)]
#![feature(fn_traits)]

use std::marker::{PhantomData, Move};
pub use std::ops::Generator;
pub use std::ops::GeneratorState as State;
use std::cell::Cell;

#[derive(Default)]
struct Immovable<'a>(PhantomData<fn(&'a ()) -> &'a ()>);

pub trait Future<'a>: ?Move {
    type Return;

    fn schedule(&'a mut self, callback: &'a mut FnMut(Self::Return));
}

impl<'a, 'b, T: ?Move + Future<'a>> Future<'a> for &'b mut T {
    type Return = T::Return;

    fn schedule(&'a mut self, callback: &'a mut FnMut(Self::Return)) {
        (*self).schedule(callback)
    }
}

pub struct AsFuture<'a, T: ?Move>(Immovable<'a>, pub T);

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

impl<'a, T: Generator<Return = PhantomData<R>, Yield = DelayAfterYield> + ?Move, R> Future<'a> for AsFuture<'a, T> where T::Return: Move {
    type Return = R;

    fn schedule(&'a mut self, callback: &'a mut FnMut(Self::Return)) {
        with_callback(GeneratorResume {
                generator: &mut self.1 as *mut _ as *mut (),
                resume: unsafe {
                    std::mem::transmute(<T as Generator>::resume as fn(&mut T) -> State<DelayAfterYield, PhantomData<R>>)
                },
        }, || {
            GENERATOR_RETURN.with(|r| {
                r.set(Some(callback as *mut FnMut(Self::Return) as *mut FnMut()));
            });
            process_generator_result(self.1.resume());
        });
    }
}

pub fn to_phanthom_data_and_callback<T>(_: &T, callback: *mut FnMut()) -> (PhantomData<T>, *mut FnMut(T)) {
    (PhantomData, callback as *mut FnMut(T))
}

#[macro_export]
macro_rules! async {
    ($($b:tt)*) => ({
        $crate::AsFuture(Immovable::default(), static move || {
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

pub fn map_e<'a, A: ?Move, F, U>(future: A, f: F) -> impl Future<'a, Return = U>
where
    A: Future<'a>,
    F: FnOnce(A::Return) -> U,
{
    AsFuture(Immovable::default(), static move || {
        let return_callback = GENERATOR_RETURN.with(|c| c.get().unwrap());
        let mut inner = static move || {
            // Force a generator by using `yield`
            if false { unsafe { yield ::std::mem::uninitialized() } };

            // START BODY

            println!("in map");
            f({
                // START AWAIT

                let mut result = None;
                {
                    let resume_callback = callback();
                    let callback = &mut |r| {
                        result = Some(r);
                        resume_callback.call();
                    };
                    let mut future = future;

                    let delay = &mut || {
                        Future::schedule(&mut future, callback);
                    };
                    yield DelayAfterYield { operation: std::mem::transmute(delay as *mut FnMut()) };
                }
                result.unwrap()

                // END AWAIT
            })

            // END BODY
        };
        let result = loop {
            match Generator::resume(&mut inner) {
                State::Complete(r) => break r,
                State::Yielded(y) => yield y,
            }
        };
        let (phanthom_result, return_callback) = to_phanthom_data_and_callback(&result, return_callback);
        let return_callback = &mut *return_callback;
        return_callback(result);
        phanthom_result
    })
}

/*
pub fn map<'a, A: ?Move, F, U>(future: A, f: F) -> impl Future<'a, Return = U>
where
    A: Future<'a>,
    F: FnOnce(A::Return) -> U,
{
    async! {
        println!("in map");
        f(await!(future))
    }
}

/// Returns the result of the first future to finish
pub fn race<'a, A: ?Move, B: ?Move, R>(mut a: A, mut b: B) -> impl Future<'a, Return = R>
where
    A: Future<'a, Return = R>,
    B: Future<'a, Return = R>,
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
pub fn join<'a, A: ?Move, B: ?Move, RA, RB>(mut a: A, mut b: B) -> impl Future<'a, Return = (RA, RB)>
where
    A: Future<'a, Return = RA>,
    B: Future<'a, Return = RB>,
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
*/