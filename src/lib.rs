#![feature(conservative_impl_trait)]
#![feature(generators)]
#![feature(generator_trait)]
#![feature(fn_traits)]
#![feature(fnbox)]
#![feature(clone_closures)]

use std::marker::{PhantomData};
pub use std::ops::Generator;
pub use std::ops::GeneratorState as State;
use std::cell::{RefCell, Cell};
use std::boxed::FnBox;

#[derive(Default)]
pub struct Immovable<'a>(PhantomData<fn(&'a ()) -> &'a ()>);

pub unsafe trait Future<'a> {
    type Return;

    fn schedule(&'a mut self, callback: &'a mut FnMut(Self::Return));

    type Fresh: Future<'static, Return=Self::Return>;

    fn freshen(self) -> Self::Fresh;
}

pub struct Fresh<'a, T>(pub Immovable<'a>, pub T);

unsafe impl<'a, T: Future<'static>> Future<'a> for Fresh<'a, T> {
    type Return = T::Return;

    fn schedule(&'a mut self, callback: &'a mut FnMut(Self::Return)) {
        let schedule_fn: fn(&'a T, &'a mut FnMut(Self::Return)) = unsafe {
            std::mem::transmute(<T as Future<'static>>::schedule as usize)
        };
        schedule_fn(&self.1, callback);
    }

    type Fresh = Fresh<'static, T>;
    
    fn freshen(self) -> Self::Fresh {
        Fresh(Immovable::default(), self.1)
    }
}

pub fn freshen<'a, 'b, T: Future<'a>>(f: T) -> Fresh<'b, T::Fresh> {
    Fresh(Immovable::default(), f.freshen())
}

pub struct AsFuture<'a, T>(pub Immovable<'a>, pub T);

pub struct DelayAfterYield {
    operation: Box<FnBox()>,
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
        State::Yielded(delay) => {
            (delay.operation)()
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

unsafe impl<'a, T: Generator<Return = PhantomData<R>, Yield = DelayAfterYield>, R> Future<'a> for AsFuture<'a, T> {
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

    type Fresh = AsFuture<'static, T>;

    fn freshen(self) -> Self::Fresh {
        AsFuture(Immovable::default(), self.1)
    }
}

pub fn to_phanthom_data_and_callback<T>(_: &T, callback: *mut FnMut()) -> (PhantomData<T>, *mut FnMut(T)) {
    (PhantomData, callback as *mut FnMut(T))
}

#[macro_export]
macro_rules! async {
    ($($b:tt)*) => ({
        $crate::Fresh($crate::Immovable::default(), $crate::AsFuture($crate::Immovable::default(), unsafe { static move || {
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
        }}))
    })
}

pub fn callback() -> GeneratorResume {
    GENERATOR_RESUME.with(|c| c.get().unwrap())
}

pub fn delay<F: FnBox()>(f: F) -> DelayAfterYield {
    let b: Box<FnBox()> = Box::new(f);
    DelayAfterYield { operation: unsafe { std::mem::transmute(b) } }
}

#[macro_export]
macro_rules! delay {
    ($($b:tt)*) => ({
        let delay = delay(|| {
            $($b)*
        });
        yield delay;
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
            let future = &mut freshen($e);
            delay! {
                let future = future;
                $crate::Future::schedule(future, callback);
            }
        }
        result.unwrap()
    })
}

fn test<'a, A, F, U>(future: A) 
where
    A: Future<'a>,
{
    let callback = &mut |r| ();
    let future = &mut freshen(future);
    future.schedule(callback);
}

pub fn map<'a, 'b, A, F, U>(future: A, f: F) -> impl Future<'b, Return = U>
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
pub fn race<'a, 'b, 'c, A, B, R>(a: A, b: B) -> impl Future<'c, Return = R>
where
    A: Future<'a, Return = R>,
    B: Future<'b, Return = R>,
{
    async! {
        let result = RefCell::new(None);
        let resume = callback();

        {
            let mut complete_a = |r| {
                if result.borrow().is_some() {
                    return;
                }
                *result.borrow_mut() = Some(r);
                resume.call();
            };

            let mut complete_b = |r| {
                if result.borrow().is_some() {
                    return;
                }
                *result.borrow_mut() = Some(r);
                resume.call();
            };

            let a = &mut freshen(a);
            let b = &mut freshen(b);

            delay!{
                let a = a;
                let b = b;
                a.schedule(&mut complete_a);
                b.schedule(&mut complete_b);
            }
        }

        return result.borrow_mut().take().unwrap();
    }
}

/// Waits for two futures to complete
pub fn join<'a, 'b, 'c, A, B, RA, RB>(a: A, b: B) -> Fresh<'c, impl Future<'static, Return = (RA, RB)>>
where
    A: Future<'a, Return = RA>,
    B: Future<'b, Return = RB>,
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

            let a = &mut freshen(a);
            let b = &mut freshen(b);

            delay!{
                let a = a;
                let b = b;
                a.schedule(ca);
                b.schedule(cb);
            }
        }

        return (ra.unwrap(), rb.unwrap());
    }
}