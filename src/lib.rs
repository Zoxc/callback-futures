#![feature(conservative_impl_trait)]
#![feature(generators)]
#![feature(generator_trait)]
#![feature(fn_traits)]
#![feature(fnbox)]
#![feature(clone_closures)]
#![feature(arbitrary_self_types)]

use std::marker::{PhantomData};
pub use std::ops::Generator;
pub use std::ops::GeneratorState as State;
use std::cell::{RefCell, Cell};
use std::boxed::FnBox;
use pin::{Pin, StackPinned};

pub mod pin {
    use std::marker::PhantomData;
    use std::ops::Deref;
    
    pub struct Pin<T>(T);
    
    impl<T> Deref for Pin<T> {
        type Target = T;
        fn deref(&self) -> &T {
            &self.0
        }
    }
    
    impl<T> Pin<T> {
        pub unsafe fn get_mut(this: &mut Self) -> &mut T {
            &mut this.0
        }
    }
    
    impl<T: ?Sized> From<Box<T>> for Pin<Box<T>> {
        fn from(x: Box<T>) -> Self {
            Pin(x)
        }
    }
    
    impl<T: ?Sized> Pin<Box<T>> {
        pub fn borrow(this: &mut Self) -> Pin<&mut T> {
            Pin(&mut *this.0)
        }
    }
    
    pub struct StackPinned<'a, T: ?Sized>(PhantomData<&'a mut &'a ()>, T);
    
    impl<'a, T> StackPinned<'a, T> {
        pub fn new(x: T) -> Self {
            StackPinned(PhantomData, x)
        }
    }
    
    impl<'a, T: ?Sized> From<&'a mut StackPinned<'a, T>> for Pin<&'a mut T> {
        fn from(x: &'a mut StackPinned<'a, T>) -> Self {
            Pin(&mut x.1)
        }
    }
    
    impl<'a, T: ?Sized> Pin<&'a mut T> {
        pub fn map<F: FnOnce(&'a mut T) -> &'a mut A, A: ?Sized>(this: Self, f: F) -> Pin<&'a mut A> {
            Pin(f(this.0))
        }

        pub fn reborrow<'b>(this: &'b mut Self) -> Pin<&'b mut T> {
            Pin(this.0)
        }
    }
}

pub trait Future {
    type Return;

    fn schedule<'a>(self: Pin<&'a mut Self>, callback: &'a mut FnMut(Self::Return));
}

pub struct AsFuture<T>(pub T);

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

impl<T: Generator<Return = PhantomData<R>, Yield = DelayAfterYield>, R> Future for AsFuture<T> {
    type Return = R;

    fn schedule<'a>(mut self: Pin<&'a mut Self>, callback: &'a mut FnMut(Self::Return)) {
        let this = unsafe { &mut Pin::get_mut(&mut self).0 };
        with_callback(GeneratorResume {
                generator: this as *mut _ as *mut (),
                resume: unsafe {
                    std::mem::transmute(<T as Generator>::resume as fn(&mut T) -> State<DelayAfterYield, PhantomData<R>>)
                },
        }, || {
            GENERATOR_RETURN.with(|r| {
                r.set(Some(callback as *mut FnMut(Self::Return) as *mut FnMut()));
            });
            process_generator_result(this.resume());
        });
    }
}

pub fn to_phanthom_data_and_callback<T>(_: &T, callback: *mut FnMut()) -> (PhantomData<T>, *mut FnMut(T)) {
    (PhantomData, callback as *mut FnMut(T))
}

#[macro_export]
macro_rules! async {
    ($($b:tt)*) => ({
        $crate::AsFuture(unsafe { static move || {
            let return_callback = $crate::GENERATOR_RETURN.with(|c| c.get().unwrap());
            let mut inner = static move || {
                // Force a generator by using `yield`
                if false { /*unsafe {*/ yield ::std::mem::uninitialized() /*}*/ };
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
        }})
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
            let future = &mut StackPinned::new($e);
            delay! {
                let future = future;
                $crate::Future::schedule(Pin::from(future), callback);
            }
        }
        result.unwrap()
    })
}

pub fn test<A, F, U>(future: A) 
where
    A: Future,
{
    let callback = &mut |_| ();
    let mut future = StackPinned::new(future);
    Pin::from(&mut future).schedule(callback);
}

pub fn map<A, F, U>(future: A, f: F) -> impl Future<Return = U>
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
pub fn race<A, B, R>(a: A, b: B) -> impl Future<Return = R>
where
    A: Future<Return = R>,
    B: Future<Return = R>,
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

            let a = &mut StackPinned::new(a);
            let b = &mut StackPinned::new(b);

            delay! {
                let a = a;
                let b = b;
                Pin::from(a).schedule(&mut complete_a);
                Pin::from(b).schedule(&mut complete_b);
            }
        }

        return result.borrow_mut().take().unwrap();
    }
}

/// Waits for two futures to complete
pub fn join<A, B, RA, RB>(a: A, b: B) -> impl Future<Return = (RA, RB)>
where
    A: Future<Return = RA>,
    B: Future<Return = RB>,
{
    async! {
        let mut ra = None;
        let mut rb = None;
        let count: Cell<usize> = Cell::new(0);

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

            let a = &mut StackPinned::new(a);
            let b = &mut StackPinned::new(b);

            delay! {
                let a = a;
                let b = b;
                Pin::from(a).schedule(ca);
                Pin::from(b).schedule(cb);
            }
        }

        return (ra.unwrap(), rb.unwrap());
    }
}