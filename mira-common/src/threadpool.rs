use std::{cell::RefCell, marker::PhantomData, rc::Rc, sync::Arc, thread::JoinHandle};

use crossbeam_channel::{Receiver, Sender};
use parking_lot::{Condvar, Mutex};

pub struct HeapJob<F: FnOnce() + Send>(F);

trait Job {
    /// Semantically, `this` has ownership of the pointer. As such, execute should drop `this` and
    /// make it invalid. It is undefined behavior to use the `this` pointer after a call to
    /// execute.
    unsafe fn execute(this: *mut ());
}

pub struct JobRef(*mut (), unsafe fn(*mut ()));

impl JobRef {
    fn new<T: Job>(value: *mut T) -> Self {
        Self(value as *mut (), <T as Job>::execute)
    }

    pub fn exec(self) {
        unsafe { (self.1)(self.0) }
    }
}

unsafe impl Send for JobRef {}

impl<F: FnOnce() + Send> HeapJob<F> {
    pub fn new(v: F) -> Box<Self> {
        Box::new(Self(v))
    }

    pub fn into_job_ref(self: Box<Self>) -> JobRef {
        JobRef::new(Box::into_raw(self))
    }
}

impl<F: FnOnce() + Send> Job for HeapJob<F> {
    unsafe fn execute(this: *mut ()) {
        let this = unsafe { Box::from_raw(this as *mut Self) };
        (this.0)()
    }
}

struct ThreadHandle {
    join: JoinHandle<()>,
    is_doing_work: Arc<(Mutex<bool>, Condvar)>,
}

enum ThreadPoolMessage {
    JobAvailable,
    Exit,
}

struct MultiThreaded {
    threads: Box<[ThreadHandle]>,
    sender: Sender<ThreadPoolMessage>,
    jobs: Arc<Mutex<Vec<JobRef>>>,
}

#[allow(private_interfaces)]
pub enum ThreadPool {
    SingleThreaded,
    MultiThreaded(MultiThreaded),
}

impl ThreadPool {
    fn thread_function(
        receiver: Receiver<ThreadPoolMessage>,
        jobs: Arc<Mutex<Vec<JobRef>>>,
        is_doing_work: Arc<(Mutex<bool>, Condvar)>,
    ) {
        loop {
            let mut job = { jobs.lock().pop() };

            let prev = std::mem::replace(&mut *is_doing_work.0.lock(), job.is_some());
            if prev != job.is_some() {
                is_doing_work.1.notify_all();
            }

            if job.is_some() {
                while let Some(inner_job) = job {
                    inner_job.exec();
                    job = jobs.lock().pop();
                    continue;
                }
            }

            let prev = std::mem::replace(&mut *is_doing_work.0.lock(), false);
            if prev {
                is_doing_work.1.notify_all();
            }

            match receiver.recv() {
                Ok(ThreadPoolMessage::JobAvailable) => {}
                Err(_) | Ok(ThreadPoolMessage::Exit) => return,
            }
        }
    }

    pub const fn new_singlethreaded() -> Self {
        Self::SingleThreaded
    }

    pub fn new(num_threads: usize) -> Self {
        if num_threads <= 1 {
            return Self::SingleThreaded;
        }

        let mut threads = Vec::with_capacity(num_threads);
        let jobs = Default::default();

        let (sender, receiver) = crossbeam_channel::unbounded();
        for _ in 0..num_threads {
            let receiver = receiver.clone();
            let jobs = Arc::clone(&jobs);
            let is_doing_work = Arc::new((Mutex::new(false), Condvar::new()));
            let _is_doing_work = is_doing_work.clone();
            let thread_handle =
                std::thread::spawn(move || Self::thread_function(receiver, jobs, _is_doing_work));
            threads.push(ThreadHandle {
                join: thread_handle,
                is_doing_work,
            });
        }

        Self::MultiThreaded(MultiThreaded {
            threads: threads.into_boxed_slice(),
            sender,
            jobs,
        })
    }

    pub fn new_auto() -> Self {
        let thread_count = match std::thread::available_parallelism() {
            Ok(v) => v.into(),
            Err(_) => 8,
        };
        Self::new(thread_count)
    }

    fn spawn<F: FnOnce() + Send>(&mut self, func: F) {
        match self {
            ThreadPool::SingleThreaded => func(),
            ThreadPool::MultiThreaded(MultiThreaded { jobs, sender, .. }) => {
                jobs.lock().push(HeapJob::new(func).into_job_ref());
                sender
                    .send(ThreadPoolMessage::JobAvailable)
                    .expect("thread pool: all threads exited");
            }
        }
    }

    pub fn enter<'env, F: for<'scope> FnOnce(&'scope ThreadpoolHandle<'scope, 'env>)>(
        &'env mut self,
        func: F,
    ) {
        let me = Rc::new(RefCell::new(&raw mut *self));
        let handle = ThreadpoolHandle(me.clone(), PhantomData, PhantomData);
        func(&handle);
        unsafe {
            if let ThreadPool::MultiThreaded(v) = &mut **handle.0.borrow_mut() {
                v.finish();
            }
        }
    }
}

impl MultiThreaded {
    fn finish(&mut self) {
        loop {
            for thread in self.threads.iter() {
                let (mutex, cond) = &*thread.is_doing_work;
                // wait until the mutex has false (== the thread isnt doing any work).
                let mut lock = mutex.lock();
                if *lock {
                    cond.wait_while(&mut lock, |v| *v);
                }
            }
            if self.jobs.lock().is_empty() {
                return;
            }
            if self.sender.is_empty() {
                unreachable!("empty sender but uncompleted jobs??");
            }
        }
    }
}

impl Drop for MultiThreaded {
    fn drop(&mut self) {
        self.finish();

        _ = self.sender.send(ThreadPoolMessage::Exit);
        self.sender = crossbeam_channel::bounded(0).0;

        let threads: Vec<ThreadHandle> = std::mem::take(&mut self.threads).into();
        for (i, handle) in threads.into_iter().enumerate() {
            if let Err(payload) = handle.join.join() {
                if let Some(v) = payload.downcast_ref::<&'static str>() {
                    panic!("threadpool thread #{i} panicked: {v}");
                } else if let Some(v) = payload.downcast_ref::<String>() {
                    panic!("threadpool thread #{i} panicked: {v}");
                } else {
                    panic!("threadpool thread #{i} panicked");
                }
            }
        }
        self.jobs.lock().clear();
    }
}

pub struct ThreadpoolHandle<'scope, 'env: 'scope>(
    Rc<RefCell<*mut ThreadPool>>,
    PhantomData<&'scope mut &'scope ()>,
    PhantomData<&'env mut &'env ()>,
);

impl<'scope> ThreadpoolHandle<'scope, '_> {
    pub fn spawn<F: FnOnce() + Send + 'scope>(&'scope self, func: F) {
        unsafe { (&mut **self.0.borrow_mut()).spawn(func) };
    }
}
