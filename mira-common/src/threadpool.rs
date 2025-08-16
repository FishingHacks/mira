use std::{
    cell::RefCell,
    marker::PhantomData,
    panic::AssertUnwindSafe,
    rc::Rc,
    sync::{Arc, Mutex},
    thread::JoinHandle,
};

use crossbeam_channel::{Receiver, Sender};

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

enum ThreadPoolMessage {
    JobAvailable,
    Exit,
}

pub struct ThreadPool {
    threads: Box<[JoinHandle<()>]>,
    sender: Sender<ThreadPoolMessage>,
    jobs: Arc<Mutex<Vec<JobRef>>>,
}

impl ThreadPool {
    fn thread_function(receiver: Receiver<ThreadPoolMessage>, jobs: Arc<Mutex<Vec<JobRef>>>) {
        loop {
            let job = { jobs.lock().expect("failed to lock the jobs mutex").pop() };
            if let Some(job) = job {
                job.exec();
            }
            match receiver.recv() {
                Ok(ThreadPoolMessage::JobAvailable) => {}
                Err(_) | Ok(ThreadPoolMessage::Exit) => return,
            }
        }
    }

    pub fn new(num_threads: usize) -> Self {
        let mut threads = Vec::with_capacity(num_threads);
        let jobs = Default::default();

        let (sender, receiver) = crossbeam_channel::unbounded();
        for _ in 0..num_threads {
            let receiver = receiver.clone();
            let jobs = Arc::clone(&jobs);
            let thread_handle = std::thread::spawn(move || Self::thread_function(receiver, jobs));
            threads.push(thread_handle);
        }

        Self {
            threads: threads.into_boxed_slice(),
            sender,
            jobs,
        }
    }

    pub fn new_auto() -> Self {
        let thread_count = match std::thread::available_parallelism() {
            Ok(v) => v.into(),
            Err(e) => {
                println!("Cannot get the number of cpu threads, using 8: {e:?}");
                8
            }
        };
        Self::new(thread_count)
    }

    fn spawn<F: FnOnce() + Send>(&mut self, func: F) {
        self.jobs
            .lock()
            .expect("failed to lock the job mutex")
            .push(HeapJob::new(func).into_job_ref());
        self.broadcast_job_availability();
    }

    fn broadcast_job_availability(&self) {
        self.sender
            .send(ThreadPoolMessage::JobAvailable)
            .expect("failed to send message to jobs");
    }
    pub fn enter<'env, F: for<'scope> FnOnce(&'scope ThreadpoolHandle<'scope, 'env>)>(
        &'env mut self,
        func: F,
    ) {
        let me = Rc::new(RefCell::new(self as *mut _));
        let handle = ThreadpoolHandle(me.clone(), PhantomData, PhantomData);
        let result = std::panic::catch_unwind(AssertUnwindSafe(|| func(&handle)));
        unsafe { (&mut **handle.0.borrow_mut()).finish() };
        if let Err(res) = result {
            std::panic::resume_unwind(res);
        }
    }

    fn finish(&mut self) {
        // println!("finishing :3");
        _ = self.sender.send(ThreadPoolMessage::Exit);
        self.sender = crossbeam_channel::bounded(0).0;

        let threads: Vec<JoinHandle<()>> = std::mem::take(&mut self.threads).into();
        for (i, handle) in threads.into_iter().enumerate() {
            // println!("joining thread #{i}");
            if let Err(payload) = handle.join() {
                if let Some(v) = payload.downcast_ref::<&'static str>() {
                    println!("threadpool thread #{i} panicked: {v:?}");
                } else if let Some(v) = payload.downcast_ref::<String>() {
                    println!("threadpool thread #{i} panicked: {v:?}");
                } else {
                    panic!("threadpool thread #{i} panicked");
                }
            }
            // println!("joined thread #{i}");
        }
        // println!("clearing jobs");
        self.jobs.lock().unwrap().clear();
    }
}

pub struct ThreadpoolHandle<'scope, 'env: 'scope>(
    Rc<RefCell<*mut ThreadPool>>,
    PhantomData<&'scope mut &'scope ()>,
    PhantomData<&'env mut &'env ()>,
);

impl<'scope, 'env> ThreadpoolHandle<'scope, 'env> {
    pub fn spawn<F: FnOnce() + Send + 'scope>(&'scope self, func: F) {
        unsafe { (&mut **self.0.borrow_mut()).spawn(func) };
    }
}
