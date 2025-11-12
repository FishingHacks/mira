use core::panic;
use std::{
    io::{Write, stdout},
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
        mpsc::{Sender, TryRecvError, channel},
    },
    thread::JoinHandle,
};

use crate::{ProgressBar, ProgressBarStyle, ProgressItemRef};

#[derive(Clone)]
#[allow(private_interfaces)]
pub enum ProgressBarThread {
    Threaded(Arc<ProgressBarThreadInner>),
    NoThread,
}

struct ProgressBarThreadInner {
    next_progbar_item_ref: AtomicUsize,
    sender: Sender<ProgressMessage>,
    handle: Option<JoinHandle<()>>,
}

impl Drop for ProgressBarThread {
    fn drop(&mut self) {
        // if this is the last reference, drop it
        let ProgressBarThread::Threaded(inner) = self else {
            return;
        };
        if let Some(v) = Arc::get_mut(inner) {
            drop(v.sender.send(ProgressMessage::StopThread));
            if let Err(e) = v
                .handle
                .take()
                .expect("thread should always exist here")
                .join()
            {
                if let Some(s) = e.downcast_ref::<&str>() {
                    panic!("Print thread panicked:\n{s}");
                } else if let Some(s) = e.downcast_ref::<String>() {
                    panic!("Print thread panicked:\n{s}");
                } else {
                    panic!("Print thread panicked");
                }
            }
        }
    }
}

impl ProgressBarThread {
    fn next_progbar_ref(&self) -> ProgressItemRef {
        let n = match self {
            ProgressBarThread::Threaded(inner) => {
                inner.next_progbar_item_ref.fetch_add(1, Ordering::Relaxed)
            }
            ProgressBarThread::NoThread => 0,
        };
        ProgressItemRef(n)
    }

    pub fn add_child(&self, parent: ProgressItemRef, name: Box<str>) -> ProgressItemRef {
        let ProgressBarThread::Threaded(inner) = self else {
            return ProgressItemRef(0);
        };
        let item = self.next_progbar_ref();
        drop(
            inner
                .sender
                .send(ProgressMessage::AddChild { name, item, parent }),
        );
        item
    }

    pub fn add_item(&self, name: Box<str>) -> ProgressItemRef {
        let ProgressBarThread::Threaded(inner) = self else {
            return ProgressItemRef(0);
        };
        let item = self.next_progbar_ref();
        drop(inner.sender.send(ProgressMessage::Add { name, item }));
        item
    }

    pub fn remove(&self, item: ProgressItemRef) {
        let ProgressBarThread::Threaded(inner) = self else {
            return;
        };
        drop(inner.sender.send(ProgressMessage::Remove(item)));
    }

    pub fn clear_children(&self, item: ProgressItemRef) {
        let ProgressBarThread::Threaded(inner) = self else {
            return;
        };
        drop(inner.sender.send(ProgressMessage::ClearChildren(item)));
    }

    pub fn print_stdout(&self, s: String) {
        match self {
            ProgressBarThread::Threaded(inner) => {
                if let Err(e) = inner.sender.send(ProgressMessage::PrintStdout(s)) {
                    let ProgressMessage::PrintStdout(s) = e.0 else {
                        unreachable!()
                    };
                    print!("{s}");
                    if !s.ends_with("\n\n") {
                        println!()
                    }
                }
            }
            ProgressBarThread::NoThread => {
                print!("{s}");
                if !s.ends_with("\n\n") {
                    println!()
                }
            }
        }
    }

    pub fn print_stderr(&self, s: String) {
        match self {
            ProgressBarThread::Threaded(inner) => {
                if let Err(e) = inner.sender.send(ProgressMessage::PrintStderr(s)) {
                    let ProgressMessage::PrintStdout(s) = e.0 else {
                        unreachable!()
                    };
                    eprint!("{s}");
                    if !s.ends_with("\n\n") {
                        eprintln!()
                    }
                }
            }
            ProgressBarThread::NoThread => {
                eprint!("{s}");
                if !s.ends_with("\n\n") {
                    eprintln!()
                }
            }
        }
    }
}

pub enum ProgressMessage {
    StopThread,
    Add {
        name: Box<str>,
        item: ProgressItemRef,
    },
    AddChild {
        name: Box<str>,
        item: ProgressItemRef,
        parent: ProgressItemRef,
    },
    Remove(ProgressItemRef),
    ClearChildren(ProgressItemRef),
    PrintStdout(String),
    PrintStderr(String),
}

/// if no style is specified, no bar will be displayed.
pub fn start_thread(style: Option<ProgressBarStyle>) -> ProgressBarThread {
    // match style {
    //     Some(style) => start_thread_with(ProgressBar::new(style)),
    //     None => ProgressBarThread::NoThread,
    // }
    ProgressBarThread::NoThread
}

fn handle_msg(msg: ProgressMessage, bar: &mut ProgressBar) -> bool {
    match msg {
        ProgressMessage::Add { name, item } => bar.add_item(name, item),
        ProgressMessage::AddChild { name, item, parent } => bar.add_child_item(parent, name, item),
        ProgressMessage::Remove(item) => bar.remove_item(item),
        ProgressMessage::ClearChildren(item) => {
            let Some(v) = bar.items.get_mut(&item.0) else {
                return false;
            };
            let children = std::mem::take(&mut v.children);
            for child in children {
                bar.remove_item_inner(child);
            }
        }
        ProgressMessage::StopThread => return true,
        ProgressMessage::PrintStderr(v) => {
            println!("{v}");
            if !v.ends_with("\n\n") {
                println!();
            }
        }
        ProgressMessage::PrintStdout(v) => {
            println!("{v}");
            if !v.ends_with("\n\n") {
                println!();
            }
        }
    }
    false
}

pub fn start_thread_with(bar: ProgressBar) -> ProgressBarThread {
    let (tx, rx) = channel();
    let handle = std::thread::spawn(move || {
        let mut bar = bar;
        'outer: loop {
            // wait for incoming message
            let Ok(msg) = rx.recv() else { break };
            if matches!(
                msg,
                ProgressMessage::PrintStdout(_) | ProgressMessage::PrintStderr(_)
            ) {
                // clean the progress bar to display any incoming diagnostics
                bar.clean(&mut stdout().lock())
                    .expect("failed to clean the progress bar up");
            }
            if handle_msg(msg, &mut bar) {
                break;
            }
            // handle any queued messages
            loop {
                match rx.try_recv() {
                    Ok(msg) => {
                        if handle_msg(msg, &mut bar) {
                            break 'outer;
                        }
                    }
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => break 'outer,
                }
            }
            // finally, print the progress bar
            let mut stdout = stdout().lock();
            if let Err(e) = bar.display(&mut stdout) {
                drop(stdout);
                eprintln!("Failed to write to stdout: {e:?}");
                return;
            }
        }
        let mut stdout = stdout().lock();
        bar.root_children.clear();
        bar.items.clear();
        bar.clean(&mut stdout)
            .expect("failed to clean up the progress bar");
        stdout.flush().expect("failed to flush stdout");
    });

    ProgressBarThread::Threaded(Arc::new(ProgressBarThreadInner {
        next_progbar_item_ref: AtomicUsize::new(0),
        sender: tx,
        handle: Some(handle),
    }))
}
