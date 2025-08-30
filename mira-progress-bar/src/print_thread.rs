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
pub struct ProgressBarThread(Arc<ProgressBarThreadInner>);

struct ProgressBarThreadInner {
    next_progbar_item_ref: AtomicUsize,
    sender: Sender<ProgressMessage>,
    handle: Option<JoinHandle<()>>,
}

impl Drop for ProgressBarThread {
    fn drop(&mut self) {
        // if this is the last reference, drop it
        if let Some(v) = Arc::get_mut(&mut self.0) {
            _ = v.sender.send(ProgressMessage::StopThread);
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
        ProgressItemRef(self.0.next_progbar_item_ref.fetch_add(1, Ordering::Relaxed))
    }

    pub fn add_child(&self, parent: ProgressItemRef, name: Box<str>) -> ProgressItemRef {
        let item = self.next_progbar_ref();
        _ = self
            .0
            .sender
            .send(ProgressMessage::AddChild { name, item, parent });
        item
    }

    pub fn add_item(&self, name: Box<str>) -> ProgressItemRef {
        let item = self.next_progbar_ref();
        _ = self.0.sender.send(ProgressMessage::Add { name, item });
        item
    }

    pub fn remove(&self, item: ProgressItemRef) {
        _ = self.0.sender.send(ProgressMessage::Remove(item));
    }

    pub fn clear_children(&self, item: ProgressItemRef) {
        _ = self.0.sender.send(ProgressMessage::ClearChildren(item));
    }

    pub fn print_stdout(&self, s: String) {
        _ = self.0.sender.send(ProgressMessage::PrintStdout(s));
    }

    pub fn print_stderr(&self, s: String) {
        _ = self.0.sender.send(ProgressMessage::PrintStdout(s));
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
    match style {
        Some(style) => start_thread_with(ProgressBar::new(style)),
        None => start_thread_nobar(),
    }
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
        _ => return handle_msg_no_bar(msg),
    }
    false
}

fn handle_msg_no_bar(msg: ProgressMessage) -> bool {
    match msg {
        ProgressMessage::Add { .. }
        | ProgressMessage::AddChild { .. }
        | ProgressMessage::Remove(_)
        | ProgressMessage::ClearChildren(_) => {}
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
        loop {
            // wait for incoming message
            let Ok(msg) = rx.recv() else { return };
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
                            break;
                        }
                    }
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => return,
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

    ProgressBarThread(Arc::new(ProgressBarThreadInner {
        next_progbar_item_ref: AtomicUsize::new(0),
        sender: tx,
        handle: Some(handle),
    }))
}

pub fn start_thread_nobar() -> ProgressBarThread {
    let (tx, rx) = channel();
    let handle = std::thread::spawn(move || {
        loop {
            match rx.try_recv() {
                Ok(msg) => {
                    if handle_msg_no_bar(msg) {
                        break;
                    }
                }
                Err(TryRecvError::Empty) => continue,
                Err(TryRecvError::Disconnected) => return,
            }
        }
    });

    ProgressBarThread(Arc::new(ProgressBarThreadInner {
        next_progbar_item_ref: AtomicUsize::new(0),
        sender: tx,
        handle: Some(handle),
    }))
}
