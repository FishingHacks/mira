use std::{
    io::stdout,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
        mpsc::{Sender, TryRecvError, channel},
    },
    thread::JoinHandle,
};

use crate::{ProgressBar, ProgressBarStyle, ProgressItemRef};

#[derive(Clone)]
pub struct ProgressBarThread {
    next_progbar_item_ref: Arc<AtomicUsize>,
    sender: Sender<ProgressMessage>,
}

impl Drop for ProgressBarThread {
    fn drop(&mut self) {
        self.stop_thread();
    }
}

impl ProgressBarThread {
    fn next_progbar_ref(&self) -> ProgressItemRef {
        ProgressItemRef(self.next_progbar_item_ref.fetch_add(1, Ordering::Relaxed))
    }

    pub fn add_child(&mut self, parent: ProgressItemRef, name: Box<str>) -> ProgressItemRef {
        let item = self.next_progbar_ref();
        _ = self
            .sender
            .send(ProgressMessage::AddChild { name, item, parent });
        item
    }

    pub fn add_item(&mut self, name: Box<str>) -> ProgressItemRef {
        let item = self.next_progbar_ref();
        _ = self.sender.send(ProgressMessage::Add { name, item });
        item
    }

    pub fn remove(&mut self, item: ProgressItemRef) {
        _ = self.sender.send(ProgressMessage::Remove(item));
    }

    pub fn clear_children(&mut self, item: ProgressItemRef) {
        _ = self.sender.send(ProgressMessage::ClearChildren(item));
    }

    pub fn stop_thread(&mut self) {
        _ = self.sender.send(ProgressMessage::StopThread);
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
}

pub fn start_thread(style: ProgressBarStyle) -> (JoinHandle<()>, ProgressBarThread) {
    start_thread_with(ProgressBar::new(style))
}

fn handle_msg(msg: ProgressMessage, bar: &mut ProgressBar) -> bool {
    match msg {
        ProgressMessage::StopThread => return true,
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
    }
    false
}

pub fn start_thread_with(bar: ProgressBar) -> (JoinHandle<()>, ProgressBarThread) {
    let (tx, rx) = channel();
    let handle = std::thread::spawn(move || {
        let mut bar = bar;
        loop {
            // wait for incoming message
            let Ok(msg) = rx.recv() else { return };
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
        if let Err(e) = bar.display(&mut stdout) {
            drop(stdout);
            eprintln!("Failed to write to stdout: {e:?}")
        }
    });

    (
        handle,
        ProgressBarThread {
            next_progbar_item_ref: Arc::default(),
            sender: tx,
        },
    )
}
