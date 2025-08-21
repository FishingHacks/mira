use std::path::PathBuf;

fn main() {
    let path = std::env::args().nth(1).expect("usage: miradoc <path>");
    let path = PathBuf::from(path);
}
