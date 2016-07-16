#[macro_use]
extern crate lazy_static;

mod cpu;
mod memory;

use cpu::Z80;

fn main() {
    let mut cpu = Z80::new();
    cpu.dispatch();
}
