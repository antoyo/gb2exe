//! Decompiler driver.

use std::fs::File;
use std::io::Read;

use asm::prettify::prettify;
use decompiler::Decompiler;
use gen::gen;
use rom::Rom;

/// Drive all the compilation steps from decoding the ROM, decompiling it and generating the final C source code.
pub fn drive(rom_file: &str) {
    let mut file = File::open(rom_file).unwrap();
    let mut bytes = vec![];
    file.read_to_end(&mut bytes).unwrap();
    let rom = Rom::new(&bytes);
    let instructions = prettify(rom.instructions);
    let decompiler = Decompiler::new(instructions);
    let program = decompiler.decompile();
    let source = gen(program);
    println!("{}", source);
}
