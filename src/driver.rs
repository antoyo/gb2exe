/*
 * Copyright (C) 2016  Boucher, Antoni <bouanto@zoho.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

//! Decompiler driver.

use std::fs::File;
use std::io::Read;

use asm::prettify::prettify;
use compiler::compile;
use decompiler::Decompiler;
use gen::gen;
use super::Args;
use rom::Rom;

/// Drive all the compilation steps from decoding the ROM, decompiling it and generating the final C source code.
pub fn drive(args: Args) {
    let c_only = args.flag_c_only;
    let rom_file = args.arg_rom_file;
    let output_file = args.flag_output;

    let mut file = File::open(rom_file).unwrap();
    let mut bytes = vec![];
    file.read_to_end(&mut bytes).unwrap();
    if let Some(rom) = Rom::new(&bytes) {
        let instructions = prettify(rom.instructions);
        let decompiler = Decompiler::new(instructions);
        let program = decompiler.decompile();
        let source = gen(program);
        compile(source, output_file, c_only);
    }
    else {
        println!("Invalid rom file.");
    }
}
