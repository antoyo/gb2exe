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

/*
 * TODO: Add command-line arguments (specify output file, does not compile and keep the C file).
 * TODO: Add automated tests.
 * TODO: Refactor.
 * TODO: Remove the clone() calls.
 * TODO: Detect basic blocks.
 * TODO: Detect conditions/loops/functions.
 */

//! Custom API:
//! Write at $FEA0: write to stdout.
//! Write at $FEB0: exit with code.

mod asm;
mod ast;
mod bios;
mod compiler;
mod decompiler;
mod driver;
mod gen;
mod rom;

use std::env::args;

use driver::drive;

fn main() {
    let mut args = args();
    args.next(); // NOTE: ignore the program executable.

    if let Some(rom_file) = args.next() {
        drive(&rom_file);
    }
    else {
        println!("Specify the ROM file to compile.");
    }
}
