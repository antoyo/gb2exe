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
 * The bios does the following:
 * * Clear the screen (done).
 * * Initialize audio.
 * * Show the Nintendo logo:
 * * * Initilize the palette.
 * * * Show the logo.
 * * * Play beeps.
 * * Check the Nintendo logo (done).
 * * Verify the checksum (done).
 *
 * TODO: Add close event (probably when the other events are checked, or on Halt).
 * TODO: Detect basic blocks.
 * TODO: Detect conditions/loops/infinite loops (game loops?).
 */

//! Custom API:
//! Write at $FEA0: write to stdout.
//! Write at $FEB0: exit with code.

extern crate docopt;
extern crate rustc_serialize;

mod asm;
mod ast;
mod bios;
mod compiler;
mod decompiler;
mod driver;
mod gen;
mod rom;

use std::env::args;

use docopt::Docopt;

use driver::drive;

const USAGE: &'static str = "
GameBoy to Executable

Usage:
    gb2exe <rom-file> [(-o <exe-file> | --output <exe-file>)] [(-C | --c-only)] [--debug]
    gb2exe (-h | --help)

Options:
    -h --help               Show this help page.
    -o --output <exe-file>  Set the output file (executable or c source file).
    -C --c-only             Output the C code only; do not compile, assemble and link.
";

#[derive(Debug, RustcDecodable)]
pub struct Args {
    arg_rom_file: String,
    flag_c_only: bool,
    flag_debug: bool,
    flag_output: Option<String>,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|docopt| docopt.decode())
        .unwrap_or_else(|error| error.exit());

    drive(args);
}
