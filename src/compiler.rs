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

//! A wrapper over the GCC compiler.

use std::env;
use std::fs::{File, create_dir, remove_dir_all};
use std::io::Write;
use std::process::Command;

const COMPILER: &'static str = "gcc";

/// Compile the C source code into an executable.
pub fn compile(source: String) {
    // Create a temporary directory.
    let temp_path = env::temp_dir().join("gb2exe");
    let temp_dir = temp_path.to_str().unwrap();
    let _ = create_dir(temp_dir);

    // Write the source code to a file.
    let c_source_path = temp_path.join("main.c");
    let c_source_file = c_source_path.to_str().unwrap();
    let mut file = File::create(c_source_file).unwrap();
    write!(&mut file, "{}", source).unwrap();

    // Compile the source code to an executable.
    Command::new(COMPILER)
        .args(&["-o", "main", c_source_file])
        .status()
        .unwrap();

    // Remove the temporary directory.
    remove_dir_all(temp_dir).unwrap();
}
