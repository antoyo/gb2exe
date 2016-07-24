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

use std::env;
use std::fs::remove_file;
use std::path::Path;
use std::process::Command;

/// Compile the rom file, returting the output of the compilation.
fn compile(rom_file: &str) -> String {
    let output_executable = get_output_executable(rom_file);

    let output = Command::new("target/debug/gb2exe")
        .arg(&format!("tests/{}", rom_file))
        .args(&["--debug", "-o", &output_executable])
        .output()
        .unwrap();

    String::from_utf8(output.stdout).unwrap()
}

/// Compile the rom file and execute the result, returning the output of the execution.
fn compile_and_execute(rom_file: &str) -> String {
    compile(rom_file);

    let output_executable = get_output_executable(rom_file);

    let output = Command::new(&output_executable)
        .output()
        .unwrap();

    remove_file(output_executable).unwrap();

    String::from_utf8(output.stdout).unwrap()
}

/// Get the path of the output executable from the rom filename.
fn get_output_executable(rom_file: &str) -> String {
    let path = Path::new(rom_file);
    let base_name = path.file_stem().unwrap();
    let temp_dir = env::temp_dir();
    let temp_file = temp_dir.join(base_name);
    temp_file.to_str().unwrap().to_string()
}

#[test]
fn header_checks() {
    let rom_files = ["invalid_checksum1.gb", "invalid_checksum2.gb", "invalid_header1.gb", "invalid_header2.gb"];
    for rom_file in &rom_files {
        let result = compile(rom_file);
        assert_eq!("Invalid rom file.\n", result);
    }
}

#[test]
fn simple_example() {
    let result = compile_and_execute("game.gb");
    assert_eq!("Hello World!\n", result);
    // TODO: check the window title.
}
