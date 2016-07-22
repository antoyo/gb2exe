use std::env;
use std::fs::remove_file;
use std::path::Path;
use std::process::Command;

/// Compile the rom file and execute the result, returning the output of the execution.
fn compile_and_execute(rom_file: &str) -> String {
    let path = Path::new(rom_file);
    let base_name = path.file_stem().unwrap();
    let temp_dir = env::temp_dir();
    let temp_file = temp_dir.join(base_name);
    let output_executable = temp_file.to_str().unwrap();

    Command::new("target/debug/gb2exe")
        .arg(&format!("tests/{}", rom_file))
        .args(&["-o", output_executable])
        .status()
        .unwrap();

    let output = Command::new(output_executable)
        .output()
        .unwrap();

    remove_file(output_executable).unwrap();

    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn simple_example() {
    let string = compile_and_execute("game.gb");
    assert_eq!("Hello World!\n", string);
}
