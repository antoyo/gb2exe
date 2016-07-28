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

//! C code generator.

use asm::decoder::ConditionCode::{self, Carry, NoCarry, NonZero, Zero};
use ast::{Function, Program};
use ast::Expression::{self, Addition, BitAnd, BitNot, BitOr, BitXor, FunctionCall, Not, ShiftLeft, ShiftRight, Subtraction, Val};
use ast::LeftValue::{self, Indirect, IndirectIncrement, Ram, Register, Var};
use ast::RightValue::{self, IntLiteral};
use ast::Statement::{self, Assignment, Decrement, Expr, Goto, If, Increment, Input, Output, LabelStatement, Return};
use ast::Value::{self, LValue, RValue};

const GLOBALS: &'static str = "
#define EIGH_K 8192

typedef struct Flags Flags;
struct Flags {
    bool zero : 1;
    bool subtract : 1;
    bool halfCarry : 1;
    bool carry : 1;
    uint8_t zeroes : 4;
};

Flags flags = { false, false, false, false, 0 };

int const BYTE_BITS = 8;

uint8_t _AF[2] = {0};
uint8_t _BC[2] = {0};
uint8_t _DE[2] = {0};
uint8_t _HL[2] = {0};

#define AF (*(uint16_t*)_AF)
#define BC (*(uint16_t*)_BC)
#define DE (*(uint16_t*)_DE)
#define HL (*(uint16_t*)_HL)

#define A (_AF[0])
#define F (_AF[1])
#define B (_BC[0])
#define C (_BC[1])
#define D (_DE[0])
#define E (_DE[1])
#define H (_HL[0])
#define L (_HL[1])

uint8_t ROM_DATA[];

uint8_t rotateLeft(uint8_t n) {
    return (n << 1) | (n >> (BYTE_BITS - 1));
}

uint8_t rotateRight(uint8_t n) {
    return (n >> 1) | (n << (BYTE_BITS - 1));
}

uint8_t swapNibbles(uint8_t n) {
    return (n & 0x0F) << 4 | (n & 0xF0) >> 4;
}

uint8_t wram[EIGH_K] = {0};

";

const MAIN_FUNCTION_GUI: &'static str = "
const int BASE_ADDRESS = 0xFF00;

void input(int value, int address) {
    printf(\"%d = *%d\\n\", value, address + BASE_ADDRESS);
}

void output(int address, int value) {
    printf(\"*%d = %d\\n\", address + BASE_ADDRESS, value);
}

int main() {
    sfVideoMode mode = {160, 144, 2};
    sfRenderWindow* window = sfRenderWindow_create(mode, \"{}\", sfClose, NULL);
    if(!window) {
        return EXIT_FAILURE;
    }

    sfRenderWindow_clear(window, sfBlack);
    sfRenderWindow_display(window);

    game();

    sfRenderWindow_destroy(window);

    return EXIT_SUCCESS;
}

";

const MAIN_FUNCTION_TERMINAL: &'static str = "
int main() {
    game();
}

";

/// Generate a correct function name.
/// If the function starts with a number, prepend "function".
fn function_name(name: &str) -> String {
    if name.chars().next().unwrap().is_digit(10) {
        format!("function{}", name)
    }
    else {
        name.to_string()
    }
}

/// Generate the C code from the AST.
pub fn gen(program: Program, title: &str, test: bool) -> String {
    let prototypes: Vec<_> =
        program.functions.iter()
            .map(gen_prototype)
            .collect();
    let codes: Vec<_> = program.functions.into_iter().map(gen_func).collect();
    let rom_data = gen_rom_data(&program.rom);
    let main =
        if test {
            MAIN_FUNCTION_TERMINAL.to_string()
        }
        else {
            MAIN_FUNCTION_GUI.replace("{}", title)
        };
    gen_includes() + GLOBALS + &prototypes.join("\n\n") + &main + &codes.join("\n\n") + "\n\n" + &rom_data
}

/// Generate the code for the condition code.
fn gen_condition_code(condition_code: ConditionCode) -> String {
    match condition_code {
        Carry => "flags.carry".to_string(),
        NoCarry => "!flags.carry".to_string(),
        NonZero => "!flags.zero".to_string(),
        Zero => "flags.zero".to_string(),
    }
}

/// Generate the C code for an expression.
fn gen_expression(expression: Expression) -> String {
    match expression {
        Addition(expr1, expr2) =>
            format!("{} + {}", gen_expression(*expr1), gen_expression(*expr2)),
        BitAnd(expr1, expr2) =>
            format!("{} & {}", gen_expression(*expr1), gen_expression(*expr2)),
        BitNot(expr) =>
            format!("~{}", gen_lvalue(expr)),
        BitOr(expr1, expr2) =>
            format!("{} | {}", gen_expression(*expr1), gen_expression(*expr2)),
        BitXor(expr1, expr2) =>
            format!("{} ^ {}", gen_expression(*expr1), gen_expression(*expr2)),
        FunctionCall(name, arguments) => {
            let args: Vec<_> = arguments.into_iter().map(gen_expression).collect();
            let args = args.join(", ");
            format!("{}({})", function_name(&name), args)
        },
        Not(expr) =>
            format!("!{}", gen_expression(*expr)),
        ShiftLeft(expr1, expr2) =>
            format!("{} << {}", gen_expression(*expr1), gen_expression(*expr2)),
        ShiftRight(expr1, expr2) =>
            format!("{} << {}", gen_expression(*expr1), gen_expression(*expr2)),
        Subtraction(expr1, expr2) =>
            format!("{} - {}", gen_expression(*expr1), gen_expression(*expr2)),
        Val(value) =>
            gen_value(value),
    }
}

/// Generate the C code for a function.
fn gen_func(function: Function) -> String {
    let statements = gen_statements(function.statements);
    format!("void {}() {{\n{}\n}}", function_name(&function.name), statements)
}

/// Generate the C code for the includes.
fn gen_includes() -> String {
    let includes = vec!["stdbool.h", "stdint.h", "stdio.h", "stdlib.h", "SFML/Graphics.h"];
    format!("#include <{}>\n\n", includes.join(">\n#include <"))
}

/// Generate the C code for an l-value.
fn gen_lvalue(lvalue: LeftValue) -> String {
    match lvalue {
        Indirect(name) => format!("ROM_DATA[{}]", name),
        IndirectIncrement(name) => format!("ROM_DATA[{}++]", name),
        Ram(address) => format!("wram[{}]", address),
        Register(name) => name,
        Var(address) => address.to_string(),
    }
}

/// Generate the C code for a function prototype.
fn gen_prototype(function: &Function) -> String {
    format!("void {}();", function_name(&function.name))
}

/// Generate the global variable containing the whole rom.
fn gen_rom_data(bytes: &[u8]) -> String {
    let bytes =
        bytes.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .chunks(16)
            .map(|chunk| chunk.join(", "))
            .collect::<Vec<_>>()
            .join(",\n    ");

    format!("uint8_t ROM_DATA[] = {{\n    {}\n}};", bytes)
}

/// Generate the C code for an r-value.
fn gen_rvalue(rvalue: RightValue) -> String {
    match rvalue {
        IntLiteral(integer) => integer.to_string(),
    }
}

/// Generate the C code for a statement.
fn gen_statement(statement: Statement) -> String {
    match statement {
        Assignment(lvalue, expression) =>
            format!("{} = {};", gen_lvalue(lvalue), gen_expression(expression)),
        Decrement(lvalue) =>
            format!("{}--;", gen_lvalue(lvalue)),
        Expr(expression) =>
            format!("{};", gen_expression(expression)),
        Goto(name) =>
            format!("goto label{};", name),
        If(condition_code, true_statements, else_statements) => {
            let else_code =
                match else_statements {
                    Some(statements) => format!("else {{\n{}\n}}", gen_statements(statements)),
                    None => "".to_string(),
                };
            format!("if({}) {{\n{}\n}}{}", gen_condition_code(condition_code), gen_statements(true_statements), else_code)
        },
        Increment(lvalue) =>
            format!("{}++;", gen_lvalue(lvalue)),
        Input(lvalue, address) =>
            gen_statement(
                Expr(FunctionCall(
                    "input".to_string(),
                    vec![lvalue, address],
                ))
            ),
        Output(address, value) =>
            gen_statement(
                Expr(FunctionCall(
                    "output".to_string(),
                    vec![address, value],
                ))
            ),
        LabelStatement(label) =>
            format!("label{}:", label),
        Return => "return;".to_string(),
    }
}

/// Generate the C code for statements.
fn gen_statements(statements: Vec<Statement>) -> String {
    let statements: Vec<_> = statements.into_iter().map(gen_statement).collect();
    statements.join("\n")
}

/// Generate the C code for a value.
fn gen_value(value: Value) -> String {
    match value {
        LValue(lvalue) => gen_lvalue(lvalue),
        RValue(rvalue) => gen_rvalue(rvalue),
    }
}
