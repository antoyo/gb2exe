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
use ast::{Function, Program, Variable};
use ast::Expression::{self, Call, Sub, Val};
use ast::LeftValue::{self, Indirect, Var};
use ast::RightValue::{self, IntLiteral, StringLiteral};
use ast::Statement::{self, Assignment, Declaration, Expr, Goto, Increment, If, LabelStatement, Return};
use ast::Value::{self, LValue, RValue};

const MAIN_FUNCTION_GUI: &'static str = "
void game();

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
void game();

int main() {
    game();
}

";

/// Generate the C code from the AST.
pub fn gen(program: Program, title: &str, test: bool) -> String {
    let codes: Vec<_> = program.variables.iter().map(gen_var)
        .chain(program.functions.iter().map(gen_func))
        .collect();
    let main =
        if test {
            MAIN_FUNCTION_TERMINAL.to_string()
        }
        else {
            MAIN_FUNCTION_GUI.replace("{}", title)
        };
    gen_includes() + &main + &codes.join("\n\n")
}

/// Generate the code for the condition code.
fn gen_condition_code(condition_code: ConditionCode) -> String {
    match condition_code {
        Carry | NoCarry => unimplemented!(),
        NonZero => "NZ".to_string(),
        Zero => "Z == 0".to_string(),
    }
}

/// Generate the C code for an expression.
fn gen_expression(expression: &Expression) -> String {
    match *expression {
        Call(ref name, ref arguments) => {
            let args: Vec<_> = arguments.iter().map(gen_expression).collect();
            let args = args.join(", ");
            format!("{}({})", name, args)
        },
        Sub(ref expr1, ref expr2) =>
            format!("{} - {}", gen_expression(expr1), gen_expression(expr2)),
        Val(ref value) =>
            gen_value(value),
    }
}

/// Generate the C code for a function.
fn gen_func(function: &Function) -> String {
    let statements = gen_statements(&function.statements);
    format!("void {}() {{\n{}\n}}", function.name, statements)
}

/// Generate the C code for the includes.
fn gen_includes() -> String {
    let includes = vec!["stdio.h", "stdlib.h", "SFML/Graphics.h"];
    format!("#include <{}>\n", includes.join(">\n#include <"))
}

/// Generate the C code for an l-value.
fn gen_lvalue(lvalue: &LeftValue) -> String {
    match *lvalue {
        Indirect(ref name) => format!("*{}", name),
        Var(ref name) => name.clone(),
    }
}

/// Generate the C code for an r-value.
fn gen_rvalue(rvalue: &RightValue) -> String {
    match *rvalue {
        IntLiteral(integer) => integer.to_string(),
        StringLiteral(ref string) => format!("{:?}", string),
    }
}

/// Generate the C code for a statement.
fn gen_statement(statement: &Statement) -> String {
    match *statement {
        Assignment(ref lvalue, ref expression) =>
            format!("{} = {};", gen_lvalue(lvalue), gen_expression(expression)),
        Declaration(ref name) =>
            // TODO: do type inference to give the right type. Not sure it will be possible because the same register can be used for different types.
            format!("{} {};", "char*", name),
        Expr(ref expression) =>
            format!("{};", gen_expression(expression)),
        Goto(ref name) =>
            format!("goto {};", name),
        Increment(ref lvalue) =>
            format!("{}++;", gen_lvalue(lvalue)),
        If(condition_code, ref true_statements, ref else_statements) => {
            let else_code =
                match *else_statements {
                    Some(ref statements) => format!("else {{\n{}\n}}", gen_statements(statements)),
                    None => "".to_string(),
                };
            format!("if({}) {{\n{}\n}}{}", gen_condition_code(condition_code), gen_statements(true_statements), else_code)
        },
        LabelStatement(ref label) =>
            format!("{}:", label),
        Return(_) =>
            unimplemented!(),
    }
}

/// Generate the C code for statements.
fn gen_statements(statements: &[Statement]) -> String {
    let statements: Vec<_> = statements.iter().map(gen_statement).collect();
    statements.join("\n")
}

/// Generate the C code for a value.
fn gen_value(value: &Value) -> String {
    match *value {
        LValue(ref lvalue) => gen_lvalue(lvalue),
        RValue(ref rvalue) => gen_rvalue(rvalue),
    }
}

/// Generate the C code for a variable.
fn gen_var(variable: &Variable) -> String {
    format!("char* {} = {};", variable.name, gen_value(&variable.value))
}
