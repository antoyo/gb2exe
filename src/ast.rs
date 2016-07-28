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

//! Abstract syntax tree of the decompiled program.

use asm::decoder::ConditionCode;
use self::Expression::*;
use self::RightValue::*;
use self::Value::*;

/// A program abstract syntax tree.
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub rom: Vec<u8>,
}

impl Program {
    /// Create a new program abstract syntax tree.
    pub fn new(bytes: &[u8]) -> Program {
        Program {
            functions: vec![],
            rom: bytes.iter().cloned().collect(),
        }
    }

    /// Add a global function.
    pub fn add_func(&mut self, function: Function) {
        self.functions.push(function);
    }
}

/// An expression.
#[derive(Debug)]
pub enum Expression {
    Addition(Box<Expression>, Box<Expression>),
    BitAnd(Box<Expression>, Box<Expression>),
    BitNot(LeftValue),
    BitOr(Box<Expression>, Box<Expression>),
    BitXor(Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    Not(Box<Expression>),
    ShiftLeft(Box<Expression>, Box<Expression>),
    ShiftRight(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Val(Value),
}

/// A function.
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub statements: Vec<Statement>,
}

impl Function {
    /// Create a new function.
    pub fn new(name: &str) -> Function {
        Function {
            name: name.to_string(),
            statements: vec![],
        }
    }

    /// Add a new statement.
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

/// An l-value.
#[derive(Clone, Debug)]
pub enum LeftValue {
    Indirect(String),
    IndirectIncrement(String),
    Ram(u16),
    Register(String),
    Var(u16),
}

/// A r-value.
#[derive(Clone, Debug)]
pub enum RightValue {
    IntLiteral(u32),
}

/// A statement.
#[derive(Debug)]
pub enum Statement {
    Assignment(LeftValue, Expression),
    Decrement(LeftValue),
    Expr(Expression),
    Goto(String),
    If(ConditionCode, Vec<Statement>, Option<Vec<Statement>>),
    Increment(LeftValue),
    Input(Expression, Expression),
    Output(Expression, Expression),
    LabelStatement(String),
    Return,
}

/// A value.
#[derive(Clone, Debug)]
pub enum Value {
    LValue(LeftValue),
    RValue(RightValue),
}

/// Create an int literal expression.
pub fn int_val(int: u32) -> Expression {
    Val(RValue(IntLiteral(int)))
}
