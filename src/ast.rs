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

/// A program abstract syntax tree.
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub variables: Vec<Variable>,
}

impl Program {
    /// Create a new program abstract syntax tree.
    pub fn new() -> Program {
        Program {
            functions: vec![],
            variables: vec![],
        }
    }

    /// Add a global function.
    pub fn add_func(&mut self, function: Function) {
        self.functions.push(function);
    }

    /// Add a global variable.
    pub fn add_var(&mut self, variable: Variable) {
        self.variables.push(variable);
    }
}

/// An expression.
#[derive(Debug)]
pub enum Expression {
    Call(String, Vec<Expression>),
    Sub(Box<Expression>, Box<Expression>),
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

    /// Add a new statement at the start of the function.
    pub fn prepend_statement(&mut self, statement: Statement) {
        self.statements.insert(0, statement);
    }
}

/// An l-value.
#[derive(Clone, Debug)]
pub enum LeftValue {
    Indirect(String),
    Var(String),
}

/// A r-value.
#[derive(Clone, Debug)]
pub enum RightValue {
    IntLiteral(u8),
    StringLiteral(String),
}

/// A statement.
#[derive(Debug)]
pub enum Statement {
    Assignment(LeftValue, Expression),
    Declaration(String),
    Expr(Expression),
    Goto(String),
    Increment(LeftValue),
    If(ConditionCode, Vec<Statement>, Option<Vec<Statement>>),
    LabelStatement(String),
    Return(Value),
}

/// A value.
#[derive(Clone, Debug)]
pub enum Value {
    LValue(LeftValue),
    RValue(RightValue),
}

/// A variable declaration.
#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub value: Value,
}

impl Variable {
    /// Create a new variable with a value.
    pub fn new(name: &str, value: Value) -> Variable {
        Variable {
            name: name.to_string(),
            value: value,
        }
    }
}
