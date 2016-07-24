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

//! A Z80 assembly to AST decompiler.

use std::collections::{HashMap, HashSet};
use std::ops::Index;

use ast::{Expression, Function, Program, Statement, Variable};
use ast::Expression::{Call, Sub, Val};
use ast::Statement::{Assignment, Declaration, Expr, Goto, Increment, If, LabelStatement, Return};
use ast::Value::{self, LValue, RValue};
use ast::LeftValue::{self, Indirect, Var};
use ast::RightValue::{IntLiteral, StringLiteral};
use asm::decoder::Address::{self, AbsoluteAddress, AddressLabel, RelativeAddress};
use asm::decoder::DirectOperand::{self, AddressOperand, Imm8, Reg, Regs};
use asm::decoder::Instruction::{Cmp, Halt, Inc, InconditionalJump, Jump, Load, Nop};
use asm::decoder::Operand::{self, Direct, Indirection};
use rom::InstructionOrData::{Ascii, Deleted, Instr};
use rom::RepeatableInstruction::{self, NoRepeat};

const EXIT_ADDRESS: u16 = 0xFEB0; // Address mapped to exit.
const START_ADDRESS: u16 = 0x100;
const WRITE_ADDRESS: u16 = 0xFEA0; // Address mapped to write to stdout.

/// Call the extract function on every statement.
macro_rules! extract_from_if {
    ($true_statements:expr, $else_statements:expr, $extract_func:path$(, $parameter:expr)*) => {{
        let false_statements =
            match *$else_statements {
                Some(ref statements) =>
                    statements.iter()
                    .flat_map(|statement| $extract_func($($parameter,)* statement))
                    .collect(),
                    None => vec![],
            };

        $true_statements.iter()
            .flat_map(|statement| $extract_func($($parameter,)* statement))
            .chain(false_statements)
            .collect()
    }};
}

/// Produce a call statement if it is not in debug mode.
macro_rules! mapped_call {
    ($decompiler:expr, $function_name:expr, $value:expr, $index:expr) => {
        if $decompiler.debug {
            Expr(Call($function_name.to_string(), vec![$decompiler.expression(&$value)]))
        }
        else {
            next_statement!($decompiler, $index)
        }
    };
}

/// Ignore the current instruction and decompile the next one.
macro_rules! next_statement {
    ($decompiler:expr, $index:expr) => {{
        *$index += 1;
        $decompiler.statement($index)
    }};
}

/// Instructions with labels.
pub struct Instructions {
    instructions: Vec<RepeatableInstruction>,
    labels: HashMap<String, usize>,
    labels_by_index: HashMap<usize, String>,
    used_labels: HashSet<String>,
}

impl Instructions {
    /// Create a new instructions struct with labels.
    pub fn new() -> Instructions {
        Instructions {
            instructions: vec![],
            labels: HashMap::new(),
            labels_by_index: HashMap::new(),
            used_labels: HashSet::new(),
        }
    }

    /// Add a new instruction.
    pub fn add_instr(&mut self, mut instruction: RepeatableInstruction, instruction_address: u16) {
        // Remove the Deleted instructions.
        if let NoRepeat(Deleted) = instruction {
            return;
        }

        let instruction_index = self.instructions.len();
        let label =
            match instruction_address {
                START_ADDRESS => "start".to_string(),
                _ => label(instruction_address),
            };
        self.labels.insert(label.clone(), instruction_index);
        self.labels_by_index.insert(instruction_index, label);

        // Replace the address in the instruction by a label.
        match instruction {
            NoRepeat(Instr(ref mut instruction)) => {
                match *instruction {
                    Cmp(Direct(Imm8(_))) => (),
                    Halt => (),
                    Inc(Direct(Regs(_, _))) => (),
                    // NOTE: an immediate value of 16 bits is an address.
                    InconditionalJump(ref mut address) =>
                        self.add_visited_label(address, instruction_address),
                    Jump(_, ref mut address) =>
                        self.add_visited_label(address, instruction_address),
                    Load(_, Direct(AddressOperand(ref mut address))) =>
                        self.add_visited_label(address, instruction_address),
                    /*Load(Indirection(AddressOperand(ref mut address)), _) =>
                        self.add_visited_label(address, instruction_address),*/ // TODO: remove if not necessary. Or use it only when the address is not known (i.e. not a write to the screen).
                    Load(_, _) => (),
                    Nop => (),
                    _ => panic!("Instruction: {:?}", instruction),
                }
            },
            _ => (),
        }

        self.instructions.push(instruction);
    }

    /// Convert the address to a labeled address and add the label to the visited labels.
    fn add_visited_label(&mut self, address: &mut Address, instruction_address: u16) {
        match *address {
            RelativeAddress(delta) =>
                // NOTE: Plus 2 because the instructions takes 2 bytes.
                *address = AbsoluteAddress((instruction_address as isize + delta as isize + 2) as u16),
            _ => (),
        }

        match *address {
            AbsoluteAddress(absolute_address) => {
                let label = label(absolute_address);
                self.used_labels.insert(label.clone());
                *address = AddressLabel(label);
            },
            AddressLabel(_) => (),
            RelativeAddress(_) => panic!("Relative address should have been converted to absolute."),
        }
    }

    /// Get the instruction after the one at label.
    fn after(&self, label: &String) -> &RepeatableInstruction {
        let index = self.index(label) + 1;
        &self.instructions[index]
    }

    /// Get the index from the label.
    fn index(&self, label: &String) -> usize {
        self.labels[label]
    }
}

/// Get an instruction by indexing with a label.
impl<'a> Index<&'a String> for Instructions {
    type Output = RepeatableInstruction;

    fn index(&self, label: &'a String) -> &Self::Output {
        let index = self.index(label);
        &self.instructions[index]
    }
}

/// The Z80 assembly decompiler.
pub struct Decompiler {
    address_map: HashMap<String, LeftValue>,
    instructions: Instructions,
    program: Program,
    debug: bool,
}

impl Decompiler {
    /// Create a new decompiler.
    pub fn new(instructions: Instructions, debug: bool) -> Decompiler {
        Decompiler {
            address_map: HashMap::new(),
            instructions: instructions,
            program: Program::new(),
            debug: debug,
        }
    }

    /// Add a label statement in the function if the label is used elsewhere.
    fn add_label(&self, function: &mut Function, index: usize) {
        let label = &self.instructions.labels_by_index[&index];
        if self.instructions.used_labels.contains(label) {
            function.add_statement(LabelStatement(label.clone()));
        }
    }

    /// Decompile instructions into an abstract syntax tree.
    pub fn decompile(mut self) -> Program {
        let start_label = "start".to_string();
        assert_eq!(self.instructions[&start_label], NoRepeat(Instr(Nop)));

        let game_label =
            if let &NoRepeat(Instr(InconditionalJump(AddressLabel(ref label)))) = self.instructions.after(&start_label) {
                label.clone()
            }
            else {
                panic!("Expecting an inconditional jump at {:X}", START_ADDRESS + 1);
            };

        let function = self.function("game", game_label);
        self.program.add_func(function);
        self.program
    }

    /// Decompile an expression.
    fn expression(&mut self, expression: &Operand) -> Expression {
        match *expression {
            Direct(ref direct) =>
                match *direct {
                    ref reg@Reg(_) | ref reg@Regs(_, _) =>
                        Val(LValue(Var(register_to_name(&reg)))),
                    AddressOperand(AddressLabel(ref label)) =>
                        Val(LValue(self.get_value_from_label(label))),
                    Imm8(number) =>
                        Val(RValue(IntLiteral(number))),
                    _ => panic!("Unimplemented direct expression {:?}.", direct),
                },
            Indirection(ref reg@Regs(_, _)) =>
                Val(LValue(Indirect(register_to_name(reg)))),
            _ => panic!("Unimplemented expression {:?}.", expression),
        }
    }

    /// Get the index of the statements pointing to the jump contained within the statement.
    fn extract_jumps(&self, statement: &Statement) -> Vec<usize> {
        match *statement {
            Goto(ref label) =>
                vec![self.instructions.labels[label]],
            If(_, ref true_statements, ref else_statements) =>
                extract_from_if!(true_statements, else_statements, Decompiler::extract_jumps, self),
            _ => vec![],
        }
    }

    /// Decompile a function.
    fn function(&mut self, name: &str, function_label: String) -> Function {
        let mut function = Function::new(name);
        let mut index = self.instructions.index(&function_label);
        let mut jumps = HashSet::new();
        let mut variables = HashSet::new();

        loop {
            self.add_label(&mut function, index);

            let statement = self.statement(&mut index);

            // Save the instruction indexes pointing to the jump contained within the statement (if any).
            for index in self.extract_jumps(&statement) {
                jumps.insert(index);
            }

            // Save the variable name to only declare variables once.
            for variable in extract_variables(&statement) {
                if !variables.contains(&variable) {
                    function.prepend_statement(Declaration(variable.clone()));
                    variables.insert(variable);
                }
            }

            let is_inconditional = is_inconditional_jump(&statement);
            function.add_statement(statement);

            if is_inconditional {
                let index_to_check = index + 1;
                if !jumps.contains(&index_to_check) {
                    // The decompiler reached the end of the function.
                    break;
                }
            }
            index += 1;
        }
        function
    }

    /// Get a value from an address.
    /// Add it into the map if it does not exist.
    fn get_value_from_label(&mut self, label: &String) -> LeftValue {
        let variable_name = format!("var{}", label);
        let instruction_label = label.clone();
        self.address_map.entry(instruction_label)
            .or_insert({
                self.program.add_var(Variable::new(&variable_name,
                    get_value_from_data(&self.instructions[label])));
                Var(variable_name)
            })
            .clone()
    }

    /// Get an l-value.
    fn left_value(&mut self, expression: &Operand) -> LeftValue {
        let expr = self.expression(expression);
        if let Val(LValue(lvalue)) = expr {
            lvalue
        }
        else {
            panic!("Expecting an l-value.");
        }
    }

    /// Decompile an instruction into a statement.
    fn statement(&mut self, index: &mut usize) -> Statement {
        let instruction = self.instructions.instructions[*index].clone();
        match instruction {
            NoRepeat(Instr(ref instruction)) => {
                match *instruction {
                    Cmp(ref value) =>
                        Assignment(Var("Z".to_string()), Sub(
                                Box::new(Val(LValue(Var("A".to_string())))),
                                Box::new(self.expression(value)))),
                    Halt => next_statement!(self, index),
                    Inc(ref operand) =>
                        Increment(self.left_value(operand)),
                    InconditionalJump(AddressLabel(ref label)) =>
                        Goto(label.clone()),
                    // TODO: output the condition code of each operation.
                    Jump(condition_code, AddressLabel(ref label)) =>
                        If(condition_code, vec![Goto(label.clone())], None),

                    // Write to mapped address.
                    Load(Indirection(AddressOperand(AbsoluteAddress(WRITE_ADDRESS))), ref value) =>
                        mapped_call!(self, "putchar", value, index),
                    Load(Indirection(AddressOperand(AbsoluteAddress(EXIT_ADDRESS))), ref value) =>
                        mapped_call!(self, "exit", value, index),

                    Load(ref destination, ref value) =>
                        Assignment(self.left_value(destination), self.expression(value)),
                    Nop => next_statement!(self, index),
                    _ => panic!("Unexpected instruction: {:?}.", instruction),
                }
            },
            ref instruction => panic!("Unexpected instruction: {:?}.", instruction),
        }
    }
}

/// Get the variable name from the instruction if it uses a variable.
fn extract_variables(statement: &Statement) -> Vec<String> {
    match *statement {
        Assignment(Var(ref name), _) =>
            vec![name.clone()],
        Expr(_) | Goto(_) | Increment(_) =>
            vec![],
        If(_, ref true_statements, ref else_statements) =>
            extract_from_if!(true_statements, else_statements, extract_variables),
        _ => panic!("{:?}", statement),
    }
}

/// Get the value from data bytes.
fn get_value_from_data(data: &RepeatableInstruction) -> Value {
    match *data {
        NoRepeat(ref data) =>
            match *data {
                Ascii(ref string) =>
                    RValue(StringLiteral(string.clone())),
                _ => panic!("Unexpected data: {:?}.", data),
            }
        ,
        ref data => panic!("Unexpected data: {:?}", data),
    }
}

/// Check if a statement is the end of the function.
fn is_inconditional_jump(statement: &Statement) -> bool {
    match *statement {
        Goto(_) | Return(_) => true,
        _ => false,
    }
}

/// Create a label from an address.
pub fn label(address: u16) -> String {
    format!("label{}", address)
}

/// Convert a register operand to its name.
fn register_to_name(operand: &DirectOperand) -> String {
    match *operand {
        Reg(ref register) => format!("{:?}", register),
        Regs(ref reg1, ref reg2) => format!("{:?}{:?}", reg1, reg2),
        _ => panic!("Expecting a register operand."),
    }
}
