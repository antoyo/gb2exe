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

use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Index;

use ast::{Expression, Function, Program, Statement, Variable, int_val};
use ast::Expression::{Addition, BitAnd, BitNot, BitOr, BitXor, FunctionCall, ShiftLeft, ShiftRight, Subtraction, Val};
use ast::Statement::{Assignment, Decrement, Expr, Goto, If, Increment, Input, Output, LabelStatement, Return};
use ast::Value::{self, LValue, RValue};
use ast::LeftValue::{self, Indirect, IndirectIncrement, Ram, Var};
use ast::RightValue::{IntLiteral, StringLiteral};
use asm::decoder::BASE_ADDRESS;
use asm::decoder::Address::{self, AbsoluteAddress, AddressLabel, RelativeAddress};
use asm::decoder::DirectOperand::{self, AddressOperand, Imm8, Reg, Regs};
use asm::decoder::Instruction::{Add, And, Bit, Call, Cmp, Complement, Dec, DisableInterrupt, EnableInterrupt, Halt, Inc, InconditionalJump, Jump, Load, LoadIncrement, Nop, Or, Pop, Push, Ret, RotateLeft, RotateLeftCarry, RotateRight, RotateRightCarry, ShiftLeftArithmetic, ShiftRightLogical, ShiftRightArithmetic, Sub, Swap, Xor};
use asm::decoder::Operand::{self, BaseIndirection, Direct, Indirection};
use asm::decoder::Register::SP;
use rom::InstructionOrData::{Ascii, Deleted, Instr};
use rom::RepeatableInstruction::{self, NoRepeat};

const EXIT_ADDRESS: u16 = 0xFEB0; // Address mapped to exit.
const GRAPHICS_RAM_START_ADDRESS: u16 = 0x8000;
const START_ADDRESS: u16 = 0x100;
const WRAM_END: u16 = 0xDFFF;
const WRAM_START: u16 = 0xC000;
const WRITE_ADDRESS: u16 = 0xFEA0; // Address mapped to write to stdout.

/// Assign the expression to reg.
macro_rules! assign {
    ($reg:expr, $expression:expr) => {
        Assignment(var!($reg), $expression)
    };
}

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

/// Create an l-value from a register name.
macro_rules! lvalue {
    ($reg:expr) => {
        Val(LValue(var!($reg)))
    };
}

/// Produce a call statement if it is not in debug mode.
macro_rules! mapped_call {
    ($decompiler:expr, $function_name:expr, $value:expr) => {
        if $decompiler.debug {
            Expr(FunctionCall($function_name.to_string(), vec![$decompiler.expression(&$value)]))
        }
        else {
            return None;
        }
    };
}

/// Create a variable from a &str.
macro_rules! var {
    ($reg:expr) => {
        Var($reg.to_string())
    };
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
                    Add(_, _) |
                        And(_) |
                        Bit(_, _) |
                        Cmp(_) |
                        Complement |
                        Dec(_) |
                        DisableInterrupt |
                        EnableInterrupt |
                        Halt |
                        Inc(_) |
                        LoadIncrement(_, _) |
                        Nop |
                        Or(_) |
                        Pop(_, _) |
                        Push(_, _) |
                        Ret |
                        RotateLeft(_) |
                        RotateLeftCarry(_) |
                        RotateRight(_) |
                        RotateRightCarry(_) |
                        ShiftLeftArithmetic(_) |
                        ShiftRightLogical(_) |
                        ShiftRightArithmetic(_) |
                        Sub(_, _) |
                        Swap(_) |
                        Xor(_)
                    => (),
                    // NOTE: an immediate value of 16 bits is an address.
                    Call(ref mut address) |
                        InconditionalJump(ref mut address) |
                        Jump(_, ref mut address) |
                        Load(_, Direct(AddressOperand(ref mut address)))
                    =>
                        self.add_visited_label(address, instruction_address),
                    /*Load(Indirection(AddressOperand(ref mut address)), _) =>
                        self.add_visited_label(address, instruction_address),*/ // TODO: remove if not necessary. Or use it only when the address is not known (i.e. not a write to the screen).
                    Load(_, _) => (),
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
                if absolute_address < GRAPHICS_RAM_START_ADDRESS {
                    let label = label(absolute_address);
                    self.used_labels.insert(label.clone());
                    *address = AddressLabel(label);
                }
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
    debug: bool,
    functions_to_visit: VecDeque<String>,
    instructions: Instructions,
    program: Program,
    visited_functions: HashSet<String>,
}

impl Decompiler {
    /// Create a new decompiler.
    pub fn new(instructions: Instructions, debug: bool) -> Decompiler {
        let mut visited_functions = HashSet::new();
        visited_functions.insert("push".to_string());
        visited_functions.insert("pop".to_string());

        Decompiler {
            address_map: HashMap::new(),
            debug: debug,
            functions_to_visit: VecDeque::new(),
            instructions: instructions,
            program: Program::new(),
            visited_functions: visited_functions,
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

        while let Some(function_label) = self.functions_to_visit.pop_front() {
            if !self.visited_functions.contains(&function_label) {
                let function = self.function(&function_label, function_label.to_string());
                self.program.add_func(function);
            }
        }

        self.program
    }

    /// Decompile a direct operand.
    fn direct_operand(&mut self, operand: &DirectOperand) -> Expression {
        match *operand {
            ref reg@Reg(_) | ref reg@Regs(_, _) =>
                Val(LValue(Var(register_to_name(&reg)))),
            AddressOperand(AbsoluteAddress(address)) =>
                int_val(address as u32),
            AddressOperand(AddressLabel(ref label)) =>
                Val(LValue(self.get_value_from_label(label))),
            Imm8(number) =>
                int_val(number as u32),
            _ => panic!("Unimplemented direct expression {:?}.", operand),
        }
    }

    /// Decompile an expression.
    fn expression(&mut self, expression: &Operand) -> Expression {
        match *expression {
            Direct(ref direct) => self.direct_operand(direct),
            Indirection(ref reg@Regs(_, _)) =>
                Val(LValue(Indirect(register_to_name(reg)))),
            Indirection(AddressOperand(AbsoluteAddress(address))) if is_wram(address) =>
                Val(LValue(Ram(address - WRAM_START))),
            _ => panic!("Unimplemented expression {:?}.", expression),
        }
    }

    /// Get the index of the statements pointing to the jump contained within the statement.
    fn extract_jumps(&mut self, statement: &Statement) -> Vec<usize> {
        match *statement {
            Expr(FunctionCall(ref label, _)) => {
                if (!self.debug || (label != "putchar" && label != "exit")) && !self.visited_functions.contains(label) {
                    self.functions_to_visit.push_back(label.clone());
                }
                vec![]
            },
            Goto(ref label) =>
                vec![self.instructions.labels[label]],
            If(_, ref true_statements, ref else_statements) =>
                extract_from_if!(true_statements, else_statements, Decompiler::extract_jumps, self),
            _ => vec![],
        }
    }

    /// Decompile a function.
    fn function(&mut self, name: &str, function_label: String) -> Function {
        self.visited_functions.insert(function_label.clone());

        let mut function = Function::new(name);
        let mut index = self.instructions.index(&function_label);
        let mut jumps = VecDeque::new();
        let mut visited = HashSet::new();

        'statement: loop {
            let mut statement;

            loop {
                visited.insert(index);
                self.add_label(&mut function, index);

                statement = self.statement(index);
                if let None = statement {
                    index += 1;
                }
                else {
                    break;
                }
            }

            let statement = statement.unwrap(); // Safe since there is a loop above to discard the None values.

            // Save the instruction indexes pointing to the jump contained within the statement (if any).
            for idx in self.extract_jumps(&statement) {
                if !visited.contains(&idx) {
                    jumps.push_back(idx);
                }
            }

            let is_inconditional = is_inconditional_jump(&statement);
            function.add_statement(statement);

            if is_inconditional {
                while let Some(idx) = jumps.pop_front() {
                    if !visited.contains(&idx) {
                        index = idx;
                        continue 'statement;
                    }
                }
                break;
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
    fn statement(&mut self, index: usize) -> Option<Statement> {
        fn stmt(decompiler: &mut Decompiler, instruction: RepeatableInstruction) -> Option<Statement> {
            let statement =
                match instruction {
                    NoRepeat(Instr(instruction)) => {
                        match instruction {
                            Add(ref expr1, ref expr2) =>
                                assign!("A", Addition(
                                    Box::new(decompiler.expression(expr1)),
                                    Box::new(decompiler.expression(expr2)),
                                )),
                            And(ref expr) =>
                                assign!("A", BitAnd(
                                    Box::new(lvalue!("A")),
                                    Box::new(decompiler.expression(expr)),
                                )),
                            Bit(bit, ref expr) =>
                                assign!("Z", BitAnd(
                                        Box::new(int_val(bit as u32)),
                                        Box::new(decompiler.expression(expr)))),
                            Call(AddressLabel(ref function_name)) =>
                                Expr(FunctionCall(function_name.clone(), vec![])),
                            Cmp(ref value) =>
                                assign!("flags.zero", Subtraction(
                                        Box::new(lvalue!("A")),
                                        Box::new(decompiler.expression(value)))),
                            Complement =>
                                assign!("A", BitNot(var!("A"))),
                            Dec(ref operand) =>
                                Decrement(decompiler.left_value(operand)),
                            DisableInterrupt => return None, // TODO: disable the interrupt.
                            EnableInterrupt => return None, // TODO: enable the interrupt.
                            Halt | Nop => return None,
                            Inc(ref operand) =>
                                Increment(decompiler.left_value(operand)),
                            InconditionalJump(AddressLabel(ref label)) =>
                                Goto(label.clone()),
                            // TODO: output the condition code of each operation.
                            Jump(condition_code, AddressLabel(ref label)) =>
                                If(condition_code, vec![Goto(label.clone())], None),

                            // Write to mapped address.
                            Load(Indirection(AddressOperand(AbsoluteAddress(WRITE_ADDRESS))), ref value) =>
                                mapped_call!(decompiler, "putchar", value),
                            Load(Indirection(AddressOperand(AbsoluteAddress(EXIT_ADDRESS))), ref value) =>
                                mapped_call!(decompiler, "exit", value),

                            // Ignore write to the stack pointer since it won't exist in the executable.
                            Load(Direct(Reg(SP)), Direct(AddressOperand(_))) =>
                                return None,

                            // Write to absolute address is input/output.
                            Load(BaseIndirection(address, ref index), ref value) if address == BASE_ADDRESS =>
                                Output(decompiler.direct_operand(index), decompiler.expression(value)),
                            Load(ref value, BaseIndirection(address, ref index)) if address == BASE_ADDRESS =>
                                Input(decompiler.expression(value), decompiler.direct_operand(index)),

                            // Ram access.
                            Load(Indirection(AddressOperand(AbsoluteAddress(address))), ref value) =>
                                if is_wram(address) {
                                    Assignment(Ram(address - WRAM_START), decompiler.expression(value))
                                }
                                else {
                                    panic!("Assignment to unknown address. IO?");
                                },

                            Load(ref destination, ref value) =>
                                Assignment(decompiler.left_value(destination), decompiler.expression(value)),

                            LoadIncrement(Direct(registers@Regs(_, _)), value) => {
                                let destination = Direct(registers.clone());
                                return stmt(decompiler, NoRepeat(Instr(Load(destination, value))))
                                    .map(|statement|
                                         if let Assignment(_, rvalue) = statement {
                                             Assignment(IndirectIncrement(register_to_name(&registers)), rvalue)
                                         }
                                         else {
                                             statement
                                         }
                                    );
                            },

                            Or(ref expr) =>
                                assign!("A", BitOr(
                                    Box::new(lvalue!("A")),
                                    Box::new(decompiler.expression(expr)),
                                )),

                            Pop(reg1, reg2) =>
                                Expr(FunctionCall("pop".to_string(), vec![
                                    // TODO: not sure it works as is.
                                    lvalue!(register_to_name(&Regs(reg1, reg2)))
                                ])),

                            Push(reg1, reg2) =>
                                Expr(FunctionCall("push".to_string(), vec![
                                    lvalue!(register_to_name(&Regs(reg1, reg2)))
                                ])),

                            Ret => Return,

                            RotateLeft(Direct(register@Reg(_))) => {
                                let name = register_to_name(&register);
                                assign!(name, FunctionCall("rotateLeft".to_string(), vec![
                                    lvalue!(name)
                                ]))
                            },

                            RotateRight(Direct(register@Reg(_))) => {
                                let name = register_to_name(&register);
                                assign!(name, FunctionCall("rotateRight".to_string(), vec![
                                    lvalue!(name)
                                ]))
                            },

                            ShiftLeftArithmetic(ref expr) =>
                                assign!("A", ShiftLeft(Box::new(lvalue!("A")), Box::new(decompiler.expression(expr)))),

                            ShiftRightLogical(ref expr) =>
                                assign!("A", ShiftRight(Box::new(lvalue!("A")), Box::new(decompiler.expression(expr)))),

                            Sub(ref expr1, ref expr2) =>
                                assign!("A", Subtraction(
                                    Box::new(decompiler.expression(expr1)),
                                    Box::new(decompiler.expression(expr2)),
                                )),

                            Swap(ref expr) => {
                                Assignment(decompiler.left_value(expr), FunctionCall("swapNibbles".to_string(), vec![
                                    decompiler.expression(expr)
                                ]))
                            },

                            Xor(ref expr) =>
                                assign!("A", BitXor(
                                    Box::new(lvalue!("A")),
                                    Box::new(decompiler.expression(expr)),
                                )),

                            _ => panic!("Unexpected instruction: {:?}.", instruction),
                        }
                    },
                    ref instruction => panic!("Unexpected instruction: {:?}.", instruction),
                };

            Some(statement)
        }

        let instruction = self.instructions.instructions[index].clone();
        stmt(self, instruction)
    }
}

/// Get the value from data bytes.
fn get_value_from_data(data: &RepeatableInstruction) -> Value {
    match *data {
        NoRepeat(ref data) =>
            match *data {
                Ascii(ref string) =>
                    RValue(StringLiteral(string.clone())),
                _ => RValue(IntLiteral(0)), // panic!("Unexpected data: {:?}.", data), TODO: remove the IntLiteral
            }
        ,
        _ => RValue(IntLiteral(0)), // panic!("Unexpected data: {:?}", data), TODO: remove the IntLiteral
    }
}

/// Check if a statement is the end of the function.
fn is_inconditional_jump(statement: &Statement) -> bool {
    match *statement {
        Goto(_) | Return => true,
        _ => false,
    }
}

/// Check if the address is within the working ram.
fn is_wram(address: u16) -> bool {
    address >= WRAM_START && address <= WRAM_END
}

/// Create a label from an address.
pub fn label(address: u16) -> String {
    address.to_string()
}

/// Convert a register operand to its name.
fn register_to_name(operand: &DirectOperand) -> String {
    match *operand {
        Reg(ref register) => format!("{:?}", register),
        Regs(ref reg1, ref reg2) => format!("{:?}{:?}", reg1, reg2),
        _ => panic!("Expecting a register operand."),
    }
}
