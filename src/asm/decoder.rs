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

//! Z80 CPU instruction decoder.

use self::Address::*;
use self::ConditionCode::*;
use self::DirectOperand::*;
use self::Instruction::*;
use self::Operand::*;
use self::Register::*;

/// Get a 16-bit address from the second byte of `$bytes`.
macro_rules! abs_address {
    ($bytes:expr) => {
        AbsoluteAddress(bits16!($bytes))
    };
}

/// Get a base indirection starting at BASE_ADDRESS.
macro_rules! base_indirect {
    ($index:expr) => {
        BaseIndirection(BASE_ADDRESS, $index)
    };
}

/// Get a 16-bit value from the second byte of `$bytes`.
macro_rules! bits16 {
    ($bytes:expr) => {
        imm16(&$bytes[1..])
    };
}

/// Get a 8-bit immediate value from the second byte of `$bytes`.
macro_rules! direct_imm8 {
    ($bytes:expr) => {
        Direct(Imm8($bytes[1]))
    };
}

/// Get a direct 16-bit immediate value from the second byte of `$bytes`.
macro_rules! direct_imm16 {
    ($bytes:expr) => {
        Direct(imm16!($bytes))
    };
}

/// Get a 8-bit immediate value from the second byte of `$bytes`.
macro_rules! imm8 {
    ($bytes:expr) => {
        Imm8($bytes[1])
    };
}

/// Get a 16-bit immediate value from the second byte of `$bytes`.
macro_rules! imm16 {
    ($bytes:expr) => {
        AddressOperand(AbsoluteAddress(bits16!($bytes)))
    };
}

/// Get an indirect register operand.
macro_rules! indirect_reg {
    ($reg:expr) => {
        Indirection(Reg($reg))
    };
}

/// Get an indirect 2-register operand.
macro_rules! indirect_regs {
    ($reg1:expr, $reg2:expr) => {
        Indirection(Regs($reg1, $reg2))
    };
}

/// Get a direct register operand.
macro_rules! reg {
    ($reg:expr) => {
        Direct(Reg($reg))
    };
}

/// Get a direct 2-register operand.
macro_rules! regs {
    ($reg1:expr, $reg2:expr) => {
        Direct(Regs($reg1, $reg2))
    };
}

/// Get a relative address.
macro_rules! rel_address {
    ($bytes:expr) => {
        RelativeAddress($bytes[1] as i8)
    };
}

const BASE_ADDRESS: u16 = 0xFF00;
const INDIRECT_HL: Operand = indirect_regs!(H, L);

/// Either an absolute address or a label.
#[derive(Clone, Debug, PartialEq)]
pub enum Address {
    AbsoluteAddress(u16),
    AddressLabel(String),
    RelativeAddress(i8),
}

/// The condition codes used in conditional jumps.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConditionCode {
    Carry,
    NoCarry,
    NonZero,
    Zero,
}

/// A direct operand is either an immediate value or a register.
#[derive(Clone, Debug, PartialEq)]
pub enum DirectOperand {
    Imm8(u8),
    AddressOperand(Address),
    Reg(Register),
    Regs(Register, Register),
}

/// The Z80 instructions.
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Add(Operand, Operand),
    And(Operand),
    Bit(u8, Operand),
    Call(Address),
    Complement,
    Cmp(Operand),
    Dec(Operand),
    DisableInterrupt,
    EnableInterrupt,
    Halt,
    Inc(Operand),
    InconditionalJump(Address),
    Jump(ConditionCode, Address),
    Load(Operand, Operand),
    LoadDecrement(Operand, Operand),
    LoadIncrement(Operand, Operand),
    Nop,
    Or(Operand),
    Pop(Register, Register),
    Push(Register, Register),
    Ret,
    RotateLeft(Operand),
    RotateLeftCarry(Operand),
    RotateRight(Operand),
    RotateRightCarry(Operand),
    ShiftLeftArithmetic(Operand),
    ShiftRight(Operand),
    ShiftRightArithmetic(Operand),
    Sub(Operand, Operand),
    Swap(Operand),
    Xor(Operand),
}

/// The different kind of operands.
#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    BaseIndirection(u16, DirectOperand),
    Direct(DirectOperand),
    Indirection(DirectOperand),
}

/// The Z80 CPU registers.
#[derive(Clone, Debug, PartialEq)]
pub enum Register {
    // 8-bit registers.
    A, B, C, D, E, F, H, L,
    // 16-bit registers
    SP,
}

/// Decode the first instruction in the sequence.
pub fn decode(bytes: &[u8]) -> (Instruction, usize) {
    let immediate_value_size = immediate_size_by_opcode(bytes[0]);
    let instruction_size = immediate_value_size + 1;
    let instruction = decode_instruction(&bytes[.. instruction_size + 1]);
    (instruction, instruction_size)
}

/// Decode one instruction.
fn decode_instruction(bytes: &[u8]) -> Instruction {
    match bytes[0] {
        0x00 => Nop,
        0x01 => Load(regs!(B, C), direct_imm16!(bytes)),
        0x02 => Load(indirect_regs!(B, C), reg!(A)),
        0x03 => Inc(regs!(B, C)),
        0x04 => Inc(reg!(B)),
        0x05 => Dec(reg!(B)),
        0x06 => Load(reg!(B), direct_imm8!(bytes)),
        0x09 => Add(regs!(H, L), regs!(B, C)),
        0x0A => Load(reg!(A), indirect_regs!(B, C)),
        0x0C => Inc(reg!(C)),
        0x0D => Dec(reg!(C)),
        0x0E => Load(reg!(C), direct_imm8!(bytes)),
        0x11 => Load(regs!(D, E), direct_imm16!(bytes)),
        0x12 => Load(indirect_regs!(D, E), reg!(A)),
        0x13 => Inc(regs!(D, E)),
        0x14 => Inc(reg!(D)),
        0x15 => Dec(reg!(D)),
        0x16 => Load(reg!(D), direct_imm8!(bytes)),
        0x17 => RotateLeft(reg!(A)),
        0x18 => InconditionalJump(rel_address!(bytes)),
        0x19 => Add(regs!(H, L), regs!(D, E)),
        0x1A => Load(reg!(A), indirect_regs!(D, E)),
        0x1C => Inc(reg!(E)),
        0x1D => Dec(reg!(E)),
        0x1E => Load(reg!(E), direct_imm8!(bytes)),
        0x20 => Jump(NonZero, rel_address!(bytes)),
        0x21 => Load(regs!(H, L), direct_imm16!(bytes)),
        0x22 => LoadIncrement(regs!(H, L), reg!(A)),
        0x23 => Inc(regs!(H, L)),
        0x24 => Inc(reg!(H)),
        0x25 => Dec(reg!(H)),
        0x26 => Load(reg!(H), direct_imm8!(bytes)),
        0x28 => Jump(Zero, rel_address!(bytes)),
        0x29 => Add(regs!(H, L), regs!(H, L)),
        0x2C => Inc(reg!(L)),
        0x2D => Dec(reg!(L)),
        0x2E => Load(reg!(L), direct_imm8!(bytes)),
        0x2F => Complement,
        0x30 => Jump(NoCarry, rel_address!(bytes)),
        0x31 => Load(reg!(SP), direct_imm16!(bytes)),
        0x32 => LoadDecrement(regs!(H, L), reg!(A)),
        0x33 => Inc(reg!(SP)),
        0x34 => Inc(INDIRECT_HL),
        0x35 => Dec(INDIRECT_HL),
        0x36 => Load(INDIRECT_HL, direct_imm8!(bytes)),
        0x38 => Jump(Carry, rel_address!(bytes)),
        0x39 => Add(regs!(H, L), reg!(SP)),
        0x3C => Inc(reg!(A)),
        0x3D => Dec(reg!(A)),
        0x3E => Load(reg!(A), direct_imm8!(bytes)),
        0x40 => Load(reg!(B), reg!(B)),
        0x41 => Load(reg!(B), reg!(C)),
        0x42 => Load(reg!(B), reg!(D)),
        0x43 => Load(reg!(B), reg!(E)),
        0x44 => Load(reg!(B), reg!(H)),
        0x45 => Load(reg!(B), reg!(L)),
        0x46 => Load(reg!(B), INDIRECT_HL),
        0x47 => Load(reg!(B), reg!(A)),
        0x48 => Load(reg!(C), reg!(B)),
        0x49 => Load(reg!(C), reg!(C)),
        0x4A => Load(reg!(C), reg!(D)),
        0x4B => Load(reg!(C), reg!(E)),
        0x4C => Load(reg!(C), reg!(H)),
        0x4D => Load(reg!(C), reg!(L)),
        0x4E => Load(reg!(C), INDIRECT_HL),
        0x4F => Load(reg!(C), reg!(A)),
        0x50 => Load(reg!(D), reg!(B)),
        0x51 => Load(reg!(D), reg!(C)),
        0x52 => Load(reg!(D), reg!(D)),
        0x53 => Load(reg!(D), reg!(E)),
        0x54 => Load(reg!(D), reg!(H)),
        0x55 => Load(reg!(D), reg!(L)),
        0x56 => Load(reg!(D), INDIRECT_HL),
        0x57 => Load(reg!(D), reg!(A)),
        0x58 => Load(reg!(E), reg!(B)),
        0x59 => Load(reg!(E), reg!(C)),
        0x5A => Load(reg!(E), reg!(D)),
        0x5B => Load(reg!(E), reg!(E)),
        0x5C => Load(reg!(E), reg!(H)),
        0x5D => Load(reg!(E), reg!(L)),
        0x5E => Load(reg!(E), INDIRECT_HL),
        0x5F => Load(reg!(E), reg!(A)),
        0x60 => Load(reg!(H), reg!(B)),
        0x61 => Load(reg!(H), reg!(C)),
        0x62 => Load(reg!(H), reg!(D)),
        0x63 => Load(reg!(H), reg!(E)),
        0x64 => Load(reg!(H), reg!(H)),
        0x65 => Load(reg!(H), reg!(L)),
        0x66 => Load(reg!(H), INDIRECT_HL),
        0x67 => Load(reg!(H), reg!(A)),
        0x68 => Load(reg!(L), reg!(B)),
        0x69 => Load(reg!(L), reg!(C)),
        0x6A => Load(reg!(L), reg!(D)),
        0x6B => Load(reg!(L), reg!(E)),
        0x6C => Load(reg!(L), reg!(H)),
        0x6D => Load(reg!(L), reg!(L)),
        0x6E => Load(reg!(L), INDIRECT_HL),
        0x6F => Load(reg!(L), reg!(A)),
        0x70 => Load(INDIRECT_HL, reg!(B)),
        0x71 => Load(INDIRECT_HL, reg!(C)),
        0x72 => Load(INDIRECT_HL, reg!(D)),
        0x73 => Load(INDIRECT_HL, reg!(E)),
        0x74 => Load(INDIRECT_HL, reg!(H)),
        0x75 => Load(INDIRECT_HL, reg!(L)),
        0x76 => Halt,
        0x77 => Load(INDIRECT_HL, reg!(A)),
        0x78 => Load(reg!(A), reg!(B)),
        0x79 => Load(reg!(A), reg!(C)),
        0x7A => Load(reg!(A), reg!(D)),
        0x7B => Load(reg!(A), reg!(E)),
        0x7C => Load(reg!(A), reg!(H)),
        0x7D => Load(reg!(A), reg!(L)),
        0x7E => Load(reg!(A), INDIRECT_HL),
        0x7F => Load(reg!(A), reg!(A)),
        0x80 => Add(reg!(A), reg!(B)),
        0x81 => Add(reg!(A), reg!(C)),
        0x82 => Add(reg!(A), reg!(D)),
        0x83 => Add(reg!(A), reg!(E)),
        0x84 => Add(reg!(A), reg!(H)),
        0x85 => Add(reg!(A), reg!(L)),
        0x86 => Add(reg!(A), INDIRECT_HL),
        0x87 => Add(reg!(A), reg!(A)),
        0x90 => Sub(reg!(A), reg!(B)),
        0x91 => Sub(reg!(A), reg!(C)),
        0x92 => Sub(reg!(A), reg!(D)),
        0x93 => Sub(reg!(A), reg!(E)),
        0x94 => Sub(reg!(A), reg!(H)),
        0x95 => Sub(reg!(A), reg!(L)),
        0x96 => Sub(reg!(A), INDIRECT_HL),
        0x97 => Sub(reg!(A), reg!(A)),
        0xA0 => And(reg!(B)),
        0xA1 => And(reg!(C)),
        0xA2 => And(reg!(D)),
        0xA3 => And(reg!(E)),
        0xA4 => And(reg!(H)),
        0xA5 => And(reg!(L)),
        0xA6 => And(INDIRECT_HL),
        0xA7 => And(reg!(A)),
        0xAF => Xor(reg!(A)),
        0xB0 => Or(reg!(B)),
        0xB1 => Or(reg!(C)),
        0xB2 => Or(reg!(D)),
        0xB3 => Or(reg!(E)),
        0xB4 => Or(reg!(H)),
        0xB5 => Or(reg!(L)),
        0xB6 => Or(INDIRECT_HL),
        0xB7 => Or(reg!(A)),
        0xB8 => Cmp(reg!(B)),
        0xC1 => Pop(B, C),
        0xC2 => Jump(NonZero, abs_address!(bytes)),
        0xC3 => InconditionalJump(abs_address!(bytes)),
        0xC5 => Push(B, C),
        0xC6 => Add(reg!(A), direct_imm8!(bytes)),
        0xC9 => Ret,
        0xCA => Jump(Zero, abs_address!(bytes)),
        0xCB => decode_extended_instruction(bytes[1]),
        0xCD => Call(abs_address!(bytes)),
        0xD1 => Pop(D, E),
        0xD2 => Jump(NoCarry, abs_address!(bytes)),
        0xD5 => Push(D, E),
        0xD6 => Sub(reg!(A), direct_imm8!(bytes)),
        0xDA => Jump(Carry, abs_address!(bytes)),
        0xE0 => Load(base_indirect!(imm8!(bytes)), reg!(A)), // TODO: check if signed or not.
        0xE1 => Pop(H, L),
        0xE2 => Load(base_indirect!(Reg(C)), reg!(A)),
        0xE5 => Push(H, L),
        0xE6 => And(direct_imm8!(bytes)),
        0xEA => Load(Indirection(imm16!(bytes)), reg!(A)),
        0xEE => Xor(direct_imm8!(bytes)),
        0xF0 => Load(reg!(A), base_indirect!(imm8!(bytes))),
        0xF1 => Pop(A, F),
        0xF2 => Load(reg!(A), base_indirect!(Reg(C))),
        0xF3 => DisableInterrupt,
        0xF5 => Push(A, F),
        0xFA => Load(reg!(A), indirect_reg!(SP)),
        0xFB => EnableInterrupt,
        0xFE => Cmp(direct_imm8!(bytes)),
        _ => panic!("Unimplemented opcode 0x{:02X}", bytes[0]),
    }
}

/// Decode an extended instruction.
fn decode_extended_instruction(byte: u8) -> Instruction {
    match byte {
        0x00 => RotateLeftCarry(reg!(B)),
        0x01 => RotateLeftCarry(reg!(C)),
        0x02 => RotateLeftCarry(reg!(D)),
        0x03 => RotateLeftCarry(reg!(E)),
        0x04 => RotateLeftCarry(reg!(H)),
        0x05 => RotateLeftCarry(reg!(L)),
        0x06 => RotateLeftCarry(INDIRECT_HL),
        0x07 => RotateLeftCarry(reg!(A)),
        0x08 => RotateRightCarry(reg!(B)),
        0x09 => RotateRightCarry(reg!(C)),
        0x0A => RotateRightCarry(reg!(D)),
        0x0B => RotateRightCarry(reg!(E)),
        0x0C => RotateRightCarry(reg!(H)),
        0x0D => RotateRightCarry(reg!(L)),
        0x0E => RotateRightCarry(INDIRECT_HL),
        0x0F => RotateRightCarry(reg!(A)),
        0x10 => RotateLeft(reg!(B)),
        0x11 => RotateLeft(reg!(C)),
        0x12 => RotateLeft(reg!(D)),
        0x13 => RotateLeft(reg!(E)),
        0x14 => RotateLeft(reg!(H)),
        0x15 => RotateLeft(reg!(L)),
        0x16 => RotateLeft(INDIRECT_HL),
        0x17 => RotateLeft(reg!(A)),
        0x18 => RotateRight(reg!(B)),
        0x19 => RotateRight(reg!(C)),
        0x1A => RotateRight(reg!(D)),
        0x1B => RotateRight(reg!(E)),
        0x1C => RotateRight(reg!(H)),
        0x1D => RotateRight(reg!(L)),
        0x1E => RotateRight(INDIRECT_HL),
        0x1F => RotateRight(reg!(A)),
        0x20 => ShiftLeftArithmetic(reg!(B)),
        0x21 => ShiftLeftArithmetic(reg!(C)),
        0x22 => ShiftLeftArithmetic(reg!(D)),
        0x23 => ShiftLeftArithmetic(reg!(E)),
        0x24 => ShiftLeftArithmetic(reg!(H)),
        0x25 => ShiftLeftArithmetic(reg!(L)),
        0x26 => ShiftLeftArithmetic(INDIRECT_HL),
        0x27 => ShiftLeftArithmetic(reg!(A)),
        0x28 => ShiftRightArithmetic(reg!(B)),
        0x29 => ShiftRightArithmetic(reg!(C)),
        0x2A => ShiftRightArithmetic(reg!(D)),
        0x2B => ShiftRightArithmetic(reg!(E)),
        0x2C => ShiftRightArithmetic(reg!(H)),
        0x2D => ShiftRightArithmetic(reg!(L)),
        0x2E => ShiftRightArithmetic(INDIRECT_HL),
        0x2F => ShiftRightArithmetic(reg!(A)),
        0x30 => Swap(reg!(B)),
        0x31 => Swap(reg!(C)),
        0x32 => Swap(reg!(D)),
        0x33 => Swap(reg!(E)),
        0x34 => Swap(reg!(H)),
        0x35 => Swap(reg!(L)),
        0x36 => Swap(INDIRECT_HL),
        0x37 => Swap(reg!(A)),
        0x38 => ShiftRight(reg!(B)),
        0x39 => ShiftRight(reg!(C)),
        0x3A => ShiftRight(reg!(D)),
        0x3B => ShiftRight(reg!(E)),
        0x3C => ShiftRight(reg!(H)),
        0x3D => ShiftRight(reg!(L)),
        0x3E => ShiftRight(INDIRECT_HL),
        0x3F => ShiftRight(reg!(A)),
        0x40 => Bit(0, reg!(B)),
        0x41 => Bit(0, reg!(C)),
        0x42 => Bit(0, reg!(D)),
        0x43 => Bit(0, reg!(E)),
        0x44 => Bit(0, reg!(H)),
        0x45 => Bit(0, reg!(L)),
        0x46 => Bit(0, INDIRECT_HL),
        0x47 => Bit(0, reg!(A)),
        0x48 => Bit(1, reg!(B)),
        0x49 => Bit(1, reg!(C)),
        0x4A => Bit(1, reg!(D)),
        0x4B => Bit(1, reg!(E)),
        0x4C => Bit(1, reg!(H)),
        0x4D => Bit(1, reg!(L)),
        0x4E => Bit(1, INDIRECT_HL),
        0x4F => Bit(1, reg!(A)),
        0x50 => Bit(2, reg!(B)),
        0x51 => Bit(2, reg!(C)),
        0x52 => Bit(2, reg!(D)),
        0x53 => Bit(2, reg!(E)),
        0x54 => Bit(2, reg!(H)),
        0x55 => Bit(2, reg!(L)),
        0x56 => Bit(2, INDIRECT_HL),
        0x57 => Bit(2, reg!(A)),
        0x58 => Bit(3, reg!(B)),
        0x59 => Bit(3, reg!(C)),
        0x5A => Bit(3, reg!(D)),
        0x5B => Bit(3, reg!(E)),
        0x5C => Bit(3, reg!(H)),
        0x5D => Bit(3, reg!(L)),
        0x5E => Bit(3, INDIRECT_HL),
        0x5F => Bit(3, reg!(A)),
        0x60 => Bit(4, reg!(B)),
        0x61 => Bit(4, reg!(C)),
        0x62 => Bit(4, reg!(D)),
        0x63 => Bit(4, reg!(E)),
        0x64 => Bit(4, reg!(H)),
        0x65 => Bit(4, reg!(L)),
        0x66 => Bit(4, INDIRECT_HL),
        0x67 => Bit(4, reg!(A)),
        0x68 => Bit(5, reg!(B)),
        0x69 => Bit(5, reg!(C)),
        0x6A => Bit(5, reg!(D)),
        0x6B => Bit(5, reg!(E)),
        0x6C => Bit(5, reg!(H)),
        0x6D => Bit(5, reg!(L)),
        0x6E => Bit(5, INDIRECT_HL),
        0x6F => Bit(5, reg!(A)),
        0x70 => Bit(6, reg!(B)),
        0x71 => Bit(6, reg!(C)),
        0x72 => Bit(6, reg!(D)),
        0x73 => Bit(6, reg!(E)),
        0x74 => Bit(6, reg!(H)),
        0x75 => Bit(6, reg!(L)),
        0x76 => Bit(6, INDIRECT_HL),
        0x77 => Bit(6, reg!(A)),
        0x78 => Bit(7, reg!(B)),
        0x79 => Bit(7, reg!(C)),
        0x7A => Bit(7, reg!(D)),
        0x7B => Bit(7, reg!(E)),
        0x7C => Bit(7, reg!(H)),
        0x7D => Bit(7, reg!(L)),
        0x7E => Bit(7, INDIRECT_HL),
        0x7F => Bit(7, reg!(A)),
        _ => panic!("Unimplemented extended opcode 0xCB 0x{:02X}", byte),
    }
}

/// Get a 16-bit immediate value at the start of bytes.
fn imm16(bytes: &[u8]) -> u16 {
    (bytes[1] as u16) << 8 | bytes[0] as u16
}

/// Get the size (in bytes) of the immediate value of the instruction.
fn immediate_size_by_opcode(byte: u8) -> usize {
    match byte {
        0x00 | 0x02 ... 0x05 | 0x09 | 0x0A | 0x0C | 0x0D | 0x12 ... 0x15 | 0x17 | 0x19 | 0x1A | 0x1C | 0x1D | 0x22 ... 0x25 | 0x2C | 0x2D | 0x2F | 0x32 ... 0x35 | 0x3C | 0x3D | 0x40 ... 0x87 | 0x90 ... 0x97 | 0xA0 ... 0xBF | 0xC1 | 0xC5 | 0xC9 | 0xD1 | 0xD5 | 0xE1 | 0xE2 | 0xE5 | 0xF1 | 0xF2 | 0xF3 | 0xF5 | 0xFB => 0,
        0x06 | 0x0E | 0x16 | 0x18 | 0x1E | 0x20 | 0x26 | 0x28 | 0x2E | 0x30 | 0x36 | 0x38 | 0x3E | 0xC6 | 0xCB | 0xD6 | 0xE0 | 0xE6 | 0xEE | 0xF0 | 0xFE => 1,
        0x01 | 0x11 | 0x21 | 0x31 | 0xC2 | 0xC3 | 0xCA | 0xCD | 0xD2 | 0xDA | 0xEA | 0xFA => 2,
        _ => panic!("Unknown size for opcode 0x{:02X}", byte),
    }
}
