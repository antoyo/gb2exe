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

const BASE_ADDRESS: u16 = 0xFF00;

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
    Bit(u8, Operand),
    Call(Address),
    Cmp(Operand),
    Dec(Operand),
    Halt,
    Inc(Operand),
    InconditionalJump(Address),
    Jump(ConditionCode, Address),
    Load(Operand, Operand),
    LoadDecrement(Operand, Operand),
    LoadIncrement(Operand, Operand),
    Nop,
    Pop(Register, Register),
    Push(Register, Register),
    Ret,
    RotateLeft(Operand),
    Sub(Operand, Operand),
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
    PC, SP,
    // Clock for the last instruction.
    M, T // TODO: to remove.
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
        0x04 => Inc(reg!(B)),
        0x05 => Dec(reg!(B)),
        0x06 => Load(reg!(B), direct_imm8!(bytes)),
        0x0C => Inc(reg!(C)),
        0x0D => Dec(reg!(C)),
        0x0E => Load(reg!(C), direct_imm8!(bytes)),
        0x11 => Load(regs!(D, E), direct_imm16!(bytes)),
        0x13 => Inc(regs!(D, E)),
        0x15 => Dec(reg!(D)),
        0x16 => Load(reg!(D), direct_imm8!(bytes)),
        0x17 => RotateLeft(reg!(A)),
        0x18 => InconditionalJump(RelativeAddress(bytes[1] as i8)),
        0x1A => Load(reg!(A), Indirection(Regs(D, E))),
        0x1D => Dec(reg!(E)),
        0x1E => Load(reg!(E), direct_imm8!(bytes)),
        0x20 => Jump(NonZero, RelativeAddress(bytes[1] as i8)),
        0x21 => Load(regs!(H, L), direct_imm16!(bytes)),
        0x22 => LoadIncrement(regs!(H, L), reg!(A)),
        0x23 => Inc(regs!(H, L)),
        0x24 => Inc(reg!(H)),
        0x28 => Jump(Zero, RelativeAddress(bytes[1] as i8)),
        0x2E => Load(reg!(L), direct_imm8!(bytes)),
        0x31 => Load(reg!(SP), direct_imm16!(bytes)),
        0x32 => LoadDecrement(regs!(H, L), reg!(A)),
        0x3D => Dec(reg!(A)),
        0x3E => Load(reg!(A), direct_imm8!(bytes)),
        0x4F => Load(reg!(C), reg!(A)),
        0x57 => Load(reg!(D), reg!(A)),
        0x5D => Load(reg!(E), reg!(L)),
        0x67 => Load(reg!(H), reg!(A)),
        0x76 => Halt,
        0x77 => Load(regs!(H, L), reg!(A)),
        0x7B => Load(reg!(A), reg!(E)),
        0x7C => Load(reg!(A), reg!(H)),
        0x7E => Load(reg!(A), Indirection(Regs(H, L))),
        0x83 => Add(reg!(A), reg!(E)),
        0x90 => Sub(reg!(A), reg!(B)),
        0xAF => Xor(reg!(A)),
        0xB8 => Cmp(reg!(B)),
        0xC1 => Pop(B, C),
        0xC3 => InconditionalJump(abs_address!(bytes)),
        0xC9 => Ret,
        0xCA => Jump(Zero, abs_address!(bytes)),
        0xCB => decode_extended_instruction(bytes[1]),
        0xCD => Call(abs_address!(bytes)),
        0xC5 => Push(B, C),
        0xE0 => Load(BaseIndirection(BASE_ADDRESS, imm8!(bytes)), reg!(A)), // TODO: check if signed or not.
        0xE1 => Pop(H, L),
        0xE2 => Load(BaseIndirection(BASE_ADDRESS, Reg(C)), reg!(A)),
        0xEA => Load(Indirection(imm16!(bytes)), reg!(A)),
        0xF0 => Load(reg!(A), BaseIndirection(BASE_ADDRESS, imm8!(bytes))),
        0xF2 => Load(reg!(A), BaseIndirection(BASE_ADDRESS, Reg(C))),
        0xFA => Load(reg!(A), Indirection(Reg(SP))),
        0xFE => Cmp(direct_imm8!(bytes)),
        _ => panic!("Unimplemented opcode 0x{:02X}", bytes[0]),
    }
}

/// Decode an extended instruction.
fn decode_extended_instruction(byte: u8) -> Instruction {
    match byte {
        0x11 => RotateLeft(reg!(C)),
        0x7C => Bit(7, reg!(H)),
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
        0x00 | 0x02 | 0x03 | 0x04 | 0x05 | 0x0C | 0x0D | 0x12 | 0x13 | 0x14 | 0x15 | 0x17 | 0x1A | 0x1C | 0x1D | 0x22 | 0x23 | 0x24 |0x25 | 0x2C | 0x2D | 0x32 | 0x33 | 0x34 | 0x35 | 0x3C | 0x3D | 0x40 ... 0x4F | 0x50 ... 0x5F | 0x60 ... 0x6F | 0x70 ... 0x7F | 0x80 ... 0x87 | 0x90 ... 0x97 | 0xA8 ... 0xAF | 0xB8 ... 0xBF | 0xC1 | 0xC5 | 0xC9 | 0xD1 | 0xD5 | 0xE1 | 0xE2 | 0xE5 | 0xF1 | 0xF2 | 0xF5 => 0,
        0x06 | 0x0E | 0x16 | 0x18 | 0x1E | 0x20 | 0x26 | 0x28 | 0x2E | 0x3E | 0xCB | 0xE0 | 0xF0 | 0xFE => 1,
        0x01 | 0x11 | 0x21 | 0x31 | 0xC2 | 0xC3 | 0xCA | 0xCD | 0xD2 | 0xDA | 0xEA | 0xFA => 2,
        _ => panic!("Unknown size for opcode 0x{:02X}", byte),
    }
}
