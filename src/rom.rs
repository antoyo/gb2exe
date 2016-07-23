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

//! GameBoy ROM decoder.

use std::collections::{HashSet, VecDeque};

use asm::decoder::{Instruction, decode};
use asm::decoder::Address::{AbsoluteAddress, RelativeAddress};
use asm::decoder::Instruction::*;
use bios;
use self::InstructionOrData::*;

const BIOS_LOGO_END_ADDRESS: usize = 0xD7;
const BIOS_LOGO_START_ADDRESS: usize = 0xA8;
const CARTRIDGE_LOGO_END_ADDRESS: usize = 0x133;
const CARTRIDGE_LOGO_START_ADDRESS: usize = 0x104;
const CHECKSUM_END_ADDRESS: usize = 0x14D;
const CHECKSUM_START_ADDRESS: usize = 0x134;

/// Either an instruction or data.
#[derive(Clone, Debug, PartialEq)]
pub enum InstructionOrData {
    Ascii(String),
    DataByte(u8),
    Deleted,
    Instr(Instruction),
}

/// An instruction that can be repeated.
#[derive(Clone, Debug, PartialEq)]
pub enum RepeatableInstruction {
    NoRepeat(InstructionOrData),
    Repeat(usize, InstructionOrData),
}

/// A GameBoy ROM.
pub struct Rom {
    pub instructions: Vec<InstructionOrData>,
}

impl Rom {
    /// Create a new `Rom` by decoding the `bytes`.
    pub fn new(bytes: &[u8]) -> Option<Rom> {
        if Rom::valid(bytes) {
            Some(Rom {
                instructions: Rom::decode_instructions(bytes),
            })
        }
        else {
            None
        }
    }

    /// Convert the `bytes` into a vector of instruction or data.
    fn decode_instructions(bytes: &[u8]) -> Vec<InstructionOrData> {
        let mut instruction_addresses = VecDeque::new();
        instruction_addresses.push_back(0x100);
        let mut visited = HashSet::new();

        let mut instructions: Vec<_> = bytes.iter()
            .map(|&byte| DataByte(byte))
            .collect();

        while let Some(address) = instruction_addresses.pop_front() {
            visited.insert(address);
            let mut cont = true;
            let mut i = address;

            while cont {
                let (instruction, size) = decode(&bytes[i..]);
                cont = is_inconditional_jump(&instruction);

                if let Some(addr) = get_address_in_instruction(&instruction, i) {
                    if !visited.contains(&addr) {
                        instruction_addresses.push_back(addr);
                    }
                }

                instructions[i] = Instr(instruction);

                for index in i + 1 .. i + size {
                    instructions[index] = Deleted;
                }

                i += size;
            }
        }

        instructions
    }

    /// Check if the rom is valid by validating the nintendo logo and the checksum.
    fn valid(bytes: &[u8]) -> bool {
        let bytes_to_check = bytes[CHECKSUM_START_ADDRESS .. CHECKSUM_END_ADDRESS + 1].iter();
        let byte_sum = bytes_to_check.fold(0, |acc, &byte| acc + byte as i32);
        let checksum = (0x19 + byte_sum) % 256;

        checksum == 0 &&
            bytes[CARTRIDGE_LOGO_START_ADDRESS .. CARTRIDGE_LOGO_END_ADDRESS + 1] ==
            bios::BIOS[BIOS_LOGO_START_ADDRESS .. BIOS_LOGO_END_ADDRESS + 1]
    }
}

/// Get the address inside the instruction if there is one (jump to address).
fn get_address_in_instruction(instruction: &Instruction, current_address: usize) -> Option<usize> {
    match *instruction {
        Call(AbsoluteAddress(addr)) | InconditionalJump(AbsoluteAddress(addr)) | Jump(_, AbsoluteAddress(addr)) =>
            Some(addr as usize),
        InconditionalJump(RelativeAddress(delta)) | Jump(_, RelativeAddress(delta)) =>
            // NOTE: plus two because the instruction is one byte and the delta is one byte.
            Some((current_address as isize + delta as isize) as usize + 2),
        _ => None,
    }
}

/// Check if `instruction` is an inconditional jump.
fn is_inconditional_jump(instruction: &Instruction) -> bool {
    match *instruction {
        InconditionalJump(_) | Ret => false,
        _ => true,
    }
}
