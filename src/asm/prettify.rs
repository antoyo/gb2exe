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

//! Assembly prettifier.

use decompiler::Instructions;
use rom::InstructionOrData::{self, DataByte};
use rom::RepeatableInstruction::{NoRepeat, Repeat};

/// Prettify the assembly.
/// Convert repeating bytes to loops.
pub fn prettify(assembly: &[InstructionOrData]) -> Instructions {
    let mut instructions = split_when(assembly, split_predicate);
    let mut result = Instructions::new();

    if instructions.last().unwrap().iter().all(data_byte_0) {
        let last_index = instructions.len() - 1;
        instructions.remove(last_index);
    }

    let mut address = 0;
    for mut slice in instructions.drain(..) {
        let len = slice.len() as u16;
        if slice.len() >= 4 {
            result.add_instr(Repeat(slice.len(), slice[0].clone()), address);
        }
        else {
            for instruction in slice.drain(..).map(|instruction| NoRepeat(instruction)) {
                result.add_instr(instruction, address);
            }
        }
        address += len;
    }

    result
}

/// Check if the instruction is a 0 data byte.
fn data_byte_0(instruction: &InstructionOrData) -> bool {
    if let DataByte(0) = *instruction {
        true
    }
    else {
        false
    }
}

/// Check if it should have a split between the two instructions.
fn split_predicate(instruction1: &InstructionOrData, instruction2: &InstructionOrData) -> bool {
    match (instruction1, instruction2) {
        (&DataByte(byte1), &DataByte(byte2)) => {
            // NOTE: Split when both are different.
            byte1 != byte2
        },
        (_, _) => true, // NOTE: identical instructions should not be aggregated (to facilitate jumps).
    }
}

/// Split the vector according to the split_predicate.
fn split_when<F>(slice: &[InstructionOrData], split_predicate: F) -> Vec<Vec<InstructionOrData>>
    where F: Fn(&InstructionOrData, &InstructionOrData) -> bool
{
    let mut result = vec![];
    let mut current_split = vec![];

    for i in 0 .. slice.len() - 1 {
        current_split.push(slice[i].clone());

        // NOTE: unwrap is safe because one element is pushed on the previous instruction.
        if split_predicate(&current_split.last().unwrap(), &slice[i + 1]) {
            result.push(current_split);
            current_split = vec![];
        }
    }

    let len = slice.len();
    current_split.push(slice[len - 1].clone());
    result.push(current_split);

    result
}
