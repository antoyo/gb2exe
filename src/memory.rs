pub type Address = u16;

/// Memory management unit.
pub struct Mmu {
}

impl Mmu {
    pub fn new() -> Mmu {
        Mmu {
        }
    }

    pub fn read_u8(&mut self, address: Address) -> u8 {
        unimplemented!();
    }

    pub fn read_u16(&mut self, address: Address) -> u16 {
        unimplemented!();
    }

    pub fn write_u8(&mut self, address: Address, value: u8) {
        unimplemented!();
    }

    pub fn write_u16(&mut self, address: Address, value: u16) {
        unimplemented!();
    }
}
