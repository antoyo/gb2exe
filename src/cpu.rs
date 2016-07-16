use memory::Mmu;

macro_rules! initialize_register_fields {
    // Initialize the struct with registers to 0.
    ($strct:ident, $($value:ident),+) => {
        $strct {
            $($value: 0,)+
        }
    };
}

macro_rules! initialize_registers {
    // Initialize the registers to 0.
    ($cpu:expr, $($value:ident),+) => {
        $($cpu.registers.$value = 0;)+
    };
}

macro_rules! read_u8 {
    // Read the value on the stack.
    ($value:ident = $cpu:expr) => {
        $cpu.registers.$value = $cpu.mmu.read_u8($cpu.registers.sp);
    };
}

macro_rules! write_u8 {
    // Write the value on the stack.
    ($cpu:expr, $value:ident) => {
        $cpu.mmu.write_u8($cpu.registers.sp, $cpu.registers.$value);
    };
}

/// CPU clock.
struct Clock {
    m: u64,
    t: u64,
}

impl Clock {
    fn new(m: u64, t: u64) -> Clock {
        Clock {
            m: m,
            t: t,
        }
    }
}

/// CPU registers.
struct Registers {
    // 8-bit registers.
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,

    // 16-bit registers.
    pc: u16,
    sp: u16,

    // Clock for the last instruction.
    m: u64,
    t: u64
}

impl Registers {
    fn new() -> Registers {
        initialize_register_fields!(Registers, a, b, c, d, e, f, h, l, pc, sp, m, t)
    }
}

/// Z80 cpu emulator.
pub struct Z80 {
    clock: Clock,
    mmu: Mmu,
    registers: Registers
}

impl Z80 {
    pub fn new() -> Z80 {
        Z80 {
            clock: Clock::new(0, 0),
            mmu: Mmu::new(),
            registers: Registers::new(),
        }
    }

    pub fn dispatch(&mut self) {
        loop {
            let instruction = self.mmu.read_u8(self.registers.pc);
            self.registers.pc += 1;
            INSTRUCTION_MAP[instruction as usize](self);
            self.clock.m += self.registers.m;
            self.clock.t += self.registers.t;
        }
    }

    /// Add E to A, leaving result in A (ADD A, E).
    fn addr_e(&mut self) {
        self.registers.a += self.registers.e;

        self.registers.f = 0; // Clear flags.
        // Check for zero.
        if self.registers.a & 255 != 0 {
            self.registers.f |= 0x80;
        }
        // Check for carry.
        if self.registers.a > 255 { // TODO: this is not possible.
            self.registers.f |= 0x10;
        }

        self.registers.m = 1;
        self.registers.t = 4;
    }

    /// Compare B to A, setting flags (CP A, B).
    fn cpr_b(&mut self) {
        let i = self.registers.a - self.registers.b;

        self.registers.f |= 0x40; // Set subtraction flag.
        // Check for zero.
        if i & 255 != 0 {
            self.registers.f |= 0x80;
        }
        // Check for underflow.
        if i < 0 { // TODO: this is not possible.
            self.registers.f |= 0x10;
        }

        self.registers.m = 1;
        self.registers.t = 4;
    }

    /// No-operation (NOP).
    fn nop(&mut self) {
        self.registers.m = 1;
        self.registers.t = 4;
    }

    /// Push registers B and C to the stack (PUSH BC).
    fn pushbc(&mut self) {
        self.drop_stack();
        write_u8!(self, b);
        self.drop_stack();
        write_u8!(self, c);
        self.registers.m = 3;
        self.registers.t = 12;
    }

    /// Pop registers H and L off the stock (POP HL).
    fn pophl(&mut self) {
        read_u8!(l = self);
        self.move_back_stack();
        read_u8!(h = self);
        self.move_back_stack();
        self.registers.m = 3;
        self.registers.t = 12;
    }

    /// Read a byte from absolute location into A (LD A, addr).
    fn ldamm(&mut self) {
        let address = self.mmu.read_u16(self.registers.pc);
        self.registers.pc += 2;
        self.registers.a = self.mmu.read_u8(address);
        self.registers.m = 4;
        self.registers.t = 16;
    }

    /// Reset the CPU.
    fn reset(&mut self) {
        initialize_registers!(self, a, b, c, d, e, f, h, l, pc, sp);
        self.clock.m = 0;
        self.clock.t = 0;
    }

    /// Drop through the stack.
    fn drop_stack(&mut self) {
        self.registers.sp -= 1;
    }

    // Move back up the stack.
    fn move_back_stack(&mut self) {
        self.registers.sp += 1;
    }
}

lazy_static! {
    static ref INSTRUCTION_MAP: [Box<Fn(&mut Z80) + Sync>; 255] = [
        Box::new(Z80::nop),
        Box::new(Z80::nop), // TODO
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
        Box::new(Z80::nop),
    ];
}
