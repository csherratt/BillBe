

use super::{Mode, Instruction};
use std::fmt;
use Symbols;

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Disasm(*self, None, None))
    }
}

struct Address<'a>(pub u32, pub Option<Symbols<'a>>);

impl<'a> fmt::Display for Address<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(sym) = self.1.as_ref().and_then(|x| x.find(self.0)) {
            write!(f, "{:x} <{}>", self.0, sym)
        } else {
            write!(f, "{:x}", self.0)
        }
    }
}

struct AddressPCRelative<'a>(pub u32, pub Option<Symbols<'a>>, pub Option<u32>);

impl<'a> fmt::Display for AddressPCRelative<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let addr = if let Some(pc) = self.2 {
            pc.wrapping_add(self.0)
        } else {
            return write!(f, ":{:x}", self.0);
        };

        if let Some(sym) = self.1.as_ref().and_then(|x| x.find(addr)) {
            write!(f, "{:x} <{}>", addr, sym)
        } else {
            write!(f, ":{:x}", addr)
        }
    }
}

pub struct DisasmMode<'a>(pub Mode, pub Option<Symbols<'a>>, pub Option<u32>);


impl<'a> fmt::Display for DisasmMode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Mode::*;
        match self.0 {
            AbsoluteByte(addr, _) => write!(f, "*${}:b", Address(addr, self.1)),
            AbsoluteUnsignedByte(addr, _) => write!(f, "*${}:ub", Address(addr, self.1)),
            AbsoluteHalfword(addr, _) => write!(f, "*${}:h", Address(addr, self.1)),
            AbsoluteUnsignedHalfword(addr, _) => write!(f, "*${}:uh", Address(addr, self.1)),
            AbsoluteWord(addr, _) => write!(f, "*${}", Address(addr, self.1)),

            StackOffsetByte(off) => write!(f, "R{}:b", off as i32),
            StackOffsetUnsignedByte(off) => write!(f, "R{}:ub", off as i32),
            StackOffsetHalfword(off) => write!(f, "R{}:h", off as i32),
            StackOffsetUnsignedHalfword(off) => write!(f, "R{}:uh", off as i32),
            StackOffsetWord(off) => write!(f, "R{}", off as i32),

            StackOffsetIndirectByte(off) => write!(f, "*R{}:b", off as i32),
            StackOffsetIndirectUnsignedByte(off) => write!(f, "*R{}:ub", off as i32),
            StackOffsetIndirectHalfword(off) => write!(f, "*R{}:h", off as i32),
            StackOffsetIndirectUnsignedHalfword(off) => write!(f, "*R{}:uh", off as i32),
            StackOffsetIndirectWord(off) => write!(f, "*R{}", off as i32),

            Immediate(imm) => write!(f, "${0:x}({0})", imm as i32),
            CpuRegister(1) => write!(f, "$msp"),
            CpuRegister(2) => write!(f, "$isp"),
            CpuRegister(3) => write!(f, "$sp"),
            CpuRegister(4) => write!(f, "$config"),
            CpuRegister(5) => write!(f, "$psw"),
            CpuRegister(6) => write!(f, "$shad"),
            CpuRegister(7) => write!(f, "$vb"),
            CpuRegister(8) => write!(f, "$stb"),
            CpuRegister(9) => write!(f, "$fault"),
            CpuRegister(10) => write!(f, "$id"),
            CpuRegister(11) => write!(f, "$timer1"),
            CpuRegister(12) => write!(f, "$timer2"),
            CpuRegister(13) => write!(f, "$fpsw"),
            CpuRegister(x) => write!(f, "$UNKNOWN_CPU_REG_{}", x),
            PCRelative(off) => write!(f, "{}", AddressPCRelative(off, self.1, self.2)),
        }
    }
}

pub struct Disasm<'a>(pub Instruction, pub Option<Symbols<'a>>, pub Option<u32>);

impl<'a> fmt::Display for Disasm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self.0 {
            Instruction::Dyandic(op, src, dst) => {
                format!("{} {}, {}",
                        op,
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::Dyandic3(op, src, dst) => {
                format!("{}3 {}, {}",
                        op,
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::DyandicI(op, src, dst) => {
                format!("{}i {}, {}",
                        op,
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::CPU => format!("cpu"),
            Instruction::Mov(src, dst) => {
                format!("mov {}, {}",
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::Dqm(src, dst) => {
                format!("dqm {}, {}",
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::Mova(src, dst) => {
                format!("mova {}, {}",
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::Jmp(to) => format!("jmp {}", DisasmMode(to, self.1, self.2)),
            Instruction::JmpIf(to, mode, _) => {
                format!("jmp{}{} {}",
                        if mode { 't' } else { 'f' },
                        if mode { 'y' } else { 'n' },
                        DisasmMode(to, self.1, self.2))
            }
            Instruction::Catch(to) => format!("catch {}", DisasmMode(to, self.1, self.2)),
            Instruction::Call(to) => format!("call {}", DisasmMode(to, self.1, self.2)),
            Instruction::Enter(to) => format!("enter {}", DisasmMode(to, self.1, self.2)),
            Instruction::Return(to) => format!("return {}", DisasmMode(to, self.1, self.2)),
            Instruction::Ldraa(to) => format!("ldraa {}", DisasmMode(to, self.1, self.2)),
            Instruction::Kcall(to) => format!("kcall {}", DisasmMode(to, self.1, self.2)),
            Instruction::Cret => format!("cret"),
            Instruction::Kret => format!("kret"),
            Instruction::Cmp(mode, src, dst) => {
                format!("cmp{} {}, {}",
                        mode,
                        DisasmMode(src, self.1, self.2),
                        DisasmMode(dst, self.1, self.2))
            }
            Instruction::Flushi => format!("flushi"),
            Instruction::Flushp => format!("flushp"),
            Instruction::Nop => format!("nop"),
            x => panic!("unhandled opcode {:?}", x),
        };
        f.pad(&s)
    }
}
