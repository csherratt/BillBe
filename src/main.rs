extern crate clap;
extern crate byteorder;
extern crate xmas_elf;
#[macro_use]
extern crate nickel;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;


mod ofs;
mod hobbit;
mod rtc;

use std::fmt::{self, Write};
use std::sync::{Arc, Mutex};

use byteorder::{ByteOrder, BigEndian};
use clap::{Arg, App};
use xmas_elf::symbol_table::Entry;
use nickel::{Nickel, HttpRouter};

use ofs::OFSDrive;
use hobbit::Hobbit;


#[derive(Debug)]
pub struct Symbol {
    name: String,
    start: u32,
    end: u32,
}

#[derive(Copy, Clone)]
pub struct Symbols<'a> {
    symbols: &'a [Symbol],
}

impl<'a> Symbols<'a> {
    pub fn new(syms: &'a [Symbol]) -> Symbols<'a> {
        Symbols { symbols: syms }
    }

    pub fn find(&self, addr: u32) -> Option<SymbolMatch> {
        return match self.symbols.binary_search_by(|x| x.start.cmp(&addr)) {
                   Ok(i) => {
                       let off = addr - self.symbols[i].start;
                       Some(SymbolMatch {
                                symbol: &self.symbols[i],
                                offset: off,
                            })
                   }
                   Err(i) => {
                       if i >= self.symbols.len() {
                           return None;
                       }
                       if self.symbols[i].start <= addr && addr <= self.symbols[i].end {
                           let off = addr - self.symbols[i].start;
                           return Some(SymbolMatch {
                                           symbol: &self.symbols[i],
                                           offset: off,
                                       });
                       }
                       if i != 0 {
                           let i = i - 1;
                           if self.symbols[i].start <= addr && addr <= self.symbols[i].end {
                               let off = addr - self.symbols[i].start;
                               return Some(SymbolMatch {
                                               symbol: &self.symbols[i],
                                               offset: off,
                                           });
                           }
                       }

                       None
                   }
               };
    }
}

#[derive(Copy, Clone)]
pub struct SymbolMatch<'a> {
    pub symbol: &'a Symbol,
    pub offset: u32,
}

impl<'a> fmt::Display for SymbolMatch<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.offset == 0 {
            write!(f, "{}", self.symbol.name)
        } else {
            write!(f, "{}+{:x}", self.symbol.name, self.offset)
        }
    }
}

struct BeBox {
    disk: std::fs::File,
    memory: Vec<u8>,
    symbols: Vec<Symbol>,
    rtc: rtc::Rtc,
    cpu1: Hobbit,
    //cpu2: Hobbit,
}

#[derive(Debug)]
pub enum Mode {
    Read,
    Write,
}


#[derive(Debug)]
pub enum BusDescription {
    BadPhysicalMemoryAddress,
    KernelSegmentFromUserMode,
    KernelPageFromUserMode,
    AddressOutOfSegmentBound,
    InvalidSegmentEntry,
    InvalidPageEntry,
    PageTableIsNotBacked,
    WriteToReadOnlySegment,
    WriteToReadOnlyPage,
}

#[derive(Debug)]
pub enum Error {
    BusFault {
        address: u32,
        mode: Mode,
        description: BusDescription,
    },
}

pub trait Memory {
    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<(), Error>;
    fn write(&mut self, addr: u32, data: &[u8]) -> Result<(), Error>;

    fn print_str(&mut self, mut addr: u32) {
        loop {
            let data = self.read_u8(addr).unwrap();
            if data == 0 {
                break;
            }
            print!("{}", data as char);
            addr += 1;
        }
        println!();
    }

    fn read_u8(&mut self, addr: u32) -> Result<u8, Error> {
        let mut data = [0u8; 1];
        try!(self.read(addr, &mut data[..]));
        Ok(data[0])
    }
    fn read_i8(&mut self, addr: u32) -> Result<i8, Error> {
        let mut data = [0u8; 1];
        try!(self.read(addr, &mut data[..]));
        Ok(data[0] as i8)
    }
    fn read_u16(&mut self, addr: u32) -> Result<u16, Error> {
        let mut data = [0u8; 2];
        try!(self.read(addr, &mut data[..]));
        Ok(BigEndian::read_u16(&data[..]))
    }
    fn read_i16(&mut self, addr: u32) -> Result<i16, Error> {
        let mut data = [0u8; 2];
        try!(self.read(addr, &mut data[..]));
        Ok(BigEndian::read_i16(&data[..]))
    }
    fn read_u32(&mut self, addr: u32) -> Result<u32, Error> {
        let mut data = [0u8; 4];
        try!(self.read(addr, &mut data[..]));
        Ok(BigEndian::read_u32(&data[..]))
    }
    fn read_i32(&mut self, addr: u32) -> Result<i32, Error> {
        let mut data = [0u8; 4];
        try!(self.read(addr, &mut data[..]));
        Ok(BigEndian::read_i32(&data[..]))
    }
    fn write_u8(&mut self, addr: u32, val: u8) -> Result<(), Error> {
        let mut data = [0u8; 1];
        data[0] = val;
        self.write(addr, &data[..])
    }
    fn write_i8(&mut self, addr: u32, val: i8) -> Result<(), Error> {
        let mut data = [0u8; 1];
        data[0] = val as u8;
        self.write(addr, &data[..])
    }
    fn write_u16(&mut self, addr: u32, val: u16) -> Result<(), Error> {
        let mut data = [0u8; 2];
        BigEndian::write_u16(&mut data[..], val);
        self.write(addr, &data[..])
    }
    fn write_i16(&mut self, addr: u32, val: i16) -> Result<(), Error> {
        let mut data = [0u8; 2];
        BigEndian::write_i16(&mut data[..], val);
        self.write(addr, &data[..])
    }
    fn write_u32(&mut self, addr: u32, val: u32) -> Result<(), Error> {
        let mut data = [0u8; 4];
        BigEndian::write_u32(&mut data[..], val);
        self.write(addr, &data[..])
    }
    fn write_i32(&mut self, addr: u32, val: i32) -> Result<(), Error> {
        let mut data = [0u8; 4];
        BigEndian::write_i32(&mut data[..], val);
        self.write(addr, &data[..])
    }
}

struct MemoryBus<'a> {
    memory: &'a mut Vec<u8>,
    rtc: &'a mut rtc::Rtc,
    id: u8,
}

impl<'a> MemoryBus<'a> {
    pub fn new(memory: &'a mut Vec<u8>, rtc: &'a mut rtc::Rtc, id: u8) -> MemoryBus<'a> {
        MemoryBus {
            memory: memory,
            rtc: rtc,
            id: id,
        }
    }
}

impl<'a> Memory for MemoryBus<'a> {
    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<(), Error> {
        match addr {
            // ignore to get shit working...
            //0 => (),
            0x800_dc3 => {
                data[0] = self.id;
                Ok(())
            }
            0x800_ff7 => {
                //println!("ser1 line status register 0x{:x}");
                data[0] = 0x20;
                Ok(())
            }
            rtc::ADDR_LOW...rtc::ADDR_HIGH => self.rtc.read(addr, data),
            0x800_0000...0x880_0000 => {
                let start = addr as usize;
                let end = start + data.len();
                if end > 0x880_0000 {
                    return Err(Error::BusFault {
                                   address: addr,
                                   mode: Mode::Read,
                                   description: BusDescription::BadPhysicalMemoryAddress,
                               });
                }
                data.copy_from_slice(&self.memory[start - 0x800_0000..end - 0x800_0000]);
                Ok(())
            }
            addr => {
                Err(Error::BusFault {
                        address: addr,
                        mode: Mode::Read,
                        description: BusDescription::BadPhysicalMemoryAddress,
                    })
            }
        }
    }

    fn write(&mut self, addr: u32, data: &[u8]) -> Result<(), Error> {
        match addr {
            0x800_007 => {
                //println!("ignore write to the cpu1 isr: 0x{:02x}", data[0]);
                Ok(())
            }
            0x800_00f => {
                //println!("ignore write to the slot isr: 0x{:02x}", data[0]);
                Ok(())
            }
            0x800_013 => {
                //println!("ignore write to the slot isr: 0x{:02x}", data[0]);
                Ok(())
            }
            0x800_ff7 => {
                //println!("ser1 line status register 0x{:x}", data[0]);
                Ok(())
            }
            0x800_fe3 => {
                std::io::Write::write(&mut std::io::stdout(), &data[..1]).unwrap();
                Ok(())
            }
            rtc::ADDR_LOW...rtc::ADDR_HIGH => self.rtc.write(addr, data),
            0x800_0000...0x880_0000 => {
                let start = addr as usize;
                let end = start + data.len();
                if end > 0x880_0000 {
                    return Err(Error::BusFault {
                                   address: addr,
                                   mode: Mode::Write,
                                   description: BusDescription::BadPhysicalMemoryAddress,
                               });
                }
                self.memory[start - 0x800_0000..end - 0x800_0000].copy_from_slice(data);
                Ok(())
            }
            addr => {
                Err(Error::BusFault {
                        address: addr,
                        mode: Mode::Write,
                        description: BusDescription::BadPhysicalMemoryAddress,
                    })
            }
        }
    }
}

impl Memory for Vec<u8> {
    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<(), Error> {
        let start = addr as usize;
        let end = start + data.len();

        match start {
            _ => {
                let from = if start >= 0x800_0000 && end < 0x880_0000 {
                    &self[start - 0x800_0000..end - 0x800_0000]
                } else {
                    return Result::Err(Error::BusFault {
                                           address: addr,
                                           mode: Mode::Read,
                                           description: BusDescription::BadPhysicalMemoryAddress,
                                       });
                };

                data.copy_from_slice(from);
            }
        }
        Ok(())
    }

    fn write(&mut self, addr: u32, data: &[u8]) -> Result<(), Error> {
        let start = addr as usize;
        let end = start + data.len();


        match start {
            _ => {
                let to = if start >= 0x800_0000 && end < 0x880_0000 {
                    &mut self[start - 0x800_0000..end - 0x800_0000]
                } else {
                    return Result::Err(Error::BusFault {
                                           address: addr,
                                           mode: Mode::Write,
                                           description: BusDescription::BadPhysicalMemoryAddress,
                                       });
                };

                to.copy_from_slice(data);
            }
        }
        Ok(())
    }
}

impl BeBox {
    /// boot will emulate the bios and load the kernel into memory
    /// after the boot sequence is done, the system will be in a `running`
    /// state
    fn boot(&mut self) {
        self.memory = std::iter::repeat(0).take(8 * 1024 * 1024).collect();

        let file = self.disk.read_file_path("system/kernel").unwrap();
        let elf = xmas_elf::ElfFile::new(&file[..]);

        //println!("{}", elf.header);
        for header in elf.program_iter() {
            match header.get_data(&elf) {
                Ok(xmas_elf::program::SegmentData::Undefined(bytes)) => {
                    self.memory
                        .write(header.virtual_addr() as u32, bytes)
                        .unwrap();
                }
                _ => (),
            }
        }

        // remove symbols
        self.symbols.clear();
        for header in elf.section_iter() {
            match header.get_data(&elf) {
                Ok(xmas_elf::sections::SectionData::SymbolTable32Be(syms)) => {
                    for s in syms {
                        let name = if let Ok(name) = s.get_name(&elf) {
                            name
                        } else {
                            continue;
                        };

                        // skip invalid symbols
                        if s.value() < 100 {
                            continue;
                        }

                        self.symbols
                            .push(Symbol {
                                      name: name.into(),
                                      start: s.value() as u32,
                                      end: s.value() as u32 + s.size() as u32,
                                  });
                    }
                }
                _ => (),
            }
        }
        // sort them by address
        self.symbols.sort_by_key(|x| x.start);

        let pt2 = elf.header.pt2.unwrap();
        self.cpu1.pc = std::num::Wrapping(pt2.entry_point() as u32);
        self.cpu1.sp = std::num::Wrapping(0x807_ff00);
        //self.cpu2.pc = std::num::Wrapping(pt2.entry_point() as u32);
        //self.cpu2.sp = std::num::Wrapping(0x80b_0000);
    }

    /// step
    fn step(&mut self) {
        //print!("1: ");
        self.cpu1
            .step(&mut MemoryBus::new(&mut self.memory, &mut self.rtc, 0x4),
                  Some(Symbols::new(&self.symbols)));
        //print!("2: ");
        //self.cpu2.step(&mut (&mut self.memory, 0x5), Some(Symbols::new(&self.symbols)));
    }

    #[allow(dead_code)]
    fn walk(&mut self) {
        for sym in &self.symbols {
            println!("{}: <{:x}-{:x}>", sym.name, sym.start, sym.end);
            self.cpu1
                .walk(&mut self.memory,
                      sym.start,
                      sym.end,
                      Some(Symbols::new(&self.symbols)));
        }
    }
}

fn send_index<'a, D>(_: &mut nickel::Request<D>,
                     res: nickel::Response<'a, D>)
                     -> nickel::MiddlewareResult<'a, D> {
    let index = std::path::Path::new("web/index.html");
    res.send_file(index)
}

#[derive(Serialize, Deserialize)]
struct MemoryBlock {
    address: u32,
    data: Vec<u32>,
}

fn main() {
    let matches = App::new("Billbe")
        .version("0.1")
        .author("Talyxian <me@talyxian.space>")
        .about("Billbe is a Hobbit BeBox simulator")
        .arg(Arg::with_name("disk")
                 .value_name("Disk File")
                 .help("Path to the disk image formatted to ofs")
                 .takes_value(true)
                 .required(true))
        .get_matches();

    let path = matches.value_of("disk").expect("no disk args");
    let file = match std::fs::File::open(path) {
        Ok(file) => file,
        Err(err) => {
            println!("Could not open disk image: {}", err);
            return;
        }
    };

    let mut be = BeBox {
        disk: file,
        memory: vec![],
        symbols: vec![],
        rtc: rtc::Rtc::new(),
        cpu1: Hobbit::new(),
        //cpu2: Hobbit::new(),
    };
    be.boot();
    //be.walk();
    //return;

    //be.cpu1.walk(&mut be.memory, 0x80c0510, 0x80c0510 + 0x800, Some(Symbols::new(&be.symbols)));
    //return;

    //be.memory.print_str(0x80e125c);
    for _ in 0..25_000_000 {
        be.step();
    }

}
