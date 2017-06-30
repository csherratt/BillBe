use {Memory, Error, Mode, BusDescription};

pub struct MMU {
    pub enabled: bool,
    pub user: bool,
    pub segment_table_base: u32,
}

const VALID_BIT: u32 = 0x1;
const WRITABLE_BIT: u32 = 0x2;
const USER_BIT: u32 = 0x4;
const SEGMENT_BIT: u32 = 0x8;
const REFERENCED_BIT: u32 = 0x8;
const MODIFIED_BIT: u32 = 0x10;

/// turns a address into a segment table offset
fn segment_number(address: u32) -> u32 {
    ((0xFFC0_0000 & address) >> 22)
}

/// turns a address into a page table offset
fn page_number(address: u32) -> u32 {
    ((0x003F_F000 & address) >> 12)
}

/// a segment is a address into a offset into a segment
fn segment_off(address: u32) -> u32 {
    address & 0x003F_FFFF
}

/// get the offset of a address into a page
fn page_off(address: u32) -> u32 {
    address & 0xFFF
}


enum SegmentEntry {
    PageTable(PageTable),
    Segment(Segment),
    Invalid,
}

struct Segment(u32);

impl Segment {
    fn writeable(&self) -> bool {
        self.0 & WRITABLE_BIT == WRITABLE_BIT
    }

    fn user(&self) -> bool {
        self.0 & USER_BIT == USER_BIT
    }

    fn base(&self) -> u32 {
        self.0 & 0xFFC0_0000
    }

    fn size(&self) -> u32 {
        ((self.0 & 0x003F_F000 >> 12) + 1) * 4096
    }
}

struct PageTable(u32);

impl PageTable {
    fn address(&self) -> u32 {
        self.0 & 0xFFFF_F000
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct Page(u32);

impl Page {
    fn valid(&self) -> bool {
        self.0 & VALID_BIT == VALID_BIT
    }

    fn writeable(&self) -> bool {
        self.0 & WRITABLE_BIT == WRITABLE_BIT
    }

    fn user(&self) -> bool {
        self.0 & USER_BIT == USER_BIT
    }

    fn address(&self) -> u32 {
        self.0 & 0xFFFF_F000
    }

    fn set_modified(&self) -> Page {
        Page(self.0 | MODIFIED_BIT)
    }

    fn set_referenced(&self) -> Page {
        Page(self.0 | REFERENCED_BIT)
    }
}


impl MMU {
    pub fn new() -> MMU {
        MMU {
            enabled: false,
            user: false,
            segment_table_base: 0,
        }
    }

    fn segment<M>(&self, memory: &mut M, addr: u32) -> SegmentEntry
        where M: Memory
    {
        let base = self.segment_table_base & 0xFFFF_F000;
        let idx = segment_number(addr);

        return if let Ok(entry) = memory.read_u32(base + idx * 4) {
                   /*println!("stb({:x})[{:x}] => {:x}",
                     self.segment_table_base,
                     idx,
                     entry);*/
                   // check if the entry is invalid
                   if entry & VALID_BIT != VALID_BIT {
                       SegmentEntry::Invalid
                       // the segment bit will determine if the this is to be
                       // treated as a single segment, or a page table
                   } else if entry & SEGMENT_BIT == SEGMENT_BIT {
                       SegmentEntry::Segment(Segment(entry))
                   } else {
                       SegmentEntry::PageTable(PageTable(entry))
                   }
               } else {
                   // if the address cannot be loaded we count
                   // this as an Invalid entry for simplicity
                   SegmentEntry::Invalid
               };
    }

    fn lookup_read<M>(&mut self, memory: &mut M, addr: u32) -> Result<u32, Error>
        where M: Memory
    {
        // if it wasn't enable, just return the vaddr
        if !self.enabled {
            Ok(addr)
        } else {
            let page_entry = match self.segment(memory, addr) {
                SegmentEntry::PageTable(table) => table,
                SegmentEntry::Segment(segment) => {
                    // segment is simple so we can just do it inplace
                    // check if the segment is not accessible in user mode
                    if self.user && !segment.user() {
                        return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::KernelSegmentFromUserMode });
                    }

                    // get the offset into the segment
                    let off = segment_off(addr);

                    // check if segment does not include this address
                    if off > segment.size() {
                        return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::AddressOutOfSegmentBound  });
                    }

                    // calculate the physical address
                    return Ok(segment.base() + off);
                }
                SegmentEntry::Invalid => {
                    return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::InvalidSegmentEntry  });
                }
            };

            // lookup the page address and the index into
            // that page table
            let (page_address, idx) = (page_entry.address(), page_number(addr));

            // fetch the page (hopefully)
            let page = if let Ok(page) = memory.read_u32(page_address + idx * 4) {
                Page(page)
            } else {
                return return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::PageTableIsNotBacked  });
            };

            // check if the page is valid
            if !page.valid() {
                return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::InvalidPageEntry  });
            }

            // check if the page is accessible to the user
            if self.user && !page.user() {
                return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::KernelPageFromUserMode });
            }

            // set the referenced bit, write the value back
            // into the table if the page was not already referenced
            let mpage = page.set_referenced();
            if mpage != page {
                memory.write_u32(page_address + idx * 4, mpage.0)?;
            }

            // get the offset into the page
            let (addr, off) = (page.address(), page_off(addr));
            Ok(addr + off)
        }
    }

    fn lookup_write<M>(&mut self, memory: &mut M, addr: u32) -> Result<u32, Error>
        where M: Memory
    {
        // if it wasn't enable, just return the vaddr
        if !self.enabled {
            Ok(addr)
        } else {
            let page_entry = match self.segment(memory, addr) {
                SegmentEntry::PageTable(table) => table,
                SegmentEntry::Segment(segment) => {
                    // segment is simple so we can just do it inplace
                    // check if the segment is not accessible in user mode
                    if self.user && !segment.user() {
                        return Err(Error::BusFault { address: addr, mode: Mode::Write, description: BusDescription::KernelSegmentFromUserMode });
                    }

                    // get the offset into the segment
                    let off = segment_off(addr);

                    // check if segment does not include this address
                    if off > segment.size() {
                        return Err(Error::BusFault { address: addr, mode: Mode::Write, description: BusDescription::AddressOutOfSegmentBound  });;
                    }

                    // check if the segment is writable
                    if !segment.writeable() {
                        return Err(Error::BusFault { address: addr, mode: Mode::Write, description: BusDescription::WriteToReadOnlySegment });
                    }

                    // calculate the physical address
                    return Ok(segment.base() + off);
                }
                SegmentEntry::Invalid => {
                    return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::InvalidSegmentEntry  });
                }
            };

            // lookup the page address and the index into
            // that page table
            let (page_address, idx) = (page_entry.address(), page_number(addr));

            // fetch the page (hopefully)
            let page = if let Ok(page) = memory.read_u32(page_address + idx * 4) {
                Page(page)
            } else {
                return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::PageTableIsNotBacked });
            };

            // check if the page is valid
            if !page.valid() {
                return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::InvalidPageEntry });
            }

            // check if the page is accessible to the user
            if self.user && !page.user() {
                return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::KernelPageFromUserMode });
            }

            // check if the segment is writable
            if !page.writeable() {
                return Err(Error::BusFault { address: addr, mode: Mode::Read, description: BusDescription::WriteToReadOnlyPage });
            }

            // set the referenced bit, write the value back
            // into the table if the page was not already referenced
            let mpage = page.set_modified();
            if mpage != page {
                memory.write_u32(page_address + idx * 4, mpage.0)?;
            }

            // get the offset into the page
            let (addr, off) = (page.address(), page_off(addr));
            Ok(addr + off)
        }
    }
}

impl<'a, M> Memory for (&'a mut MMU, &'a mut M)
    where M: Memory
{
    fn read(&mut self, vaddr: u32, data: &mut [u8]) -> Result<(), Error> {
        let paddr = try!(self.0.lookup_read(self.1, vaddr));
        self.1.read(paddr, data)
    }

    fn write(&mut self, vaddr: u32, data: &[u8]) -> Result<(), Error> {
        let paddr = try!(self.0.lookup_write(self.1, vaddr));
        self.1.write(paddr, data)
    }
}
