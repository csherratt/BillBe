use {Memory, Error, Mode, BusDescription};

pub struct Rtc {
    addr: u8,
    uip: u8,
    daylight_saving_time: bool,
    hours_24_or_12: bool,
    binary_or_bcd: bool,
    update_ended_interrupt_enabled: bool,
    alarm_interrupt_enabled: bool,
    periodic_interrupt_enabled: bool,
    set_command: bool,
    update_ended_interrupt: bool,
    alarm_interrupt: bool,
    periodic_interrupt: bool,
    interrupt_request: bool,
}

// The address range of the rtc
pub const ADDR_LOW: u32 = 0x80_01c3;
pub const ADDR_HIGH: u32 = 0x80_01c7;

const ADDRESS_PORT: u32 = 0x80_01c3;
const DATA_PORT: u32 = 0x80_01c7;

impl Rtc {
    pub fn new() -> Rtc {
        Rtc {
            addr: 0,
            uip: 0,
            daylight_saving_time: false,
            hours_24_or_12: false,
            binary_or_bcd: false,
            update_ended_interrupt_enabled: false,
            alarm_interrupt_enabled: false,
            periodic_interrupt_enabled: false,
            set_command: false,

            update_ended_interrupt: false,
            alarm_interrupt: false,
            periodic_interrupt: false,
            interrupt_request: false,
        }
    }
}

impl Memory for Rtc {
    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<(), Error> {
        match (addr, self.addr) {
            (ADDRESS_PORT, _) => {
                data[0] = self.addr;
                Ok(())
            }
            (DATA_PORT, 0xa) => {
                data[0] = self.uip;
                Ok(())
            }
            (DATA_PORT, 0xb) => {
                data[0] = if self.daylight_saving_time { 0x1 } else { 0 } |
                          if self.hours_24_or_12 { 0x2 } else { 0 } |
                          if self.binary_or_bcd { 0x4 } else { 0 } |
                          if self.update_ended_interrupt_enabled {
                              0x10
                          } else {
                              0
                          } |
                          if self.alarm_interrupt_enabled {
                              0x20
                          } else {
                              0
                          } |
                          if self.periodic_interrupt_enabled {
                              0x40
                          } else {
                              0
                          } | if self.set_command { 0x80 } else { 0 };
                Ok(())
            }
            (DATA_PORT, 0xc) => {
                data[0] = if self.update_ended_interrupt { 0x10 } else { 0 } |
                          if self.alarm_interrupt { 0x20 } else { 0 } |
                          if self.periodic_interrupt { 0x40 } else { 0 } |
                          if self.interrupt_request { 0x80 } else { 0 };
                Ok(())
            }
            (DATA_PORT, 0xd) => {
                // the chip has maintained power
                data[0] = 0x80;
                Ok(())
            }
            (DATA_PORT, 0xe...0xff) => {
                data[0] = 0;
                Ok(())
            }
            (DATA_PORT, _) => {
                //panic!("todo address={:x}", x);
                Ok(())
            }
            _ => {
                Err(Error::BusFault {
                        address: addr,
                        mode: Mode::Read,
                        description: BusDescription::BadPhysicalMemoryAddress,
                    })
            }
        }
    }

    fn write(&mut self, addr: u32, data: &[u8]) -> Result<(), Error> {
        match (addr, self.addr) {
            (ADDRESS_PORT, _) => {
                self.addr = data[0];
                Ok(())
            }
            (DATA_PORT, 0xb) => {
                self.daylight_saving_time = data[0] & 0x1 != 0;
                self.hours_24_or_12 = data[0] & 0x2 != 0;
                self.binary_or_bcd = data[0] & 0x4 != 0;
                self.update_ended_interrupt_enabled = data[0] & 0x10 != 0;
                self.alarm_interrupt_enabled = data[0] & 0x20 != 0;
                self.periodic_interrupt_enabled = data[0] & 0x40 != 0;
                self.set_command = data[0] & 0x80 != 0;
                Ok(())
            }
            (DATA_PORT, 0xe...0xff) => Ok(()),
            (DATA_PORT, x) => {
                panic!("todo address={}  val={:x}", x, data[0]);
                //Ok(())
            }
            _ => {
                Err(Error::BusFault {
                        address: addr,
                        mode: Mode::Write,
                        description: BusDescription::BadPhysicalMemoryAddress,
                    })
            }
        }
    }
}
