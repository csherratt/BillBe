#![allow(dead_code)]

use byteorder::{BigEndian, ReadBytesExt};
use std::io::{Read, Seek, SeekFrom};
use std::ffi::CStr;
use std;

pub trait OFSDrive: Read + Seek {
    fn read_string(&mut self) -> Result<String, std::io::Error> {
        let mut bytes = [0; 32];
        self.read(&mut bytes[..])?;

        let (null_pos, _) = bytes.iter().enumerate().find(|&(_, x)| *x == 0).unwrap();
        let volume = CStr::from_bytes_with_nul(&bytes[0..null_pos + 1]).unwrap();

        Ok(volume.to_string_lossy().into())
    }

    fn seek_sector(&mut self, sector: u32) -> Result<u64, std::io::Error> {
        self.seek(SeekFrom::Start(sector as u64 * 512))
    }

    fn read_volume_header(&mut self) -> Result<VolumeEntry, std::io::Error> {
        // it's always at the start of the disk
        self.seek_sector(0)?;

        // read the version
        let version = self.read_u32::<BigEndian>()?;

        // skip the first 12 bytes, they are not useful
        self.seek(SeekFrom::Current(12))?;

        // read the first sector
        let first_director_sector = self.read_u32::<BigEndian>()?;

        // skip the next 24 bytes, they are not useful
        self.seek(SeekFrom::Current(24))?;

        // read the volume data
        let volume = self.read_string()?;

        Ok(VolumeEntry {
               version: version,
               first_director_sector: first_director_sector,
               volume: volume,
           })
    }

    fn read_file_entry(&mut self) -> Result<FileEntry, std::io::Error> {
        let name = self.read_string()?;
        let first = self.read_u32::<BigEndian>()?;
        let last = self.read_u32::<BigEndian>()?;
        let _type = self.read_u32::<BigEndian>()?;
        let created = self.read_u32::<BigEndian>()?;
        let modified = self.read_u32::<BigEndian>()?;
        let logical_size = self.read_u32::<BigEndian>()?;
        let physical_size = self.read_u32::<BigEndian>()?;
        let creator = self.read_u32::<BigEndian>()?;

        Ok(FileEntry {
               name: name,
               first: first,
               last: last,
               ftype: _type,
               created: created,
               modified: modified,
               size: logical_size,
               physical_size: physical_size,
               creator: creator,
           })
    }

    fn read_directory(&mut self, sector: u32) -> Result<Directory, std::io::Error> {
        self.seek_sector(sector)?;

        let mut entries = vec![];
        loop {
            let next_block = self.read_u32::<BigEndian>()?;
            self.seek(SeekFrom::Current(4 * 15))?;

            for _ in 0..63 {
                let entry = self.read_file_entry()?;
                if entry.is_null() {
                    break;
                }
                entries.push(entry);
            }

            if next_block == 0 {
                break;
            }
        }

        Ok(Directory { entries: entries })
    }

    // read the fat block from the disk
    fn read_fat(&mut self, sector: u32) -> Result<Vec<Fat>, std::io::Error> {
        self.seek_sector(sector)?;
        let mut out = Vec::new();
        for _ in 0..64 {
            let block = self.read_u32::<BigEndian>()?;
            let count = self.read_u32::<BigEndian>()?;
            if block == !0 {
                break;
            }
            out.push(Fat {
                         block: block,
                         count: count,
                     });
        }
        Ok(out)
    }

    fn read_file(&mut self, file: &FileEntry) -> Result<Vec<u8>, std::io::Error> {
        // allocate a buffer for output equal to the file side
        let mut out: Vec<u8> = std::iter::repeat(0u8).take(file.size as usize).collect();

        // if it does not use a FAT table it will first with a 0x8000_0000 prefix
        if file.first & 0x8000_0000 != 0 {
            let first = file.first & 0x7fff_ffff;
            let _ = self.seek_sector(first)?;
            self.read(&mut out[..])?;
        } else {
            let table = self.read_fat(file.first)?;
            let mut read = 0;
            for fat in table {
                let _ = self.seek_sector(fat.block)?;
                let size = std::cmp::min(out.len() - read, fat.count as usize * 512);
                read += self.read(&mut out[read..read + size])?;
            }
        }
        Ok(out)
    }

    fn read_file_path(&mut self, path: &str) -> Result<Vec<u8>, std::io::Error> {
        let volume = self.read_volume_header().unwrap();
        let mut sector = volume.first_director_sector;
        'outer: for name in path.split(|x| x == '/') {
            let directory = self.read_directory(sector)?;
            for entry in directory.entries {
                match (name == entry.name, entry.is_directory()) {
                    (true, true) => {
                        sector = entry.first;
                        continue 'outer;
                    }
                    (true, false) => {
                        return self.read_file(&entry);
                    }
                    (_, _) => continue,
                }
            }
        }
        return Err(std::io::Error::new(std::io::ErrorKind::NotFound, "not found"));
    }
}

impl<T> OFSDrive for T where T: Read + Seek {}

#[derive(Debug)]
pub struct VolumeEntry {
    version: u32,
    first_director_sector: u32,
    volume: String,
}

#[derive(Debug)]
pub struct Fat {
    block: u32,
    count: u32,
}

#[derive(Debug)]
pub struct FileEntry {
    name: String,
    first: u32,
    last: u32,
    ftype: u32,
    created: u32,
    modified: u32,
    size: u32,
    physical_size: u32,
    creator: u32,
}

impl FileEntry {
    fn is_null(&self) -> bool {
        self.first == 0
    }

    fn is_file(&self) -> bool {
        !self.is_null() && self.ftype != !0 && self.name.len() > 0
    }

    fn is_directory(&self) -> bool {
        !self.is_null() && self.ftype == !0 && self.name.len() > 0
    }
}

#[derive(Debug)]
pub struct Directory {
    entries: Vec<FileEntry>,
}

/*fn dump_tree<S>(src: &mut S, dir: &str, sector: u32)
    where S: OFSDrive
{
    src.seek_sector(sector);
    let directory = src.read_directory().unwrap();
    for entry in &directory.entries {
        println!("{}/{} {:x}-{:x}", dir, entry.name, entry.first, entry.last);
        if entry.is_directory() {
            dump_tree(src, &format!("{}/{}", dir, entry.name), entry.first);
        }
    }
}

fn dump_file<S>(src: &mut S, file: &str, dir: &str, sector: u32)
    where S: OFSDrive
{
    src.seek_sector(sector);
    let directory = src.read_directory().unwrap();
    for entry in &directory.entries {
        let name = format!("{}/{}", dir, entry.name);
        if name == file && entry.is_file() {
            let data = src.read_file(entry).unwrap();
            std::io::stdout().write(&data[..]);
            break;
        }
        if entry.is_directory() {
            dump_file(src, file, &format!("{}/{}", dir, entry.name), entry.first);
        }
    }
}

fn dump_folder<S>(src: &mut S, out_base: &str, dir: &str, sector: u32)
    where S: OFSDrive
{
    src.seek_sector(sector);
    let directory = src.read_directory().unwrap();
    for entry in &directory.entries {
        let name = format!("{}/{}", dir, entry.name);
        let out = format!("{}{}", out_base, name);
        if entry.is_directory() {
            let _ = std::fs::create_dir_all(&out);
            dump_folder(src, out_base, &name, entry.first);
        } else if entry.is_file() {
            let data = src.read_file(entry).unwrap();
            let mut file = std::fs::File::create(out).unwrap();
            file.write(&data[..]);
        }
    }
}*/
