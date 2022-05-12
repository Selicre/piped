use wdc65816::{Cpu, Mem};
use std::collections::HashSet;

#[derive(Clone)]
struct CheckedMem {
    cart: Vec<u8>,
    wram: Vec<u8>,
    regs: Vec<u8>,
    vram: Vec<u8>,
    extram: Vec<u8>,
    error: Option<(u8, u16)>,
    uninit: HashSet<usize>,
    err_value: u8,
    last_store: Option<u32>,
}

impl CheckedMem {
    fn load_u16(&mut self, bank: u8, addr: u16) -> u16 {
        let l = self.load(bank, addr) as u16;
        let h = self.load(bank, addr + 1) as u16;
        l | (h << 8)
    }
    fn load_u24(&mut self, bank: u8, addr: u16) -> u32 {
        let l = self.load(bank, addr) as u32;
        let h = self.load(bank, addr + 1) as u32;
        let b = self.load(bank, addr + 2) as u32;
        l | (h << 8) | (b << 16)
    }
    fn process_dma_ch(&mut self, ch: u16) {
        let a = self.load_u16(0, 0x4302 + ch);
        let a_bank = self.load(0, 0x4304 + ch);
        let size = self.load_u16(0, 0x4305 + ch);
        let b = self.load(0, 0x4301 + ch);
        let params = self.load(0, 0x4300 + ch);
        if b == 0x18 {
            let dest = self.load_u16(0, 0x2116);
            //println!("DMA size {:04X}: VRAM ${:02X}:{:04X} => ${:04X}", size, a_bank, a, dest);
            if params & 0x8 != 0 { // fill transfer
                /*let value = self.load(a_bank, a);
                for i in dest..dest+size {
                    self.vram[i as usize * 2] = value;
                }*/
            } else {
                for i in 0..size {
                    self.vram[(dest*2 + i) as usize] = self.load(a_bank, a + i);
                }
            }
        } else if b == 0x19 {
            let dest = self.load_u16(0, 0x2116);
            //println!("DMA size {:04X}: VRAMh ${:02X}:{:04X} => ${:04X}", size, a_bank, a, dest);
            if params & 0x8 != 0 { // fill transfer
                /*let value = self.load(a_bank, a);
                for i in dest..dest+size {
                    self.vram[i as usize * 2] = value;
                }*/
            }
        } else {
            println!("DMA size {:04X}: ${:02X} ${:02X}:{:04X}", size, b, a_bank, a);
        }
    }
    fn process_dma(&mut self) {
        let dma = self.load(0, 0x420B);
        if dma != 0 {
            for i in 0..8 {
                if dma & (1<<i) != 0 {
                    self.process_dma_ch(i * 0x10);
                }
            }
            self.store(0, 0x420B, 0);
        }
    }
    fn map(&mut self, bank: u8, addr: u16, write: bool) -> &mut u8 {
        let track_uninit = false;
        if bank & 0xFE == 0x7E {
            let ptr = ((bank as usize & 0x1) << 16) + addr as usize;
            if track_uninit {
                if !write && !self.uninit.contains(&ptr) {
                    println!("Uninit read: ${:06X}", 0x7E0000 + ptr);
                }
                self.uninit.insert(ptr);
            }
            &mut self.wram[ptr]
        } else if bank == 0x60 {
            let ptr = addr as usize;
            &mut self.extram[ptr]
        } else if addr < 0x2000 {
            let ptr = addr as usize;
            if track_uninit {
                if !write && !self.uninit.contains(&ptr) {
                    println!("Uninit read: ${:06X}", 0x7E0000 + ptr);
                }
                self.uninit.insert(ptr);
            }
            &mut self.wram[ptr]
        } else if addr < 0x8000 {
            let ptr = addr as usize;
            if track_uninit {
                if !write && !self.uninit.contains(&ptr) {
                    //println!("Uninit read: ${:04X}", ptr);
                }
                self.uninit.insert(ptr);
            }
            &mut self.regs[ptr-0x2000]
        } else if addr > 0x8000 {
            let a = ((bank as usize) << 15) + (addr & 0x7FFF) as usize;
            if let Some(c) = self.cart.get_mut(a) {
                c
            } else {
                self.error = Some((bank, addr));
                &mut self.err_value
            }
        } else {
            self.error = Some((bank, addr));
            &mut self.err_value
        }
    }
}
impl Mem for CheckedMem {
    fn load(&mut self, bank: u8, addr: u16) -> u8 {
        *self.map(bank, addr, false)
    }
    fn store(&mut self, bank: u8, addr: u16, value: u8) {
        //println!("store ${:02X}:{:04X} = {:02X}", bank, addr, value);
        *self.map(bank, addr, true) = value;
        self.last_store = Some(addr as u32 | ((bank as u32) << 16));
    }
}

// todo: not this
fn addr_to_file(addr: u32) -> usize {
    let bank = addr >> 16;
    let addr = addr & 0x7FFF;
    (bank << 15 | addr) as _
}
pub fn render_area(rom: &[u8], params: [u32; 4], cy_limit: u64, obj_limit: usize) -> (Vec<u8>, Vec<u8>, Vec<(u32, Vec<u32>)>) {
    let now = std::time::Instant::now();
    let mut mem = CheckedMem {
        cart: rom.to_vec(),
        wram: std::fs::read("SMAS-wram.bin").unwrap(),
        regs: vec![0; 0x6000],
        vram: vec![0xAA; 0x10000],
        extram: vec![],
        error: None,
        uninit: HashSet::default(),
        err_value: 0,
        last_store: None
    };
    mem.wram[0x70A] = params[0] as u8;
    //mem.wram[0x727] = params[0] as u8;
    mem.wram[0x2B] = params[2] as u8;
    mem.wram[0x2C] = (params[2] >> 8) as u8;
    mem.wram[0x2D] = (params[2] >> 16) as u8;
    //mem.wram[0x43] = params[2];
    //mem.wram[0x47] = params[1];

    println!("{:02X}, {:02X}, {:02X}", mem.wram[0x43], mem.wram[0x45], mem.wram[0x47]);

    let mut objects = 0;
    let mut obj_list: Vec<(u32,Vec<u32>)> = vec![];
    let mut cur_obj = vec![];

    let mut cpu = Cpu::new(mem);
    cpu.emulation = false;
    cpu.s = 0x1FC;
    cpu.pc = 0x86FB;
    cpu.pbr = 0x20;
    cpu.dbr = 0x21;
    //cpu.trace = true;
    let mut cy = 0;
    loop {
        cy += cpu.dispatch() as u64;
        cpu.mem.process_dma();
        if let Some(c) = cpu.mem.last_store.take() {
            if c >= 0x7E2000 && c < 0x7E4000 {
                cur_obj.push(c);
            }
        }
        if cpu.pc == 0x9AA5 && cpu.pbr == 0x20 {
            objects += 1;
            if objects > obj_limit {
                cpu.pc = 0x9AEE;
            }
            let obj = cpu.mem.load_u24(0x7E, 0x2B);
            if let Some(c) = obj_list.last_mut() {
                c.1 = std::mem::take(&mut cur_obj);
            }
            obj_list.push((obj, vec![]));
        }
        //let stop = 0x8789;
        let stop = 0x8a60;
        if cpu.pc == stop && cpu.pbr == 0x20 {
            break;
        }
        if cy > cy_limit { break; }
        /*if let Some(c) = cpu.mem.error.take() {
        }*/
    }
    println!("cycles used: {}", cy);
    println!("{}ms", now.elapsed().as_secs_f64() * 1000.0);
    println!("{:04X}", cpu.mem.load_u16(0x7E, 0x070A));
    (cpu.mem.wram, cpu.mem.vram, obj_list)
}

pub fn render_area_edit(rom: &[u8], params: [u32; 4], obj_src: &[(u32, Vec<u8>)], cy_limit: u64, obj_limit: usize) -> (Vec<u8>, Vec<u8>, Vec<(u32, Vec<u32>)>) {
    let now = std::time::Instant::now();
    let mut mem = CheckedMem {
        cart: rom.to_vec(),
        wram: std::fs::read("SMAS-wram.bin").unwrap(),
        regs: vec![0; 0x6000],
        vram: vec![0xAA; 0x10000],
        extram: vec![0x00; 0x10000],
        error: None,
        uninit: HashSet::default(),
        err_value: 0,
        last_store: None
    };
    mem.wram[0x70A] = params[0] as u8;
    //mem.wram[0x727] = params[0] as u8;
    let addr = 0x600000;
    mem.wram[0x2B] = addr as u8;
    mem.wram[0x2C] = (addr >> 8) as u8;
    mem.wram[0x2D] = (addr >> 16) as u8;

    mem.extram[0..0x0D].copy_from_slice(&mem.cart[addr_to_file(params[2])..][..0x0D]);
    //mem.wram[0x43] = params[2];
    //mem.wram[0x47] = params[1];

    println!("{:02X}, {:02X}, {:02X}", mem.wram[0x43], mem.wram[0x45], mem.wram[0x47]);

    let mut obj_idx = 0;
    let mut cur_obj = vec![];
    let mut obj_list: Vec<(u32,Vec<u32>)> = vec![];

    let mut cpu = Cpu::new(mem);
    cpu.emulation = false;
    cpu.s = 0x1FC;
    cpu.pc = 0x86FB;
    cpu.pbr = 0x20;
    cpu.dbr = 0x21;
    //cpu.trace = true;
    let mut cy = 0;
    let mut in_objects = false;
    loop {
        let cycles = cpu.dispatch() as u64;
        cy += cycles;
        cpu.mem.process_dma();
        /*if let Some(c) = cpu.mem.last_store.take() {
            if c >= 0x7E2000 && c < 0x7E4000 {
                cur_obj.push(c);
            }
        }*/
        if let Some(c) = cpu.mem.last_store.take() {
            if c >= 0x7E2000 && c < 0x7E4000 && in_objects {
                cur_obj.push(c);
            }
        }
        if cpu.pc == 0x9AA5 && cpu.pbr == 0x20 {
            cy = 0;
            in_objects = true;
            let obj = cpu.mem.load_u16(0x7E, 0x2B) as usize;
            //println!("{:04X}", obj);
            if obj_idx == obj_src.len() {
                in_objects = false;
                if let Some(c) = obj_list.last_mut() {
                    c.1 = std::mem::take(&mut cur_obj);
                }
                // terminator 2
                cpu.mem.extram[obj] = 0xFF;
            } else {
                cpu.mem.extram[obj..obj+obj_src[obj_idx].1.len()].copy_from_slice(&obj_src[obj_idx].1);
                if let Some(c) = obj_list.last_mut() {
                    c.1 = std::mem::take(&mut cur_obj);
                }
                obj_list.push((obj_src[obj_idx].0, vec![]));
                obj_idx += 1;
            }
        }
        //let stop = 0x8789;
        let stop = 0x8a60;
        if cpu.pc == stop && cpu.pbr == 0x20 {
            break;
        }
        if cy > cy_limit { break; }
        /*if let Some(c) = cpu.mem.error.take() {
        }*/
    }
    println!("cycles used: {}", cy);
    println!("{}ms", now.elapsed().as_secs_f64() * 1000.0);
    println!("{:04X}", cpu.mem.load_u16(0x7E, 0x070A));
    (cpu.mem.wram, cpu.mem.vram, obj_list)
}
