use imgui::Ui;
use glow::Context as Ctx;
use glow::HasContext;
use crate::draw::GlData;
use std::collections::HashSet;
use std::collections::HashMap;


pub struct MainWindow {
    levels: Vec<Level>,
    new: Option<New>,
    rom: crate::rom::Rom,
    pub gl_data: GlData,
    proj_view: ProjectView,
}


impl MainWindow {
    pub fn new() -> Self {
        let rom = crate::rom::Rom::new(std::fs::read("SMAS.sfc").unwrap());
        MainWindow {
            levels: vec![],
            new: None,
            proj_view: ProjectView::new(&rom),
            rom,
            gl_data: Default::default()
        }
    }
    pub fn show_window(&mut self, ui: &Ui, gl: &Ctx) -> bool {
        if self.menubar(ui) { return true; }
        if let Some(c) = self.proj_view.show_window(ui,gl) {
            self.levels.push(Level::new(format!("{c:06X?}"), c));
        }
        self.levels.retain_mut(|c| c.show(ui, gl, &self.gl_data));
        false
    }
    pub fn menubar(&mut self, ui: &Ui) -> bool {
        macro_rules! menu {
            ($label:expr, $shortcut:expr, { }) => {
                if ui.menu_item_config($label).shortcut($shortcut).enabled(false).build() { }
            };
            ($label:expr, $shortcut:expr, $data:block) => {
                if ui.menu_item_config($label).shortcut($shortcut).build() $data
            };
            ($label:expr, { }) => {
                if ui.menu_item_config($label).enabled(false).build() { }
            };
            ($label:expr, $data:block) => {
                if ui.menu_item_config($label).build() $data
            };
        }
        // TODO: get active window type
        let mut quit = false;
        ui.main_menu_bar(|| {
            ui.menu("Project", || {
                menu!("New...", "Ctrl+N", { self.new = Some(New::default()); });
                menu!("Open", "Ctrl+O", { });
                menu!("Save", "Ctrl+S", { });
                menu!("Save as...", "Ctrl+Shift+S", { });
                ui.separator();
                menu!("Open Project Directory", { });
                menu!("Build ROM", "F5", { });
                menu!("Project Settings...", "Ctrl+Alt+P", { });
                ui.separator();
                menu!("Close Project", "Ctrl+Shift+W", { });
                menu!("Quit", "Ctrl+Q", { quit = true });
            });
            ui.menu("Edit", || {
                menu!("Undo", "Ctrl+Z", { });
                menu!("Redo", "Ctrl+Shift+Z", { });
                ui.separator();
                menu!("Cut", "Ctrl+X", { });
                menu!("Copy", "Ctrl+C", { });
                menu!("Paste", "Ctrl+V", { });
                menu!("Paste as..", "Ctrl+Shift+V", { });
                ui.separator();
                menu!("Delete", "Del", { });
                menu!("Clear All", "Ctrl-Shift-Del", { });
                ui.separator();
                menu!("Select All", "Ctrl-A", { });
                menu!("Deselect All", "Ctrl-Shift-A", { });
            });
        });
        quit
    }
}

pub struct ProjectView {
    levels: Vec<Vec<(String, [u32;4])>>
}

impl ProjectView {
    pub fn new(rom: &crate::rom::Rom) -> Self {
        let levels = (0..9).map(|world| {
            let y = rom.load_u16(0x21D87D + world * 2) as u32;
            let x = rom.load_u16(0x21D88F + world * 2) as u32;
            let spr = rom.load_u16(0x21D8A1 + world * 2) as u32;
            let ter = rom.load_u16(0x21D8B3 + world * 2) as u32;
            let mut prev = std::collections::HashSet::new();
            (0..x-y).filter_map(|i| {
                let x = rom.load(0x210000 + x + i);
                let y = rom.load(0x210000 + y + i);
                let tileset = y & 0x0F;
                let spr = rom.load_u24(0x210000 + spr + i * 3);
                let ter = rom.load_u24(0x210000 + ter + i * 3);
                if prev.contains(&ter) {
                    return None;
                }
                prev.insert(ter);
                let s = format!("{:02X} {:02X} {:X} {:06X} {:06X}", x, y, tileset, spr, ter);
                Some((s, [tileset as u32, tileset as u32, ter, spr]))
            }).collect()
        }).collect();
        Self { levels }
    }
    pub fn show_window(&mut self, ui: &Ui, gl: &Ctx) -> Option<[u32;4]> {
        let mut out = None;
        ui.window("Project View").build(|| {
            for (world,w) in self.levels.iter().enumerate() {
                if let Some(_c) = ui.tree_node(format!("World {}",world+1)) {
                    for (n,p) in w {
                        if ui.selectable(n) {
                            out = Some(*p);
                        }
                    }
                }
            }
        });
        out
    }
}

#[derive(Default)]
pub struct New {
}

#[derive(Default)]
pub struct Level {
    name: String,
    blocks: Vec<[i32;4]>,
    wram: Vec<u8>,
    vram: Vec<u8>,
    rom: Vec<u8>,
    params: [u32;4],
    update: bool,
    obj: Vec<(u32, Vec<u32>)>,
    selection: HashSet<u32>,
    drag_delta: [u32;2],
    obj_set: Vec<(u32, Vec<u8>)>,
    scale: f32,
    placed_obj: String,
    cur_obj_id: u32,
}
// todo: not this
fn addr_to_file(addr: u32) -> usize {
    let bank = addr >> 16;
    let addr = addr & 0x7FFF;
    (bank << 15 | addr) as _
}
impl Level {
    pub fn new(name: String, params: [u32;4]) -> Self {
        //let blocks = blocks.array_chunks::<2>().copied().map(u16::from_le_bytes).collect();
        let rom = std::fs::read("SMAS.sfc").unwrap();
        let mut this = Level {
            name, params, rom, update: true, scale: 32.0, ..Default::default()
        };
        this.init_decomp();
        this
    }
    pub fn init_decomp(&mut self) {
        let (_,_,obj) = crate::emu::render_area(&self.rom, self.params, 1000000, usize::MAX);
        for (idx,i) in obj.windows(2).enumerate() {
            self.obj_set.push((idx as _, self.rom[addr_to_file(i[0].0)..addr_to_file(i[1].0)].to_vec()));
        }
        self.cur_obj_id = obj.len() as _;
    }
    pub fn decompress(&mut self) {
        let (wram, vram, obj) = crate::emu::render_area_edit(&self.rom, self.params, &self.obj_set, 1000000, usize::MAX);
        self.wram = wram; self.vram = vram; self.obj = obj;
        self.redraw_objects();
    }
    pub fn redraw_objects(&mut self) {
        let tileset = self.wram[0x70A] as u32;
        let ptr = u16::from_le_bytes(self.rom[addr_to_file(0x21CE5A + tileset * 2)..][..2].try_into().unwrap()) as u32;
        let bank = self.rom[addr_to_file(0x21CE80 + tileset)] as u32;

        let tileset = (bank << 16) | ptr;
        println!("{:06X}", tileset);

        let mappings = &self.rom[addr_to_file(tileset)..];

        let tile_size = self.scale as i32 / 2;

        let block_map = mappings.chunks(8).take(0x200).map(|i| {
            i.chunks(2).enumerate().map(|(r,i)| {
                let tile = u16::from_le_bytes(i.try_into().unwrap()) as i32;
                let id = tile & 0x3FF;
                let pal = (tile >> 10) & 0x7;
                let flip = tile >> 14;
                let x = (r as i32 / 2) * tile_size;
                let y = (r as i32 % 2) * tile_size;
                let params = (tile_size) | (pal << 8) | (flip << 12);
                [x,y,id,params]
            }).collect::<Vec<_>>()
        }).collect::<Vec<_>>();
        let mut blocks = vec![];
        let sel = self.get_selection();
        for page in 0..15 {
            for x in 0..16 {
                for y in 0..27 {
                    let block_lo = self.wram[0x2000 + page * 0x1B0 + y * 0x10 + x] as usize;
                    let block_hi = self.wram[0x4000 + page * 0x1B0 + y * 0x10 + x] as usize;
                    let sel = sel.get(&((0x7E2000 + page * 0x1b0 + y * 0x10 + x) as u32));
                    let block = (block_hi << 8) + block_lo;
                    blocks.extend(block_map.get(block).unwrap_or(&vec![]).iter().cloned().map(|mut c| {
                        c[0] += (x as i32 * tile_size + page as i32 * 16 * tile_size) * 2;
                        c[1] += (y as i32 * tile_size) * 2;
                        match sel {
                            Some(0) => c[3] |= 1<<16,
                            Some(1) => c[3] |= 1<<17,
                            _ => {}
                        }
                        c
                    }));
                }
            }
        }
        self.blocks = blocks;
    }
    pub fn redraw_mappings(&mut self) {
        let tileset = self.wram[0x70A] as u32;
        let ptr = u16::from_le_bytes(self.rom[addr_to_file(0x21CE5A + tileset * 2)..][..2].try_into().unwrap()) as u32;
        let bank = self.rom[addr_to_file(0x21CE80 + tileset)] as u32;

        let tileset = (bank << 16) | ptr;
        println!("{:06X}", tileset);

        let mappings = &self.rom[addr_to_file(tileset)..];

        let tile_size = self.scale as i32 / 2;

        let block_map = mappings.chunks(8).take(0x200).map(|i| {
            i.chunks(2).enumerate().map(|(r,i)| {
                let tile = u16::from_le_bytes(i.try_into().unwrap()) as i32;
                let id = tile & 0x3FF;
                let pal = (tile >> 10) & 0x7;
                let flip = tile >> 14;
                let x = (r as i32 / 2) * tile_size;
                let y = (r as i32 % 2) * tile_size;
                let params = (tile_size) | (pal << 8) | (flip << 12);
                [x,y,id,params]
            }).collect::<Vec<_>>()
        }).collect::<Vec<_>>();
        let mut blocks = vec![];
        for x in 0..16 {
            for y in 0..32 {
                let block = x + y * 16;
                blocks.extend(block_map.get(block).unwrap_or(&vec![]).iter().cloned().map(|mut c| {
                    c[0] += (x as i32 * tile_size) * 2;
                    c[1] += (y as i32 * tile_size) * 2;
                    c
                }));
            }
        }
        self.blocks = blocks;
    }
    // 0: regular
    // 1: occluded
    pub fn get_selection(&mut self) -> HashMap<u32, u32> {
        let mut blocks = HashMap::new();
        for (addr,c) in self.obj.iter() {
            if self.selection.contains(addr) {
                blocks.extend(c.iter().map(|c| (*c, 0u32)));
            } else {
                for i in c {
                    blocks.entry(*i).and_modify(|c| *c |= 1);
                }
            }
        }
        blocks
    }
    pub fn get_obj(&mut self, id: u32) -> &(u32, Vec<u8>) {
        self.obj_set.iter().find(|c| c.0 == id).unwrap()
    }
    pub fn show(&mut self, ui: &Ui, gl: &Ctx, gl_data: &GlData) -> bool {
        if std::mem::take(&mut self.update) {
            self.decompress();
        }
        let scale = self.scale;
        let mut close = true;
        let id = ui.push_id_ptr(self);
        ui.window(format!("Level {}", self.name)).horizontal_scrollbar(true).opened(&mut close).build(|| {
            // middle click pan
            if ui.is_window_focused() && ui.is_mouse_dragging(imgui::MouseButton::Middle) {
                ui.set_scroll_x(ui.scroll_x() - ui.io().mouse_delta[0]);
                ui.set_scroll_y(ui.scroll_y() - ui.io().mouse_delta[1]);
            }
            let offset = ui.cursor_screen_pos();
            let cur = ui.cursor_pos();
            ui.invisible_button("level", [224.0 * scale, 27.0 * scale]);
            ui.set_item_allow_overlap();
            let edit_ter = ui.is_item_hovered();
            ui.set_cursor_pos(cur);
            if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::C as i32) {
                self.redraw_mappings();
            }
            let c = self.blocks.clone();
            gl_data.upload_color(gl, &self.wram[0x1300..]);
            gl_data.upload_gfx(gl, &self.vram[0x4000..]);
            gl_data.draw_tiles(offset, gl, c);
            // Process selecting blocks
            let sel_x;
            let sel_y;
            {
                let x = ui.io().mouse_pos[0] - ui.window_pos()[0] + ui.scroll_x() - cur[0];
                let y = ui.io().mouse_pos[1] - ui.window_pos()[1] + ui.scroll_y() - cur[1];
                let x = (x / scale) as u32;
                let y = (y / scale) as u32;
                sel_x = x;
                sel_y = y;
                if edit_ter && y < 0x1B && x < 0x200 {
                    let page = x >> 4;
                    let inner = x & 0xF;
                    let blockp = (page * 0x1B0 + y * 0x10 + inner) as usize;
                    let block = ((self.wram[0x4000 + blockp] as u32) << 8)
                                + self.wram[0x2000 + blockp] as u32;
                    if ui.is_window_hovered() || ui.is_mouse_down(imgui::MouseButton::Left) {
                        ui.tooltip(|| {
                            ui.text(format!("{:02X}:{:02X}: {:03X}", x,y, block));
                            for (addr,c) in self.obj.iter() {
                                if c.contains(&(blockp as u32 + 0x7E2000)) {
                                    let c = &mut self.obj_set.iter_mut().find(|c| c.0 == *addr).unwrap().1;
                                    ui.text(format!("Object {:02X?} @ {:02X}", &c, addr));
                                }
                            }
                        });
                    }
                    if ui.is_mouse_clicked(imgui::MouseButton::Left) {
                        let mut clicked = false;
                        for (addr,c) in self.obj.iter().rev() {
                            if c.contains(&(blockp as u32 + 0x7E2000)) {
                                if !ui.io().key_ctrl && !self.selection.contains(addr) {
                                    self.selection.clear();
                                }
                                self.selection.insert(*addr);
                                self.update = true;
                                clicked = true;
                                break;
                            }
                        }
                        if !clicked && !ui.io().key_ctrl {
                            self.selection.clear();
                            self.update = true;
                        }
                    }
                }
            }
            let cursor = [cur[0] + ui.scroll_x(), cur[1] + ui.scroll_y()];
            ui.set_cursor_pos(cursor);
            ui.group(|| {
                ui.set_next_item_width(200.0);
                ui.input_text("Placed", &mut self.placed_obj).build();
            });
            let names = include_str!("../sprites.txt");
            for c in self.rom[addr_to_file(self.params[3])+1..].chunks(3) {
                if c[0] == 0xFF { break; }
                let cursor = [cur[0] + c[1] as f32 * self.scale, cur[1] + c[2] as f32 * self.scale];
                ui.set_cursor_pos(cursor);
                ui.button(format!("{}",names.lines().nth(c[0] as _).unwrap_or("<OOB>")));
            }

            fn parse(x: u8, y: u8, s: &str) -> Option<Vec<u8>> {
                let mut obj = s.split_whitespace().map(|c| u8::from_str_radix(c, 0x10));
                let mut o = vec![];
                println!("{:?}", obj.clone().collect::<Vec<_>>());
                o.push((obj.next()?.ok()? << 5) | y);
                o.push(x);
                for i in obj {
                    o.push(i.ok()?);
                }
                Some(o)
            }
            // obj placement
            if edit_ter && ui.is_mouse_clicked(imgui::MouseButton::Right) {
                let o = parse(sel_x as _, sel_y as _, &self.placed_obj);
                println!("{:?}", self.obj_set);
                println!("{:?}", self.obj);
                if let Some(o) = o {
                    self.obj_set.push((self.cur_obj_id, o));
                    self.selection.insert(self.cur_obj_id);
                    self.cur_obj_id += 1;
                    self.update = true;
                }
            }
            // obj manipulation - will be replaced
            if !self.selection.is_empty() && edit_ter {
                if ui.is_key_pressed(imgui::Key::Z) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            c[2] -= 1;
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_pressed(imgui::Key::X) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            c[2] += 1;
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_pressed(imgui::Key::A) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            let mut bank = c[0] >> 5;
                            bank -= 1;
                            c[0] = (c[0] & 0x1F) | (bank << 5);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::S as i32) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            let mut bank = c[0] >> 5;
                            bank += 1;
                            c[0] = (c[0] & 0x1F) | (bank << 5);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Q as i32) {
                    for (addr,c) in self.obj.iter() {
                        let pos = self.obj_set.iter().position(|c| c.0 == *addr).unwrap();
                        if self.selection.contains(addr) && pos != 0 {
                            self.obj_set.swap(pos, pos-1);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::W as i32) {
                    for (addr,c) in self.obj.iter() {
                        let pos = self.obj_set.iter().position(|c| c.0 == *addr).unwrap();
                        if self.selection.contains(addr) && pos != self.obj_set.len()-1 {
                            self.obj_set.swap(pos, pos+1);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Delete as i32) {
                    for (addr,c) in self.obj.iter() {
                        let pos = self.obj_set.iter().position(|c| c.0 == *addr).unwrap();
                        if self.selection.contains(addr) {
                            self.obj_set.remove(pos);
                            self.update = true;
                        }
                    }
                }
                if ui.is_mouse_dragging(imgui::MouseButton::Left) {
                    let offset_x = sel_x - self.drag_delta[0];
                    let offset_y = sel_y - self.drag_delta[1];
                    if offset_x != 0 || offset_y != 0 {
                        self.update = true;
                        for (addr,c) in self.obj.iter() {
                            //println!("{:06X?}, {:06X}", self.selection, addr);
                            if self.selection.contains(addr) {
                                //let c = &mut self.obj_set[*addr as usize];//&mut self.rom[addr_to_file(*addr)..];
                                let c = &mut self.obj_set.iter_mut().find(|c| c.0 == *addr).unwrap().1;
                                let y = c[0] & 0x1F;
                                let x = c[1];
                                let new_x = x + offset_x as u8;
                                let mut new_y = y + offset_y as u8;
                                if new_y >= 0x20 { new_y = y; }
                                c[0] = (c[0] & 0xE0) + (new_y);
                                c[1] = new_x;
                            }
                        }
                    }
                }
            }
            self.drag_delta = [sel_x, sel_y];
        });
        close
    }
}
