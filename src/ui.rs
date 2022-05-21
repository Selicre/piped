use imgui::Ui;
use glow::Context as Ctx;
use glow::HasContext;
use crate::draw::GlData;
use std::collections::HashSet;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;


pub struct MainWindow {
    data: ProjectData,
    pub gl_data: GlData,

    levels: Vec<Level>,
    new: Option<New>,
    proj_view: ProjectView,
    sprmap: SpriteMapEdit,
    help: bool
}

pub struct ProjectData {
    focused_level: usize,
    rom: crate::rom::Rom,
}

const HELP: &str =
r#"Piped v0.2 user "manual"
d: switch terrain/entities
e: export level to export.asm (will not be written to the rom, use asar)
f: load sublevel (warning: can crash)
Terrain editing:
z/x: cycle ID (mostly the type of object)
a/s: cycle metadata (mostly width or height)
b/n: cycle extra byte (mostly width of long objects)
c: show block mappings
v: show vram
numpad+/numpad-: change zoom

Placed terrain format: [bank] [id] [extra bytes..]
Note: terrain names only valid for the first tileset. Will be broken elsewhere.
There's also no undo yet, be careful.
"#;

impl MainWindow {
    pub fn new() -> Self {
        let rom = crate::rom::Rom::new(std::fs::read("SMAS.sfc").unwrap());
        MainWindow {
            proj_view: ProjectView::new(&rom),
            data: ProjectData {
                focused_level: 0,
                rom,
            },
            gl_data: Default::default(),
            levels: vec![],
            new: None,
            sprmap: SpriteMapEdit::new(),
            help: false,
        }
    }
    pub fn show_window(&mut self, ui: &Ui, gl: &Ctx) -> bool {
        if self.menubar(ui) { return true; }
        if self.sprmap.shown {
            self.sprmap.show(ui,gl, &self.gl_data);
        }
        if let Some(c) = self.proj_view.show_window(ui,gl) {
            if true {
                self.levels.clear();
            }
            self.levels.push(Level::new(format!("{c:06X?}"), c, &mut self.data));
        }
        self.levels.retain_mut(|c| {
            c.show(ui, gl, &mut self.data, &self.gl_data)
        });
        if self.help {
            ui.window("Help").opened(&mut self.help).build(|| {
                ui.text(HELP);
            });
        }
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
            menu!("Help", { self.help = true; });
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

static mut SPRITE_MAPS: Vec<Vec<[i32;4]>> = vec![];
static mut SPRITE_MAP_SEL: usize = 0xA5;

pub struct SpriteMapEdit {
    shown: bool,
    tilemap_id: usize,
    entity_tiles: Vec<[i32;4]>,
    placed: i32,
    selection: HashSet<usize>,
    update: bool,
    scale: f32,
    drag_delta: [i32;2],
    palette: i32,
    copy_from_box: String,
}

impl SpriteMapEdit {
    pub fn new() -> Self {
        let data = String::from_utf8(std::fs::read("sprite_maps.txt").unwrap()).unwrap();
        let mut maps = vec![];
        let mut cur = vec![];
        for i in data.lines() {
            if i == "#" {
                maps.push(std::mem::take(&mut cur));
            } else {
                let mut line = i.split_whitespace().map(|c| c.parse::<i32>().unwrap_or(0));
                cur.push([();4].map(|c| line.next().unwrap_or(0)));
            }
        }
        unsafe {
            SPRITE_MAPS = maps;
        }
        Self {
            shown: false,
            tilemap_id: 0xAB,
            entity_tiles: vec![],
            placed: 0x400,
            selection: HashSet::new(),
            update: true,
            scale: 64.0,
            drag_delta: [0,0],
            palette: 9,
            copy_from_box: "".into()
        }
    }
    pub fn redraw_entities(&mut self) {
        let mut blocks = vec![];
        let x = 256 + 128;
        let y = 128;
        let sc = self.scale as i32;
        let hsc = sc / 16;
        let tilemap = unsafe { &mut SPRITE_MAPS[SPRITE_MAP_SEL] };
        for (i,&[tx,ty,t,p]) in tilemap.iter().enumerate() {
            let sel = if self.selection.contains(&i) { 1<<16 } else { 0 };
            blocks.push([x+tx*hsc, y+ty*hsc,t,p|sel|(sc/2)]);
        }
        let mut iter = 0x400..;
        for y in 0..32 {
            for x in 0..16 {
                let c = iter.next().unwrap();
                let sel = if self.placed == c { 1 << 16 } else { 0 };
                blocks.push([x * 16,y * 16, c,0x10|sel|(self.palette<<8)]);
            }
        }
        self.entity_tiles = blocks;
    }
    pub fn show(&mut self, ui: &Ui, gl: &Ctx, gl_data: &GlData) {
        if std::mem::take(&mut self.update) {
            self.redraw_entities();
        }
        let scale = self.scale;
        let mut close = true;
        let _id = ui.push_id_ptr(self);
        ui.window("Spritemap Editor").horizontal_scrollbar(true).build(|| {
            // middle click pan
            if ui.is_window_focused() && ui.is_mouse_dragging(imgui::MouseButton::Middle) {
                ui.set_scroll_x(ui.scroll_x() - ui.io().mouse_delta[0]);
                ui.set_scroll_y(ui.scroll_y() - ui.io().mouse_delta[1]);
            }
            let offset = ui.cursor_screen_pos();
            let cur = ui.cursor_pos();
            ui.invisible_button("level", [512.0, 512.0]);
            ui.set_item_allow_overlap();
            let edit = ui.is_item_hovered();
            ui.set_cursor_pos([cur[0]+512.0,cur[1]]);

            gl_data.draw_tiles(offset, gl, self.entity_tiles.clone());

            let tilemap = unsafe { &mut SPRITE_MAPS[SPRITE_MAP_SEL] };

            ui.group(|| {
                ui.text(format!("Sprite {:02X}", unsafe { SPRITE_MAP_SEL }));
                for i in tilemap.iter() {
                    ui.text(format!("{:X?}",i));
                }
                ui.input_text("##copy", &mut self.copy_from_box).build();
                if ui.button("Copy") {
                    unsafe {
                        if let Ok(id) = usize::from_str_radix(&self.copy_from_box, 16) {
                            tilemap.clear();
                            tilemap.extend(&SPRITE_MAPS[id]);
                        }
                    }
                }
            });


            let sel_x;
            let sel_y;
            {
                let x = ui.io().mouse_pos[0] - ui.window_pos()[0] + ui.scroll_x() - cur[0];
                let y = ui.io().mouse_pos[1] - ui.window_pos()[1] + ui.scroll_y() - cur[1];
                sel_x = x as i32;
                sel_y = y as i32;
            }
            if ui.is_mouse_released(imgui::MouseButton::Left) {
                self.update = true;
            }
            if edit {
                if ui.is_mouse_clicked(imgui::MouseButton::Left) {
                    if sel_x < 256 && sel_y < 512 {
                        let x = sel_x / 16;
                        let y = sel_y / 16;
                        self.placed = 0x400 + x + y * 0x10;
                        self.update = true;
                    } else {
                        let mut clicked = false;
                        for (idx,i) in tilemap.iter().enumerate().rev() {
                            let x = sel_x - 384;
                            let y = sel_y - 128;
                            let tx = i[0] * self.scale as i32 / 16;
                            let ty = i[1] * self.scale as i32 / 16;
                            if x >= tx && x < tx + self.scale as i32 / 2
                            && y >= ty && y < ty + self.scale as i32 / 2 {
                                if !ui.io().key_ctrl && !self.selection.contains(&idx) {
                                    self.selection.clear();
                                }
                                self.placed = i[2];
                                self.selection.insert(idx);
                                self.update = true;
                                clicked = true;
                                break;
                            }
                        }
                        if !clicked {
                            self.selection.clear();
                            self.update = true;
                        }
                    }
                }
                let sel_x = (sel_x - 384) * 16 / self.scale as i32;
                let sel_y = (sel_y - 128) * 16 / self.scale as i32;
                if ui.is_mouse_dragging(imgui::MouseButton::Left) {
                    let offset_x = sel_x - self.drag_delta[0];
                    let offset_y = sel_y - self.drag_delta[1];
                    if offset_x != 0 || offset_y != 0 {
                        self.update = true;
                        for (addr,c) in tilemap.iter_mut().enumerate() {
                            if self.selection.contains(&addr) {
                                c[0] += offset_x;
                                c[1] += offset_y;
                            }
                        }
                    }
                }
                if edit && ui.is_mouse_clicked(imgui::MouseButton::Right) {
                    if ui.io().key_shift {
                        tilemap.push([sel_x,  sel_y,  self.placed+0,self.palette<<8]);
                        tilemap.push([sel_x,  sel_y+8,self.placed+1,self.palette<<8]);
                        tilemap.push([sel_x+8,sel_y,  self.placed+2,self.palette<<8]);
                        tilemap.push([sel_x+8,sel_y+8,self.placed+3,self.palette<<8]);
                        self.selection.clear();
                        self.selection.insert(tilemap.len()-1);
                        self.selection.insert(tilemap.len()-2);
                        self.selection.insert(tilemap.len()-3);
                        self.selection.insert(tilemap.len()-4);
                        self.update = true;
                    } else {
                        tilemap.push([sel_x,sel_y,self.placed,self.palette<<8]);
                        self.selection.clear();
                        self.selection.insert(tilemap.len()-1);
                        self.update = true;
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Z as i32) {
                    if self.selection.is_empty() {
                        self.palette -= 1;
                    } else {
                        for (i,c) in tilemap.iter_mut().enumerate().filter(|c| self.selection.contains(&c.0)) {
                            let mut pal = c[3] & 0xF00;
                            pal += 0x100;
                            c[3] &= !0xF00;
                            c[3] |= pal & 0xF00;
                        }
                    }
                    self.update = true;
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::X as i32) {
                    if self.selection.is_empty() {
                        self.palette += 1;
                    } else {
                        for (i,c) in tilemap.iter_mut().enumerate().filter(|c| self.selection.contains(&c.0)) {
                            let mut pal = c[3] & 0xF00;
                            pal += 0x100;
                            c[3] &= !0xF00;
                            c[3] |= pal & 0xF00;
                        }
                    }
                    self.update = true;
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Q as i32) {
                    let c = self.selection.clone();
                    for i in c.iter().copied() {
                        if i != 0 {
                            self.selection.remove(&i);
                            self.selection.insert(i-1);
                            tilemap.swap(i,i-1);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::W as i32) {
                    let c = self.selection.clone();
                    for i in c.iter().copied() {
                        if i != tilemap.len()-1 {
                            self.selection.remove(&i);
                            self.selection.insert(i+1);
                            tilemap.swap(i,i+1);
                            self.update = true;
                        }
                    }
                }
                // hard align
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::D as i32) {
                    for (i,c) in tilemap.iter_mut().enumerate().filter(|c| self.selection.contains(&c.0)) {
                        c[0] &= !0x7;
                        c[1] &= !0x7;
                        self.update = true;
                    }
                }
                // hflip
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::A as i32) {
                    for (i,c) in tilemap.iter_mut().enumerate().filter(|c| self.selection.contains(&c.0)) {
                        c[3] ^= 0x1000;
                        self.update = true;
                    }
                }
                // vflip
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::S as i32) {
                    for (i,c) in tilemap.iter_mut().enumerate().filter(|c| self.selection.contains(&c.0)) {
                        c[3] ^= 0x2000;
                        self.update = true;
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Delete as i32) {
                    let mut i = 0;
                    tilemap.retain(|c| { let d = !self.selection.contains(&i); i += 1; d });
                    self.selection.clear();
                    self.update = true;
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Return as i32) {
                    use std::fmt::Write;
                    let mut out = String::new();
                    for i in unsafe { SPRITE_MAPS.iter() } {
                        for [x,y,t,p] in i {
                            writeln!(out, "{} {} {} {}", x, y, t, p);
                        }
                        writeln!(out, "#");
                    }
                    std::fs::write("sprite_maps.txt", out);
                }
                self.drag_delta = [sel_x, sel_y];
            }
        });
    }
}

#[derive(Debug)]
pub struct Terrain {
    pub is_meta: bool,
    pub x: u8,
    pub y: u8,
    pub id: u8,
    pub meta: u8,
}
impl Terrain {
    pub fn parse(c: &[u8]) -> Self {
        if c[2] < 0x10 {
            Self {
                is_meta: false,
                x: c[1],
                y: c[0] & 0x1F,
                id: (c[0] >> 5) * 0x10 + (c[2]),
                meta: 0
            }
        } else {
            Self {
                is_meta: true,
                x: c[1],
                y: c[0] & 0x1F,
                id: (c[0] >> 5) * 0xF + ((c[2] - 0x10) >> 4),
                meta: c[2] & 0x0F
            }
        }
    }
    pub fn ser(&self) -> [u8;3] {
        let &Self { x,y,id,meta,.. } = self;
        if self.is_meta {
            [y|((id/0xF)<<5),x,((id%0xF) << 4) + meta + 0x10]
        } else {
            [y|((id/0x10)<<5),x,id%0x10]
        }
    }
    pub fn show(&self) -> String {
        if self.is_meta {
            format!("{:02X}:{:02X}:M{:02X}:{:01X}", self.x, self.y, self.id, self.meta)
        } else {
            format!("{:02X}:{:02X}:S{:02X}", self.x, self.y, self.id)
        }
    }
}


#[derive(Default)]
pub struct Level {
    name: String,
    blocks: Vec<[i32;4]>,       // tiles actually
    wram: Vec<u8>,
    vram: Vec<u8>,
    params: [u32;4],
    update: bool,
    redraw: bool,
    // 0: terrain, 1: entities
    mode: u32,
    // Block ptrs occupied by terrain IDs (and sizes)
    obj: Vec<(u32, Vec<u32>, usize)>,
    selection: HashSet<u32>,
    drag_delta: [u32;2],
    drag_delta2: [f32;2],
    // Terrain data
    obj_set: Vec<(u32, Vec<u8>)>,
    // Entity data
    entities: Vec<(u32,[u8;3])>,
    entity_tiles: Vec<[i32;4]>,
    entity_selection: HashSet<u32>,
    entity_search: String,
    scale: f32,
    placed_obj: String,
    placed_ent: usize,
    cur_obj_id: u32,
    cur_ent_id: u32,
}
// todo: not this
fn addr_to_file(addr: u32) -> usize {
    let bank = addr >> 16;
    let addr = addr & 0x7FFF;
    (bank << 15 | addr) as _
}
impl Level {
    pub fn new(name: String, params: [u32;4], data: &mut ProjectData) -> Self {
        //let blocks = blocks.array_chunks::<2>().copied().map(u16::from_le_bytes).collect();
        let mut this = Level {
            name, params, update: true, scale: 32.0, ..Default::default()
        };
        this.init_decomp(data);
        this
    }
    pub fn load_sublevel(&mut self) {
        /*
        use std::convert::TryInto;
        let h = self.params[2];
        let ptr: &[u8;4] = (data.rom[addr_to_file(h)..][..4]).try_into().unwrap();
        self.params[2] = u32::from_le_bytes(*ptr) & 0xFFFFFF;
        let ptr: &[u8;4] = (data.rom[addr_to_file(h+3)..][..4]).try_into().unwrap();
        self.params[3] = u32::from_le_bytes(*ptr) & 0xFFFFFF;
        self.params[0] = self.wram[0x1EBA] as _;
        self.params[1] = self.wram[0x1EBA] as _;
        self.selection.clear();
        self.entity_selection.clear();
        self.init_decomp();*/
    }
    pub fn init_decomp(&mut self, data: &mut ProjectData) {
        let (_,_,obj) = crate::emu::render_area(&data.rom, self.params, 1000000, usize::MAX);
        self.obj_set.clear();
        self.entities.clear();
        for (idx,i) in obj.windows(2).enumerate() {
            self.obj_set.push((idx as _, data.rom[addr_to_file(i[0].0)..addr_to_file(i[1].0)].to_vec()));
        }
        self.entities = data.rom[addr_to_file(self.params[3])+1..]
            .array_chunks::<3>()
            .cloned()
            .take_while(|c| c[0] != 0xFF)
            .enumerate()
            .map(|(i,c)| (i as u32,c))
            .collect::<Vec<_>>();
        self.cur_ent_id = self.entities.len() as _;
        self.cur_obj_id = obj.len() as _;
    }
    pub fn decompress(&mut self, data: &mut ProjectData) {
        let (wram, vram, obj) = crate::emu::render_area_edit(&data.rom, self.params, &self.obj_set, 1000000, usize::MAX);
        self.wram = wram; self.vram = vram; self.obj = obj;

        for ((_,_,len),(_,e)) in self.obj.iter().zip(self.obj_set.iter_mut()) {
            if e.len() < *len {
                e.resize(*len, 0);
            }
        }
        self.redraw_objects(data);
        self.redraw_entities(data);
    }
    pub fn redraw_entities(&mut self, data: &mut ProjectData) {
        let mut blocks = vec![];
        for (i,c) in self.entities.iter() {
            if c[0] == 0xFF { break; }
            let y = c[2] as i32 & 0x1F;
            let x = c[1] as i32;
            let tilemap = unsafe { &mut SPRITE_MAPS[c[0] as usize] };
            let sel = if self.entity_selection.contains(i) { 1<<16 } else { 0 };
            let sc = self.scale as i32;
            let hsc = sc / 16;
            let mut a_blocks = vec![];
            for &[tx,ty,t,p] in tilemap.iter() {
                if sel != 0 && self.drag_delta2 != [0.0;2] {
                    let d = self.drag_delta2;
                    blocks.push([x*sc+tx*hsc, y*sc+ty*hsc,0x600,sel|(sc/2)]);
                    a_blocks.push([x*sc+tx*hsc+d[0] as i32, y*sc+ty*hsc+d[1] as i32,t,p|(sc/2)]);
                } else {
                    blocks.push([x*sc+tx*hsc as i32, y*sc+ty*hsc as i32,t,p|sel|(sc/2)]);
                }
            }
            blocks.append(&mut a_blocks);
            //ui.set_cursor_pos(cursor);
            //ui.button(format!("{}:{:02X?}",names.lines().nth(c[0] as _).unwrap_or("OOB!"), c));
        }
        self.entity_tiles = blocks;
    }
    pub fn redraw_objects(&mut self, data: &mut ProjectData) {
        let tileset = self.wram[0x70A] as u32;
        let ptr = u16::from_le_bytes(data.rom[addr_to_file(0x21CE5A + tileset * 2)..][..2].try_into().unwrap()) as u32;
        let bank = data.rom[addr_to_file(0x21CE80 + tileset)] as u32;

        let tileset = (bank << 16) | ptr;
        println!("{:06X}", tileset);

        let mappings = &data.rom[addr_to_file(tileset)..];

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
        let mut sel_blocks = vec![];
        let sel = self.get_selection();
        for page in 0..15 {
            for x in 0..16 {
                for y in 0..27 {
                    let block_lo = self.wram[0x2000 + page * 0x1B0 + y * 0x10 + x] as usize;
                    let block_hi = self.wram[0x4000 + page * 0x1B0 + y * 0x10 + x] as usize;
                    let sel = sel.get(&((0x7E2000 + page * 0x1b0 + y * 0x10 + x) as u32));
                    let block = (block_hi << 8) + block_lo;
                    let c = if sel == Some(&0) { &mut sel_blocks } else { &mut blocks };
                    c.extend(block_map.get(block).unwrap_or(&vec![]).iter().cloned().map(|mut c| {
                        c[0] += (x as i32 * tile_size + page as i32 * 16 * tile_size) * 2;
                        c[1] += (y as i32 * tile_size) * 2;
                        if sel == Some(&0) {
                            c[0] += self.drag_delta2[0] as i32;
                            c[1] += self.drag_delta2[1] as i32;
                        }
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
        blocks.append(&mut sel_blocks);
        self.blocks = blocks;
    }
    pub fn redraw_mappings(&mut self, data: &mut ProjectData) {
        let tileset = self.wram[0x70A] as u32;
        let ptr = u16::from_le_bytes(data.rom[addr_to_file(0x21CE5A + tileset * 2)..][..2].try_into().unwrap()) as u32;
        let bank = data.rom[addr_to_file(0x21CE80 + tileset)] as u32;

        let tileset = (bank << 16) | ptr;
        println!("{:06X}", tileset);

        let mappings = &data.rom[addr_to_file(tileset)..];

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
    pub fn redraw_spr(&mut self) {
        let mut blocks = vec![];
        let mut iter = 0..;
        for y in 0..32 {
            for x in 0..8 {
                blocks.push([x * 32,   32+y * 32,   (iter.next().unwrap())|0x0000,0x910]);
                blocks.push([x * 32,   32+y * 32+16,(iter.next().unwrap())|0x0000,0x910]);
                blocks.push([x * 32+16,32+y * 32,   (iter.next().unwrap())|0x0000,0x910]);
                blocks.push([x * 32+16,32+y * 32+16,(iter.next().unwrap())|0x0000,0x910]);
            }
        }
        self.blocks = blocks;
    }
    // 0: regular
    // 1: occluded
    pub fn get_selection(&mut self) -> HashMap<u32, u32> {
        let mut blocks = HashMap::new();
        for (addr,c,_) in self.obj.iter() {
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
    pub fn save(&mut self, data: &mut ProjectData) {
        use std::fmt::Write;
        let mut out = String::new();
        writeln!(out, "; piped level export");
        writeln!(out, "org ${:06X}", self.params[2]);    // todo
        writeln!(out, "TerrainData:");
        // header (todo)
        write!(out, "\tdb ");
        for i in 0..0x0D {
            // todo: not this
            write!(out, "${:02X}, ", &data.rom[addr_to_file(self.params[2])+i]);
        }
        out.pop(); out.pop(); writeln!(out);
        for (i,v) in self.obj_set.iter() {
            let c = self.obj.iter().find(|c| *i == c.0).unwrap();
            write!(out, "\tdb ");
            for i in 0..c.2 {
                write!(out, "${:02X}, ", v.get(i).unwrap_or(&0));
            }
            out.pop(); out.pop(); writeln!(out);
        }
        writeln!(out, "\tdb $FF");
        writeln!(out);
        writeln!(out, "org ${:06X}", self.params[3]);    // todo
        writeln!(out, "EntityData:");
        write!(out, "\tdb ");
        for i in 0..1 {
            // todo: not this
            write!(out, "${:02X}, ", &data.rom[addr_to_file(self.params[3])+i]);
        }
        out.pop(); out.pop(); writeln!(out);

        self.entities.sort_by_key(|c| c.1[1]);
        for (i,v) in self.entities.iter() {
            write!(out, "\tdb ");
            for i in v {
                write!(out, "${:02X}, ", i);
            }
            out.pop(); out.pop(); writeln!(out);
        }
        writeln!(out, "\tdb $FF");
        writeln!(out);
        std::fs::write("export.asm", &out);
    }
    pub fn show(&mut self, ui: &Ui, gl: &Ctx, data: &mut ProjectData, gl_data: &GlData) -> bool {
        if std::mem::take(&mut self.update) {
            self.decompress(data);
        }
        if std::mem::take(&mut self.redraw) {
            self.redraw_objects(data);
            self.redraw_entities(data);
        }
        let scale = self.scale;
        let mut close = true;
        let _id = ui.push_id_ptr(self);
        ui.window(format!("Level {}###LevelView", self.name)).horizontal_scrollbar(true).opened(&mut close).build(|| {
            // TODO: assumes single window active
            let active = true;
            // middle click pan
            if /*ui.is_window_focused_with_flags(imgui::WindowFocusedFlags::CHILD_WINDOWS) &&*/ ui.is_mouse_dragging(imgui::MouseButton::Middle) {
                ui.set_scroll_x(ui.scroll_x() - ui.io().mouse_delta[0]);
                ui.set_scroll_y(ui.scroll_y() - ui.io().mouse_delta[1]);
            }
            let offset = ui.cursor_screen_pos();
            let cur = ui.cursor_pos();
            ui.invisible_button("level", [240.0 * scale, 32.0 * scale]);
            ui.set_item_allow_overlap();
            let edit_ter = ui.is_item_hovered() && self.mode == 0;
            let edit_ent = ui.is_item_hovered() && self.mode == 1;
            ui.set_cursor_pos(cur);
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::C as i32) {
                self.redraw_mappings(data);
            }
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::V as i32) {
                self.redraw_spr();
            }
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::D as i32) {
                self.mode ^= 1;
            }
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::NumpadAdd as i32) {
                self.scale *= 2.0;
                self.update = true;
            }
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::NumpadSubtract as i32) {
                self.scale /= 2.0;
                self.update = true;
            }
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::E as i32) {
                self.save(data);
            }
            if ui.is_window_focused() && ui.is_key_index_pressed(glutin::event::VirtualKeyCode::F as i32) {
                self.load_sublevel();
                self.update = true;
            }
            let c = self.blocks.clone();
            gl_data.upload_color(gl, &self.wram[0x1300..]);
            gl_data.upload_gfx(gl, &self.vram[0x4000..]);
            gl_data.draw_tiles(offset, gl, c);
            gl_data.draw_tiles(offset, gl, self.entity_tiles.clone());
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
            }
            if ui.is_mouse_released(imgui::MouseButton::Left) {
                self.update = true;
            }
            let terrain_text = include_str!("../terrain.txt");
            if self.mode == 0 {
                ui.window("Tools").build(|| {
                    ui.input_text("Placed Terrain Data", &mut self.placed_obj).build();
                    ui.child_window("ToolsTerrain").build(|| {
                        let mut is_meta = false;
                        let mut id = 0;
                        for i in terrain_text.lines() {
                            if i == "# 0 S" { continue; }
                            if i == "# 0 R" { is_meta = true; id = 0; continue; }
                            if ui.selectable(format!("{:02X} {}", id, i)) {
                                let c = Terrain { is_meta, id, x: 0, y: 0, meta: 4 }.ser();
                                self.placed_obj = format!("{:01X} {:02X}", c[0] >> 5, c[2]);
                            }
                            id += 1;
                        }
                    });
                });
                let cursor = [cur[0] + ui.scroll_x(), cur[1] + ui.scroll_y()];
                ui.set_cursor_pos(cursor);
                for (i,c) in self.obj_set.iter_mut() {
                    if self.selection.contains(i) {
                        ui.text(format!("{} {:02X?}", Terrain::parse(c).show(), &c[3..]));
                    }
                }
            }
            let names = include_str!("../sprites.txt");
            if self.mode == 1 && active {
                //let cursor = [cur[0] + ui.scroll_x(), cur[1] + ui.scroll_y()];
                //ui.set_cursor_pos(cursor);
                ui.window("Tools").build(|| {
                    ui.input_text("Search", &mut self.entity_search).build();
                    ui.child_window("ToolsEntities").build(|| {
                        //ui.text("Editing entities");
                        //ui.set_next_item_width(200.0);
                        //ui.input_text("Placed", &mut self.placed_ent).build();
                        /*let name = match u8::from_str_radix(&self.placed_ent,16) {
                            Ok(c) => format!("{c:02X} {}", names.lines().nth(c as _).unwrap_or("OOB")),
                            Err(e) => format!("{e:?}"),
                        };*/
                        for (i,c) in names.lines().enumerate() {
                            if !c.is_empty() && !c.to_lowercase().contains(&self.entity_search.to_lowercase()) { continue; }
                            if ui.selectable_config(format!("{:02X} {}",i,c)).selected(self.placed_ent == i).build() {
                                self.placed_ent = i;
                            }
                        }
                    });
                    //ui.text(name);
                });
            }
            // EDIT ENTITIES
            if edit_ent {
                // tooltip
                if ui.is_window_hovered() || ui.is_mouse_down(imgui::MouseButton::Left) {
                    ui.tooltip(|| {
                        ui.text(format!("{:02X}:{:02X}", sel_x,sel_y));
                        for (_,c) in self.entities.iter() {
                            if c[1] as u32 == sel_x && c[2] as u32 &0x1F == sel_y {
                                ui.text(format!("{} {:02X?}",names.lines().nth(c[0] as _).unwrap_or("OOB!"), c));
                            }
                        }
                    });
                }
                if ui.is_mouse_clicked(imgui::MouseButton::Left) {
                    let mut clicked = false;
                    for &(i,[id,x,y]) in self.entities.iter().rev() {
                        if x as u32 == sel_x && (y as u32) & 0x1F == sel_y {
                            if !ui.io().key_ctrl && !self.entity_selection.contains(&i) {
                                self.entity_selection.clear();
                            }
                            self.entity_selection.insert(i as u32);
                            unsafe { SPRITE_MAP_SEL = id as _; }
                            self.update = true;
                            clicked = true;
                        }
                    }
                    if !clicked && !ui.io().key_ctrl {
                        self.entity_selection.clear();
                        self.update = true;
                    }
                }
                if ui.is_mouse_clicked(imgui::MouseButton::Right) {
                    self.entities.push((self.cur_ent_id, [self.placed_ent as u8,sel_x as u8,sel_y as u8]));
                    self.entity_selection.clear();
                    self.entity_selection.insert(self.cur_ent_id);
                    self.cur_ent_id += 1;
                    self.update = true;
                }
                // drag
                if ui.is_mouse_dragging(imgui::MouseButton::Left) {
                    let offset_x = sel_x - self.drag_delta[0];
                    let offset_y = sel_y - self.drag_delta[1];
                    self.drag_delta2[0] += ui.io().mouse_delta[0];
                    self.drag_delta2[1] += ui.io().mouse_delta[1];
                    self.redraw = true;
                    if offset_x != 0 || offset_y != 0 {
                        self.drag_delta2[0] -= offset_x as i32 as f32 * self.scale;
                        self.drag_delta2[1] -= offset_y as i32 as f32 * self.scale;
                        //self.update = true;
                        for (i,c) in self.entities.iter_mut() {
                            if self.entity_selection.contains(i) {
                                let y = c[2] & 0x1F;
                                let x = c[1];
                                let new_x = x + offset_x as u8;
                                let mut new_y = y + offset_y as u8;
                                if new_y >= 0x20 { new_y = y; }
                                c[2] = (c[2] & 0xE0) + (new_y);
                                c[1] = new_x;
                            }
                        }
                    }
                } else {
                    self.drag_delta2 = [0.0;2];
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Delete as i32) {
                    self.entities.retain(|(i,_)| !self.entity_selection.contains(i));
                    self.update = true;
                }
            } else if self.mode == 1 && ui.is_window_hovered() {  // deselect
                if ui.is_mouse_clicked(imgui::MouseButton::Left) && !ui.io().key_ctrl {
                    self.entity_selection.clear();
                    self.update = true;
                }
            }

            // EDIT TERRAIN
            if edit_ter && sel_y < 0x1B {
                let page = sel_x >> 4;
                let inner = sel_x & 0xF;
                let blockp = (page * 0x1B0 + sel_y * 0x10 + inner) as usize;
                let block = ((self.wram[0x4000 + blockp] as u32) << 8)
                            + self.wram[0x2000 + blockp] as u32;
                if ui.is_window_hovered() || ui.is_mouse_down(imgui::MouseButton::Left) {
                    ui.tooltip(|| {
                        ui.text(format!("{:02X}:{:02X}: {:03X}", sel_x,sel_y, block));
                        for (addr,c,_) in self.obj.iter() {
                            if c.contains(&(blockp as u32 + 0x7E2000)) {
                                let c = &mut self.obj_set.iter_mut().find(|c| c.0 == *addr).unwrap().1;
                                ui.text(format!("{} @ {:02X}", Terrain::parse(&c).show(), addr));
                            }
                        }
                    });
                }
                if ui.is_mouse_clicked(imgui::MouseButton::Left) {
                    let mut clicked = false;
                    for (addr,c,_) in self.obj.iter().rev() {
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
            } else if self.mode == 0 && ui.is_window_hovered() {  // deselect
                if ui.is_mouse_clicked(imgui::MouseButton::Left) && !ui.io().key_ctrl {
                    self.selection.clear();
                    self.update = true;
                }
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
                            let mut t = Terrain::parse(c);
                            t.id -= 1;
                            c[..3].copy_from_slice(&t.ser());
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_pressed(imgui::Key::X) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            let mut t = Terrain::parse(c);
                            t.id += 1;
                            c[..3].copy_from_slice(&t.ser());
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_pressed(imgui::Key::A) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            let mut t = Terrain::parse(c);
                            t.meta -= 1;
                            t.meta &= 0x0F;
                            c[..3].copy_from_slice(&t.ser());
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::S as i32) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            let mut t = Terrain::parse(c);
                            t.meta += 1;
                            t.meta &= 0x0F;
                            c[..3].copy_from_slice(&t.ser());
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::B as i32) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            if c.len() >= 4 {
                                c[3] -= 1;
                                self.update = true;
                            }
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::N as i32) {
                    for (addr,c) in self.obj_set.iter_mut() {
                        if self.selection.contains(addr) {
                            if c.len() >= 4 {
                                c[3] += 1;
                                self.update = true;
                            }
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Q as i32) {
                    for (addr,c,_) in self.obj.iter() {
                        let pos = self.obj_set.iter().position(|c| c.0 == *addr).unwrap();
                        if self.selection.contains(addr) && pos != 0 {
                            self.obj_set.swap(pos, pos-1);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::W as i32) {
                    for (addr,c,_) in self.obj.iter() {
                        let pos = self.obj_set.iter().position(|c| c.0 == *addr).unwrap();
                        if self.selection.contains(addr) && pos != self.obj_set.len()-1 {
                            self.obj_set.swap(pos, pos+1);
                            self.update = true;
                        }
                    }
                }
                if ui.is_key_index_pressed(glutin::event::VirtualKeyCode::Delete as i32) {
                    for (addr,c,_) in self.obj.iter() {
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
                    self.drag_delta2[0] += ui.io().mouse_delta[0];
                    self.drag_delta2[1] += ui.io().mouse_delta[1];
                    self.redraw = true;
                    if offset_x != 0 || offset_y != 0 {
                        self.drag_delta2[0] -= offset_x as i32 as f32 * self.scale;
                        self.drag_delta2[1] -= offset_y as i32 as f32 * self.scale;
                        self.update = true;
                        for (addr,c,_) in self.obj.iter() {
                            //println!("{:06X?}, {:06X}", self.selection, addr);
                            if self.selection.contains(addr) {
                                //let c = &mut self.obj_set[*addr as usize];//&mut data.rom[addr_to_file(*addr)..];
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
                } else {
                    self.drag_delta2 = [0.0;2];
                }
            }
            self.drag_delta = [sel_x, sel_y];
        });
        close
    }
}
