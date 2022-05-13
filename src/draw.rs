// Utilities to draw raw GL in imgui windows

use imgui::DrawListMut;
use imgui_sys as sys;
use glow::{HasContext, Context as Ctx};


use sys::{ImDrawList,ImDrawCmd};

pub fn register(f: impl FnOnce() + 'static) {
    unsafe {
        let f = Box::new(Box::new(f) as Box<dyn FnOnce()>);
        let dl = sys::igGetWindowDrawList();
        let f = Box::into_raw(f);
        sys::ImDrawList_AddCallback(dl, Some(callback), f as _);
    }
}

unsafe extern "C" fn callback(parent_list: *const ImDrawList, cmd: *const ImDrawCmd) {
    let f = Box::<Box<dyn FnOnce()>>::from_raw((*cmd).UserCallbackData as _);
    f();
}
/*
// Alternate impl in case the other one does weird shit
use std::collections::HashMap;
use std::any::TypeId;
use std::mem::MaybeUninit;

static mut CUR_CB: usize = 0;
static mut CALLBACKS: MaybeUninit<HashMap<usize,Box<dyn FnOnce()>>> = MaybeUninit::uninit();

pub fn register(f: impl FnOnce() + 'static) {
    unsafe {
        // init map
        if CUR_CB == 0 {
            CALLBACKS.write(HashMap::new());
        }
        CUR_CB += 1;
        println!("{}", CALLBACKS.assume_init_mut().len());
        let f = Box::new(f) as Box<dyn FnOnce()>;
        CALLBACKS.assume_init_mut().insert(CUR_CB, f);
        let dl = sys::igGetWindowDrawList();
        sys::ImDrawList_AddCallback(dl, Some(callback), CUR_CB as _);
        CUR_CB += 1;
    }
}

unsafe extern "C" fn callback(parent_list: *const ImDrawList, cmd: *const ImDrawCmd) {
    
    let data = ((*cmd).UserCallbackData as _);
    if let Some(f) = CALLBACKS.assume_init_mut().remove(&data) {
        f();
    } else {
        println!("uhh no {}", data);
    }
    //let f = Box::<Box<dyn FnOnce()>>::from_raw((*cmd).UserCallbackData as _);
    //f();
}*/

pub fn reset() {
    unsafe {
        let dl = sys::igGetWindowDrawList();
        sys::ImDrawList_AddCallback(dl, Some(std::mem::transmute(std::usize::MAX)), std::ptr::null_mut());
    }
}

pub fn get_clipping() -> (sys::ImVec2, sys::ImVec2) {
    let mut min = sys::ImVec2::zero();
    let mut max = sys::ImVec2::zero();
    unsafe {
        let dl = sys::igGetWindowDrawList();
        sys::ImDrawList_GetClipRectMin(&mut min, dl);
        sys::ImDrawList_GetClipRectMax(&mut max, dl);
    }
    (min, max)
}

#[derive(Default, Copy, Clone)]
pub struct GlData {
    pub vao: glow::VertexArray,
    pub vbo: glow::Buffer,
    pub prog: glow::Program,
    pub color_buf: glow::Buffer,
    pub vram_buf: glow::Buffer,
    pub window_size: [u32;2]
}

impl GlData {
    pub fn setup(gl: &Ctx) -> Self { unsafe {
        let vs = gl.create_shader(glow::VERTEX_SHADER).unwrap();
        gl.shader_source(vs, &String::from_utf8(std::fs::read("shaders/ptile.vs.glsl").unwrap()).unwrap());
        gl.compile_shader(vs);
        if !gl.get_shader_compile_status(vs) {
            println!("vs compile {}", gl.get_shader_info_log(vs));
            std::process::exit(1);
        }
        let gs = gl.create_shader(glow::GEOMETRY_SHADER).unwrap();
        gl.shader_source(gs, &String::from_utf8(std::fs::read("shaders/ptile.gs.glsl").unwrap()).unwrap());
        gl.compile_shader(gs);
        if !gl.get_shader_compile_status(gs) {
            println!("gs compile {}", gl.get_shader_info_log(gs));
            std::process::exit(1);
        }
        let fs = gl.create_shader(glow::FRAGMENT_SHADER).unwrap();
        gl.shader_source(fs, &String::from_utf8(std::fs::read("shaders/ptile.fs.glsl").unwrap()).unwrap());
        gl.compile_shader(fs);
        if !gl.get_shader_compile_status(fs) {
            println!("fs compile {}", gl.get_shader_info_log(fs));
            std::process::exit(1);
        }
        let prog = gl.create_program().unwrap();
        gl.attach_shader(prog, vs);
        gl.attach_shader(prog, gs);
        gl.attach_shader(prog, fs);
        gl.link_program(prog);
        if !gl.get_program_link_status(prog) {
            println!("link {}", gl.get_program_info_log(prog));
            std::process::exit(1);
        }
        gl.use_program(Some(prog));

        let color_bind = 0;
        let vram_bind = 1;

        let color_buf = gl.create_buffer().unwrap();
        gl.bind_buffer(glow::ARRAY_BUFFER, Some(color_buf));
        gl.buffer_data_size(glow::ARRAY_BUFFER, 256 * 16, glow::DYNAMIC_DRAW);
        gl.bind_buffer_base(glow::UNIFORM_BUFFER, color_bind, Some(color_buf));
        let color_block = gl.get_uniform_block_index(prog, "Color").unwrap();
        gl.uniform_block_binding(prog, color_block, color_bind);

        let vram_buf = gl.create_buffer().unwrap();
        gl.bind_buffer(glow::ARRAY_BUFFER, Some(vram_buf));
        gl.buffer_data_size(glow::ARRAY_BUFFER, 65536, glow::DYNAMIC_DRAW);
        gl.bind_buffer_base(glow::UNIFORM_BUFFER, vram_bind, Some(vram_buf));
        let vram_block = gl.get_uniform_block_index(prog, "Graphics").unwrap();
        gl.uniform_block_binding(prog, vram_block, vram_bind);


        let vao = gl.create_vertex_array().unwrap();
        gl.bind_vertex_array(Some(vao));
        let vbo = gl.create_buffer().unwrap();
        gl.bind_buffer(glow::ARRAY_BUFFER, Some(vbo));
        gl.enable_vertex_attrib_array(0);
        gl.vertex_attrib_pointer_i32(0, 4, glow::INT, 0, 0);
        GlData { prog, vao, vbo, vram_buf, color_buf, window_size: [0, 0] }
    } }
    pub fn upload_gfx(&self, gl: &Ctx, data: &[u8]) { unsafe {
        gl.use_program(Some(self.prog));
        gl.bind_buffer(glow::ARRAY_BUFFER, Some(self.vram_buf));
        gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, data, glow::DYNAMIC_DRAW);
    } }
    pub fn upload_color(&self, gl: &Ctx, data: &[u8]) { unsafe {
        use snesgfx::color::*;
        // convert to snes
        let data = Palette::from_format(Snes, data);
        let buf = data.0.iter().flat_map(|c| c.0.map(|c| c as f32 / 256.0)).collect::<Vec<_>>();

        gl.use_program(Some(self.prog));
        gl.bind_buffer(glow::ARRAY_BUFFER, Some(self.color_buf));
        gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, buf.align_to().1, glow::DYNAMIC_DRAW);
    } }
    pub fn draw_tiles(&self, offset: [f32; 2], gl: &Ctx, data: Vec<[i32;4]>) {
        let &GlData { prog, vao, vbo, window_size, .. } = self;
        let (min,max) = crate::draw::get_clipping();
        let glp = gl as *const Ctx;
        register(move || unsafe {
            let gl = &*glp;
            gl.use_program(Some(prog));
            let u = gl.get_uniform_location(prog, "offset");
            gl.uniform_2_f32(u.as_ref(), offset[0], offset[1]);
            gl.bind_vertex_array(Some(vao));
            gl.bind_buffer(glow::ARRAY_BUFFER, Some(vbo));
            gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, data.align_to().1, glow::DYNAMIC_DRAW);
            gl.scissor(min.x as _, (window_size[1] as f32 - max.y) as _, (max.x-min.x) as _, (max.y-min.y) as _);
            gl.draw_arrays(glow::POINTS, 0, data.len() as _);
        });
        reset();
    }
}
