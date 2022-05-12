#![feature(array_chunks)]

use std::time::Instant;

use glow::HasContext;
use glutin::{event_loop::EventLoop, WindowedContext};
use imgui_winit_support::WinitPlatform;

mod ui;
mod draw;
mod rom;
mod emu;

type Window = WindowedContext<glutin::PossiblyCurrent>;

fn main() {
    // Common setup for creating a winit window and imgui context, not specifc
    // to this renderer at all except that glutin is used to create the window
    // since it will give us access to a GL context
    let (event_loop, window) = create_window();
    let (mut winit_platform, mut imgui_context) = imgui_init(&window);

    // OpenGL context from glow
    let gl = glow_context(&window);

    let mut tex = imgui_glow_renderer::SimpleTextureMap::default();
    // OpenGL renderer from this crate
    let mut ig_renderer = imgui_glow_renderer::Renderer::initialize(
        &gl,
        &mut imgui_context,
        &mut tex,
        true
    ).expect("failed to create renderer");

    let mut last_frame = Instant::now();
    let mut state = ui::MainWindow::new();

    // TODO: this might be useful for loading UI images
    /*
    state.map_texture = Some(unsafe {
        let img = image::open("mappings.png").unwrap().to_rgba8();
        let tex = gl.create_texture().unwrap();
        gl.bind_texture(glow::TEXTURE_2D, Some(tex));
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MAG_FILTER, glow::NEAREST as _);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MIN_FILTER, glow::NEAREST as _);
        gl.tex_image_2d(glow::TEXTURE_2D, 0, glow::SRGB_ALPHA as _, img.width() as _, img.height() as _, 0, glow::RGBA, glow::UNSIGNED_BYTE, Some(&img));
        imgui::TextureId::new(tex as _)
    });*/

    state.gl_data = crate::draw::GlData::setup(&gl);

    window.window().set_visible(true);

    // Standard winit event loop
    event_loop.run(move |event, _, control_flow| {
        match event {
            glutin::event::Event::NewEvents(_) => {
                let now = Instant::now();
                imgui_context
                    .io_mut()
                    .update_delta_time(now.duration_since(last_frame));
                last_frame = now;
            }
            glutin::event::Event::MainEventsCleared => {
                winit_platform
                    .prepare_frame(imgui_context.io_mut(), window.window())
                    .unwrap();
                window.window().request_redraw();
            }
            glutin::event::Event::RedrawRequested(_) => {
                // The renderer assumes you'll be clearing the buffer yourself
                let size = window.window().inner_size();
                unsafe {
                    gl.clear(glow::COLOR_BUFFER_BIT);
                    gl.use_program(Some(state.gl_data.prog));
                    let u = gl.get_uniform_location(state.gl_data.prog, "screen_size");
                    gl.uniform_2_f32(u.as_ref(), size.width as _, size.height as _);
                }
                state.gl_data.window_size[0] = size.width;
                state.gl_data.window_size[1] = size.height;

                let ui = imgui_context.frame();
                unsafe { imgui_sys::igDockSpaceOverViewport(std::ptr::null(), 0, std::ptr::null()) };

                let close = state.show_window(&ui, &gl);
                if close { *control_flow = glutin::event_loop::ControlFlow::Exit; }

                winit_platform.prepare_render(ui, window.window());
                let draw_data = imgui_context.render();

                // This is the only extra render step to add
                ig_renderer
                    .render(&gl, &mut tex, draw_data)
                    .expect("error rendering imgui");

                window.swap_buffers().unwrap();
            }
            glutin::event::Event::RedrawEventsCleared { .. } => {}
            glutin::event::Event::WindowEvent {
                event: glutin::event::WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = glutin::event_loop::ControlFlow::Exit;
            }
            event => {
                winit_platform.handle_event(imgui_context.io_mut(), window.window(), &event);
            }
        }
    });
}

fn create_window() -> (EventLoop<()>, Window) {
    let event_loop = glutin::event_loop::EventLoop::new();
    let window = glutin::window::WindowBuilder::new()
        .with_title("piped v0.2")
        .with_visible(false)
        .with_inner_size(glutin::dpi::LogicalSize::new(1280, 720));
    let window = glutin::ContextBuilder::new()
        .with_vsync(true)
        .build_windowed(window, &event_loop)
        .expect("could not create window");
    let window = unsafe {
        window
            .make_current()
            .expect("could not make window context current")
    };
    (event_loop, window)
}

pub fn glow_context(window: &Window) -> glow::Context {
    unsafe { glow::Context::from_loader_function(|s| window.get_proc_address(s).cast()) }
}

fn imgui_init(window: &Window) -> (WinitPlatform, imgui::Context) {
    let mut imgui_context = imgui::Context::create();
    imgui_context.set_ini_filename(Some(std::path::PathBuf::from("imgui.ini")));

    let mut winit_platform = WinitPlatform::init(&mut imgui_context);
    winit_platform.attach_window(
        imgui_context.io_mut(),
        window.window(),
        imgui_winit_support::HiDpiMode::Rounded,
    );

    imgui_context
        .fonts()
        .add_font(&[
            imgui::FontSource::TtfData {
                data: include_bytes!("/usr/share/fonts/liberation/LiberationSans-Regular.ttf"),
                size_pixels: 16.0,
                config: None
            },
            imgui::FontSource::DefaultFontData { config: None }
        ]);

    imgui_context.io_mut().font_global_scale = (1.0 / winit_platform.hidpi_factor()) as f32;
    imgui_context.io_mut().config_flags |= imgui::ConfigFlags::DOCKING_ENABLE;

    (winit_platform, imgui_context)
}
