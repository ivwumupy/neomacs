//! Simple GTK4 window example demonstrating the display engine.
//!
//! Run with: cargo run --example gtk4_window

use gtk4::prelude::*;
use gtk4::{glib, Application, ApplicationWindow};

use neomacs_display::backend::gtk4::Gtk4Backend;
use neomacs_display::core::scene::{Scene, WindowScene, CursorState, CursorStyle};
use neomacs_display::core::types::{Color, Rect};
use neomacs_display::core::glyph::{Glyph, GlyphRow};
use neomacs_display::core::face::Face;
use neomacs_display::backend::DisplayBackend;

const APP_ID: &str = "org.neomacs.display.example";

fn main() -> glib::ExitCode {
    // Initialize GTK4
    let app = Application::builder()
        .application_id(APP_ID)
        .build();

    app.connect_activate(build_ui);
    app.run()
}

fn build_ui(app: &Application) {
    // Create backend and drawing area
    let mut backend = Gtk4Backend::new();
    backend.init().expect("Failed to initialize GTK4 backend");
    
    // Register some faces
    let mut default_face = Face::new(0);
    default_face.foreground = Color::rgb(0.9, 0.9, 0.9);
    default_face.background = Color::TRANSPARENT;
    default_face.font_family = "Monospace".to_string();
    default_face.font_size = 14.0;
    backend.register_face(default_face);
    
    let mut keyword_face = Face::new(1);
    keyword_face.foreground = Color::rgb(0.6, 0.4, 0.9);
    keyword_face.background = Color::TRANSPARENT;
    keyword_face.font_family = "Monospace".to_string();
    keyword_face.font_size = 14.0;
    keyword_face.attributes = neomacs_display::core::face::FaceAttributes::BOLD;
    backend.register_face(keyword_face);
    
    let mut string_face = Face::new(2);
    string_face.foreground = Color::rgb(0.4, 0.8, 0.4);
    string_face.background = Color::TRANSPARENT;
    string_face.font_family = "Monospace".to_string();
    string_face.font_size = 14.0;
    backend.register_face(string_face);
    
    let mut comment_face = Face::new(3);
    comment_face.foreground = Color::rgb(0.5, 0.5, 0.6);
    comment_face.background = Color::TRANSPARENT;
    comment_face.font_family = "Monospace".to_string();
    comment_face.font_size = 14.0;
    comment_face.attributes = neomacs_display::core::face::FaceAttributes::ITALIC;
    backend.register_face(comment_face);
    
    let drawing_area = backend.create_drawing_area();

    // Create a test scene with glyph rows
    let scene = create_test_scene();
    
    // Update the backend with our scene
    backend.update_scene(scene);

    // Create the window
    let window = ApplicationWindow::builder()
        .application(app)
        .title("Neomacs Display Engine Test")
        .default_width(800)
        .default_height(600)
        .child(&drawing_area)
        .build();

    // Set up animation timer for cursor blink
    let da_clone = drawing_area.clone();
    glib::timeout_add_local(std::time::Duration::from_millis(500), move || {
        da_clone.queue_draw();
        glib::ControlFlow::Continue
    });

    window.present();
}

/// Create glyphs for a text string with a given face
fn text_to_glyphs(text: &str, face_id: u32) -> Vec<Glyph> {
    text.chars().map(|c| Glyph::char_simple(c, face_id)).collect()
}

fn create_test_scene() -> Scene {
    let mut scene = Scene::new(800.0, 600.0);
    scene.background = Color::rgb(0.1, 0.1, 0.15); // Dark background

    // Create test glyph rows simulating Rust code
    let mut rows = Vec::new();
    
    // Line 1: "// Hello from Neomacs!" (comment)
    let mut row1 = GlyphRow::new(0, 20, 16);
    row1.glyphs.extend(text_to_glyphs("// Hello from Neomacs!", 3));
    rows.push(row1);
    
    // Line 2: "fn main() {" (keyword + default)
    let mut row2 = GlyphRow::new(20, 20, 16);
    row2.glyphs.extend(text_to_glyphs("fn", 1));
    row2.glyphs.extend(text_to_glyphs(" main() {", 0));
    rows.push(row2);
    
    // Line 3: '    println!("Hello, world!");' (keyword + string)
    let mut row3 = GlyphRow::new(40, 20, 16);
    row3.glyphs.extend(text_to_glyphs("    println!", 1));
    row3.glyphs.extend(text_to_glyphs("(", 0));
    row3.glyphs.extend(text_to_glyphs("\"Hello, world!\"", 2));
    row3.glyphs.extend(text_to_glyphs(");", 0));
    rows.push(row3);
    
    // Line 4: "}"
    let mut row4 = GlyphRow::new(60, 20, 16);
    row4.glyphs.extend(text_to_glyphs("}", 0));
    rows.push(row4);
    
    // Line 5: empty
    let row5 = GlyphRow::new(80, 20, 16);
    rows.push(row5);
    
    // Line 6: "// GPU-accelerated display!" (comment)
    let mut row6 = GlyphRow::new(100, 20, 16);
    row6.glyphs.extend(text_to_glyphs("// GPU-accelerated display engine", 3));
    rows.push(row6);

    // Create a test window
    let window = WindowScene {
        window_id: 1,
        bounds: Rect::new(50.0, 50.0, 700.0, 500.0),
        background: Color::rgb(0.15, 0.15, 0.2),
        rows,
        cursor: Some(CursorState {
            x: 70.0,  // Position cursor at 'f' in 'fn main()'
            y: 70.0,
            width: 10.0,
            height: 20.0,
            style: CursorStyle::Box,
            color: Color::rgb(0.8, 0.8, 0.2),
            visible: true,
        }),
        scroll_offset: 0.0,
        selected: true,
        mode_line_height: 20,
        header_line_height: 0,
    };

    scene.windows.push(window);

    // Add a second window (like a minibuffer)
    let mut minibuffer_row = GlyphRow::new(0, 20, 16);
    minibuffer_row.glyphs.extend(text_to_glyphs("M-x: ", 1));
    
    let minibuffer = WindowScene {
        window_id: 2,
        bounds: Rect::new(50.0, 560.0, 700.0, 30.0),
        background: Color::rgb(0.2, 0.2, 0.25),
        rows: vec![minibuffer_row],
        cursor: Some(CursorState {
            x: 95.0,
            y: 5.0,
            width: 2.0,
            height: 16.0,
            style: CursorStyle::Bar,
            color: Color::rgb(0.9, 0.9, 0.9),
            visible: true,
        }),
        scroll_offset: 0.0,
        selected: false,
        mode_line_height: 0,
        header_line_height: 0,
    };

    scene.windows.push(minibuffer);

    scene
}
