# Always Threaded Mode Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Migrate neomacs to always use the two-thread architecture, removing old polling/callback code.

**Architecture:** Replace `neomacs_display_init()` with `neomacs_display_init_threaded()` in the C initialization path. Fix input handling in render thread (modifiers, mouse position, function keys). Remove dead code (timerfd, callbacks).

**Tech Stack:** Rust, C, winit, wgpu, crossbeam-channel

---

## Task 1: Fix Input Handling - Modifier Keys

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add modifier tracking field to RenderApp**

In `RenderApp` struct, add:

```rust
struct RenderApp {
    // ... existing fields ...
    modifiers: u32,  // Current modifier state (NEOMACS_*_MASK flags)
}
```

Initialize in `RenderApp::new()`:
```rust
modifiers: 0,
```

**Step 2: Handle ModifiersChanged event**

In `window_event()`, add a new match arm before the catch-all:

```rust
WindowEvent::ModifiersChanged(mods) => {
    let state = mods.state();
    self.modifiers = 0;
    if state.shift_key() {
        self.modifiers |= 1; // NEOMACS_SHIFT_MASK
    }
    if state.control_key() {
        self.modifiers |= 4; // NEOMACS_CTRL_MASK
    }
    if state.alt_key() {
        self.modifiers |= 8; // NEOMACS_META_MASK
    }
    if state.super_key() {
        self.modifiers |= 16; // NEOMACS_SUPER_MASK
    }
}
```

**Step 3: Update all input events to use self.modifiers**

Replace `modifiers: 0` with `modifiers: self.modifiers` in:
- `InputEvent::Key` (KeyboardInput handler)
- `InputEvent::MouseButton` (MouseInput handler)
- `InputEvent::MouseMove` (CursorMoved handler)
- `InputEvent::MouseScroll` (MouseWheel handler)

**Step 4: Build to verify**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat: add modifier key tracking to render thread"
```

---

## Task 2: Fix Input Handling - Mouse Position

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add mouse position field to RenderApp**

In `RenderApp` struct, add:

```rust
struct RenderApp {
    // ... existing fields ...
    mouse_pos: (f32, f32),  // Last known cursor position
}
```

Initialize in `RenderApp::new()`:
```rust
mouse_pos: (0.0, 0.0),
```

**Step 2: Update CursorMoved to store position**

In the `WindowEvent::CursorMoved` handler, add before sending:

```rust
WindowEvent::CursorMoved { position, .. } => {
    self.mouse_pos = (position.x as f32, position.y as f32);
    self.comms.send_input(InputEvent::MouseMove {
        x: position.x as f32,
        y: position.y as f32,
        modifiers: self.modifiers,
    });
}
```

**Step 3: Update MouseInput to use stored position**

Replace the MouseInput handler:

```rust
WindowEvent::MouseInput { state, button, .. } => {
    let btn = match button {
        MouseButton::Left => 1,
        MouseButton::Middle => 2,
        MouseButton::Right => 3,
        MouseButton::Back => 4,
        MouseButton::Forward => 5,
        MouseButton::Other(n) => n as u32,
    };
    self.comms.send_input(InputEvent::MouseButton {
        button: btn,
        x: self.mouse_pos.0,
        y: self.mouse_pos.1,
        pressed: state == ElementState::Pressed,
        modifiers: self.modifiers,
    });
}
```

**Step 4: Update MouseWheel to use stored position**

Update the MouseWheel handler to use `self.mouse_pos`:

```rust
WindowEvent::MouseWheel { delta, .. } => {
    let (dx, dy) = match delta {
        winit::event::MouseScrollDelta::LineDelta(x, y) => (x, y),
        winit::event::MouseScrollDelta::PixelDelta(pos) => {
            (pos.x as f32 / 10.0, pos.y as f32 / 10.0)
        }
    };
    self.comms.send_input(InputEvent::MouseScroll {
        delta_x: dx,
        delta_y: dy,
        x: self.mouse_pos.0,
        y: self.mouse_pos.1,
        modifiers: self.modifiers,
    });
}
```

**Step 5: Build to verify**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat: add mouse position tracking to render thread"
```

---

## Task 3: Fix Input Handling - Function Keys

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Expand translate_key() for function keys**

Replace the `translate_key` function with expanded version:

```rust
/// Translate winit key to X11 keysym
fn translate_key(key: &Key) -> u32 {
    match key {
        Key::Named(named) => match named {
            // Function keys
            NamedKey::F1 => 0xffbe,
            NamedKey::F2 => 0xffbf,
            NamedKey::F3 => 0xffc0,
            NamedKey::F4 => 0xffc1,
            NamedKey::F5 => 0xffc2,
            NamedKey::F6 => 0xffc3,
            NamedKey::F7 => 0xffc4,
            NamedKey::F8 => 0xffc5,
            NamedKey::F9 => 0xffc6,
            NamedKey::F10 => 0xffc7,
            NamedKey::F11 => 0xffc8,
            NamedKey::F12 => 0xffc9,
            // Navigation
            NamedKey::Escape => 0xff1b,
            NamedKey::Enter => 0xff0d,
            NamedKey::Tab => 0xff09,
            NamedKey::Backspace => 0xff08,
            NamedKey::Delete => 0xffff,
            NamedKey::Insert => 0xff63,
            NamedKey::Home => 0xff50,
            NamedKey::End => 0xff57,
            NamedKey::PageUp => 0xff55,
            NamedKey::PageDown => 0xff56,
            NamedKey::ArrowLeft => 0xff51,
            NamedKey::ArrowUp => 0xff52,
            NamedKey::ArrowRight => 0xff53,
            NamedKey::ArrowDown => 0xff54,
            // Whitespace
            NamedKey::Space => 0x20,
            // Modifier keys (as keys themselves)
            NamedKey::Shift => 0xffe1,
            NamedKey::Control => 0xffe3,
            NamedKey::Alt => 0xffe9,
            NamedKey::Super => 0xffeb,
            NamedKey::CapsLock => 0xffe5,
            // Other
            NamedKey::PrintScreen => 0xff61,
            NamedKey::ScrollLock => 0xff14,
            NamedKey::Pause => 0xff13,
            NamedKey::NumLock => 0xff7f,
            _ => 0,
        },
        Key::Character(c) => {
            c.chars().next().map(|ch| ch as u32).unwrap_or(0)
        }
        _ => 0,
    }
}
```

**Step 2: Update tests for new keys**

Add test for function keys in the tests module:

```rust
#[test]
fn test_translate_key_function_keys() {
    assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::F1)), 0xffbe);
    assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::F12)), 0xffc9);
    assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Insert)), 0xff63);
    assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::PrintScreen)), 0xff61);
}
```

**Step 3: Run tests**

Run: `cd rust/neomacs-display && cargo test --features "winit-backend" -- render_thread`
Expected: All tests pass

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat: add function key translation to render thread"
```

---

## Task 4: Update C Initialization to Use Threaded Mode

**Files:**
- Modify: `src/neomacsterm.c`

**Step 1: Update neomacs_open_display() to use threaded init**

Replace the initialization section (around lines 297-313) with:

```c
/* Create a new Neomacs display connection */
struct neomacs_display_info *
neomacs_open_display (const char *display_name)
{
  struct neomacs_display_info *dpyinfo;
  static bool gtk_initialized = false;

  /* Initialize GTK if not already done */
  if (!gtk_initialized)
    {
      gtk_init ();
      gtk_initialized = true;
    }

  dpyinfo = xzalloc (sizeof *dpyinfo);
  neomacs_initialize_display_info (dpyinfo);

  /* Initialize the Rust display engine in threaded mode */
  int wakeup_fd = neomacs_display_init_threaded (dpyinfo->width, dpyinfo->height, "Emacs");

  if (wakeup_fd < 0)
    {
      xfree (dpyinfo);
      error ("Failed to initialize Neomacs threaded display engine");
    }

  /* Store the wakeup fd for event loop integration */
  dpyinfo->connection = wakeup_fd;

  /* Register wakeup handler with Emacs event loop */
  add_read_fd (wakeup_fd, neomacs_display_wakeup_handler, dpyinfo);

  /* Note: No longer need neomacs_term_init() - events come via wakeup handler */

  /* Add to display list */
  dpyinfo->next = neomacs_display_list;
  neomacs_display_list = dpyinfo;

  return dpyinfo;
}
```

**Step 2: Remove neomacs_term_init() call**

The old code called `neomacs_term_init()` which registered the callback. This is no longer needed since events flow through the wakeup handler.

**Step 3: Update wakeup handler to receive dpyinfo**

The wakeup handler needs access to dpyinfo. Update its signature usage - it already receives `void *data` which we now pass as `dpyinfo`.

**Step 4: Build to verify**

Run: `make -j8` (from worktree root)
Expected: Build succeeds (may have warnings)

**Step 5: Commit**

```bash
git add src/neomacsterm.c
git commit -m "feat: switch to threaded display initialization"
```

---

## Task 5: Remove Old Callback Code from C

**Files:**
- Modify: `src/neomacsterm.c`

**Step 1: Remove neomacs_event_callback function**

Delete the `neomacs_event_callback` function (around lines 147-217) - it's no longer called.

**Step 2: Remove neomacs_term_init function**

Delete the `neomacs_term_init` function (around lines 269-278) - it only registered the callback.

**Step 3: Remove declaration from header if present**

Check `src/neomacsterm.h` for any declarations of removed functions and remove them.

**Step 4: Build to verify**

Run: `make -j8`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add src/neomacsterm.c src/neomacsterm.h
git commit -m "refactor: remove old callback-based event handling"
```

---

## Task 6: Remove Old FFI Functions from Rust

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`
- Modify: `rust/neomacs-display/include/neomacs_display.h`

**Step 1: Remove old init function**

Remove `neomacs_display_init()` function from ffi.rs - replaced by `neomacs_display_init_threaded()`.

**Step 2: Remove event_fd function**

Remove `neomacs_display_get_event_fd()` function - timerfd no longer used.

**Step 3: Remove callback registration**

Remove `neomacs_display_set_event_callback()` function and related types.

**Step 4: Update C header**

Remove corresponding declarations from `rust/neomacs-display/include/neomacs_display.h` and `src/neomacs_display.h`.

**Step 5: Build to verify**

Run: `cd rust/neomacs-display && cargo build --features "winit-backend"`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs rust/neomacs-display/include/neomacs_display.h src/neomacs_display.h
git commit -m "refactor: remove old polling-based FFI functions"
```

---

## Task 7: Integration Test

**Files:**
- None (manual testing)

**Step 1: Build full Emacs**

Run: `make -j8`
Expected: Build succeeds

**Step 2: Run Emacs**

Run: `./src/emacs`
Expected: Window appears

**Step 3: Test keyboard input**

Type in scratch buffer. Verify characters appear.

**Step 4: Test modifier keys**

Try C-x C-f (find file), M-x (execute command), C-g (quit).

**Step 5: Test mouse**

Click in buffer. Verify cursor moves to click position.

**Step 6: Verify idle CPU**

Run `top` or `htop`. With Emacs idle, CPU should be ~0% (no polling).

**Step 7: Commit any fixes**

If any fixes were needed, commit them.

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Modifier key tracking | render_thread.rs |
| 2 | Mouse position tracking | render_thread.rs |
| 3 | Function key translation | render_thread.rs |
| 4 | Switch to threaded init | neomacsterm.c |
| 5 | Remove old C callback code | neomacsterm.c |
| 6 | Remove old Rust FFI | ffi.rs, headers |
| 7 | Integration test | manual |
