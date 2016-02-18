/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! The high-level interface from script to constellation. Using this abstract interface helps
//! reduce coupling between these two components.

use euclid::scale_factor::ScaleFactor;
use euclid::size::TypedSize2D;
use hyper::header::Headers;
use hyper::method::Method;
use ipc_channel::ipc::{self, IpcReceiver, IpcSender, IpcSharedMemory};
use layers::geometry::DevicePixel;
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::fmt;
use url::Url;
use util::geometry::{PagePx, ViewportPx};
use webdriver_msg::{LoadStatus, WebDriverScriptCommand};
use webrender_traits;

#[derive(Deserialize, Serialize)]
pub struct ConstellationChan<T: Deserialize + Serialize>(pub IpcSender<T>);

impl<T: Deserialize + Serialize> ConstellationChan<T> {
    pub fn new() -> (IpcReceiver<T>, ConstellationChan<T>) {
        let (chan, port) = ipc::channel().unwrap();
        (port, ConstellationChan(chan))
    }
}

impl<T: Serialize + Deserialize> Clone for ConstellationChan<T> {
    fn clone(&self) -> ConstellationChan<T> {
        ConstellationChan(self.0.clone())
    }
}

// We pass this info to various threads, so it lives in a separate, cloneable struct.
#[derive(Clone, Copy, Deserialize, Serialize)]
pub struct Failure {
    pub pipeline_id: PipelineId,
    pub parent_info: Option<(PipelineId, SubpageId)>,
}

#[derive(Copy, Clone, Deserialize, Serialize, HeapSizeOf)]
pub struct WindowSizeData {
    /// The size of the initial layout viewport, before parsing an
    /// http://www.w3.org/TR/css-device-adapt/#initial-viewport
    pub initial_viewport: TypedSize2D<ViewportPx, f32>,

    /// The "viewing area" in page px. See `PagePx` documentation for details.
    pub visible_viewport: TypedSize2D<PagePx, f32>,

    /// The resolution of the window in dppx, not including any "pinch zoom" factor.
    pub device_pixel_ratio: ScaleFactor<ViewportPx, DevicePixel, f32>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Deserialize, Serialize)]
pub enum KeyState {
    Pressed,
    Released,
    Repeated,
}

//N.B. Based on the glutin key enum
#[derive(Debug, PartialEq, Eq, Copy, Clone, Deserialize, Serialize, HeapSizeOf)]
pub enum Key {
    Space,
    Apostrophe,
    Comma,
    Minus,
    Period,
    Slash,
    Num0,
    Num1,
    Num2,
    Num3,
    Num4,
    Num5,
    Num6,
    Num7,
    Num8,
    Num9,
    Semicolon,
    Equal,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    LeftBracket,
    Backslash,
    RightBracket,
    GraveAccent,
    World1,
    World2,

    Escape,
    Enter,
    Tab,
    Backspace,
    Insert,
    Delete,
    Right,
    Left,
    Down,
    Up,
    PageUp,
    PageDown,
    Home,
    End,
    CapsLock,
    ScrollLock,
    NumLock,
    PrintScreen,
    Pause,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    Kp0,
    Kp1,
    Kp2,
    Kp3,
    Kp4,
    Kp5,
    Kp6,
    Kp7,
    Kp8,
    Kp9,
    KpDecimal,
    KpDivide,
    KpMultiply,
    KpSubtract,
    KpAdd,
    KpEnter,
    KpEqual,
    LeftShift,
    LeftControl,
    LeftAlt,
    LeftSuper,
    RightShift,
    RightControl,
    RightAlt,
    RightSuper,
    Menu,
}

bitflags! {
    #[derive(Deserialize, Serialize)]
    flags KeyModifiers: u8 {
        const NONE = 0x00,
        const SHIFT = 0x01,
        const CONTROL = 0x02,
        const ALT = 0x04,
        const SUPER = 0x08,
    }
}

#[derive(Deserialize, Serialize)]
pub enum WebDriverCommandMsg {
    LoadUrl(PipelineId, LoadData, IpcSender<LoadStatus>),
    Refresh(PipelineId, IpcSender<LoadStatus>),
    ScriptCommand(PipelineId, WebDriverScriptCommand),
    SendKeys(PipelineId, Vec<(Key, KeyModifiers, KeyState)>),
    TakeScreenshot(PipelineId, IpcSender<Option<Image>>),
}

#[derive(Clone, Copy, Deserialize, Eq, PartialEq, Serialize, HeapSizeOf)]
pub enum PixelFormat {
    K8,         // Luminance channel only
    KA8,        // Luminance + alpha
    RGB8,       // RGB, 8 bits per channel
    RGBA8,      // RGB + alpha, 8 bits per channel
}

#[derive(Clone, Deserialize, Eq, PartialEq, Serialize, HeapSizeOf)]
pub struct ImageMetadata {
    pub width: u32,
    pub height: u32,
}

#[derive(Deserialize, Serialize, HeapSizeOf)]
pub struct Image {
    pub width: u32,
    pub height: u32,
    pub format: PixelFormat,
    #[ignore_heap_size_of = "Defined in ipc-channel"]
    pub bytes: IpcSharedMemory,
    #[ignore_heap_size_of = "Defined in webrender_traits"]
    pub id: Option<webrender_traits::ImageKey>,
}

/// Similar to net::resource_thread::LoadData
/// can be passed to LoadUrl to load a page with GET/POST
/// parameters or headers
#[derive(Clone, Deserialize, Serialize)]
pub struct LoadData {
    pub url: Url,
    pub method: Method,
    pub headers: Headers,
    pub data: Option<Vec<u8>>,
}

impl LoadData {
    pub fn new(url: Url) -> LoadData {
        LoadData {
            url: url,
            method: Method::Get,
            headers: Headers::new(),
            data: None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Copy, Hash, Debug, Deserialize, Serialize)]
pub enum NavigationDirection {
    Forward,
    Back,
}

#[derive(Clone, PartialEq, Eq, Copy, Hash, Debug, Deserialize, Serialize)]
pub struct FrameId(pub u32);

/// Each pipeline ID needs to be unique. However, it also needs to be possible to
/// generate the pipeline ID from an iframe element (this simplifies a lot of other
/// code that makes use of pipeline IDs).
///
/// To achieve this, each pipeline index belongs to a particular namespace. There is
/// a namespace for the constellation thread, and also one for every script thread.
/// This allows pipeline IDs to be generated by any of those threads without conflicting
/// with pipeline IDs created by other script threads or the constellation. The
/// constellation is the only code that is responsible for creating new *namespaces*.
/// This ensures that namespaces are always unique, even when using multi-process mode.
///
/// It may help conceptually to think of the namespace ID as an identifier for the
/// thread that created this pipeline ID - however this is really an implementation
/// detail so shouldn't be relied upon in code logic. It's best to think of the
/// pipeline ID as a simple unique identifier that doesn't convey any more information.
#[derive(Clone, Copy)]
pub struct PipelineNamespace {
    id: PipelineNamespaceId,
    next_index: PipelineIndex,
}

impl PipelineNamespace {
    pub fn install(namespace_id: PipelineNamespaceId) {
        PIPELINE_NAMESPACE.with(|tls| {
            assert!(tls.get().is_none());
            tls.set(Some(PipelineNamespace {
                id: namespace_id,
                next_index: PipelineIndex(0),
            }));
        });
    }

    fn next(&mut self) -> PipelineId {
        let pipeline_id = PipelineId {
            namespace_id: self.id,
            index: self.next_index,
        };

        let PipelineIndex(current_index) = self.next_index;
        self.next_index = PipelineIndex(current_index + 1);

        pipeline_id
    }
}

thread_local!(pub static PIPELINE_NAMESPACE: Cell<Option<PipelineNamespace>> = Cell::new(None));

#[derive(Clone, PartialEq, Eq, Copy, Hash, Debug, Deserialize, Serialize, HeapSizeOf)]
pub struct PipelineNamespaceId(pub u32);

#[derive(Clone, PartialEq, Eq, Copy, Hash, Debug, Deserialize, Serialize, HeapSizeOf)]
pub struct PipelineIndex(pub u32);

#[derive(Clone, PartialEq, Eq, Copy, Hash, Debug, Deserialize, Serialize, HeapSizeOf)]
pub struct PipelineId {
    pub namespace_id: PipelineNamespaceId,
    pub index: PipelineIndex
}

impl PipelineId {
    pub fn new() -> PipelineId {
        PIPELINE_NAMESPACE.with(|tls| {
            let mut namespace = tls.get().expect("No namespace set for this thread!");
            let new_pipeline_id = namespace.next();
            tls.set(Some(namespace));
            new_pipeline_id
        })
    }

    // TODO(gw): This should be removed. It's only required because of the code
    // that uses it in the devtools lib.rs file (which itself is a TODO). Once
    // that is fixed, this should be removed. It also relies on the first
    // call to PipelineId::new() returning (0,0), which is checked with an
    // assert in handle_init_load().
    pub fn fake_root_pipeline_id() -> PipelineId {
        PipelineId {
            namespace_id: PipelineNamespaceId(0),
            index: PipelineIndex(0),
        }
    }
}

impl fmt::Display for PipelineId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let PipelineNamespaceId(namespace_id) = self.namespace_id;
        let PipelineIndex(index) = self.index;
        write!(fmt, "({},{})", namespace_id, index)
    }
}

#[derive(Clone, PartialEq, Eq, Copy, Hash, Debug, Deserialize, Serialize, HeapSizeOf)]
pub struct SubpageId(pub u32);

pub trait ConvertPipelineIdToWebRender {
    fn to_webrender(&self) -> webrender_traits::PipelineId;
}

pub trait ConvertPipelineIdFromWebRender {
    fn from_webrender(&self) -> PipelineId;
}

impl ConvertPipelineIdToWebRender for PipelineId {
    fn to_webrender(&self) -> webrender_traits::PipelineId {
        let PipelineNamespaceId(namespace_id) = self.namespace_id;
        let PipelineIndex(index) = self.index;
        webrender_traits::PipelineId(namespace_id, index)
    }
}

impl ConvertPipelineIdFromWebRender for webrender_traits::PipelineId {
    fn from_webrender(&self) -> PipelineId {
        PipelineId {
            namespace_id: PipelineNamespaceId(self.0),
            index: PipelineIndex(self.1),
        }
    }
}
