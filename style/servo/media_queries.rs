/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Servo's media-query device and expression representation.

use crate::color::AbsoluteColor;
use crate::context::QuirksMode;
use crate::custom_properties::CssEnvironment;
use crate::derives::*;
use crate::font_metrics::FontMetrics;
use crate::logical_geometry::WritingMode;
use crate::media_queries::MediaType;
use crate::properties::style_structs::Font;
use crate::properties::ComputedValues;
use crate::queries::feature::{AllowsRanges, Evaluator, FeatureFlags, QueryFeatureDescription};
use crate::queries::values::PrefersColorScheme;
use crate::values::computed::font::GenericFontFamily;
use crate::values::computed::{
    CSSPixelLength, Context, Length, LineHeight, NonNegativeLength, Resolution,
};
use crate::values::specified::color::{ColorSchemeFlags, ForcedColors};
use crate::values::specified::font::{
    QueryFontMetricsFlags, FONT_MEDIUM_CAP_PX, FONT_MEDIUM_CH_PX, FONT_MEDIUM_EX_PX,
    FONT_MEDIUM_IC_PX, FONT_MEDIUM_LINE_HEIGHT_PX, FONT_MEDIUM_PX,
};
use crate::values::specified::ViewportVariant;
use crate::values::KeyframesName;
use app_units::{Au, AU_PER_PX};
use euclid::default::Size2D as UntypedSize2D;
use euclid::{Scale, SideOffsets2D, Size2D};
use mime::Mime;
use parking_lot::RwLock;
use servo_arc::Arc;
use std::fmt::Debug;
use std::mem;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use style_traits::{CSSPixel, DevicePixel};

/// A trait used to query font metrics in clients of Stylo. This is used by Device to
/// query font metrics in a way that is specific to the client using Stylo.
pub trait FontMetricsProvider: Debug + Sync {
    /// Query the font metrics for the given font and the given base font size.
    fn query_font_metrics(
        &self,
        vertical: bool,
        font: &Font,
        base_size: CSSPixelLength,
        flags: QueryFontMetricsFlags,
    ) -> FontMetrics;
    /// Gets the base size given a generic font family.
    fn base_size_for_generic(&self, generic: GenericFontFamily) -> Length;
}

/// A device is a structure that represents the current media a given document
/// is displayed in.
///
/// This is the struct against which media queries are evaluated.
/// and contains all the viewport rule state.
///
/// This structure also contains atomics used for computing root font-relative
/// units. These atomics use relaxed ordering, since when computing the style
/// of the root element, there can't be any other style being computed at the
/// same time (given we need the style of the parent to compute everything else).
#[derive(Debug, MallocSizeOf)]
pub struct Device {
    /// The current media type used by de device.
    media_type: MediaType,
    /// The current viewport size, in CSS pixels.
    viewport_size: Size2D<f32, CSSPixel>,
    /// The current device pixel ratio, from CSS pixels to device pixels.
    device_pixel_ratio: Scale<f32, CSSPixel, DevicePixel>,
    /// The current quirks mode.
    #[ignore_malloc_size_of = "Pure stack type"]
    quirks_mode: QuirksMode,

    /// Current computed style of the root element, used for calculations of
    /// root font-relative units.
    #[ignore_malloc_size_of = "Arc"]
    root_style: RwLock<Arc<ComputedValues>>,
    /// Font size of the root element, used for rem units in other elements.
    #[ignore_malloc_size_of = "Pure stack type"]
    root_font_size: AtomicU32,
    /// Line height of the root element, used for rlh units in other elements.
    #[ignore_malloc_size_of = "Pure stack type"]
    root_line_height: AtomicU32,
    /// X-height of the root element, used for rex units in other elements.
    #[ignore_malloc_size_of = "Pure stack type"]
    root_font_metrics_ex: AtomicU32,
    /// Cap-height of the root element, used for rcap units in other elements.
    #[ignore_malloc_size_of = "Pure stack type"]
    root_font_metrics_cap: AtomicU32,
    /// Advance measure (ch) of the root element, used for rch units in other elements.
    #[ignore_malloc_size_of = "Pure stack type"]
    root_font_metrics_ch: AtomicU32,
    /// Ideographic advance measure of the root element, used for ric units in other elements.
    #[ignore_malloc_size_of = "Pure stack type"]
    root_font_metrics_ic: AtomicU32,
    /// Whether any styles computed in the document relied on the root font-size
    /// by using rem units.
    #[ignore_malloc_size_of = "Pure stack type"]
    used_root_font_size: AtomicBool,
    /// Whether any styles computed in the document relied on the root line-height
    /// by using rlh units.
    #[ignore_malloc_size_of = "Pure stack type"]
    used_root_line_height: AtomicBool,
    /// Whether any styles computed in the document relied on the root font metrics
    /// by using rcap, rch, rex, or ric units. This is a lock instead of an atomic
    /// in order to prevent concurrent writes to the root font metric values.
    #[ignore_malloc_size_of = "Pure stack type"]
    used_root_font_metrics: RwLock<bool>,
    /// Whether any styles computed in the document relied on font metrics.
    used_font_metrics: AtomicBool,
    /// Whether any styles computed in the document relied on the viewport size.
    #[ignore_malloc_size_of = "Pure stack type"]
    used_viewport_units: AtomicBool,
    /// Whether the user prefers light mode or dark mode
    #[ignore_malloc_size_of = "Pure stack type"]
    prefers_color_scheme: PrefersColorScheme,
    /// The CssEnvironment object responsible of getting CSS environment
    /// variables.
    environment: CssEnvironment,
    /// An implementation of a trait which implements support for querying font metrics.
    #[ignore_malloc_size_of = "Owned by embedder"]
    font_metrics_provider: Box<dyn FontMetricsProvider>,
    /// The default computed values for this Device.
    #[ignore_malloc_size_of = "Arc is shared"]
    default_computed_values: Arc<ComputedValues>,
}

impl Device {
    /// Trivially construct a new `Device`.
    pub fn new(
        media_type: MediaType,
        quirks_mode: QuirksMode,
        viewport_size: Size2D<f32, CSSPixel>,
        device_pixel_ratio: Scale<f32, CSSPixel, DevicePixel>,
        font_metrics_provider: Box<dyn FontMetricsProvider>,
        default_computed_values: Arc<ComputedValues>,
        prefers_color_scheme: PrefersColorScheme,
    ) -> Device {
        let default_values =
            ComputedValues::initial_values_with_font_override(Font::initial_values());
        let root_style = RwLock::new(Arc::clone(&default_values));
        Device {
            media_type,
            viewport_size,
            device_pixel_ratio,
            quirks_mode,
            root_style,
            root_font_size: AtomicU32::new(FONT_MEDIUM_PX.to_bits()),
            root_line_height: AtomicU32::new(FONT_MEDIUM_LINE_HEIGHT_PX.to_bits()),
            root_font_metrics_ex: AtomicU32::new(FONT_MEDIUM_EX_PX.to_bits()),
            root_font_metrics_cap: AtomicU32::new(FONT_MEDIUM_CAP_PX.to_bits()),
            root_font_metrics_ch: AtomicU32::new(FONT_MEDIUM_CH_PX.to_bits()),
            root_font_metrics_ic: AtomicU32::new(FONT_MEDIUM_IC_PX.to_bits()),
            used_root_font_size: AtomicBool::new(false),
            used_root_line_height: AtomicBool::new(false),
            used_root_font_metrics: RwLock::new(false),
            used_font_metrics: AtomicBool::new(false),
            used_viewport_units: AtomicBool::new(false),
            prefers_color_scheme,
            environment: CssEnvironment,
            font_metrics_provider,
            default_computed_values,
        }
    }

    /// Get the relevant environment to resolve `env()` functions.
    #[inline]
    pub fn environment(&self) -> &CssEnvironment {
        &self.environment
    }

    /// Return the default computed values for this device.
    pub fn default_computed_values(&self) -> &ComputedValues {
        &self.default_computed_values
    }

    /// Store a pointer to the root element's computed style, for use in
    /// calculation of root font-relative metrics.
    pub fn set_root_style(&self, style: &Arc<ComputedValues>) {
        *self.root_style.write() = style.clone();
    }

    /// Get the font size of the root element (for rem)
    pub fn root_font_size(&self) -> CSSPixelLength {
        self.used_root_font_size.store(true, Ordering::Relaxed);
        CSSPixelLength::new(f32::from_bits(self.root_font_size.load(Ordering::Relaxed)))
    }

    /// Set the font size of the root element (for rem), in zoom-independent CSS pixels.
    pub fn set_root_font_size(&self, size: f32) {
        self.root_font_size.store(size.to_bits(), Ordering::Relaxed)
    }

    /// Get the line height of the root element (for rlh)
    pub fn root_line_height(&self) -> CSSPixelLength {
        self.used_root_line_height.store(true, Ordering::Relaxed);
        CSSPixelLength::new(f32::from_bits(
            self.root_line_height.load(Ordering::Relaxed),
        ))
    }

    /// Set the line height of the root element (for rlh), in zoom-independent CSS pixels.
    pub fn set_root_line_height(&self, size: f32) {
        self.root_line_height
            .store(size.to_bits(), Ordering::Relaxed);
    }

    /// Get the x-height of the root element (for rex)
    pub fn root_font_metrics_ex(&self) -> Length {
        self.ensure_root_font_metrics_updated();
        Length::new(f32::from_bits(
            self.root_font_metrics_ex.load(Ordering::Relaxed),
        ))
    }

    /// Set the x-height of the root element (for rex), in zoom-independent CSS pixels.
    pub fn set_root_font_metrics_ex(&self, size: f32) -> bool {
        let size = size.to_bits();
        let previous = self.root_font_metrics_ex.swap(size, Ordering::Relaxed);
        previous != size
    }

    /// Get the cap-height of the root element (for rcap)
    pub fn root_font_metrics_cap(&self) -> Length {
        self.ensure_root_font_metrics_updated();
        Length::new(f32::from_bits(
            self.root_font_metrics_cap.load(Ordering::Relaxed),
        ))
    }

    /// Set the cap-height of the root element (for rcap), in zoom-independent CSS pixels.
    pub fn set_root_font_metrics_cap(&self, size: f32) -> bool {
        let size = size.to_bits();
        let previous = self.root_font_metrics_cap.swap(size, Ordering::Relaxed);
        previous != size
    }

    /// Get the advance measure of the root element (for rch)
    pub fn root_font_metrics_ch(&self) -> Length {
        self.ensure_root_font_metrics_updated();
        Length::new(f32::from_bits(
            self.root_font_metrics_ch.load(Ordering::Relaxed),
        ))
    }

    /// Set the advance measure of the root element (for rch), in zoom-independent CSS pixels.
    pub fn set_root_font_metrics_ch(&self, size: f32) -> bool {
        let size = size.to_bits();
        let previous = self.root_font_metrics_ch.swap(size, Ordering::Relaxed);
        previous != size
    }

    /// Get the ideographic advance measure of the root element (for ric)
    pub fn root_font_metrics_ic(&self) -> Length {
        self.ensure_root_font_metrics_updated();
        Length::new(f32::from_bits(
            self.root_font_metrics_ic.load(Ordering::Relaxed),
        ))
    }

    /// Set the ideographic advance measure of the root element (for ric), in zoom-independent CSS pixels.
    pub fn set_root_font_metrics_ic(&self, size: f32) -> bool {
        let size = size.to_bits();
        let previous = self.root_font_metrics_ic.swap(size, Ordering::Relaxed);
        previous != size
    }

    /// Returns the computed line-height for the font in a given computed values instance.
    ///
    /// If you pass down an element, then the used line-height is returned.
    pub fn calc_line_height(
        &self,
        font: &crate::properties::style_structs::Font,
        _writing_mode: WritingMode,
        _element: Option<()>,
    ) -> NonNegativeLength {
        (match font.line_height {
            // TODO: compute `normal` from the font metrics
            LineHeight::Normal => CSSPixelLength::new(0.),
            LineHeight::Number(number) => font.font_size.computed_size() * number.0,
            LineHeight::Length(length) => length.0,
        })
        .into()
    }

    /// Get the quirks mode of the current device.
    pub fn quirks_mode(&self) -> QuirksMode {
        self.quirks_mode
    }

    /// Sets the body text color for the "inherit color from body" quirk.
    ///
    /// <https://quirks.spec.whatwg.org/#the-tables-inherit-color-from-body-quirk>
    pub fn set_body_text_color(&self, _color: AbsoluteColor) {
        // Servo doesn't implement this quirk (yet)
    }

    /// Gets the base size given a generic font family.
    pub fn base_size_for_generic(&self, generic: GenericFontFamily) -> Length {
        self.font_metrics_provider.base_size_for_generic(generic)
    }

    /// Whether a given animation name may be referenced from style.
    pub fn animation_name_may_be_referenced(&self, _: &KeyframesName) -> bool {
        // Assume it is, since we don't have any good way to prove it's not.
        true
    }

    /// Returns whether we ever looked up the root font size of the Device.
    pub fn used_root_font_size(&self) -> bool {
        self.used_root_font_size.load(Ordering::Relaxed)
    }

    /// Returns whether we ever looked up the root line-height of the device.
    pub fn used_root_line_height(&self) -> bool {
        self.used_root_line_height.load(Ordering::Relaxed)
    }

    /// Returns whether we ever looked up the root font metrics of the device.
    pub fn used_root_font_metrics(&self) -> bool {
        *self.used_root_font_metrics.read()
    }

    /// Returns whether font metrics have been queried.
    pub fn used_font_metrics(&self) -> bool {
        self.used_font_metrics.load(Ordering::Relaxed)
    }

    /// Get the viewport size on this [`Device`].
    pub fn viewport_size(&self) -> Size2D<f32, CSSPixel> {
        self.viewport_size
    }

    /// Set the viewport size on this [`Device`].
    ///
    /// Note that this does not update any associated `Stylist`. For this you must call
    /// `Stylist::media_features_change_changed_style` and
    /// `Stylist::force_stylesheet_origins_dirty`.
    pub fn set_viewport_size(&mut self, viewport_size: Size2D<f32, CSSPixel>) {
        self.viewport_size = viewport_size;
    }

    /// Returns the viewport size of the current device in app units, needed,
    /// among other things, to resolve viewport units.
    #[inline]
    pub fn au_viewport_size(&self) -> UntypedSize2D<Au> {
        Size2D::new(
            Au::from_f32_px(self.viewport_size.width),
            Au::from_f32_px(self.viewport_size.height),
        )
    }

    /// Like the above, but records that we've used viewport units.
    pub fn au_viewport_size_for_viewport_unit_resolution(
        &self,
        _: ViewportVariant,
    ) -> UntypedSize2D<Au> {
        self.used_viewport_units.store(true, Ordering::Relaxed);
        // Servo doesn't have dynamic UA interfaces that affect the viewport,
        // so we can just ignore the ViewportVariant.
        self.au_viewport_size()
    }

    /// Whether viewport units were used since the last device change.
    pub fn used_viewport_units(&self) -> bool {
        self.used_viewport_units.load(Ordering::Relaxed)
    }

    /// Returns the number of app units per device pixel we're using currently.
    pub fn app_units_per_device_pixel(&self) -> i32 {
        (AU_PER_PX as f32 / self.device_pixel_ratio.0) as i32
    }

    /// Returns the device pixel ratio, ignoring the full zoom factor.
    pub fn device_pixel_ratio_ignoring_full_zoom(&self) -> Scale<f32, CSSPixel, DevicePixel> {
        self.device_pixel_ratio
    }

    /// Returns the device pixel ratio.
    pub fn device_pixel_ratio(&self) -> Scale<f32, CSSPixel, DevicePixel> {
        self.device_pixel_ratio
    }

    /// Set a new device pixel ratio on this [`Device`].
    ///
    /// Note that this does not update any associated `Stylist`. For this you must call
    /// `Stylist::media_features_change_changed_style` and
    /// `Stylist::force_stylesheet_origins_dirty`.
    pub fn set_device_pixel_ratio(
        &mut self,
        device_pixel_ratio: Scale<f32, CSSPixel, DevicePixel>,
    ) {
        self.device_pixel_ratio = device_pixel_ratio;
    }

    /// Gets the size of the scrollbar in CSS pixels.
    pub fn scrollbar_inline_size(&self) -> CSSPixelLength {
        // TODO: implement this.
        CSSPixelLength::new(0.0)
    }

    /// Queries font metrics using the [`FontMetricsProvider`] interface.
    pub fn query_font_metrics(
        &self,
        vertical: bool,
        font: &Font,
        base_size: CSSPixelLength,
        flags: QueryFontMetricsFlags,
        track_usage: bool,
    ) -> FontMetrics {
        if track_usage {
            self.used_font_metrics.store(true, Ordering::Relaxed);
        }
        self.font_metrics_provider
            .query_font_metrics(vertical, font, base_size, flags)
    }

    fn ensure_root_font_metrics_updated(&self) {
        let mut guard = self.used_root_font_metrics.write();
        let previously_computed = mem::replace(&mut *guard, true);
        if !previously_computed {
            self.update_root_font_metrics();
        }
    }

    /// Compute the root element's font metrics, and returns a bool indicating whether
    /// the font metrics have changed since the previous restyle.
    pub fn update_root_font_metrics(&self) -> bool {
        let root_style = self.root_style.read();
        let root_effective_zoom = (*root_style).effective_zoom;
        let root_font_size = (*root_style).get_font().clone_font_size().computed_size();

        let root_font_metrics = self.query_font_metrics(
            (*root_style).writing_mode.is_upright(),
            &(*root_style).get_font(),
            root_font_size,
            QueryFontMetricsFlags::USE_USER_FONT_SET
                | QueryFontMetricsFlags::NEEDS_CH
                | QueryFontMetricsFlags::NEEDS_IC,
            /* track_usage = */ false,
        );

        let mut root_font_metrics_changed = false;
        root_font_metrics_changed |= self.set_root_font_metrics_ex(
            root_effective_zoom.unzoom(root_font_metrics.x_height_or_default(root_font_size).px()),
        );
        root_font_metrics_changed |= self.set_root_font_metrics_ch(
            root_effective_zoom.unzoom(
                root_font_metrics
                    .zero_advance_measure_or_default(
                        root_font_size,
                        (*root_style).writing_mode.is_upright(),
                    )
                    .px(),
            ),
        );
        root_font_metrics_changed |= self.set_root_font_metrics_cap(
            root_effective_zoom.unzoom(root_font_metrics.cap_height_or_default().px()),
        );
        root_font_metrics_changed |= self.set_root_font_metrics_ic(
            root_effective_zoom.unzoom(root_font_metrics.ic_width_or_default(root_font_size).px()),
        );

        root_font_metrics_changed
    }

    /// Return the media type of the current device.
    pub fn media_type(&self) -> MediaType {
        self.media_type.clone()
    }

    /// Returns whether document colors are enabled.
    pub fn forced_colors(&self) -> ForcedColors {
        ForcedColors::None
    }

    /// Returns the default background color.
    pub fn default_background_color(&self) -> AbsoluteColor {
        AbsoluteColor::WHITE
    }

    /// Returns the default foreground color.
    pub fn default_color(&self) -> AbsoluteColor {
        AbsoluteColor::BLACK
    }

    /// Set the [`PrefersColorScheme`] value on this [`Device`].
    ///
    /// Note that this does not update any associated `Stylist`. For this you must call
    /// `Stylist::media_features_change_changed_style` and
    /// `Stylist::force_stylesheet_origins_dirty`.
    pub fn set_color_scheme(&mut self, new_color_scheme: PrefersColorScheme) {
        self.prefers_color_scheme = new_color_scheme;
    }

    /// Returns the color scheme of this [`Device`].
    pub fn color_scheme(&self) -> PrefersColorScheme {
        self.prefers_color_scheme
    }

    pub(crate) fn is_dark_color_scheme(&self, _: ColorSchemeFlags) -> bool {
        false
    }

    /// Returns safe area insets
    pub fn safe_area_insets(&self) -> SideOffsets2D<f32, CSSPixel> {
        SideOffsets2D::zero()
    }

    /// Returns true if the given MIME type is supported
    pub fn is_supported_mime_type(&self, mime_type: &str) -> bool {
        match mime_type.parse::<Mime>() {
            Ok(m) => {
                // Keep this in sync with 'image_classifer' from
                // components/net/mime_classifier.rs
                m == mime::IMAGE_BMP
                    || m == mime::IMAGE_GIF
                    || m == mime::IMAGE_PNG
                    || m == mime::IMAGE_JPEG
                    || m == "image/x-icon"
                    || m == "image/webp"
            },
            _ => false,
        }
    }

    /// Return whether the document is a chrome document.
    #[inline]
    pub fn chrome_rules_enabled_for_document(&self) -> bool {
        false
    }
}

/// https://drafts.csswg.org/mediaqueries-4/#width
fn eval_width(context: &Context) -> CSSPixelLength {
    CSSPixelLength::new(context.device().au_viewport_size().width.to_f32_px())
}

#[derive(Clone, Copy, Debug, FromPrimitive, Parse, ToCss)]
#[repr(u8)]
enum Scan {
    Progressive,
    Interlace,
}

/// https://drafts.csswg.org/mediaqueries-4/#scan
fn eval_scan(_: &Context, _: Option<Scan>) -> bool {
    // Since we doesn't support the 'tv' media type, the 'scan' feature never
    // matches.
    false
}

/// https://drafts.csswg.org/mediaqueries-4/#resolution
fn eval_resolution(context: &Context) -> Resolution {
    Resolution::from_dppx(context.device().device_pixel_ratio.0)
}

/// https://compat.spec.whatwg.org/#css-media-queries-webkit-device-pixel-ratio
fn eval_device_pixel_ratio(context: &Context) -> f32 {
    eval_resolution(context).dppx()
}

fn eval_prefers_color_scheme(context: &Context, query_value: Option<PrefersColorScheme>) -> bool {
    match query_value {
        Some(v) => context.device().prefers_color_scheme == v,
        None => true,
    }
}

/// A list with all the media features that Servo supports.
pub static MEDIA_FEATURES: [QueryFeatureDescription; 6] = [
    feature!(
        atom!("width"),
        AllowsRanges::Yes,
        Evaluator::Length(eval_width),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("scan"),
        AllowsRanges::No,
        keyword_evaluator!(eval_scan, Scan),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("resolution"),
        AllowsRanges::Yes,
        Evaluator::Resolution(eval_resolution),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("device-pixel-ratio"),
        AllowsRanges::Yes,
        Evaluator::Float(eval_device_pixel_ratio),
        FeatureFlags::WEBKIT_PREFIX,
    ),
    feature!(
        atom!("-moz-device-pixel-ratio"),
        AllowsRanges::Yes,
        Evaluator::Float(eval_device_pixel_ratio),
        FeatureFlags::empty(),
    ),
    feature!(
        atom!("prefers-color-scheme"),
        AllowsRanges::No,
        keyword_evaluator!(eval_prefers_color_scheme, PrefersColorScheme),
        FeatureFlags::empty(),
    ),
];
