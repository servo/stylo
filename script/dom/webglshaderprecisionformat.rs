/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// https://www.khronos.org/registry/webgl/specs/latest/1.0/webgl.idl
use dom::bindings::codegen::Bindings::WebGLShaderPrecisionFormatBinding;
use dom::bindings::codegen::Bindings::WebGLShaderPrecisionFormatBinding::WebGLShaderPrecisionFormatMethods;
use dom::bindings::global::GlobalRef;
use dom::bindings::js::{Temporary, JSRef};
use dom::bindings::utils::{Reflector,reflect_dom_object};

#[dom_struct]
pub struct WebGLShaderPrecisionFormat {
    reflector_: Reflector,
    range_min: i32,
    range_max: i32,
    precision: i32,
}

impl WebGLShaderPrecisionFormat {
    fn new_inherited(range_min: i32, range_max: i32, precision: i32) -> WebGLShaderPrecisionFormat {
        WebGLShaderPrecisionFormat {
            reflector_: Reflector::new(),
            range_min: range_min,
            range_max: range_max,
            precision: precision,
        }
    }

    pub fn new(global: GlobalRef,
               range_min: i32,
               range_max: i32,
               precision: i32) -> Temporary<WebGLShaderPrecisionFormat> {
        reflect_dom_object(
            box WebGLShaderPrecisionFormat::new_inherited(range_min, range_max, precision),
            global,
            WebGLShaderPrecisionFormatBinding::Wrap)
    }
}

impl<'a> WebGLShaderPrecisionFormatMethods for JSRef<'a, WebGLShaderPrecisionFormat> {
    fn RangeMin(self) -> i32 {
        self.range_min
    }

    fn RangeMax(self) -> i32 {
        self.range_max
    }

    fn Precision(self) -> i32 {
        self.precision
    }
}
