/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use gecko_bindings::structs::{nsTimingFunction, nsTimingFunction_Type};
use std::mem;
use values::computed::ToComputedValue;
use values::computed::easing::TimingFunction as ComputedTimingFunction;
use values::generics::easing::{StepPosition, TimingKeyword};
use values::generics::easing::TimingFunction as GenericTimingFunction;
use values::specified::easing::TimingFunction;

impl nsTimingFunction {
    fn set_as_step(&mut self, function_type: nsTimingFunction_Type, steps: u32) {
        debug_assert!(
            function_type == nsTimingFunction_Type::StepStart ||
                function_type == nsTimingFunction_Type::StepEnd,
            "function_type should be step-start or step-end"
        );
        self.mType = function_type;
        unsafe {
            self.__bindgen_anon_1
                .__bindgen_anon_1
                .as_mut()
                .mSteps = steps;
        }
    }

    fn set_as_bezier(
        &mut self,
        function_type: nsTimingFunction_Type,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
    ) {
        self.mType = function_type;
        unsafe {
            let ref mut gecko_cubic_bezier = self.__bindgen_anon_1.mFunc.as_mut();
            gecko_cubic_bezier.mX1 = x1;
            gecko_cubic_bezier.mY1 = y1;
            gecko_cubic_bezier.mX2 = x2;
            gecko_cubic_bezier.mY2 = y2;
        }
    }
}

impl From<ComputedTimingFunction> for nsTimingFunction {
    fn from(function: ComputedTimingFunction) -> nsTimingFunction {
        TimingFunction::from_computed_value(&function).into()
    }
}

impl From<TimingFunction> for nsTimingFunction {
    fn from(function: TimingFunction) -> nsTimingFunction {
        let mut tf: nsTimingFunction = unsafe { mem::zeroed() };

        match function {
            GenericTimingFunction::Steps(steps, StepPosition::Start) => {
                debug_assert!(steps.value() >= 0);
                tf.set_as_step(nsTimingFunction_Type::StepStart, steps.value() as u32);
            },
            GenericTimingFunction::Steps(steps, StepPosition::End) => {
                debug_assert!(steps.value() >= 0);
                tf.set_as_step(nsTimingFunction_Type::StepEnd, steps.value() as u32);
            },
            GenericTimingFunction::CubicBezier { x1, y1, x2, y2 } => {
                tf.set_as_bezier(
                    nsTimingFunction_Type::CubicBezier,
                    x1.get(),
                    y1.get(),
                    x2.get(),
                    y2.get(),
                );
            },
            GenericTimingFunction::Keyword(keyword) => {
                let (x1, y1, x2, y2) = keyword.to_bezier();
                tf.set_as_bezier(keyword.into(), x1, y1, x2, y2);
            },
        }
        tf
    }
}

impl From<nsTimingFunction> for ComputedTimingFunction {
    fn from(function: nsTimingFunction) -> ComputedTimingFunction {
        match function.mType {
            nsTimingFunction_Type::StepStart => GenericTimingFunction::Steps(
                unsafe {
                    function
                        .__bindgen_anon_1
                        .__bindgen_anon_1
                        .as_ref()
                        .mSteps
                },
                StepPosition::Start,
            ),
            nsTimingFunction_Type::StepEnd => GenericTimingFunction::Steps(
                unsafe {
                    function
                        .__bindgen_anon_1
                        .__bindgen_anon_1
                        .as_ref()
                        .mSteps
                },
                StepPosition::End,
            ),
            nsTimingFunction_Type::Ease => GenericTimingFunction::Keyword(TimingKeyword::Ease),
            nsTimingFunction_Type::Linear => GenericTimingFunction::Keyword(TimingKeyword::Linear),
            nsTimingFunction_Type::EaseIn => GenericTimingFunction::Keyword(TimingKeyword::EaseIn),
            nsTimingFunction_Type::EaseOut => {
                GenericTimingFunction::Keyword(TimingKeyword::EaseOut)
            },
            nsTimingFunction_Type::EaseInOut => {
                GenericTimingFunction::Keyword(TimingKeyword::EaseInOut)
            },
            nsTimingFunction_Type::CubicBezier => unsafe {
                GenericTimingFunction::CubicBezier {
                    x1: function.__bindgen_anon_1.mFunc.as_ref().mX1,
                    y1: function.__bindgen_anon_1.mFunc.as_ref().mY1,
                    x2: function.__bindgen_anon_1.mFunc.as_ref().mX2,
                    y2: function.__bindgen_anon_1.mFunc.as_ref().mY2,
                }
            },
        }
    }
}

impl From<TimingKeyword> for nsTimingFunction_Type {
    fn from(keyword: TimingKeyword) -> Self {
        match keyword {
            TimingKeyword::Linear => nsTimingFunction_Type::Linear,
            TimingKeyword::Ease => nsTimingFunction_Type::Ease,
            TimingKeyword::EaseIn => nsTimingFunction_Type::EaseIn,
            TimingKeyword::EaseOut => nsTimingFunction_Type::EaseOut,
            TimingKeyword::EaseInOut => nsTimingFunction_Type::EaseInOut,
        }
    }
}
