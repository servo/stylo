/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! CSS transitions and animations.

// NOTE(emilio): This code isn't really executed in Gecko, but we don't want to
// compile it out so that people remember it exists.

use crate::bezier::Bezier;
use crate::context::SharedStyleContext;
use crate::dom::{OpaqueNode, TElement, TNode};
use crate::font_metrics::FontMetricsProvider;
use crate::properties::animated_properties::AnimatedProperty;
use crate::properties::longhands::animation_direction::computed_value::single_value::T as AnimationDirection;
use crate::properties::longhands::animation_play_state::computed_value::single_value::T as AnimationPlayState;
use crate::properties::{self, CascadeMode, ComputedValues, LonghandId};
#[cfg(feature = "servo")]
use crate::properties::LonghandIdSet;
use crate::stylesheets::keyframes_rule::{KeyframesAnimation, KeyframesStep, KeyframesStepValue};
use crate::stylesheets::Origin;
use crate::values::computed::Time;
use crate::values::computed::TimingFunction;
use crate::values::generics::box_::AnimationIterationCount;
use crate::values::generics::easing::{StepPosition, TimingFunction as GenericTimingFunction};
use crate::Atom;
use servo_arc::Arc;
use std::fmt;

/// This structure represents a keyframes animation current iteration state.
///
/// If the iteration count is infinite, there's no other state, otherwise we
/// have to keep track the current iteration and the max iteration count.
#[derive(Clone, Debug)]
pub enum KeyframesIterationState {
    /// Infinite iterations, so no need to track a state.
    Infinite,
    /// Current and max iterations.
    Finite(f32, f32),
}

/// This structure represents wether an animation is actually running.
///
/// An animation can be running, or paused at a given time.
#[derive(Clone, Debug)]
pub enum KeyframesRunningState {
    /// This animation is paused. The inner field is the percentage of progress
    /// when it was paused, from 0 to 1.
    Paused(f64),
    /// This animation is actually running.
    Running,
}

/// This structure represents the current keyframe animation state, i.e., the
/// duration, the current and maximum iteration count, and the state (either
/// playing or paused).
// TODO: unify the use of f32/f64 in this file.
#[derive(Clone)]
pub struct KeyframesAnimationState {
    /// The time this animation started at.
    pub started_at: f64,
    /// The duration of this animation.
    pub duration: f64,
    /// The delay of the animation.
    pub delay: f64,
    /// The current iteration state for the animation.
    pub iteration_state: KeyframesIterationState,
    /// Werther this animation is paused.
    pub running_state: KeyframesRunningState,
    /// The declared animation direction of this animation.
    pub direction: AnimationDirection,
    /// The current animation direction. This can only be `normal` or `reverse`.
    pub current_direction: AnimationDirection,
    /// The original cascade style, needed to compute the generated keyframes of
    /// the animation.
    pub cascade_style: Arc<ComputedValues>,
}

impl KeyframesAnimationState {
    /// Given the current time, advances this animation to the next iteration,
    /// updates times, and then toggles the direction if appropriate. Otherwise
    /// does nothing.
    pub fn iterate_if_necessary(&mut self, time: f64) {
        if !self.iteration_over(time) {
            return;
        }

        match self.running_state {
            KeyframesRunningState::Paused(_) => return,
            KeyframesRunningState::Running => {},
        }

        if let KeyframesIterationState::Finite(ref mut current, max) = self.iteration_state {
            // If we are already on the final iteration, just exit now.
            // NB: This prevent us from updating the direction, which might be
            // needed for the correct handling of animation-fill-mode.
            if (max - *current) <= 1.0 {
                return;
            }

            *current += 1.0;
        }

        // Update the next iteration direction if applicable.
        // TODO(mrobinson): The duration might now be wrong for floating point iteration counts.
        self.started_at += self.duration + self.delay;
        match self.direction {
            AnimationDirection::Alternate | AnimationDirection::AlternateReverse => {
                self.current_direction = match self.current_direction {
                    AnimationDirection::Normal => AnimationDirection::Reverse,
                    AnimationDirection::Reverse => AnimationDirection::Normal,
                    _ => unreachable!(),
                };
            },
            _ => {},
        }
    }

    fn iteration_over(&self, time: f64) -> bool {
        time > (self.started_at + self.duration)
    }

    fn has_ended(&self, time: f64) -> bool {
        if !self.iteration_over(time) {
            return false;
        }

        // If we are paused then we haven't ended.
        match self.running_state {
            KeyframesRunningState::Paused(_) => return false,
            KeyframesRunningState::Running => {},
        }

        // If we have a limited number of iterations and we cannot advance to another
        // iteration, then we have ended.
        return match self.iteration_state {
            KeyframesIterationState::Finite(current, max) if (max - current) <= 1.0 => true,
            KeyframesIterationState::Finite(..) | KeyframesIterationState::Infinite => false,
        };
    }

    /// Updates the appropiate state from other animation.
    ///
    /// This happens when an animation is re-submitted to layout, presumably
    /// because of an state change.
    ///
    /// There are some bits of state we can't just replace, over all taking in
    /// account times, so here's that logic.
    pub fn update_from_other(&mut self, other: &Self, now: f64) {
        use self::KeyframesRunningState::*;

        debug!(
            "KeyframesAnimationState::update_from_other({:?}, {:?})",
            self, other
        );

        // NB: We shall not touch the started_at field, since we don't want to
        // restart the animation.
        let old_started_at = self.started_at;
        let old_duration = self.duration;
        let old_direction = self.current_direction;
        let old_running_state = self.running_state.clone();
        let old_iteration_state = self.iteration_state.clone();
        *self = other.clone();

        let mut new_started_at = old_started_at;

        // If we're unpausing the animation, fake the start time so we seem to
        // restore it.
        //
        // If the animation keeps paused, keep the old value.
        //
        // If we're pausing the animation, compute the progress value.
        match (&mut self.running_state, old_running_state) {
            (&mut Running, Paused(progress)) => new_started_at = now - (self.duration * progress),
            (&mut Paused(ref mut new), Paused(old)) => *new = old,
            (&mut Paused(ref mut progress), Running) => {
                *progress = (now - old_started_at) / old_duration
            },
            _ => {},
        }

        // Don't update the iteration count, just the iteration limit.
        // TODO: see how changing the limit affects rendering in other browsers.
        // We might need to keep the iteration count even when it's infinite.
        match (&mut self.iteration_state, old_iteration_state) {
            (
                &mut KeyframesIterationState::Finite(ref mut iters, _),
                KeyframesIterationState::Finite(old_iters, _),
            ) => *iters = old_iters,
            _ => {},
        }

        self.current_direction = old_direction;
        self.started_at = new_started_at;
    }

    /// Calculate the active-duration of this animation according to
    /// https://drafts.csswg.org/css-animations/#active-duration. active-duration
    /// is not really meaningful for infinite animations so we just return 0
    /// here in that case.
    pub fn active_duration(&self) -> f64 {
        match self.iteration_state {
            KeyframesIterationState::Finite(_, max) => self.duration * (max as f64),
            KeyframesIterationState::Infinite => 0.,
        }
    }
}

impl fmt::Debug for KeyframesAnimationState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("KeyframesAnimationState")
            .field("started_at", &self.started_at)
            .field("duration", &self.duration)
            .field("delay", &self.delay)
            .field("iteration_state", &self.iteration_state)
            .field("running_state", &self.running_state)
            .field("direction", &self.direction)
            .field("current_direction", &self.current_direction)
            .field("cascade_style", &())
            .finish()
    }
}

/// State relating to an animation.
#[derive(Clone, Debug)]
pub enum Animation {
    /// A transition is just a single frame triggered at a time, with a reflow.
    ///
    /// the f64 field is the start time as returned by `time::precise_time_s()`.
    Transition(OpaqueNode, f64, PropertyAnimation),

    /// A keyframes animation is identified by a name, and can have a
    /// node-dependent state (i.e. iteration count, etc.).
    ///
    /// TODO(emilio): The animation object could be refcounted.
    Keyframes(
        OpaqueNode,
        KeyframesAnimation,
        Atom,
        KeyframesAnimationState,
    ),
}

impl Animation {
    /// The opaque node that owns the animation.
    #[inline]
    pub fn node(&self) -> &OpaqueNode {
        match *self {
            Animation::Transition(ref node, _, _) => node,
            Animation::Keyframes(ref node, _, _, _) => node,
        }
    }

    /// Whether this animation is a transition.
    #[inline]
    pub fn is_transition(&self) -> bool {
        match *self {
            Animation::Transition(..) => true,
            Animation::Keyframes(..) => false,
        }
    }

    /// Whether or not this animation has ended at the provided time. This does
    /// not take into account canceling i.e. when an animation or transition is
    /// canceled due to changes in the style.
    pub fn has_ended(&self, time: f64) -> bool {
        match *self {
            Animation::Transition(_, started_at, ref property_animation) => {
                time >= started_at + (property_animation.duration)
            },
            Animation::Keyframes(_, _, _, ref state) => state.has_ended(time),
        }
    }

    /// Whether this animation has the same end value as another one.
    #[inline]
    fn is_transition_with_same_end_value(&self, other_animation: &PropertyAnimation) -> bool {
        match *self {
            Animation::Transition(_, _, ref animation) => {
                animation.has_the_same_end_value_as(other_animation)
            },
            Animation::Keyframes(..) => false,
        }
    }

    /// Whether or not this animation is cancelled by changes from a new style.
    fn is_animation_cancelled_in_new_style(&self, new_style: &Arc<ComputedValues>) -> bool {
        let name = match *self {
            Animation::Transition(..) => return false,
            Animation::Keyframes(_, _, ref name, _) => name,
        };

        let index = new_style
            .get_box()
            .animation_name_iter()
            .position(|animation_name| Some(name) == animation_name.as_atom());
        let index = match index {
            Some(index) => index,
            None => return true,
        };

        new_style.get_box().animation_duration_mod(index).seconds() == 0.
    }
}

/// Represents an animation for a given property.
#[derive(Clone, Debug)]
pub struct PropertyAnimation {
    /// An `AnimatedProperty` that this `PropertyAnimation` corresponds to.
    property: AnimatedProperty,

    /// The timing function of this `PropertyAnimation`.
    timing_function: TimingFunction,

    /// The duration of this `PropertyAnimation` in seconds.
    pub duration: f64,
}

impl PropertyAnimation {
    /// Returns the given property longhand id.
    pub fn property_id(&self) -> LonghandId {
        self.property.id()
    }

    /// Returns the given property name.
    pub fn property_name(&self) -> &'static str {
        self.property.name()
    }

    fn from_longhand(
        longhand: LonghandId,
        timing_function: TimingFunction,
        duration: Time,
        old_style: &ComputedValues,
        new_style: &ComputedValues,
    ) -> Option<PropertyAnimation> {
        let animated_property = AnimatedProperty::from_longhand(longhand, old_style, new_style)?;

        let property_animation = PropertyAnimation {
            property: animated_property,
            timing_function,
            duration: duration.seconds() as f64,
        };

        if property_animation.does_animate() {
            Some(property_animation)
        } else {
            None
        }
    }

    /// Update the given animation at a given point of progress.
    pub fn update(&self, style: &mut ComputedValues, time: f64) {
        let epsilon = 1. / (200. * self.duration);
        let progress = match self.timing_function {
            GenericTimingFunction::CubicBezier { x1, y1, x2, y2 } => {
                Bezier::new(x1, y1, x2, y2).solve(time, epsilon)
            },
            GenericTimingFunction::Steps(steps, pos) => {
                let mut current_step = (time * (steps as f64)).floor() as i32;

                if pos == StepPosition::Start ||
                    pos == StepPosition::JumpStart ||
                    pos == StepPosition::JumpBoth
                {
                    current_step = current_step + 1;
                }

                // FIXME: We should update current_step according to the "before flag".
                // In order to get the before flag, we have to know the current animation phase
                // and whether the iteration is reversed. For now, we skip this calculation.
                // (i.e. Treat before_flag is unset,)
                // https://drafts.csswg.org/css-easing/#step-timing-function-algo

                if time >= 0.0 && current_step < 0 {
                    current_step = 0;
                }

                let jumps = match pos {
                    StepPosition::JumpBoth => steps + 1,
                    StepPosition::JumpNone => steps - 1,
                    StepPosition::JumpStart |
                    StepPosition::JumpEnd |
                    StepPosition::Start |
                    StepPosition::End => steps,
                };

                if time <= 1.0 && current_step > jumps {
                    current_step = jumps;
                }

                (current_step as f64) / (jumps as f64)
            },
            GenericTimingFunction::Keyword(keyword) => {
                let (x1, x2, y1, y2) = keyword.to_bezier();
                Bezier::new(x1, x2, y1, y2).solve(time, epsilon)
            },
        };

        self.property.update(style, progress);
    }

    #[inline]
    fn does_animate(&self) -> bool {
        self.property.does_animate() && self.duration != 0.0
    }

    /// Whether this animation has the same end value as another one.
    #[inline]
    pub fn has_the_same_end_value_as(&self, other: &Self) -> bool {
        self.property.has_the_same_end_value_as(&other.property)
    }
}

/// Holds the animation state for a particular element.
#[derive(Default)]
pub struct ElementAnimationState {
    /// The animations running for this element.
    pub running_animations: Vec<Animation>,

    /// The animations that have finished for this element, but not canceled. These are cleared
    /// upon triggering a DOM event for each finished animation.
    pub finished_animations: Vec<Animation>,

    /// The animations that have been cancelled for this element. These are cleared
    /// upon triggering a DOM event for each cancelled animation.
    pub cancelled_animations: Vec<Animation>,

    /// New animations created for this element.
    pub new_animations: Vec<Animation>,
}

impl ElementAnimationState {
    /// Cancel all animations in this `ElementAnimationState`. This is typically called
    /// when the element has been removed from the DOM.
    pub fn cancel_all_animations(&mut self) {
        self.cancelled_animations.extend(
            self.finished_animations
                .drain(..)
                .chain(self.running_animations.drain(..))
                .chain(self.new_animations.drain(..)),
        );
    }

    pub(crate) fn cancel_transitions_with_nontransitioning_properties(
        &mut self,
        properties_that_transition: &LonghandIdSet,
    ) {
        if self.running_animations.is_empty() {
            return;
        }

        // TODO(mrobinson): We should make this more efficient perhaps by using
        // a linked-list or by using something like `partition`.
        let animation_count = self.running_animations.len();
        let mut previously_running_animations = std::mem::replace(
            &mut self.running_animations,
            Vec::with_capacity(animation_count),
        );
        for running_animation in previously_running_animations.drain(..) {
            if let Animation::Transition(_, _, ref property_animation) = running_animation {
                if !properties_that_transition.contains(property_animation.property_id()) {
                    self.cancelled_animations.push(running_animation);
                    continue;
                }
            }
            self.running_animations.push(running_animation);
        }
    }

    fn has_transition_with_same_end_value(&self, property_animation: &PropertyAnimation) -> bool {
        if self
            .running_animations
            .iter()
            .any(|animation| animation.is_transition_with_same_end_value(&property_animation))
        {
            debug!(
                "Running transition found with the same end value for {:?}",
                property_animation,
            );
            return true;
        }

        if self
            .finished_animations
            .iter()
            .any(|animation| animation.is_transition_with_same_end_value(&property_animation))
        {
            debug!(
                "Expired transition found with the same end value for {:?}",
                property_animation,
            );
            return true;
        }

        false
    }

    pub(crate) fn apply_new_and_running_animations<E>(
        &mut self,
        context: &SharedStyleContext,
        style: &mut Arc<ComputedValues>,
        font_metrics: &dyn crate::font_metrics::FontMetricsProvider,
    ) where
        E: TElement,
    {
        if !self.running_animations.is_empty() {
            let style = Arc::make_mut(style);
            for animation in self.running_animations.iter_mut() {
                update_style_for_animation::<E>(context, animation, style, font_metrics);
            }
        }

        if !self.new_animations.is_empty() {
            let style = Arc::make_mut(style);
            for animation in self.new_animations.iter_mut() {
                update_style_for_animation::<E>(context, animation, style, font_metrics);
            }
        }
    }

    /// Whether this `ElementAnimationState` is empty, which means it doesn't
    /// hold any animations in any state.
    pub fn is_empty(&self) -> bool {
        self.running_animations.is_empty() &&
            self.finished_animations.is_empty() &&
            self.cancelled_animations.is_empty() &&
            self.new_animations.is_empty()
    }

    fn add_new_animation(&mut self, animation: Animation) {
        self.new_animations.push(animation);
    }

    /// Update our animations given a new style, canceling or starting new animations
    /// when appropriate.
    pub fn update_animations_for_new_style<E>(
        &mut self,
        element: E,
        context: &SharedStyleContext,
        new_style: &Arc<ComputedValues>,
    ) where
        E: TElement,
    {
        // Cancel any animations that no longer exist in the style.
        // TODO(mrobinson): We should make this more efficient perhaps by using
        // a linked-list or by using something like `partition`.
        if !self.running_animations.is_empty() {
            let animation_count = self.running_animations.len();
            let mut previously_running_animations = std::mem::replace(
                &mut self.running_animations,
                Vec::with_capacity(animation_count),
            );
            for animation in previously_running_animations.drain(..) {
                if animation.is_animation_cancelled_in_new_style(new_style) {
                    self.cancelled_animations.push(animation);
                } else {
                    self.running_animations.push(animation);
                }
            }
        }

        maybe_start_animations(element, &context, &new_style, self);

        self.iterate_running_animations_if_necessary(context.current_time_for_animations);
    }

    /// Update our transitions given a new style, canceling or starting new animations
    /// when appropriate.
    pub fn update_transitions_for_new_style<E>(
        &mut self,
        context: &SharedStyleContext,
        opaque_node: OpaqueNode,
        old_style: Option<&Arc<ComputedValues>>,
        after_change_style: &Arc<ComputedValues>,
        font_metrics: &dyn crate::font_metrics::FontMetricsProvider,
    ) where
        E: TElement,
    {
        // If this is the first style, we don't trigger any transitions and we assume
        // there were no previously triggered transitions.
        let mut before_change_style = match old_style {
            Some(old_style) => Arc::clone(old_style),
            None => return,
        };

        // We convert old values into `before-change-style` here.
        // See https://drafts.csswg.org/css-transitions/#starting. We need to clone the
        // style because this might still be a reference to the original `old_style` and
        // we want to preserve that so that we can later properly calculate restyle damage.
        if self.running_animations.is_empty() || self.new_animations.is_empty() {
            before_change_style = before_change_style.clone();
            self.apply_new_and_running_animations::<E>(
                context,
                &mut before_change_style,
                font_metrics,
            );
        }

        let transitioning_properties = start_transitions_if_applicable(
            context,
            opaque_node,
            &before_change_style,
            after_change_style,
            self,
        );
        self.cancel_transitions_with_nontransitioning_properties(&transitioning_properties);
    }

    /// When necessary, iterate our running animations to the next iteration.
    pub fn iterate_running_animations_if_necessary(&mut self, time: f64) {
        for animation in self.running_animations.iter_mut() {
            match *animation {
                Animation::Keyframes(_, _, _, ref mut state) => state.iterate_if_necessary(time),
                _ => {},
            }
        }
    }
}

/// Kick off any new transitions for this node and return all of the properties that are
/// transitioning. This is at the end of calculating style for a single node.
pub fn start_transitions_if_applicable(
    context: &SharedStyleContext,
    opaque_node: OpaqueNode,
    old_style: &ComputedValues,
    new_style: &Arc<ComputedValues>,
    animation_state: &mut ElementAnimationState,
) -> LonghandIdSet {
    // If the style of this element is display:none, then we don't start any transitions
    // and we cancel any currently running transitions by returning an empty LonghandIdSet.
    if new_style.get_box().clone_display().is_none() {
        return LonghandIdSet::new();
    }

    let mut properties_that_transition = LonghandIdSet::new();
    for transition in new_style.transition_properties() {
        let physical_property = transition.longhand_id.to_physical(new_style.writing_mode);
        if properties_that_transition.contains(physical_property) {
            continue;
        } else {
            properties_that_transition.insert(physical_property);
        }

        let property_animation = match PropertyAnimation::from_longhand(
            transition.longhand_id,
            new_style
                .get_box()
                .transition_timing_function_mod(transition.index),
            new_style
                .get_box()
                .transition_duration_mod(transition.index),
            old_style,
            new_style,
        ) {
            Some(property_animation) => property_animation,
            None => continue,
        };

        // Per [1], don't trigger a new transition if the end state for that
        // transition is the same as that of a transition that's running or
        // completed.
        // [1]: https://drafts.csswg.org/css-transitions/#starting
        if animation_state.has_transition_with_same_end_value(&property_animation) {
            continue;
        }

        // Kick off the animation.
        debug!("Kicking off transition of {:?}", property_animation);
        let box_style = new_style.get_box();
        let start_time = context.current_time_for_animations +
            (box_style.transition_delay_mod(transition.index).seconds() as f64);
        animation_state.add_new_animation(Animation::Transition(
            opaque_node,
            start_time,
            property_animation,
        ));
    }

    properties_that_transition
}

fn compute_style_for_animation_step<E>(
    context: &SharedStyleContext,
    step: &KeyframesStep,
    previous_style: &ComputedValues,
    style_from_cascade: &Arc<ComputedValues>,
    font_metrics_provider: &dyn FontMetricsProvider,
) -> Arc<ComputedValues>
where
    E: TElement,
{
    match step.value {
        KeyframesStepValue::ComputedValues => style_from_cascade.clone(),
        KeyframesStepValue::Declarations {
            block: ref declarations,
        } => {
            let guard = declarations.read_with(context.guards.author);

            // This currently ignores visited styles, which seems acceptable,
            // as existing browsers don't appear to animate visited styles.
            let computed = properties::apply_declarations::<E, _>(
                context.stylist.device(),
                /* pseudo = */ None,
                previous_style.rules(),
                &context.guards,
                // It's possible to have !important properties in keyframes
                // so we have to filter them out.
                // See the spec issue https://github.com/w3c/csswg-drafts/issues/1824
                // Also we filter our non-animatable properties.
                guard
                    .normal_declaration_iter()
                    .filter(|declaration| declaration.is_animatable())
                    .map(|decl| (decl, Origin::Author)),
                Some(previous_style),
                Some(previous_style),
                Some(previous_style),
                font_metrics_provider,
                CascadeMode::Unvisited {
                    visited_rules: None,
                },
                context.quirks_mode(),
                /* rule_cache = */ None,
                &mut Default::default(),
                /* element = */ None,
            );
            computed
        },
    }
}

/// Triggers animations for a given node looking at the animation property
/// values.
pub fn maybe_start_animations<E>(
    element: E,
    context: &SharedStyleContext,
    new_style: &Arc<ComputedValues>,
    animation_state: &mut ElementAnimationState,
) where
    E: TElement,
{
    let box_style = new_style.get_box();
    for (i, name) in box_style.animation_name_iter().enumerate() {
        let name = match name.as_atom() {
            Some(atom) => atom,
            None => continue,
        };

        debug!("maybe_start_animations: name={}", name);
        let duration = box_style.animation_duration_mod(i).seconds();
        if duration == 0. {
            continue;
        }

        let anim = match context.stylist.get_animation(name, element) {
            Some(animation) => animation,
            None => continue,
        };

        debug!("maybe_start_animations: animation {} found", name);

        // If this animation doesn't have any keyframe, we can just continue
        // without submitting it to the compositor, since both the first and
        // the second keyframes would be synthetised from the computed
        // values.
        if anim.steps.is_empty() {
            continue;
        }

        let delay = box_style.animation_delay_mod(i).seconds();
        let animation_start = context.current_time_for_animations + delay as f64;
        let iteration_state = match box_style.animation_iteration_count_mod(i) {
            AnimationIterationCount::Infinite => KeyframesIterationState::Infinite,
            AnimationIterationCount::Number(n) => KeyframesIterationState::Finite(0.0, n),
        };

        let animation_direction = box_style.animation_direction_mod(i);

        let initial_direction = match animation_direction {
            AnimationDirection::Normal | AnimationDirection::Alternate => {
                AnimationDirection::Normal
            },
            AnimationDirection::Reverse | AnimationDirection::AlternateReverse => {
                AnimationDirection::Reverse
            },
        };

        let running_state = match box_style.animation_play_state_mod(i) {
            AnimationPlayState::Paused => KeyframesRunningState::Paused(0.),
            AnimationPlayState::Running => KeyframesRunningState::Running,
        };

        let new_state = KeyframesAnimationState {
            started_at: animation_start,
            duration: duration as f64,
            delay: delay as f64,
            iteration_state,
            running_state,
            direction: animation_direction,
            current_direction: initial_direction,
            cascade_style: new_style.clone(),
        };

        // If the animation was already present in the list for the node, just update its state.
        for existing_animation in animation_state.running_animations.iter_mut() {
            match existing_animation {
                Animation::Keyframes(_, _, ref old_name, ref mut old_state)
                    if *old_name == *name =>
                {
                    old_state.update_from_other(&new_state, context.current_time_for_animations);
                    return;
                }
                _ => {},
            }
        }

        animation_state.add_new_animation(Animation::Keyframes(
            element.as_node().opaque(),
            anim.clone(),
            name.clone(),
            new_state,
        ));
    }
}

/// Updates a single animation and associated style based on the current time.
pub fn update_style_for_animation<E>(
    context: &SharedStyleContext,
    animation: &Animation,
    style: &mut ComputedValues,
    font_metrics_provider: &dyn FontMetricsProvider,
) where
    E: TElement,
{
    debug!("update_style_for_animation: {:?}", animation);
    match *animation {
        Animation::Transition(_, start_time, ref property_animation) => {
            let now = context.current_time_for_animations;
            let progress = (now - start_time) / (property_animation.duration);
            let progress = progress.min(1.0);
            if progress >= 0.0 {
                property_animation.update(style, progress);
            }
        },
        Animation::Keyframes(_, ref animation, ref name, ref state) => {
            let duration = state.duration;
            let started_at = state.started_at;

            let now = match state.running_state {
                KeyframesRunningState::Running => context.current_time_for_animations,
                KeyframesRunningState::Paused(progress) => started_at + duration * progress,
            };

            debug_assert!(!animation.steps.is_empty());
            let mut total_progress = (now - started_at) / duration;
            if total_progress < 0. {
                warn!("Negative progress found for animation {:?}", name);
                return;
            }
            if total_progress > 1. {
                total_progress = 1.;
            }

            // Get the target and the last keyframe position.
            let last_keyframe_position;
            let target_keyframe_position;
            match state.current_direction {
                AnimationDirection::Normal => {
                    target_keyframe_position = animation
                        .steps
                        .iter()
                        .position(|step| total_progress as f32 <= step.start_percentage.0);

                    last_keyframe_position = target_keyframe_position
                        .and_then(|pos| if pos != 0 { Some(pos - 1) } else { None })
                        .unwrap_or(0);
                },
                AnimationDirection::Reverse => {
                    target_keyframe_position = animation
                        .steps
                        .iter()
                        .rev()
                        .position(|step| total_progress as f32 <= 1. - step.start_percentage.0)
                        .map(|pos| animation.steps.len() - pos - 1);

                    last_keyframe_position = target_keyframe_position
                        .and_then(|pos| {
                            if pos != animation.steps.len() - 1 {
                                Some(pos + 1)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(animation.steps.len() - 1);
                },
                _ => unreachable!(),
            }

            debug!(
                "update_style_for_animation: keyframe from {:?} to {:?}",
                last_keyframe_position, target_keyframe_position
            );

            let target_keyframe = match target_keyframe_position {
                Some(target) => &animation.steps[target],
                None => return,
            };

            let last_keyframe = &animation.steps[last_keyframe_position];

            let relative_timespan =
                (target_keyframe.start_percentage.0 - last_keyframe.start_percentage.0).abs();
            let relative_duration = relative_timespan as f64 * duration;
            let last_keyframe_ended_at = match state.current_direction {
                AnimationDirection::Normal => {
                    state.started_at + (duration * last_keyframe.start_percentage.0 as f64)
                },
                AnimationDirection::Reverse => {
                    state.started_at + (duration * (1. - last_keyframe.start_percentage.0 as f64))
                },
                _ => unreachable!(),
            };
            let relative_progress = (now - last_keyframe_ended_at) / relative_duration;

            // TODO: How could we optimise it? Is it such a big deal?
            let from_style = compute_style_for_animation_step::<E>(
                context,
                last_keyframe,
                style,
                &state.cascade_style,
                font_metrics_provider,
            );

            // NB: The spec says that the timing function can be overwritten
            // from the keyframe style.
            let timing_function = if last_keyframe.declared_timing_function {
                // NB: animation_timing_function can never be empty, always has
                // at least the default value (`ease`).
                from_style.get_box().animation_timing_function_at(0)
            } else {
                // TODO(mrobinson): It isn't optimal to have to walk this list every
                // time. Perhaps this should be stored in the animation.
                let index = match style
                    .get_box()
                    .animation_name_iter()
                    .position(|animation_name| Some(name) == animation_name.as_atom())
                {
                    Some(index) => index,
                    None => return warn!("Tried to update a style with a cancelled animation."),
                };
                style.get_box().animation_timing_function_mod(index)
            };

            let target_style = compute_style_for_animation_step::<E>(
                context,
                target_keyframe,
                &from_style,
                &state.cascade_style,
                font_metrics_provider,
            );

            let mut new_style = (*style).clone();

            for property in animation.properties_changed.iter() {
                debug!(
                    "update_style_for_animation: scanning prop {:?} for animation \"{}\"",
                    property, name
                );
                let animation = PropertyAnimation::from_longhand(
                    property,
                    timing_function,
                    Time::from_seconds(relative_duration as f32),
                    &from_style,
                    &target_style,
                );

                match animation {
                    Some(property_animation) => {
                        debug!(
                            "update_style_for_animation: got property animation for prop {:?}",
                            property
                        );
                        debug!("update_style_for_animation: {:?}", property_animation);
                        property_animation.update(&mut new_style, relative_progress);
                    },
                    None => {
                        debug!(
                            "update_style_for_animation: property animation {:?} not animating",
                            property
                        );
                    },
                }
            }

            debug!(
                "update_style_for_animation: got style change in animation \"{}\"",
                name
            );
            *style = new_style;
        },
    }
}
