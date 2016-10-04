/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use dom::attr::Attr;
use dom::bindings::cell::DOMRefCell;
use dom::bindings::codegen::Bindings::HTMLMetaElementBinding;
use dom::bindings::codegen::Bindings::HTMLMetaElementBinding::HTMLMetaElementMethods;
use dom::bindings::codegen::Bindings::NodeBinding::NodeMethods;
use dom::bindings::inheritance::Castable;
use dom::bindings::js::{Root, RootedReference};
use dom::bindings::str::DOMString;
use dom::document::Document;
use dom::element::{AttributeMutation, Element};
use dom::htmlelement::HTMLElement;
use dom::htmlheadelement::HTMLHeadElement;
use dom::node::{Node, UnbindContext, document_from_node};
use dom::virtualmethods::VirtualMethods;
use std::ascii::AsciiExt;
use std::sync::Arc;
use string_cache::Atom;
use style::attr::AttrValue;
use style::str::HTML_SPACE_CHARACTERS;
use style::stylesheets::{Stylesheet, CSSRule, Origin};
use style::viewport::ViewportRule;

#[dom_struct]
pub struct HTMLMetaElement {
    htmlelement: HTMLElement,
    #[ignore_heap_size_of = "Arc"]
    stylesheet: DOMRefCell<Option<Arc<Stylesheet>>>,
}

impl HTMLMetaElement {
    fn new_inherited(local_name: Atom,
                     prefix: Option<DOMString>,
                     document: &Document) -> HTMLMetaElement {
        HTMLMetaElement {
            htmlelement: HTMLElement::new_inherited(local_name, prefix, document),
            stylesheet: DOMRefCell::new(None),
        }
    }

    #[allow(unrooted_must_root)]
    pub fn new(local_name: Atom,
               prefix: Option<DOMString>,
               document: &Document) -> Root<HTMLMetaElement> {
        Node::reflect_node(box HTMLMetaElement::new_inherited(local_name, prefix, document),
                           document,
                           HTMLMetaElementBinding::Wrap)
    }

    pub fn get_stylesheet(&self) -> Option<Arc<Stylesheet>> {
        self.stylesheet.borrow().clone()
    }

    fn process_attributes(&self) {
        let element = self.upcast::<Element>();
        if let Some(name) = element.get_attribute(&ns!(), &atom!("name")).r() {
            let name = name.value().to_ascii_lowercase();
            let name = name.trim_matches(HTML_SPACE_CHARACTERS);

            if name == "viewport" {
                self.apply_viewport();
            }

            if name == "referrer" {
                self.apply_referrer();
            }
        }
    }

    fn apply_viewport(&self) {
        if !::util::prefs::PREFS.get("layout.viewport.enabled").as_boolean().unwrap_or(false) {
            return;
        }
        let element = self.upcast::<Element>();
        if let Some(content) = element.get_attribute(&ns!(), &atom!("content")).r() {
            let content = content.value();
            if !content.is_empty() {
                if let Some(translated_rule) = ViewportRule::from_meta(&**content) {
                    *self.stylesheet.borrow_mut() = Some(Arc::new(Stylesheet {
                        rules: vec![CSSRule::Viewport(Arc::new(translated_rule))],
                        origin: Origin::Author,
                        media: None,
                        // Viewport constraints are always recomputed on resize; they don't need to
                        // force all styles to be recomputed.
                        dirty_on_viewport_size_change: false,
                    }));
                    let doc = document_from_node(self);
                    doc.invalidate_stylesheets();
                }
            }
        }
    }

    fn process_referrer_attribute(&self) {
        let element = self.upcast::<Element>();
        if let Some(name) = element.get_attribute(&ns!(), &atom!("name")).r() {
            let name = name.value().to_ascii_lowercase();
            let name = name.trim_matches(HTML_SPACE_CHARACTERS);

            if name == "referrer" {
                self.apply_referrer();
            }
        }
    }

    /// https://html.spec.whatwg.org/multipage/#meta-referrer
    fn apply_referrer(&self) {
        if let Some(parent) = self.upcast::<Node>().GetParentElement() {
            if let Some(head) = parent.downcast::<HTMLHeadElement>() {
                head.set_document_referrer();
            }
        }
    }
}

impl HTMLMetaElementMethods for HTMLMetaElement {
    // https://html.spec.whatwg.org/multipage/#dom-meta-name
    make_getter!(Name, "name");

    // https://html.spec.whatwg.org/multipage/#dom-meta-name
    make_atomic_setter!(SetName, "name");

    // https://html.spec.whatwg.org/multipage/#dom-meta-content
    make_getter!(Content, "content");

    // https://html.spec.whatwg.org/multipage/#dom-meta-content
    make_setter!(SetContent, "content");
}

impl VirtualMethods for HTMLMetaElement {
    fn super_type(&self) -> Option<&VirtualMethods> {
        Some(self.upcast::<HTMLElement>() as &VirtualMethods)
    }

    fn bind_to_tree(&self, tree_in_doc: bool) {
        if let Some(ref s) = self.super_type() {
            s.bind_to_tree(tree_in_doc);
        }

        if tree_in_doc {
            self.process_attributes();
        }
    }

    fn parse_plain_attribute(&self, name: &Atom, value: DOMString) -> AttrValue {
        match name {
            &atom!("name") => AttrValue::from_atomic(value.into()),
            _ => self.super_type().unwrap().parse_plain_attribute(name, value),
        }
    }

    fn attribute_mutated(&self, attr: &Attr, mutation: AttributeMutation) {
        if let Some(s) = self.super_type() {
            s.attribute_mutated(attr, mutation);
        }

        self.process_referrer_attribute();
    }

    fn unbind_from_tree(&self, context: &UnbindContext) {
        if let Some(ref s) = self.super_type() {
            s.unbind_from_tree(context);
        }

        if context.tree_in_doc {
            self.process_referrer_attribute();
        }
    }
}
