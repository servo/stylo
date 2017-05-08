#!/usr/bin/env python

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

import re
import os
import sys

from io import BytesIO


PRELUDE = """
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

/* Autogenerated file created by components/style/binding_tools/regen_atoms.py, DO NOT EDIT DIRECTLY */
"""[1:]


def gnu_symbolify(source, ident):
    return "_ZN{}{}{}{}E".format(len(source.CLASS), source.CLASS, len(ident), ident)


def msvc64_symbolify(source, ident):
    return "?{}@{}@@2PEAV{}@@EA".format(ident, source.CLASS, source.TYPE)


def msvc32_symbolify(source, ident):
    # Prepend "\x01" to avoid LLVM prefixing the mangled name with "_".
    # See https://github.com/rust-lang/rust/issues/36097
    return "\\x01?{}@{}@@2PAV{}@@A".format(ident, source.CLASS, source.TYPE)


class GkAtomSource:
    PATTERN = re.compile('^GK_ATOM\((.+),\s*"(.*)"\)')
    FILE = "include/nsGkAtomList.h"
    CLASS = "nsGkAtoms"
    TYPE = "nsIAtom"


class CSSPseudoElementsAtomSource:
    PATTERN = re.compile('^CSS_PSEUDO_ELEMENT\((.+),\s*"(.*)",')
    FILE = "include/nsCSSPseudoElementList.h"
    CLASS = "nsCSSPseudoElements"
    # NB: nsICSSPseudoElement is effectively the same as a nsIAtom, but we need
    # this for MSVC name mangling.
    TYPE = "nsICSSPseudoElement"


class CSSAnonBoxesAtomSource:
    PATTERN = re.compile('^(?:CSS_ANON_BOX|CSS_NON_INHERITING_ANON_BOX)\((.+),\s*"(.*)"(\,|\))')
    FILE = "include/nsCSSAnonBoxList.h"
    CLASS = "nsCSSAnonBoxes"
    TYPE = "nsICSSAnonBoxPseudo"


SOURCES = [
    GkAtomSource,
    CSSPseudoElementsAtomSource,
    CSSAnonBoxesAtomSource,
]


def map_atom(ident):
    if ident in {"box", "loop", "match", "mod", "ref",
                 "self", "type", "use", "where", "in"}:
        return ident + "_"
    return ident


class Atom:
    def __init__(self, source, ident, value):
        self.ident = "{}_{}".format(source.CLASS, ident)
        self._original_ident = ident
        self.value = value
        self.source = source

    def cpp_class(self):
        return self.source.CLASS

    def gnu_symbol(self):
        return gnu_symbolify(self.source, self._original_ident)

    def msvc32_symbol(self):
        return msvc32_symbolify(self.source, self._original_ident)

    def msvc64_symbol(self):
        return msvc64_symbolify(self.source, self._original_ident)

    def type(self):
        return self.source.TYPE


def collect_atoms(objdir):
    atoms = []
    for source in SOURCES:
        path = os.path.abspath(os.path.join(objdir, source.FILE))
        print("cargo:rerun-if-changed={}".format(path))
        with open(path) as f:
            for line in f.readlines():
                result = re.match(source.PATTERN, line)
                if result:
                    atoms.append(Atom(source, result.group(1), result.group(2)))
    return atoms


class FileAvoidWrite(BytesIO):
    """File-like object that buffers output and only writes if content changed."""
    def __init__(self, filename):
        BytesIO.__init__(self)
        self.name = filename

    def write(self, buf):
        if isinstance(buf, unicode):
            buf = buf.encode('utf-8')
        BytesIO.write(self, buf)

    def close(self):
        buf = self.getvalue()
        BytesIO.close(self)
        try:
            with open(self.name, 'rb') as f:
                old_content = f.read()
                if old_content == buf:
                    print("{} is not changed, skip".format(self.name))
                    return
        except IOError:
            pass
        with open(self.name, 'wb') as f:
            f.write(buf)

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        if not self.closed:
            self.close()


IMPORTS = ("\nuse gecko_bindings::structs::nsIAtom;"
           "\nuse string_cache::Atom;\n\n")

ATOM_TEMPLATE = ("            #[link_name = \"{link_name}\"]\n"
                 "            pub static {name}: *mut {type};")

UNSAFE_STATIC = ("#[inline(always)]\n"
                 "pub unsafe fn atom_from_static(ptr: *mut nsIAtom) -> Atom {\n"
                 "    Atom::from_static(ptr)\n"
                 "}\n\n")

CFG_IF = '''
cfg_if! {{
    if #[cfg(not(target_env = "msvc"))] {{
        extern {{
{gnu}
        }}
    }} else if #[cfg(target_pointer_width = "64")] {{
        extern {{
{msvc64}
        }}
    }} else {{
        extern {{
{msvc32}
        }}
    }}
}}
'''

RULE_TEMPLATE = ('("{atom}") =>\n  '
                 '{{ '
                 # FIXME(bholley): Uncomment this when rust 1.14 is released.
                 # See the comment in components/style/lib.rs.
                 # ' #[allow(unsafe_code)] #[allow(unused_unsafe)] '
                 'unsafe {{ $crate::string_cache::atom_macro::atom_from_static'
                 '($crate::string_cache::atom_macro::{name} as *mut _) }}'
                 ' }};')

MACRO = '''
#[macro_export]
macro_rules! atom {{
{}
}}
'''


def write_atom_macro(atoms, file_name):
    def get_symbols(func):
        return '\n'.join([ATOM_TEMPLATE.format(name=atom.ident,
                                               link_name=func(atom),
                                               type=atom.type()) for atom in atoms])

    with FileAvoidWrite(file_name) as f:
        f.write(PRELUDE)
        f.write(IMPORTS)

        for source in SOURCES:
            if source.TYPE != "nsIAtom":
                f.write("pub enum {} {{}}\n\n".format(source.TYPE))

        f.write(UNSAFE_STATIC)

        gnu_symbols = get_symbols(Atom.gnu_symbol)
        msvc32_symbols = get_symbols(Atom.msvc32_symbol)
        msvc64_symbols = get_symbols(Atom.msvc64_symbol)
        f.write(CFG_IF.format(gnu=gnu_symbols, msvc32=msvc32_symbols, msvc64=msvc64_symbols))

        macro_rules = [RULE_TEMPLATE.format(atom=atom.value, name=atom.ident) for atom in atoms]
        f.write(MACRO.format('\n'.join(macro_rules)))


PSEUDO_ELEMENT_HEADER = """
/*
 * This file contains a helper macro invocation to aid Gecko's style system
 * pseudo-element integration.
 *
 * This file is NOT INTENDED to be compiled as a standalone module.
 *
 * Also, it guarantees the property that normal pseudo-elements are processed
 * before anonymous boxes.
 *
 * Expected usage is as follows:
 *
 * ```
 * fn have_to_use_pseudo_elements() {
 *     macro_rules! pseudo_element {
 *         ($pseudo_str_with_colon:expr, $pseudo_atom:expr, $is_anon_box:true) => {{
 *             // Stuff stuff stuff.
 *         }}
 *     }
 *     include!("path/to/helper.rs")
 * }
 * ```
 *
 */
"""

PSEUDO_ELEMENT_MACRO_INVOCATION = """
    pseudo_element!(\"{}\",
                    atom!(\"{}\"),
                    {});
"""[1:]


def write_pseudo_element_helper(atoms, target_filename):
    with FileAvoidWrite(target_filename) as f:
        f.write(PRELUDE)
        f.write(PSEUDO_ELEMENT_HEADER)
        f.write("{\n")
        for atom in atoms:
            if atom.type() == "nsICSSPseudoElement":
                f.write(PSEUDO_ELEMENT_MACRO_INVOCATION.format(atom.value, atom.value, "false"))
            elif atom.type() == "nsICSSAnonBoxPseudo":
                f.write(PSEUDO_ELEMENT_MACRO_INVOCATION.format(atom.value, atom.value, "true"))
        f.write("}\n")


def generate_atoms(dist, out):
    atoms = collect_atoms(dist)
    write_atom_macro(atoms, os.path.join(out, "atom_macro.rs"))
    write_pseudo_element_helper(atoms, os.path.join(out, "pseudo_element_helper.rs"))


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: {} dist out".format(sys.argv[0]))
        exit(2)
    generate_atoms(sys.argv[1], sys.argv[2])
