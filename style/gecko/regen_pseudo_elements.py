#!/usr/bin/env python

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

import os
import sys

from pseudo_elements import PseudoElementData

import build


def generate_pseudo_elements(out):
    data = PseudoElementData()
    pseudo_definition_template = os.path.join(
        os.path.dirname(__file__), "pseudo_element_definition.mako.rs"
    )

    print(f"cargo:rerun-if-changed={pseudo_definition_template}")
    for f in data.path_dependencies:
        print(f"cargo:rerun-if-changed={f}")

    target = os.path.join(out, "pseudo_element_definition.rs")
    build.write(out, "pseudo_element_definition.rs", build.render(pseudo_definition_template, PSEUDOS=data.all_pseudos()))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: {} out".format(sys.argv[0]))
        exit(2)
    generate_pseudo_elements(sys.argv[1])
