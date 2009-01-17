#! /bin/sh

ocamlbuild -I tests                             \
    tests/dps.native                            \
    tests/gen.native                            \
    tests/graph.native                          \
    tests/main.native                           \
    tests/projection.native                     \
    tests/rewriting.native                      \
    tests/term.native
