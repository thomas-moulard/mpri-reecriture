#! /bin/sh

# to compile with ocamlgraph: -lib graph

ocamlbuild                      \
    main.native                 \
    test_dps.native             \
    test_graph.native           \
    test_components.native
