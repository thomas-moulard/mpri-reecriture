#! /bin/sh

# to compile with ocamlgraph: -lib graph

ocamlbuild                      \
    main.native                 \
    test_compute_reds.native    \
    test_dps.native             \
    test_graph.native           \
    test_main.native            \
    test_proj.native            \
    test_components.native
