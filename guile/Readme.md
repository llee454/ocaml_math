Guile Scheme Extension
======================

This package defines a Guile Scheme extension that exposes some of the
functions defined in the OCaml Math Library as Scheme functions.

The Guile extension is a shared object file named `guile_dahlia.so`. 

Compiling
---------

A Guile Scheme extension is a standard shared object file (guile_dahlia.so
in Linux). To compile this shared object file, simply run `dune` from the
this project's root directory.

```bash
opam switch create . 5.3.0+options --no-install
eval $(opam env)
opam update
opam upgrade
dune build ocaml_math.opam # to generate OPAM package file
opam install --deps-only . -y

# compile the extension and other libraries.
dune build
```

By default, `dune` will generate the shared object in
`_build/default/guile/guile_dahlia.so`. It will then promote this file into
the `guile` source directory.

When you rune `dune clean`, Dune will delete this file. To use this extension,
copy it into your Guile program's extension folder. For example, on my
(unconventional) machine:
`cp guile/guile_dahlia.so /usr/local/guile-3.0.10/lib/guile/3.0/extensions/`

Using the Extension
-------------------

To use this extension, you can load the shared object using the
`load-extension` function in Guile Scheme. For example:

```scheme
(load-extension "./guile/guile_dahlia.so" "init")
```
