Asemio Stats Library Readme
===========================

The Asemio Stats library defines a collection of mathematical and scientific functions that Asemio uses to perform statistical analysis and research. The goal of this library is to provide an intuitive, lightweight, and performant mathematics library for OCaml.

The Asemio Stats library extends the Gnu Scientific Library (GSL). The GSL defines a comprehensive and very mature collection of functions that perform numerical computations such as numerical integration, curve fitting, and linear algebra. It has excellent documentation and is very extensive compare to other libraries available within OCaml.

This library includes a demo program in bin that shows how to use this library.

Initializing the Build Environment
----------------------------------

You will need to install the GSL library as a shared object library (`apt install libgsl-dev`).

```bash
opam switch create . 5.1.0+options --no-install
eval $(opam env)
opam update
opam upgrade
dune build asemio_stats.opam # to generate OPAM package file
opam install --deps-only . -y
dune build
dune runtest
dune exec src/main.exe
```

Additional MacOS Installation Instructions
------------------------------------------

If you get the following error message: `symbol not found in flat namespace '_cblas_caxpy', reinstall conf-openblas while passing `pkgconfig` the correct install path:

```bash
PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/Cellar/openblas/0.3.21/lib/pkgconfig opam reinstall conf-openblas
```

Running Unit Tests
------------------

```bash
dune runtest
```
