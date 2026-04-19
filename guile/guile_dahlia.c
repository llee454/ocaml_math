#include <libguile.h>
#include <math.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy

typedef double (*c_real_fn) (double);

/*
  Accepts a real function pointer and returns an OCaml value that represents
  the given function.
*/
CAMLprim value c_real_fn_to_ocaml (c_real_fn fn) {
  CAMLparam0 ();
  CAMLlocal1 (res);
  res = caml_alloc (1, Abstract_tag);
  Store_field (res, 0, (value) fn);
  CAMLreturn (res);
}

/*
  Accepts a Scheme function and returns an OCaml value that represents the
  given function.
*/
CAMLprim value scm_real_fn_to_ocaml (SCM sfn) {
  CAMLparam0 ();
  CAMLlocal1 (res);
  res = caml_alloc (1, Abstract_tag);
  Store_field (res, 0, (value) sfn);
  CAMLreturn (res);
}

/*
  An OCaml function that accepts C real function pointer and returns an
  OCaml function that will apply the referenced function.
*/
CAMLprim value call_ocaml_c_real_fn (value cfn, value x) {
  CAMLparam2 (cfn, x); 
  c_real_fn fn = (c_real_fn) Field (cfn, 0);
  CAMLreturn (caml_copy_double ((*fn) (Double_val (x))));
}

/*
  An OCaml function that accepts a scheme function and returns an OCaml
  function that will apply the referenced function.
*/
CAMLprim value call_scm_real_fn (value fn, value x) {
  CAMLparam2 (fn, x);
  SCM sfn = (SCM) Field (fn, 0);
  CAMLreturn (caml_copy_double (scm_to_double (scm_call_1 (sfn, scm_from_double (Double_val (x))))));
}

/* A C wrapper function for the integration test function. */
SCM c_fn_int_test (c_real_fn f) {
  return scm_from_double (Double_val (caml_callback (*caml_named_value ("int_test"), c_real_fn_to_ocaml (f))));
}

/*
  A Scheme function that accepts a scheme function and integrates the given
  function over the interval [0, 1].
*/
SCM scm_fn_int_test (SCM f) {
  return scm_from_double (Double_val (caml_callback (*caml_named_value ("int_test"), scm_real_fn_to_ocaml (f))));
}

/*
  Returns an array of strings that contains the arguments passed to the
  current program.
*/
char** get_argv () {
  SCM args = scm_program_arguments ();
  int args_len = scm_to_int (scm_length (args));
  char** argv = calloc (args_len + 1, sizeof (SCM));
  SCM arg = scm_car (args);
  for (size_t i = 0; i < args_len; i ++, args = scm_cdr (args)) {
    argv[i] = scm_to_locale_string (arg);
  }
  return argv;
}

/*
  Initializes the guile extension.
  Note: when you load this Guile extension use:
    (load-extension "./_build/default/guile/mod.so" "init")
*/
void init () {
  caml_startup (get_argv ());
  scm_c_define_gsubr ("scm-fn-int-test", 1, 0, 0, scm_fn_int_test);
}