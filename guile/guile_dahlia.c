#include <libguile.h>
#include <math.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy

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
  An OCaml function that accepts a scheme function and returns an OCaml
  function that will apply the referenced function.
*/
CAMLprim value apply_scm_real_fn (value fn, value x) {
  CAMLparam2 (fn, x);
  SCM sfn = (SCM) Field (fn, 0);
  CAMLreturn (caml_copy_double (scm_to_double (scm_call_1 (sfn, scm_from_double (Double_val (x))))));
}

/*
  Accepts an OCaml Integrate.t record and returns a Scheme list that
  represents it.
*/
SCM integrate_t_to_scm (value x) {
   return scm_cons (
     scm_from_double (Double_val (Field (x, 0))),
     scm_cons (
       scm_from_double (Double_val (Field (x, 1))),
       scm_cons (
         scm_from_int (Int_val (Field (x, 2))),
         SCM_EOL)));
}

/*
  Accepts a Scheme list that represents an OCaml Integrate.t record and
  returns the corresponding OCaml record.
*/
value integrate_qag_params_t_from_scm (SCM x) {
  CAMLparam0 ();
  CAMLlocal1 (result);
  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (scm_to_double (scm_list_ref (x, scm_from_int (0))))); // epsabs
  Store_field (result, 1, caml_copy_double (scm_to_double (scm_list_ref (x, scm_from_int (1))))); // epsrel
  Store_field (result, 2, Val_int          (scm_to_int    (scm_list_ref (x, scm_from_int (2))))); // limit
  CAMLreturn (result);
}

/*
  A Scheme function that accepts a scheme function and integrates the given
  function over the given interval.
*/
SCM integrate_qag (SCM params, SCM f, SCM lower, SCM upper) {
  const size_t num_args = 5;
  value args[] = {
    integrate_qag_params_t_from_scm (params),
    scm_real_fn_to_ocaml (f),
    caml_copy_double (scm_to_double (lower)),
    caml_copy_double (scm_to_double (upper)),
    Val_int(0)
  };
  return integrate_t_to_scm (
    caml_callbackN (
      *caml_named_value ("integrate_qag"), num_args, args
  ));
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
  scm_c_define_gsubr ("dahlia-qag", 4, 0, 0, integrate_qag);
}