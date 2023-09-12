#include <stdio.h>
#include <string.h> // for memcpy
#include <complex.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy

#include <gsl_errno.h>
#include <gsl_math.h> // log1p
#include <gsl_complex.h>
#include <gsl_complex_math.h>

CAMLprim value ocaml_from_polar (value x) {
  CAMLparam1 (x);
  CAMLlocal1 (result);
  gsl_complex z = gsl_complex_polar (
    Double_field (x, 0),
    Double_field (x, 1)
  );
  result = caml_alloc (2, 0);
  Store_double_field (result, 0, GSL_REAL (z));
  Store_double_field (result, 1, GSL_IMAG (z));
  CAMLreturn (result);
}

CAMLprim value ocaml_complex_add (value x, value y) {
  CAMLparam2 (x, y);
  CAMLlocal1 (result);
  gsl_complex z = gsl_complex_add (
    gsl_complex_rect (
      Double_field (x, 0),
      Double_field (x, 1)
    ),
    gsl_complex_rect (
      Double_field (y, 0),
      Double_field (y, 1)
    )
  );
  result = caml_alloc (2, 0);
  Store_double_field (result, 0, GSL_REAL (z));
  Store_double_field (result, 1, GSL_IMAG (z));
  CAMLreturn (result);
}

CAMLprim value ocaml_complex_sub (value x, value y) {
  CAMLparam2 (x, y);
  CAMLlocal1 (result);
  gsl_complex z = gsl_complex_sub (
    gsl_complex_rect (
      Double_field (x, 0),
      Double_field (x, 1)
    ),
    gsl_complex_rect (
      Double_field (y, 0),
      Double_field (y, 1)
    )
  );
  result = caml_alloc (2, 0);
  Store_double_field (result, 0, GSL_REAL (z));
  Store_double_field (result, 1, GSL_IMAG (z));
  CAMLreturn (result);
}

CAMLprim value ocaml_complex_mul (value x, value y) {
  CAMLparam2 (x, y);
  CAMLlocal1 (result);
  gsl_complex z = gsl_complex_mul (
    gsl_complex_rect (
      Double_field (x, 0),
      Double_field (x, 1)
    ),
    gsl_complex_rect (
      Double_field (y, 0),
      Double_field (y, 1)
    )
  );
  result = caml_alloc (2, 0);
  Store_double_field (result, 0, GSL_REAL (z));
  Store_double_field (result, 1, GSL_IMAG (z));
  CAMLreturn (result);
}

CAMLprim value ocaml_complex_div (value x, value y) {
  CAMLparam2 (x, y);
  CAMLlocal1 (result);
  gsl_complex z = gsl_complex_div (
    gsl_complex_rect (
      Double_field (x, 0),
      Double_field (x, 1)
    ),
    gsl_complex_rect (
      Double_field (y, 0),
      Double_field (y, 1)
    )
  );
  result = caml_alloc (2, 0);
  Store_double_field (result, 0, GSL_REAL (z));
  Store_double_field (result, 1, GSL_IMAG (z));
  CAMLreturn (result);
}