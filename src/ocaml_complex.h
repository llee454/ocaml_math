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
  result = caml_alloc (2, Double_array_tag);
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
  result = caml_alloc (2, Double_array_tag);
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
  result = caml_alloc (2, Double_array_tag);
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
  result = caml_alloc (2, Double_array_tag);
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
  result = caml_alloc (2, Double_array_tag);
  Store_double_field (result, 0, GSL_REAL (z));
  Store_double_field (result, 1, GSL_IMAG (z));
  CAMLreturn (result);
}

/* Accepts a complex matrix and returns its inverse. */
CAMLprim value ocaml_gsl_matrix_complex_inv (value M) {
  CAMLparam1 (M);
  CAMLlocal3 (result, result_row, result_elem);

  const size_t nrows = Wosize_val (M);
  if (nrows == 0) {
    GSL_ERROR("[ocaml_gsl_matrix_complex_inv] the given matrix is empty.", GSL_EINVAL);
  }
  const size_t ncols = nrows > 0 ? Wosize_val (Field (M, 0)) : 0;
  if (nrows != ncols) {
    GSL_ERROR("[ocaml_gsl_matrix_complex_inv] the given matrix is not invertable.", GSL_EINVAL);
  }

  gsl_complex* x = malloc (sizeof (gsl_complex));
  gsl_matrix_complex* m = gsl_matrix_complex_alloc (nrows, ncols);
  for (size_t i = 0; i < nrows; i ++) {
    for (size_t j = 0; j < ncols; j ++) {
      GSL_SET_COMPLEX (x,
        Double_field (Field (Field (M, i), j), 0),
        Double_field (Field (Field (M, i), j), 1));
      gsl_matrix_complex_set (m, i, j, *x);
    }
  }
  free (x);

  int* signum = malloc (nrows * sizeof (int));

  gsl_permutation* perm = gsl_permutation_alloc (nrows);
  gsl_linalg_complex_LU_decomp (m, perm, signum);
  free (signum);

  gsl_matrix_complex* inv = gsl_matrix_complex_alloc (nrows, ncols);
  int status = gsl_linalg_complex_LU_invert (m, perm, inv);
  if (status != 0) {
    gsl_matrix_complex_free (m);
    gsl_matrix_complex_free (inv);
    gsl_permutation_free (perm);
    GSL_ERROR("[ocaml_gsl_matrix_complex_inv] An error occured while trying to compute the inverse of a complex matrix.", status);
  }

  result = caml_alloc (nrows, 0);
  for (size_t i = 0; i < nrows; i ++) {
    result_row = caml_alloc (ncols, 0);
    for (size_t j = 0; j < ncols; j ++) {
      result_elem = caml_alloc (2, Double_array_tag);
      Store_double_field (result_elem, 0, GSL_REAL (gsl_matrix_complex_get (inv, i, j)));
      Store_double_field (result_elem, 1, GSL_IMAG (gsl_matrix_complex_get (inv, i, j)));
      Store_field (result_row, j, result_elem);
    }
    Store_field (result, i, result_row);
  }

  gsl_matrix_complex_free (m);
  gsl_matrix_complex_free (inv);
  gsl_permutation_free (perm);

  CAMLreturn (result);
}