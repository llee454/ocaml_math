#include <stdio.h>
#include <string.h> // for memcpy

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy

#include <gsl_errno.h>
#include <gsl_math.h> // log1p
#include <gsl/gsl_fft_real.h>
#include <gsl/gsl_fft_halfcomplex.h>

/*
  Accepts a float array and returns the discrete fourier transform
  coefficients in a half complex array. See the GSL manual for
  details.
*/
CAMLprim value ocaml_fft_real_transform (value data) {
  CAMLparam1 (data);
  CAMLlocal1 (result);

  size_t n = Wosize_val (data);
  double* xs = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    xs [i] = Double_field (data, i);
  }

  gsl_fft_real_wavetable* table = gsl_fft_real_wavetable_alloc (n);
  gsl_fft_real_workspace* work = gsl_fft_real_workspace_alloc (n);

  int status = gsl_fft_real_transform (xs, 1, n, table, work);

  gsl_fft_real_wavetable_free (table);
  gsl_fft_real_workspace_free (work);

  if (status != 0) {
    free (xs);
    GSL_ERROR("[ocaml_fft_real_transform] Error: an error occured while trying to compute the fast fourier transform for a dataset.", status);
  }

  result = caml_alloc (n, Double_array_tag);
  for (size_t i = 0; i < n; i ++) {
    Store_double_field (result, i, xs [i]);
  }
  free (xs);
  CAMLreturn (result);
}

CAMLprim value ocaml_fft_halfcomplex_unpack (value coeffs) {
  CAMLparam1 (coeffs);
  CAMLlocal2 (result, coeff);

  size_t n = Wosize_val (coeffs);
  if (n == 0) {
    result = caml_alloc (0, Double_array_tag);
    CAMLreturn (result);
  }

  double* xs = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    xs [i] = Double_field (coeffs, i);
  }
  double* ys = malloc (2 * n * sizeof (double));
  int status = gsl_fft_halfcomplex_unpack (xs, ys, 1, n - 1);

  free (xs);

  if (status != 0) {
    free (ys);
    GSL_ERROR("[ocaml_fft_halfcomplex_unpack] Error: an error occured while trying to unpack a halfcomplex array.", status);
  }

  result = caml_alloc (n - 1, 0);
  for (size_t i = 0; i < n - 1; i ++) {
    coeff = caml_alloc (2, Double_array_tag);
    Store_double_field (coeff, 0, ys [2 * i]);
    Store_double_field (coeff, 1, ys [2 * i + 1]);
    Store_field (result, i, coeff);
  }
  free (ys);
  CAMLreturn (result);
}

/*
  Accepts a float array containing the discrete fourier transform
  coefficients in a half complex array and returns the approximation
  of the original dataset produced by the fourier series approximation
  of the original dataset function.
*/
CAMLprim value ocaml_fft_halfcomplex_inverse (value coeffs) {
  CAMLparam1 (coeffs);
  CAMLlocal1 (result);

  size_t n = Wosize_val (coeffs);
  double* xs = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    xs [i] = Double_field (coeffs, i);
  }

  gsl_fft_halfcomplex_wavetable* table = gsl_fft_halfcomplex_wavetable_alloc (n);
  gsl_fft_real_workspace* work = gsl_fft_real_workspace_alloc (n);

  int status = gsl_fft_halfcomplex_inverse (xs, 1, n, table, work);

  gsl_fft_halfcomplex_wavetable_free (table);
  gsl_fft_real_workspace_free (work);

  if (status != 0) {
    free (xs);
    GSL_ERROR("[ocaml_fft_halfcomplex_inverse] Error: an error occured while trying to compute the inverse fast fourier transform for a dataset.", status);
  }

  result = caml_alloc (n, Double_array_tag);
  for (size_t i = 0; i < n; i ++) {
    Store_double_field (result, i, xs [i]);
  }
  free (xs);
  CAMLreturn (result);
}