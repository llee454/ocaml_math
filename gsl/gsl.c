#include <stdio.h>
#include <string.h> // for memcpy

// Prevent OCaml from exporting short macro names.
#define CAML_NAME_SPACE 1

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy

#include <gsl_errno.h>
#include <gsl_math.h> // log1p
#include <gsl_statistics_double.h>
#include <gsl_sf_erf.h>
#include <gsl_fit.h> // gsl_fit_linear
#include <gsl_integration.h>
#include <gsl_multifit_nlinear.h>
#include <gsl_cdf.h>
#include <gsl_sf_gamma.h> // gsl_sf_fact
#include <gsl_randist.h> // gsl_ran_binomial_pdf

#include <ocaml_siman.h>
#include <ocaml_nonlinear_fit.h>

CAMLprim value ocaml_gsl_pow_int (value x, value n) {
  CAMLparam2 (x, n);
  CAMLreturn (caml_copy_double (gsl_pow_int (Double_val (x), Int_val (n))));
}

CAMLprim value ocaml_mean (value xs) {
  CAMLparam1 (xs);
  size_t n = Wosize_val (xs);
  double* x = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    x [i] = Double_field (xs, i);
  }
  CAMLreturn (caml_copy_double (gsl_stats_mean (x, 1, n)));
}

CAMLprim value ocaml_gsl_sf_erf_Z (value x) {
  CAMLparam1 (x);
  CAMLreturn (caml_copy_double (gsl_sf_erf_Z (Double_val (x))));
}

CAMLprim value ocaml_gsl_sf_erf_Q (value x) {
  CAMLparam1 (x);
  CAMLreturn (caml_copy_double (gsl_sf_erf_Q (Double_val (x))));
}

CAMLprim value ocaml_gsl_sf_fact (value x) {
  CAMLparam1 (x);
  CAMLreturn (caml_copy_double (gsl_sf_fact ((unsigned int) (Int_val (x)))));
}

CAMLprim value ocaml_gsl_cdf_gaussian_P (value x, value std) {
  CAMLparam2 (x, std);
  CAMLreturn (caml_copy_double (gsl_cdf_gaussian_P (Double_val (x), Double_val (std))));
}

CAMLprim value ocaml_gsl_cdf_gaussian_Q (value x, value std) {
  CAMLparam2 (x, std);
  CAMLreturn (caml_copy_double (gsl_cdf_gaussian_Q (Double_val (x), Double_val (std))));
}

CAMLprim value ocaml_gsl_cdf_chisq_P (value x, value nu) {
  CAMLparam2 (x, nu);
  CAMLreturn (caml_copy_double (gsl_cdf_chisq_P (Double_val (x), Double_val (nu))));
}

CAMLprim value ocaml_gsl_cdf_gaussian_Pinv (value x, value ndf) {
  CAMLparam2 (x, ndf);
  CAMLreturn (caml_copy_double (gsl_cdf_gaussian_Pinv (Double_val (x), Double_val (ndf))));
}

CAMLprim value ocaml_gsl_ran_binomial_pdf (value k, value p, value n) {
  CAMLparam3 (k, p, n);
  CAMLlocal1 (result);
  CAMLreturn (caml_copy_double (gsl_ran_binomial_pdf (
    (unsigned int) (Int_val (k)),
    Double_val (p),
    (unsigned int) (Int_val (n)))));
}

CAMLprim value ocaml_gsl_fit_linear (value xs, value ys) {
  CAMLparam2 (xs, ys);
  CAMLlocal1 (result);
  // BUG: wrong allocation size (Wosize * sizeof double)
  double* x = malloc (Wosize_val (xs));
  for (size_t i = 0; i < Wosize_val (xs); i ++) {
    x [i] = Double_field (xs, i);
  }
  double* y = malloc (Wosize_val (ys));
  for (size_t i = 0; i < Wosize_val (ys); i ++) {
    y [i] = Double_field (ys, i);
  }
  const size_t xstride = 1;
  const size_t ystride = 1;
  const size_t n = 3;
  double c0;
  double c1;
  double cov00;
  double cov01;
  double cov11;
  double sumsq;
  int status = gsl_fit_linear (x, xstride, y, ystride, n, &c0, &c1, &cov00, &cov01, &cov11, &sumsq);
  free (x);
  free (y);

  // Note: OCaml represents records whose fields are all floats as double array blocks.
  result = caml_alloc (2, Double_array_tag);
  Store_double_field (result, 0, c0);
  Store_double_field (result, 1, c1);
  CAMLreturn (result);
}

struct callback_params { value h; };

double callback (double x, void* params) {
  CAMLparam0 ();
  CAMLlocal2 (y, result);
  struct callback_params* p = (struct callback_params*) params;
  y = caml_copy_double (x);
  result = caml_callback (p->h, y);
  CAMLreturnT (double, Double_val (result));
}

CAMLprim value ocaml_integrate (value f, value lower, value upper) {
  CAMLparam3 (f, lower, upper);
  CAMLlocal1 (result);
  double out;
  double err;
  size_t neval;
  struct callback_params params = {
    .h = f
  };
  gsl_function F = {
    .function = &callback,
    .params   = &params
  };
  int status = gsl_integration_qng (&F, Double_val (lower), Double_val (upper), 0.0001, 0, &out, &err, &neval);
  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (out));
  Store_field (result, 1, caml_copy_double (err));
  Store_field (result, 2, Val_long (neval));
  CAMLreturn (result);
}

CAMLprim value ocaml_integration_qag (value f, value lower, value upper) {
  CAMLparam3 (f, lower, upper);
  CAMLlocal1 (result);
  double out;
  double err;
  size_t limit = 10;
  struct callback_params params = {
    .h = f
  };
  gsl_function F = {
    .function = &callback,
    .params   = &params
  };
  gsl_integration_workspace* w = gsl_integration_workspace_alloc (limit);
  int status =  gsl_integration_qag (&F, Double_val (lower), Double_val (upper), 0.0001, 0, limit, GSL_INTEG_GAUSS61, w, &out, &err);
  gsl_integration_workspace_free (w);
  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (out));
  Store_field (result, 1, caml_copy_double (err));
  Store_field (result, 2, Val_long (0));
  CAMLreturn (result);
}

CAMLprim value ocaml_integration_qagi (value f) {
  CAMLparam1 (f);
  CAMLlocal1 (result);
  double out;
  double err;
  size_t limit = 10;
  struct callback_params params = {
    .h = f
  };
  gsl_function F = {
    .function = &callback,
    .params   = &params
  };
  gsl_integration_workspace* w = gsl_integration_workspace_alloc (limit);
  int status =  gsl_integration_qagi (&F, 0.0001, 0, limit, w, &out, &err);
  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (out));
  Store_field (result, 1, caml_copy_double (err));
  Store_field (result, 2, Val_long (0));
  gsl_integration_workspace_free (w);
  CAMLreturn (result);
}
