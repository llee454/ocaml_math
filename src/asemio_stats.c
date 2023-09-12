#include <stdio.h>
#include <string.h> // for memcpy

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy
#include <caml/signals.h> // control the runtime

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
#include <gsl_eigen.h> // gsl_eigen_symmv
#include <gsl_blas.h> // gsl_blas_dgemm
#include <gsl_linalg.h>

#include <ocaml_siman.h>
#include <ocaml_nonlinear_fit.h>
#include <ocaml_deriv.h>
#include <ocaml_fft.h>
#include <ocaml_complex.h>

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
  double res = gsl_stats_mean (x, 1, n);
  free (x);
  CAMLreturn (caml_copy_double (res));
}

CAMLprim value ocaml_gsl_stats_variance (value xs) {
  CAMLparam1 (xs);
  size_t n = Wosize_val (xs);
  double* x = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    x [i] = Double_field (xs, i);
  }
  double res = gsl_stats_variance (x, 1, n);
  free (x);
  CAMLreturn (caml_copy_double (res));
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

CAMLprim value ocaml_gsl_sf_gamma (value x) {
  CAMLparam1 (x);
  CAMLreturn (caml_copy_double (gsl_sf_gamma ((Double_val (x)))));
}

CAMLprim value ocaml_matrix_mult (value xs, value ys) {
  CAMLparam2 (xs, ys);
  CAMLlocal2 (result, result_row);
  const size_t xs_nrows = Wosize_val (xs);
  const size_t xs_ncols = xs_nrows > 0 ? Wosize_val (Field (xs, 0)) : 0;
  const size_t ys_nrows = Wosize_val (ys);
  const size_t ys_ncols = ys_nrows > 0 ? Wosize_val (Field (ys, 0)) : 0;
  if (xs_ncols != ys_nrows) {
    GSL_ERROR("[ocaml_matrix_mult] the given matrices have invalid dimensions.", GSL_EINVAL);
  }
  gsl_matrix* x = gsl_matrix_alloc (xs_nrows, xs_ncols);
  for (size_t i = 0; i < xs_nrows; i ++) {
    for (size_t j = 0; j < xs_ncols; j ++) {
      gsl_matrix_set (x, i, j, Double_field (Field (xs, i), j));
    }
  }
  gsl_matrix* y = gsl_matrix_alloc (ys_nrows, ys_ncols);
  for (size_t i = 0; i < ys_nrows; i ++) {
    for (size_t j = 0; j < ys_ncols; j ++) {
      gsl_matrix_set (y, i, j, Double_field (Field (ys, i), j));
    }
  }
  gsl_matrix* z = gsl_matrix_alloc (xs_nrows, ys_ncols);
  const int status = gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, x, y, 0.0, z);
  if (status != 0) {
    GSL_ERROR("[ocaml_matrix_mult] gsl_blas_dgemm failed.", status);
  }
  result = caml_alloc (xs_nrows, 0);
  for (size_t i = 0; i < xs_nrows; i ++) {
    result_row = caml_alloc (ys_ncols, Double_array_tag);
    for (size_t j = 0; j < ys_ncols; j ++) {
      Store_double_field (result_row, j, gsl_matrix_get (z, i, j));
    }
    Store_field (result, i, result_row);
  }
  fflush (stdout);
  gsl_matrix_free (x);
  gsl_matrix_free (y);
  gsl_matrix_free (z);
  CAMLreturn (result);
}

CAMLprim value ocaml_gsl_matrix_inv (value xs) {
  CAMLparam1 (xs);
  CAMLlocal2 (result, result_row);

  const size_t nrows = Wosize_val (xs);
  const size_t ncols = nrows > 0 ? Wosize_val (Field (xs, 0)) : 0;
  if (nrows != ncols) {
    GSL_ERROR("[ocaml_gsl_matrix_inv] the given matrix is not invertable.", GSL_EINVAL);
  }

  gsl_matrix* x = gsl_matrix_alloc (nrows, ncols);
  for (size_t i = 0; i < nrows; i ++) {
    for (size_t j = 0; j < ncols; j ++) {
      gsl_matrix_set (x, i, j, Double_field (Field (xs, i), j));
    }
  }

  int* signum = malloc (nrows * sizeof (int));

  gsl_permutation* perm = gsl_permutation_alloc (nrows);
  gsl_linalg_LU_decomp (x, perm, signum);
  free (signum);

  gsl_matrix* inv = gsl_matrix_alloc (nrows, ncols);
  gsl_linalg_LU_invert (x, perm, inv);

  result = caml_alloc (nrows, 0);
  for (size_t i = 0; i < nrows; i ++) {
    result_row = caml_alloc (ncols, Double_array_tag);
    for (size_t j = 0; j < ncols; j ++) {
      Store_double_field (result_row, j, gsl_matrix_get (inv, i, j));
    }
    Store_field (result, i, result_row);
  }
  gsl_matrix_free (x);
  gsl_matrix_free (inv);
  gsl_permutation_free (perm);
  CAMLreturn (result);
}

CAMLprim value ocaml_gsl_ran_gaussian_pdf (value mean, value std, value x) {
  CAMLparam3 (mean, std, x);
  CAMLreturn (caml_copy_double (gsl_ran_gaussian_pdf (Double_val (x) - Double_val (mean), Double_val (std))));
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
  CAMLreturn (caml_copy_double (gsl_ran_binomial_pdf (
    (unsigned int) (Int_val (k)),
    Double_val (p),
    (unsigned int) (Int_val (n)))));
}

CAMLprim value ocaml_gsl_cdf_binomial_cdf (value k, value p, value n) {
  CAMLparam3 (k, p, n);
  CAMLreturn (caml_copy_double (gsl_cdf_binomial_P (
    (unsigned int) (Int_val (k)),
    Double_val (p),
    (unsigned int) (Int_val (n)))));
}

CAMLprim value ocaml_gsl_ran_gamma_pdf (value a, value b, value x) {
  CAMLparam3 (a, b, x);
   CAMLreturn (caml_copy_double (gsl_ran_gamma_pdf (
     Double_val (x),
     Double_val (a),
     Double_val (b))));
}

CAMLprim value ocaml_gsl_stats_covariance (value xs, value ys) {
  CAMLparam2 (xs, ys);
  const size_t nxs = Wosize_val (xs);
  const size_t nys = Wosize_val (ys);
  const size_t n = nxs < nys ? nxs : nys;
  double* x = malloc (n * sizeof (double));
  double* y = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    x [i] = Double_field (xs, i);
    y [i] = Double_field (ys, i);
  }
  double result = gsl_stats_covariance (x, 1, y, 1, n);
  free (x);
  free (y);
  CAMLreturn (caml_copy_double (result));
}

CAMLprim value ocaml_gsl_stats_correlation (value xs, value ys) {
  CAMLparam2 (xs, ys);
  const size_t nxs = Wosize_val (xs);
  const size_t nys = Wosize_val (ys);
  const size_t n = nxs < nys ? nxs : nys;
  double* x = malloc (n * sizeof (double));
  double* y = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    x [i] = Double_field (xs, i);
    y [i] = Double_field (ys, i);
  }
  double result = gsl_stats_correlation (x, 1, y, 1, n);
  free (x);
  free (y);
  CAMLreturn (caml_copy_double (result));
}

CAMLprim value ocaml_gsl_eigen_symmv (value m) {
  CAMLparam1 (m);
  CAMLlocal4 (result, result_vals, result_vecs, result_vec);
  const size_t nrows = Wosize_val (m);
  const size_t ncols = nrows > 0 ? Wosize_val (Field (m, 0)) : 0;
  if (nrows != ncols) {
    GSL_ERROR("[ocaml_gsl_eigen_symmv] the given matrix is not symmatrix.", GSL_EINVAL);
  }
  gsl_matrix* matrix = gsl_matrix_alloc (nrows, ncols);
  for (size_t i = 0; i < nrows; i ++) {
    for (size_t j = 0; j < ncols; j ++) {
      gsl_matrix_set (matrix, i, j, Double_field (Field (m, i), j));
    }
  }
  gsl_vector* eigen_vals = gsl_vector_alloc (nrows);
  gsl_vector_set_all (eigen_vals, 0.0);
  gsl_matrix* eigen_vecs = gsl_matrix_alloc (nrows, ncols);
  gsl_matrix_set_all (eigen_vecs, 0.0);
  gsl_eigen_symmv_workspace* w = gsl_eigen_symmv_alloc (nrows);
  int status = gsl_eigen_symmv (matrix, eigen_vals, eigen_vecs, w);
  if (status) {
    GSL_ERROR("[ocaml_gsl_eigen_symmv] gsl_eigen_symmv failed.", status);
  }
  result_vals = caml_alloc (nrows, Double_array_tag);
  for (size_t i = 0; i < nrows; i ++) {
    Store_double_field (result_vals, i, gsl_vector_get (eigen_vals, i));
  }
  result_vecs = caml_alloc (ncols, 0);
  for (size_t i = 0; i < ncols; i ++) {
    result_vec = caml_alloc (nrows, Double_array_tag);
    for (size_t j = 0; j < nrows; j ++) {
      Store_double_field (result_vec, j, gsl_matrix_get (eigen_vecs, j, i));
    }
    Store_field (result_vecs, i, result_vec);
  }
  result = caml_alloc (2, 0);
  Store_field (result, 0, result_vals);
  Store_field (result, 1, result_vecs);
  gsl_matrix_free (matrix);
  gsl_vector_free (eigen_vals);
  gsl_matrix_free (eigen_vecs);
  gsl_eigen_symmv_free (w);
  CAMLreturn (result);
}

CAMLprim value ocaml_gsl_fit_linear (value xs, value ys) {
  CAMLparam2 (xs, ys);
  CAMLlocal1 (result);
  double* x = malloc (Wosize_val (xs) * sizeof (double));
  for (size_t i = 0; i < Wosize_val (xs); i ++) {
    x [i] = Double_field (xs, i);
  }
  double* y = malloc (Wosize_val (ys) * sizeof (double));
  for (size_t i = 0; i < Wosize_val (ys); i ++) {
    y [i] = Double_field (ys, i);
  }
  caml_enter_blocking_section ();
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
  caml_leave_blocking_section ();

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
  size_t limit = 100;

  struct callback_params* params = malloc (sizeof (struct callback_params));
  params->h = f;
  caml_register_global_root (&params->h);

  gsl_function* F = malloc (sizeof (gsl_function));
  F->function = &callback;
  F->params   = params;

  gsl_integration_workspace* w = gsl_integration_workspace_alloc (limit);
  int status =  gsl_integration_qag (F, Double_val (lower), Double_val (upper), 0.0001, 0, limit, GSL_INTEG_GAUSS61, w, &out, &err);
  gsl_integration_workspace_free (w);
  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (out));
  Store_field (result, 1, caml_copy_double (err));
  Store_field (result, 2, Val_long (0));

  caml_remove_global_root (&params->h);
  free (params);
  free (F);

  CAMLreturn (result);
}

CAMLprim value ocaml_integration_qagp (value f, value lower, value upper, value singularities) {
  CAMLparam4 (f, lower, upper, singularities);
  CAMLlocal1 (result);
  double out;
  double err;
  size_t limit = 100;

  struct callback_params* params = malloc (sizeof (struct callback_params));
  params->h = f;
  caml_register_global_root (&params->h);

  gsl_function* F = malloc (sizeof (gsl_function));
  F->function = &callback;
  F->params   = params;

  size_t n = Wosize_val (singularities) + 2;
  double* ps = malloc (n * sizeof (double));
  ps [0] = Double_val (lower);
  ps [n - 1] = Double_val (upper);
  for (size_t i = 1; i < n - 1; i ++) {
    ps [i] = Double_field (singularities, i);
  }
  gsl_integration_workspace* w = gsl_integration_workspace_alloc (limit);
  int status =  gsl_integration_qagp (F, ps, n, 0.0001, 0, limit, w, &out, &err);
  gsl_integration_workspace_free (w);
  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (out));
  Store_field (result, 1, caml_copy_double (err));
  Store_field (result, 2, Val_long (0));

  caml_remove_global_root (&params->h);
  free (params);
  free (F);
  free (ps);

  CAMLreturn (result);
}

CAMLprim value ocaml_integration_qagi (value f) {
  CAMLparam1 (f);
  CAMLlocal1 (result);
  double out;
  double err;
  size_t limit = 100;
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
