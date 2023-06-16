#include <stdio.h>
#include <string.h> // for memcpy

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h> // CAMLreturn
#include <caml/alloc.h> // caml_copy

#include <gsl_errno.h>
#include <gsl_math.h> // log1p
#include <gsl_fit.h> // gsl_fit_linear
#include <gsl_integration.h>
#include <gsl_multifit_nlinear.h>

// an auxiliary data structure used to store the parameters and data points fitted by ocaml_get_fit_nlinear
struct ocaml_get_fit_nlinear_f_data {
  size_t  n_xs;
  size_t  n_ks;
  value   f;
  double* xs;
  double* ys;
  value   f_ks;
  value   f_arg;
};

// computes the value of the fitted function for the given function parameters and data values.
int ocaml_get_fit_nlinear_f (
  const gsl_vector* f_ks, // the values of constants within the fitted function
  void* data,             // the fitted function parameters and data values
  gsl_vector* f_res       // the fitted function output values (may be 2 or more if f is multidimensional)
) {
  size_t n_xs   = ((struct ocaml_get_fit_nlinear_f_data*) data)->n_xs;   // the number of data points
  size_t n_ks   = ((struct ocaml_get_fit_nlinear_f_data*) data)->n_ks;   // the number of fitted function parameters
  value ocaml_f = ((struct ocaml_get_fit_nlinear_f_data*) data)->f; // the fitted function
  double* xs    = ((struct ocaml_get_fit_nlinear_f_data*) data)->xs;
  double* ys    = ((struct ocaml_get_fit_nlinear_f_data*) data)->ys;
  value ocaml_f_ks  = ((struct ocaml_get_fit_nlinear_f_data*) data)->f_ks;
  value ocaml_f_arg = ((struct ocaml_get_fit_nlinear_f_data*) data)->f_arg;

  caml_register_global_root (&ocaml_f);
  caml_register_global_root (&ocaml_f_ks);
  caml_register_global_root (&ocaml_f_arg);

  // the fitted function parameters
  for (size_t i = 0; i < n_ks; i ++) {
    Store_double_field (ocaml_f_ks, i, gsl_vector_get (f_ks, i));
  }

  // call the fitted function on the given data values with the current parameter values.
  for (size_t i = 0; i < n_xs; i ++) {
    // construct the argument passed to the fitted function
    Store_field (ocaml_f_arg, 0, ocaml_f_ks);
    Store_field (ocaml_f_arg, 1, caml_copy_double (xs [i]));

    // call the fitted function and store the result
    gsl_vector_set (f_res, i, ys [i] - Double_val (caml_callback (ocaml_f, ocaml_f_arg)));
  }

  caml_remove_global_root (&ocaml_f);
  caml_remove_global_root (&ocaml_f_ks);
  caml_remove_global_root (&ocaml_f_arg);

  return GSL_SUCCESS;
}

/*
  Accepts a function that has parameters ks, and a dataset ys over over
  values xs and estimates the parameter values ks' that makes f
  approximate ys over xs.
*/
CAMLprim value ocaml_gsl_fit_nlinear (
  value ocaml_f,
  value ocaml_f_ks_init,
  value ocaml_xs,
  value ocaml_ys
) {
  CAMLparam4 (ocaml_f, ocaml_f_ks_init, ocaml_xs, ocaml_ys);
  CAMLlocal3 (ocaml_f_ks_final, ocaml_f_arg, ocaml_f_ks);

  const size_t max_iter = 100; // maximum number of iterations
  const double x_tol = 0.00001; // x error tolerance
  const double g_tol = 0.00001; // error tolerance
  const double f_tol = 0.00001; // f error tolerance
  int info; // the reason for convergence if curve fitting succeeds

  size_t n_xs = Wosize_val (ocaml_xs);
  size_t n_ys = Wosize_val (ocaml_ys);
  size_t n = n_xs < n_ys ? n_xs : n_ys;
  size_t p = Wosize_val (ocaml_f_ks_init);

  double* xs = malloc (n * sizeof (double));
  double* ys = malloc (n * sizeof (double));
  for (size_t i = 0; i < n; i ++) {
    xs [i] = Double_field (ocaml_xs, i);
    ys [i] = Double_field (ocaml_ys, i);
  }

  ocaml_f_ks  = caml_alloc (p, Double_array_tag);
  ocaml_f_arg = caml_alloc (2, 0);

  struct ocaml_get_fit_nlinear_f_data f_params = {
    .n_xs  = n,
    .n_ks  = p,
    .f     = ocaml_f,
    .xs    = xs,
    .ys    = ys,
    .f_ks  = ocaml_f_ks,
    .f_arg = ocaml_f_arg
  };
  caml_register_global_root (&f_params.f);
  caml_register_global_root (&f_params.f_ks);
  caml_register_global_root (&f_params.f_arg);

  gsl_vector* ks_init = gsl_vector_alloc (p);
  for (size_t i = 0; i < p; i ++) {
    gsl_vector_set (ks_init, i, Double_field (ocaml_f_ks_init, i));
  }

  // configure the solver.
  const gsl_multifit_nlinear_type* T = gsl_multifit_nlinear_trust;

  gsl_multifit_nlinear_fdf fdf;
  fdf.f      = ocaml_get_fit_nlinear_f;
  fdf.df     = NULL; // use finite-difference Jacobin (slope approximation)
  fdf.fvv    = NULL; // disable geodesic acceleration
  fdf.n      = n;
  fdf.p      = p;
  fdf.params = &f_params;

  // allocate workspace with initial solver parameters.
  gsl_multifit_nlinear_workspace* w;

  gsl_multifit_nlinear_parameters fdf_params =
    gsl_multifit_nlinear_default_parameters();

  w = gsl_multifit_nlinear_alloc (T, &fdf_params, n, p);

  // initialize the solver.
  gsl_multifit_nlinear_init (ks_init, &fdf, w);

  int status = gsl_multifit_nlinear_driver (max_iter,
    x_tol, g_tol, f_tol,
    NULL, // no callback function
    NULL, // no callback function params
    &info, w
  );

  // get the estimate fitted function parameters.
  ocaml_f_ks_final = caml_alloc (p, Double_array_tag);
  for (size_t i = 0; i < p; i ++) {
    Store_double_field (ocaml_f_ks_final, i,
      gsl_vector_get (w->x, i));
  }

  // clean up allocated memory resources.
  gsl_multifit_nlinear_free (w);
  gsl_vector_free (ks_init);
  free (xs);
  free (ys);
  caml_remove_global_root (&f_params.f);
  caml_remove_global_root (&f_params.f_ks);
  caml_remove_global_root (&f_params.f_arg);

  CAMLreturn (ocaml_f_ks_final);
}