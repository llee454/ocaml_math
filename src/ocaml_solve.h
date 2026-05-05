#include <stdio.h>
#include <math.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_roots.h>

double ocaml_solve_bisection_fn (double x, void *f) {
  return Double_val (caml_callback (*(value *) f, caml_copy_double (x)));
}

CAMLprim value ocaml_solve_bisection (value params, value f) {
  CAMLparam2 (params, f);
  CAMLlocal1 (result);

  const double lower  = Double_val (Field(params, 0));
  const double upper  = Double_val (Field(params, 1));
  const int max_iters = Int_val    (Field(params, 2));
  const double epsabs = Double_val (Field(params, 3));

  gsl_function F = {
    .function = ocaml_solve_bisection_fn,
    .params = &f
  };

  const gsl_root_fsolver_type *solver_type = gsl_root_fsolver_bisection;
  gsl_root_fsolver* solver = gsl_root_fsolver_alloc (solver_type);
  int status = gsl_root_fsolver_set (solver, &F, lower, upper);
  if (status) goto error;

  double est_lower, est_upper;
  double est_err = fabs (upper - lower);

  size_t i = 1;
  for (; i < (size_t) max_iters && est_err >= epsabs; i++) {
    status = gsl_root_fsolver_iterate (solver);
    if (status) goto error;

    est_lower = gsl_root_fsolver_x_lower (solver);
    est_upper = gsl_root_fsolver_x_upper (solver);
    est_err = fabs (est_upper - est_lower);
  }

  result = caml_alloc (3, 0);
  Store_field (result, 0, caml_copy_double (gsl_root_fsolver_root (solver)));
  Store_field (result, 1, caml_copy_double (est_err));
  Store_field (result, 2, Val_int (i));

  gsl_root_fsolver_free (solver);
  CAMLreturn (result);

error:
  gsl_root_fsolver_free (solver);
  GSL_ERROR ("[ocaml_solve_bisection] failed", status);
}
