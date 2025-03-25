#include <gsl_deriv.h>

double ocaml_deriv_central_f (double x, value* f) {
  return Double_val (caml_callback (*f, caml_copy_double (x)));
}

struct ocaml_deriv_central_t {
  double result;
  double abserr;
};

value ocaml_deriv_central (value f, value x, value h) {
  CAMLparam3 (f, x, h);
  CAMLlocal1 (res);
  struct ocaml_deriv_central_t* s = malloc (sizeof (struct ocaml_deriv_central_t));

  value* fp = malloc (sizeof (value*));
  caml_register_global_root (fp);
  *fp = f;

  gsl_function* F = malloc (sizeof (gsl_function));
  F->function = (double (*)(double, void *)) &ocaml_deriv_central_f;
  F->params   = (void*) fp;
  caml_register_global_root (F->params);

  gsl_deriv_central (F,
    Double_val (x),
    Double_val (h),
    &s->result,
    &s->abserr
  );
  res = caml_copy_double ((double) s->result);
  free (s);
  CAMLreturn (res);
}

