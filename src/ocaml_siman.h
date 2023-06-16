#include <gsl_siman.h> // simulated annealing
#include <gsl_rng.h> // random number generator

double ocaml_siman_energy (value* xp) {
  return Double_val (caml_callback (Field (Field (*xp, 0), 1), *xp));
}

void ocaml_siman_step (const gsl_rng* rng, value* xp, double step_size) {
  CAMLparam0 ();
  CAMLlocal1 (step);
  step = caml_copy_double (step_size);
  *xp = caml_callback2 (Field (Field (*xp, 0), 2), *xp, step);
  CAMLreturn0;
}

double ocaml_siman_dist (value* xp, value* yp) {
  return Double_val (caml_callback2 (Field (Field (*xp, 0), 3), *xp, *yp));
}

void* ocaml_siman_copy (value* xp) {
  value* yp = malloc (sizeof (value*));
  caml_register_global_root (yp);
  *yp = caml_callback (Field (Field (*xp, 0), 0), *xp);
  return yp;
}

void ocaml_siman_copy_into (value* xp, value* yp) {
  *yp = caml_callback (Field (Field (*xp, 0), 0), *xp);
  return;
}

void ocaml_siman_destroy (value* xp) {
  caml_remove_global_root (xp);
  free (xp);
}

void ocaml_siman_print (value* xp) {
  CAMLparam0 ();
  CAMLlocal1 (print);
  print = Field (Field (*xp, 0), 4);
  if (Is_some (print)) {
    caml_callback (Field (print, 0), *xp);
  }
  CAMLreturn0;
}

CAMLprim value ocaml_siman_solve (value num_iters, value step_size, value state) {
  CAMLparam3 (num_iters, step_size, state);
  CAMLlocal1 (res);

  value* init = malloc (sizeof (value*));
  caml_register_global_root (init);
  *init = state;

  gsl_rng_env_setup ();
  gsl_rng* rng = gsl_rng_alloc (gsl_rng_default);

  gsl_siman_params_t params = {
    .n_tries = 200, // 200
    .iters_fixed_T = Int_val (num_iters), // 1000
    .step_size = Double_val (step_size), // 1
    .k = 1.0, // Boltzman constant
    .t_initial = 0.008, // initial temperature
    .mu_t = 1.003,
    .t_min = 2.0e-6 // minimum temperature
  };

  gsl_siman_print_t printer = NULL;
  if (Is_some (Field (Field (state, 0), 4))) {
    printer = ocaml_siman_print;
  }

  gsl_siman_solve (
    rng,
    (void*) init,
    (gsl_siman_Efunc_t) ocaml_siman_energy,
    (gsl_siman_step_t) ocaml_siman_step,
    (gsl_siman_metric_t) ocaml_siman_dist,
    printer,
    (gsl_siman_copy_t) ocaml_siman_copy_into,
    (gsl_siman_copy_construct_t) ocaml_siman_copy,
    (gsl_siman_destroy_t) ocaml_siman_destroy,
    0, // element size - signal variable.
    params
  );
  gsl_rng_free (rng);

  res = Field (*init, 1);
  ocaml_siman_destroy (init);
  CAMLreturn (res);
}