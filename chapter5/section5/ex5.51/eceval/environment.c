#include "environment.h"

#include <stdio.h>

#include "../machine/utils.h"

// environment: frameのリスト。レキシカル環境を表現する
tlisp_value_t* enclosing_environment(tlisp_value_t* tenv) { return cdr(tenv); }
tlisp_value_t* first_frame(tlisp_value_t* tenv) { return car(tenv); }

// frame: 変数リストと値リストのペア。束縛を表現する
tlisp_value_t* make_frame(tlisp_value_t* tvariables, tlisp_value_t* tvalues) { return cons(tvariables, tvalues); }
tlisp_value_t* frame_variables(tlisp_value_t* tframe) { return car(tframe); }
tlisp_value_t* frame_values(tlisp_value_t* tframe) { return cdr(tframe); }
void add_binding_to_frame(tlisp_value_t* tvar, tlisp_value_t* tval, tlisp_value_t* tframe) {
  set_car(tframe, cons(tvar, car(tframe)));
  set_cdr(tframe, cons(tval, cdr(tframe)));
}

// utils
tlisp_value_t* extend_environment(tlisp_value_t* tvars, tlisp_value_t* tvals, tlisp_value_t* tbase_env) {
  size_t vars_len = length(tvars);
  size_t vals_len = length(tvals);
  if (vars_len < vals_len) {
    fprintf(stderr, "Too many arguments supplied\n");
    exit(1);
  }
  if (vars_len > vals_len) {
    fprintf(stderr, "Too many arguments supplied\n");
    exit(1);
  }

  return cons(make_frame(tvars, tvals), tbase_env);
}

tlisp_value_t* lookup_variable_value(tlisp_value_t* tvar, tlisp_value_t* tenv) {
  while (1) {
    if (GET_TAG(tenv) == LISP_NULL_TYPE) break;

    tlisp_value_t* tvars = frame_variables(first_frame(tenv));
    tlisp_value_t* tvals = frame_values(first_frame(tenv));
    while (1) {
      if (GET_TAG(tvars) == LISP_NULL_TYPE) break;
      if (is_eq(tvar, car(tvars))) return car(tvals);

      tvars = cdr(tvars);
      tvals = cdr(tvals);
    }

    tenv = enclosing_environment(tenv);
  }

  fprintf(stderr, "Unbound variable: %s\n", REM_TAG(tvar)->symbol);
  exit(1);
}

void set_variable_value(tlisp_value_t* tvar, tlisp_value_t* tval, tlisp_value_t* tenv) {
  while (1) {
    if (GET_TAG(tenv) == LISP_NULL_TYPE) break;

    tlisp_value_t* tvars = frame_variables(first_frame(tenv));
    tlisp_value_t* tvals = frame_values(first_frame(tenv));
    while (1) {
      if (GET_TAG(tvars) == LISP_NULL_TYPE) break;
      if (is_eq(tvar, car(tvars))) {
        set_car(tvals, tval);
        return;
      }

      tvars = cdr(tvars);
      tvals = cdr(tvals);
    }

    tenv = enclosing_environment(tenv);
  }

  fprintf(stderr, "Unbound variable: %s\n", REM_TAG(tvar)->symbol);
  exit(1);
}

void define_variable(tlisp_value_t* tvar, tlisp_value_t* tval, tlisp_value_t* tenv) {
  tlisp_value_t* tframe = first_frame(tenv);
  tlisp_value_t* tvars = frame_variables(tframe);
  tlisp_value_t* tvals = frame_values(tframe);
  while (1) {
    if (GET_TAG(tvars) == LISP_NULL_TYPE) {
      add_binding_to_frame(tvar, tval, tframe);
      return;
    }
    if (is_eq(tvar, car(tvars))) {
      set_car(tvals, tval);
      return;
    }

    tvars = cdr(tvars);
    tvals = cdr(tvals);
  }
}
