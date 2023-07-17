#include "environment.h"

#include <stdio.h>

#include "../machine/utils.h"

// environment: frameのリスト。レキシカル環境を表現する
lisp_value_t* enclosing_environment(lisp_value_t* env) { return cdr(env); }
lisp_value_t* first_frame(lisp_value_t* env) { return car(env); }

// frame: 変数リストと値リストのペア。束縛を表現する
lisp_value_t* make_frame(lisp_value_t* variables, lisp_value_t* values) { return cons(variables, values); }
lisp_value_t* frame_variables(lisp_value_t* frame) { return car(frame); }
lisp_value_t* frame_values(lisp_value_t* frame) { return cdr(frame); }
void add_binding_to_frame(lisp_value_t* var, lisp_value_t* val, lisp_value_t* frame) {
  set_car(frame, cons(var, car(frame)));
  set_cdr(frame, cons(val, cdr(frame)));
}

// utils
lisp_value_t* extend_environment(lisp_value_t* vars, lisp_value_t* vals, lisp_value_t* base_env) {
  size_t vars_len = length(vars);
  size_t vals_len = length(vals);
  if (vars_len < vals_len) {
    fprintf(stderr, "Too many arguments supplied\n");
    exit(1);
  }
  if (vars_len > vals_len) {
    fprintf(stderr, "Too many arguments supplied\n");
    exit(1);
  }

  return cons(make_frame(vars, vals), base_env);
}

lisp_value_t* lookup_variable_value(lisp_value_t* var, lisp_value_t* env) {
  while (1) {
    if (env->type == lisp_null_type) break;

    lisp_value_t* vars = frame_variables(first_frame(env));
    lisp_value_t* vals = frame_values(first_frame(env));
    while (1) {
      if (vars->type == lisp_null_type) break;
      if (is_eq(var, car(vars))) return car(vals);

      vars = cdr(vars);
      vals = cdr(vals);
    }

    env = enclosing_environment(env);
  }

  fprintf(stderr, "Unbound variable: %s\n", var->symbol);
  exit(1);
}

void set_variable_value(lisp_value_t* var, lisp_value_t* val, lisp_value_t* env) {
  while (1) {
    if (env->type == lisp_null_type) break;

    lisp_value_t* vars = frame_variables(first_frame(env));
    lisp_value_t* vals = frame_values(first_frame(env));
    while (1) {
      if (vars->type == lisp_null_type) break;
      if (is_eq(var, car(vars))) {
        set_car(vals, val);
        return;
      }

      vars = cdr(vars);
      vals = cdr(vals);
    }

    env = enclosing_environment(env);
  }

  fprintf(stderr, "Unbound variable: %s\n", var->symbol);
  exit(1);
}

void define_variable(lisp_value_t* var, lisp_value_t* val, lisp_value_t* env) {
  lisp_value_t* frame = first_frame(env);
  lisp_value_t* vars = frame_variables(frame);
  lisp_value_t* vals = frame_values(frame);
  while (1) {
    if (vars->type == lisp_null_type) {
      add_binding_to_frame(var, val, frame);
      return;
    }
    if (is_eq(var, car(vars))) {
      set_car(vals, val);
      return;
    }

    vars = cdr(vars);
    vals = cdr(vals);
  }
}
