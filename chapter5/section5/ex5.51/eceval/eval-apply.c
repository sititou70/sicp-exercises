#include "eval-apply.h"

#include <stdio.h>

#include "../machine/registers.h"
#include "../machine/stack.h"
#include "../machine/utils.h"
#include "environment.h"
#include "expression.h"
#include "global-environment.h"
#include "procedure.h"

#define goto(dest) return dest
#define assign(dest, src) \
  dest = src;             \
  gc_check()
#define perform(x) x
#define test(res) \
  reg_flag = res; \
  gc_check()
#define branch(dest) \
  if (reg_flag) return dest
#define save(reg) push_stack(reg)
#define restore(reg) reg = pop_stack()

void* eval_dispatch(void) {
  test(is_self_evaluating(reg_exp));
  branch(ev_self_eval);
  test(is_variable(reg_exp));
  branch(ev_variable);
  test(is_quoted(reg_exp));
  branch(ev_quoted);
  test(is_assignment(reg_exp));
  branch(ev_assignment);
  test(is_definition(reg_exp));
  branch(ev_definition);
  test(is_if(reg_exp));
  branch(ev_if);
  test(is_lambda(reg_exp));
  branch(ev_lambda);
  test(is_begin(reg_exp));
  branch(ev_begin);
  test(is_cond(reg_exp));
  branch(ev_cond);
  test(is_let(reg_exp));
  branch(ev_let);
  test(is_application(reg_exp));
  branch(ev_application);
  goto(unknown_expression_type);
}

void* ev_self_eval(void) {
  assign(reg_val, reg_exp);
  goto(reg_continue->internal_label);
}

void* ev_variable(void) {
  assign(reg_val, lookup_variable_value(reg_exp, reg_env));
  goto(reg_continue->internal_label);
}

void* ev_quoted(void) {
  assign(reg_val, text_of_quotation(reg_exp));
  goto(reg_continue->internal_label);
}

void* ev_assignment(void) {
  assign(reg_unev, assignment_variable(reg_exp));
  save(reg_unev);  // 後で使うために変数を保存
  assign(reg_exp, assignment_value(reg_exp));
  save(reg_env);
  save(reg_continue);
  assign(reg_continue, make_internal_label(ev_assignment_1));
  goto(eval_dispatch);  // 代⼊値を評価
}
void* ev_assignment_1(void) {
  restore(reg_continue);
  restore(reg_env);
  restore(reg_unev);
  perform(set_variable_value(reg_unev, reg_val, reg_env));
  assign(reg_val, list(make_symbol("quote"), make_symbol("ok")));
  goto(reg_continue->internal_label);
}

void* ev_definition(void) {
  assign(reg_unev, definition_variable(reg_exp));
  save(reg_unev);  // 後で使うために変数を保存
  assign(reg_exp, definition_value(reg_exp));
  save(reg_env);
  save(reg_continue);
  assign(reg_continue, make_internal_label(ev_definition_1));
  goto(eval_dispatch);  // 定義値を評価
}
void* ev_definition_1(void) {
  restore(reg_continue);
  restore(reg_env);
  restore(reg_unev);
  perform(define_variable(reg_unev, reg_val, reg_env));
  assign(reg_val, list(make_symbol("quote"), make_symbol("ok")));
  goto(reg_continue->internal_label);
}

void* ev_if(void) {
  save(reg_exp);  // 後で使うために式を保存
  save(reg_env);
  save(reg_continue);
  assign(reg_continue, make_internal_label(ev_if_decide));
  assign(reg_exp, if_predicate(reg_exp));
  goto(eval_dispatch);  // 述語を評価
}
void* ev_if_decide(void) {
  restore(reg_continue);
  restore(reg_env);
  restore(reg_exp);
  test(is_true(reg_val));
  branch(ev_if_consequent);
  goto(ev_if_alternative);
}
void* ev_if_alternative(void) {
  assign(reg_exp, if_alternative(reg_exp));
  goto(eval_dispatch);
}
void* ev_if_consequent(void) {
  assign(reg_exp, if_consequent(reg_exp));
  goto(eval_dispatch);
}

void* ev_lambda(void) {
  assign(reg_unev, lambda_parameters(reg_exp));
  assign(reg_exp, lambda_body(reg_exp));
  assign(reg_val, make_procedure(reg_unev, reg_exp, reg_env));
  goto(reg_continue->internal_label);
}

void* ev_begin(void) {
  assign(reg_unev, begin_actions(reg_exp));
  save(reg_continue);
  goto(ev_sequence);
}

void* ev_cond(void) {
  assign(reg_exp, cond_to_if(reg_exp));
  goto(eval_dispatch);
}

void* ev_let(void) {
  assign(reg_exp, let_to_combination(reg_exp));
  goto(eval_dispatch);
}

void* ev_application(void) {
  save(reg_continue);
  save(reg_env);
  assign(reg_unev, operands(reg_exp));
  save(reg_unev);
  assign(reg_exp, operator(reg_exp));
  assign(reg_continue, make_internal_label(ev_appl_did_operator));
  goto(eval_dispatch);
}
void* ev_appl_did_operator(void) {
  restore(reg_unev);  // the operands
  restore(reg_env);
  assign(reg_argl, get_empty_arglist());
  assign(reg_proc, reg_val);  // the operator
  test(is_no_operands(reg_unev));
  branch(apply_dispatch);
  save(reg_proc);
  goto(ev_appl_operand_loop);
}
void* ev_appl_operand_loop(void) {
  save(reg_argl);
  assign(reg_exp, first_operand(reg_unev));
  test(is_last_operand(reg_unev));
  branch(ev_appl_last_arg);
  save(reg_env);
  save(reg_unev);
  assign(reg_continue, make_internal_label(ev_appl_accumulate_arg));
  goto(eval_dispatch);
}
void* ev_appl_accumulate_arg(void) {
  restore(reg_unev);
  restore(reg_env);
  restore(reg_argl);
  assign(reg_argl, adjoin_arg(reg_val, reg_argl));
  assign(reg_unev, rest_operands(reg_unev));
  goto(ev_appl_operand_loop);
}
void* ev_appl_last_arg(void) {
  assign(reg_continue, make_internal_label(ev_appl_accum_last_arg));
  goto(eval_dispatch);
}
void* ev_appl_accum_last_arg(void) {
  restore(reg_argl);
  assign(reg_argl, adjoin_arg(reg_val, reg_argl));
  restore(reg_proc);
  goto(apply_dispatch);
}

// eval utils
void* ev_sequence(void) {
  assign(reg_exp, first_exp(reg_unev));
  test(is_last_exp(reg_unev));
  branch(ev_sequence_last_exp);
  save(reg_unev);
  save(reg_env);
  assign(reg_continue, make_internal_label(ev_sequence_continue));
  goto(eval_dispatch);
}
void* ev_sequence_continue(void) {
  restore(reg_env);
  restore(reg_unev);
  assign(reg_unev, rest_exps(reg_unev));
  goto(ev_sequence);
}
void* ev_sequence_last_exp(void) {
  restore(reg_continue);
  goto(eval_dispatch);
}

// apply
void* apply_dispatch(void) {
  test(is_primitive_procedure(reg_proc));
  branch(primitive_apply);
  test(is_compound_procedure(reg_proc));
  branch(compound_apply);
  goto(unknown_procedure_type);
}

void* primitive_apply(void) {
  assign(reg_val, apply_primitive_procedure(reg_proc, reg_argl));
  restore(reg_continue);
  goto(reg_continue->internal_label);
}

void* compound_apply(void) {
  assign(reg_unev, procedure_parameters(reg_proc));
  assign(reg_env, procedure_environment(reg_proc));
  assign(reg_env, extend_environment(reg_unev, reg_argl, reg_env));
  assign(reg_unev, procedure_body(reg_proc));
  goto(ev_sequence);
}

// error
void* unknown_expression_type(void) {
  assign(reg_val, make_symbol("unknown_expression_type"));
  goto(NULL);
}
void* unknown_procedure_type(void) {
  assign(reg_val, make_symbol("unknown_procedure_type"));
  goto(NULL);
}

// eval-apply utils
bool is_true(lisp_value_t* val) { return !is_false(val); }
bool is_false(lisp_value_t* val) { return val->type == lisp_null_type; }

lisp_value_t* get_empty_arglist() { return make_null(); }
lisp_value_t* adjoin_arg(lisp_value_t* arg, lisp_value_t* arglist) { return append(arglist, list(arg)); }

bool is_last_operand(lisp_value_t* ops) { return cdr(ops)->type == lisp_null_type; }

lisp_value_t* eval(lisp_value_t* exp) {
  init_stack();
  reg_exp = exp;
  reg_env = get_global_environment();
  reg_continue = make_internal_label(NULL);

  void* (*next_label)() = eval_dispatch;

  while (next_label != NULL) {
    next_label = next_label();
  }

  return reg_val;
}
