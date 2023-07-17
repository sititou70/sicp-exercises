#include "expression.h"

#include <stdbool.h>
#include <stdio.h>

#include "../machine/utils.h"
#include "tag.h"

bool is_self_evaluating(lisp_value_t* exp) {
  if (exp->type == lisp_number_type) return true;
  return false;
}

bool is_variable(lisp_value_t* exp) {
  if (exp->type == lisp_symbol_type) return true;
  return false;
}

bool is_quoted(lisp_value_t* exp) { return tagged_list(exp, "quote"); }
lisp_value_t* text_of_quotation(lisp_value_t* exp) { return cadr(exp); }

bool is_assignment(lisp_value_t* exp) { return tagged_list(exp, "set!"); }
lisp_value_t* assignment_variable(lisp_value_t* exp) { return cadr(exp); }
lisp_value_t* assignment_value(lisp_value_t* exp) { return caddr(exp); }

bool is_definition(lisp_value_t* exp) { return tagged_list(exp, "define"); }
lisp_value_t* definition_variable(lisp_value_t* exp) {
  if (cadr(exp)->type == lisp_symbol_type) {
    return cadr(exp);
  } else {
    return caadr(exp);
  }
}
lisp_value_t* definition_value(lisp_value_t* exp) {
  if (cadr(exp)->type == lisp_symbol_type) {
    return caddr(exp);
  } else {
    return make_lambda(cdadr(exp), cddr(exp));
  }
}

bool is_if(lisp_value_t* exp) { return tagged_list(exp, "if"); }
lisp_value_t* if_predicate(lisp_value_t* exp) { return cadr(exp); }
lisp_value_t* if_consequent(lisp_value_t* exp) { return caddr(exp); }
lisp_value_t* if_alternative(lisp_value_t* exp) {
  if (cdddr(exp)->type != lisp_null_type) {
    return cadddr(exp);
  } else {
    return make_symbol("false");
  }
}
lisp_value_t* make_if(lisp_value_t* predicate, lisp_value_t* consequent, lisp_value_t* alternative) {
  return list(make_symbol("if"), predicate, consequent, alternative);
}

bool is_lambda(lisp_value_t* exp) { return tagged_list(exp, "lambda"); }
lisp_value_t* lambda_parameters(lisp_value_t* exp) { return cadr(exp); }
lisp_value_t* lambda_body(lisp_value_t* exp) { return cddr(exp); }
lisp_value_t* make_lambda(lisp_value_t* parameters, lisp_value_t* body) {
  return cons(make_symbol("lambda"), cons(parameters, body));
}

bool is_begin(lisp_value_t* exp) { return tagged_list(exp, "begin"); }
lisp_value_t* begin_actions(lisp_value_t* exp) { return cdr(exp); }
bool is_last_exp(lisp_value_t* seq) { return cdr(seq)->type == lisp_null_type; }
lisp_value_t* first_exp(lisp_value_t* seq) { return car(seq); }
lisp_value_t* rest_exps(lisp_value_t* seq) { return cdr(seq); }
lisp_value_t* sequence_to_exp(lisp_value_t* seq) {
  if (seq->type == lisp_null_type) return seq;
  if (is_last_exp(seq)) return first_exp(seq);
  return make_begin(seq);
}
lisp_value_t* make_begin(lisp_value_t* seq) { return cons(make_symbol("begin"), seq); }

bool is_cond(lisp_value_t* exp) { return tagged_list(exp, "cond"); }
lisp_value_t* cond_clauses(lisp_value_t* exp) { return cdr(exp); }
bool is_cond_else_clause(lisp_value_t* clause) { return is_eq(cond_predicate(clause), make_symbol("else")); }
lisp_value_t* cond_predicate(lisp_value_t* clause) { return car(clause); }
lisp_value_t* cond_actions(lisp_value_t* clause) { return cdr(clause); }
lisp_value_t* cond_to_if(lisp_value_t* exp) { return expand_clauses(cond_clauses(exp)); }
lisp_value_t* expand_clauses(lisp_value_t* clauses) {
  if (clauses->type == lisp_null_type) return make_symbol("false");

  lisp_value_t* first = car(clauses);
  lisp_value_t* rest = cdr(clauses);

  if (is_cond_else_clause(first)) {
    if (rest->type == lisp_null_type) {
      return sequence_to_exp(cond_actions(first));
    } else {
      fprintf(stderr, "ELSE clause isn't last: COND->IF\n");
      exit(1);
    }
  } else {
    return make_if(cond_predicate(first), sequence_to_exp(cond_actions(first)), expand_clauses(rest));
  }
}

bool is_let(lisp_value_t* exp) { return tagged_list(exp, "let"); }
lisp_value_t* let_binds(lisp_value_t* exp) { return cadr(exp); }
lisp_value_t* let_body(lisp_value_t* exp) { return cddr(exp); }
lisp_value_t* let_to_combination(lisp_value_t* exp) {
  lisp_value_t* vars = map(car, let_binds(exp));
  lisp_value_t* exps = map(cadr, let_binds(exp));
  lisp_value_t* body = let_body(exp);

  return append(list(make_lambda(vars, body)), exps);
}

bool is_application(lisp_value_t* exp) { return exp->type == lisp_pair_type; }
lisp_value_t* operator(lisp_value_t* exp) { return car(exp); }
lisp_value_t* operands(lisp_value_t* exp) { return cdr(exp); }
bool is_no_operands(lisp_value_t* ops) { return ops->type == lisp_null_type; }
lisp_value_t* first_operand(lisp_value_t* ops) { return car(ops); }
lisp_value_t* rest_operands(lisp_value_t* ops) { return cdr(ops); }
