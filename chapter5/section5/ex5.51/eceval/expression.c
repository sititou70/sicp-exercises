#include "expression.h"

#include <stdbool.h>
#include <stdio.h>

#include "../machine/utils.h"
#include "tag.h"

bool is_self_evaluating(tlisp_value_t* texp) {
  if (GET_TAG(texp) == LISP_NUMBER_TYPE) return true;
  return false;
}

bool is_variable(tlisp_value_t* texp) {
  if (GET_TAG(texp) == LISP_SYMBOL_TYPE) return true;
  return false;
}

bool is_quoted(tlisp_value_t* texp) { return tagged_list(texp, "quote"); }
tlisp_value_t* text_of_quotation(tlisp_value_t* texp) { return cadr(texp); }

bool is_assignment(tlisp_value_t* texp) { return tagged_list(texp, "set!"); }
tlisp_value_t* assignment_variable(tlisp_value_t* texp) { return cadr(texp); }
tlisp_value_t* assignment_value(tlisp_value_t* texp) { return caddr(texp); }

bool is_definition(tlisp_value_t* texp) { return tagged_list(texp, "define"); }
tlisp_value_t* definition_variable(tlisp_value_t* texp) {
  if (GET_TAG(cadr(texp)) == LISP_SYMBOL_TYPE) {
    return cadr(texp);
  } else {
    return caadr(texp);
  }
}
tlisp_value_t* definition_value(tlisp_value_t* texp) {
  if (GET_TAG(cadr(texp)) == LISP_SYMBOL_TYPE) {
    return caddr(texp);
  } else {
    return make_lambda(cdadr(texp), cddr(texp));
  }
}

bool is_if(tlisp_value_t* texp) { return tagged_list(texp, "if"); }
tlisp_value_t* if_predicate(tlisp_value_t* texp) { return cadr(texp); }
tlisp_value_t* if_consequent(tlisp_value_t* texp) { return caddr(texp); }
tlisp_value_t* if_alternative(tlisp_value_t* texp) {
  if (GET_TAG(cdddr(texp)) != LISP_NULL_TYPE) {
    return cadddr(texp);
  } else {
    return make_symbol("false");
  }
}
tlisp_value_t* make_if(tlisp_value_t* tpredicate, tlisp_value_t* tconsequent, tlisp_value_t* talternative) {
  return list(make_symbol("if"), tpredicate, tconsequent, talternative);
}

bool is_lambda(tlisp_value_t* texp) { return tagged_list(texp, "lambda"); }
tlisp_value_t* lambda_parameters(tlisp_value_t* texp) { return cadr(texp); }
tlisp_value_t* lambda_body(tlisp_value_t* texp) { return cddr(texp); }
tlisp_value_t* make_lambda(tlisp_value_t* tparameters, tlisp_value_t* tbody) {
  return cons(make_symbol("lambda"), cons(tparameters, tbody));
}

bool is_begin(tlisp_value_t* texp) { return tagged_list(texp, "begin"); }
tlisp_value_t* begin_actions(tlisp_value_t* texp) { return cdr(texp); }
bool is_last_exp(tlisp_value_t* tseq) { return GET_TAG(cdr(tseq)) == LISP_NULL_TYPE; }
tlisp_value_t* first_exp(tlisp_value_t* tseq) { return car(tseq); }
tlisp_value_t* rest_exps(tlisp_value_t* tseq) { return cdr(tseq); }
tlisp_value_t* sequence_to_exp(tlisp_value_t* tseq) {
  if (GET_TAG(tseq) == LISP_NULL_TYPE) return tseq;
  if (is_last_exp(tseq)) return first_exp(tseq);
  return make_begin(tseq);
}
tlisp_value_t* make_begin(tlisp_value_t* tseq) { return cons(make_symbol("begin"), tseq); }

bool is_cond(tlisp_value_t* texp) { return tagged_list(texp, "cond"); }
tlisp_value_t* cond_clauses(tlisp_value_t* texp) { return cdr(texp); }
bool is_cond_else_clause(tlisp_value_t* tclause) { return is_eq(cond_predicate(tclause), make_symbol("else")); }
tlisp_value_t* cond_predicate(tlisp_value_t* tclause) { return car(tclause); }
tlisp_value_t* cond_actions(tlisp_value_t* tclause) { return cdr(tclause); }
tlisp_value_t* cond_to_if(tlisp_value_t* texp) { return expand_clauses(cond_clauses(texp)); }
tlisp_value_t* expand_clauses(tlisp_value_t* tclauses) {
  if (GET_TAG(tclauses) == LISP_NULL_TYPE) return make_symbol("false");

  tlisp_value_t* tfirst = car(tclauses);
  tlisp_value_t* trest = cdr(tclauses);

  if (is_cond_else_clause(tfirst)) {
    if (GET_TAG(trest) == LISP_NULL_TYPE) {
      return sequence_to_exp(cond_actions(tfirst));
    } else {
      fprintf(stderr, "ELSE clause isn't last: COND->IF\n");
      exit(1);
    }
  } else {
    return make_if(cond_predicate(tfirst), sequence_to_exp(cond_actions(tfirst)), expand_clauses(trest));
  }
}

bool is_let(tlisp_value_t* texp) { return tagged_list(texp, "let"); }
tlisp_value_t* let_binds(tlisp_value_t* texp) { return cadr(texp); }
tlisp_value_t* let_body(tlisp_value_t* texp) { return cddr(texp); }
tlisp_value_t* let_to_combination(tlisp_value_t* texp) {
  tlisp_value_t* tvars = map(car, let_binds(texp));
  tlisp_value_t* texps = map(cadr, let_binds(texp));
  tlisp_value_t* tbody = let_body(texp);

  return append(list(make_lambda(tvars, tbody)), texps);
}

bool is_application(tlisp_value_t* texp) { return GET_TAG(texp) == LISP_PAIR_TYPE; }
tlisp_value_t* operator(tlisp_value_t* texp) { return car(texp); }
tlisp_value_t* operands(tlisp_value_t* texp) { return cdr(texp); }
bool is_no_operands(tlisp_value_t* tops) { return GET_TAG(tops) == LISP_NULL_TYPE; }
tlisp_value_t* first_operand(tlisp_value_t* tops) { return car(tops); }
tlisp_value_t* rest_operands(tlisp_value_t* tops) { return cdr(tops); }
