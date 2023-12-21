#ifndef EXPRESSION_H_INCLUDED
#define EXPRESSION_H_INCLUDED

#include "../machine/primitives.h"

bool is_self_evaluating(tlisp_value_t* texp);

bool is_variable(tlisp_value_t* texp);

bool is_quoted(tlisp_value_t* texp);
tlisp_value_t* text_of_quotation(tlisp_value_t* texp);

bool is_assignment(tlisp_value_t* texp);
tlisp_value_t* assignment_variable(tlisp_value_t* texp);
tlisp_value_t* assignment_value(tlisp_value_t* texp);

bool is_definition(tlisp_value_t* texp);
tlisp_value_t* definition_variable(tlisp_value_t* texp);
tlisp_value_t* definition_value(tlisp_value_t* texp);

bool is_if(tlisp_value_t* texp);
tlisp_value_t* if_predicate(tlisp_value_t* texp);
tlisp_value_t* if_consequent(tlisp_value_t* texp);
tlisp_value_t* if_alternative(tlisp_value_t* texp);
tlisp_value_t* make_if(tlisp_value_t* tpredicate, tlisp_value_t* tconsequent, tlisp_value_t* talternative);

bool is_lambda(tlisp_value_t* texp);
tlisp_value_t* lambda_parameters(tlisp_value_t* texp);
tlisp_value_t* lambda_body(tlisp_value_t* texp);
tlisp_value_t* make_lambda(tlisp_value_t* tparameters, tlisp_value_t* tbody);

bool is_begin(tlisp_value_t* texp);
tlisp_value_t* begin_actions(tlisp_value_t* texp);
bool is_last_exp(tlisp_value_t* tseq);
tlisp_value_t* first_exp(tlisp_value_t* tseq);
tlisp_value_t* rest_exps(tlisp_value_t* tseq);
tlisp_value_t* sequence_to_exp(tlisp_value_t* tseq);
tlisp_value_t* make_begin(tlisp_value_t* tseq);

bool is_cond(tlisp_value_t* texp);
tlisp_value_t* cond_clauses(tlisp_value_t* texp);
bool is_cond_else_clause(tlisp_value_t* tclause);
tlisp_value_t* cond_predicate(tlisp_value_t* tclause);
tlisp_value_t* cond_actions(tlisp_value_t* tclause);
tlisp_value_t* cond_to_if(tlisp_value_t* texp);
tlisp_value_t* expand_clauses(tlisp_value_t* tclauses);

bool is_let(tlisp_value_t* texp);
tlisp_value_t* let_binds(tlisp_value_t* texp);
tlisp_value_t* let_body(tlisp_value_t* texp);
tlisp_value_t* let_to_combination(tlisp_value_t* texp);

bool is_application(tlisp_value_t* texp);
tlisp_value_t* operator(tlisp_value_t* texp);
tlisp_value_t* operands(tlisp_value_t* texp);
bool is_no_operands(tlisp_value_t* tops);
tlisp_value_t* first_operand(tlisp_value_t* tops);
tlisp_value_t* rest_operands(tlisp_value_t* tops);

#endif
