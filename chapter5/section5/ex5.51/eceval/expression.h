#ifndef EXPRESSION_H_INCLUDED
#define EXPRESSION_H_INCLUDED

#include "../machine/primitives.h"

bool is_self_evaluating(lisp_value_t* exp);

bool is_variable(lisp_value_t* exp);

bool is_quoted(lisp_value_t* exp);
lisp_value_t* text_of_quotation(lisp_value_t* exp);

bool is_assignment(lisp_value_t* exp);
lisp_value_t* assignment_variable(lisp_value_t* exp);
lisp_value_t* assignment_value(lisp_value_t* exp);

bool is_definition(lisp_value_t* exp);
lisp_value_t* definition_variable(lisp_value_t* exp);
lisp_value_t* definition_value(lisp_value_t* exp);

bool is_if(lisp_value_t* exp);
lisp_value_t* if_predicate(lisp_value_t* exp);
lisp_value_t* if_consequent(lisp_value_t* exp);
lisp_value_t* if_alternative(lisp_value_t* exp);
lisp_value_t* make_if(lisp_value_t* predicate, lisp_value_t* consequent, lisp_value_t* alternative);

bool is_lambda(lisp_value_t* exp);
lisp_value_t* lambda_parameters(lisp_value_t* exp);
lisp_value_t* lambda_body(lisp_value_t* exp);
lisp_value_t* make_lambda(lisp_value_t* parameters, lisp_value_t* body);

bool is_begin(lisp_value_t* exp);
lisp_value_t* begin_actions(lisp_value_t* exp);
bool is_last_exp(lisp_value_t* seq);
lisp_value_t* first_exp(lisp_value_t* seq);
lisp_value_t* rest_exps(lisp_value_t* seq);
lisp_value_t* sequence_to_exp(lisp_value_t* seq);
lisp_value_t* make_begin(lisp_value_t* seq);

bool is_cond(lisp_value_t* exp);
lisp_value_t* cond_clauses(lisp_value_t* exp);
bool is_cond_else_clause(lisp_value_t* clause);
lisp_value_t* cond_predicate(lisp_value_t* clause);
lisp_value_t* cond_actions(lisp_value_t* clause);
lisp_value_t* cond_to_if(lisp_value_t* exp);
lisp_value_t* expand_clauses(lisp_value_t* clauses);

bool is_let(lisp_value_t* exp);
lisp_value_t* let_binds(lisp_value_t* exp);
lisp_value_t* let_body(lisp_value_t* exp);
lisp_value_t* let_to_combination(lisp_value_t* exp);

bool is_application(lisp_value_t* exp);
lisp_value_t* operator(lisp_value_t* exp);
lisp_value_t* operands(lisp_value_t* exp);
bool is_no_operands(lisp_value_t* ops);
lisp_value_t* first_operand(lisp_value_t* ops);
lisp_value_t* rest_operands(lisp_value_t* ops);

#endif
