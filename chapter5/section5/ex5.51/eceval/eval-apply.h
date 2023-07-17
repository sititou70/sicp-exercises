#ifndef EVAL_APPLY_H_INCLUDED
#define EVAL_APPLY_H_INCLUDED

#include "../machine/primitives.h"

void* eval_dispatch(void);
void* ev_self_eval(void);
void* ev_variable(void);
void* ev_quoted(void);
void* ev_assignment(void);
void* ev_assignment_1(void);
void* ev_definition(void);
void* ev_definition_1(void);
void* ev_if(void);
void* ev_if_decide(void);
void* ev_if_alternative(void);
void* ev_if_consequent(void);
void* ev_lambda(void);
void* ev_begin(void);
void* ev_cond(void);
void* ev_let(void);
void* ev_application(void);
void* ev_appl_did_operator(void);
void* ev_appl_operand_loop(void);
void* ev_appl_accumulate_arg(void);
void* ev_appl_last_arg(void);
void* ev_appl_accum_last_arg(void);
void* ev_sequence(void);
void* ev_sequence_continue(void);
void* ev_sequence_last_exp(void);
void* apply_dispatch(void);
void* primitive_apply(void);
void* compound_apply(void);
void* unknown_expression_type(void);
void* unknown_procedure_type(void);

bool is_true(lisp_value_t* val);
bool is_false(lisp_value_t* val);

lisp_value_t* get_empty_arglist();
lisp_value_t* adjoin_arg(lisp_value_t* arg, lisp_value_t* arglist);

bool is_last_operand(lisp_value_t* ops);

lisp_value_t* eval(lisp_value_t* exp);

#endif
