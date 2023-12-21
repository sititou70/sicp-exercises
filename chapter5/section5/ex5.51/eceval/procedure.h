#ifndef PROCEDURE_H_INCLUDED
#define PROCEDURE_H_INCLUDED

#include "../machine/primitives.h"

// primitive procedure
bool is_primitive_procedure(tlisp_value_t* proc);
tlisp_value_t* primitive_implementation(tlisp_value_t* proc);

void register_argv(char** v);
tlisp_value_t* get_primitive_procedures();

tlisp_value_t* get_primitive_procedure_names();
tlisp_value_t* get_primitive_procedure_objects_lambda(tlisp_value_t* tproc);
tlisp_value_t* get_primitive_procedure_objects();

tlisp_value_t* apply_primitive_procedure(tlisp_value_t* tproc, tlisp_value_t* targs);

// compound procedure
tlisp_value_t* make_procedure(tlisp_value_t* tparameters, tlisp_value_t* tbody, tlisp_value_t* tenv);
bool is_compound_procedure(tlisp_value_t* tproc);
tlisp_value_t* procedure_parameters(tlisp_value_t* tproc);
tlisp_value_t* procedure_body(tlisp_value_t* tproc);
tlisp_value_t* procedure_environment(tlisp_value_t* tproc);

// compiled procedure
tlisp_value_t* make_compiled_procedure(tlisp_value_t* tentry, tlisp_value_t* tenv);
bool is_compiled_procedure(tlisp_value_t* tproc);
tlisp_value_t* compiled_procedure_entry(tlisp_value_t* tcproc);
tlisp_value_t* compiled_procedure_env(tlisp_value_t* tcproc);

#endif
