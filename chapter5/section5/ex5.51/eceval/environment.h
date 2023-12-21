#ifndef ENVIRONMENT_H_INCLUDED
#define ENVIRONMENT_H_INCLUDED

#include "../machine/primitives.h"

tlisp_value_t* enclosing_environment(tlisp_value_t* tenv);
tlisp_value_t* first_frame(tlisp_value_t* tenv);

tlisp_value_t* make_frame(tlisp_value_t* tvariables, tlisp_value_t* tvalues);
tlisp_value_t* frame_variables(tlisp_value_t* tframe);
tlisp_value_t* frame_values(tlisp_value_t* tframe);
void add_binding_to_frame(tlisp_value_t* tvar, tlisp_value_t* tval, tlisp_value_t* tframe);

tlisp_value_t* extend_environment(tlisp_value_t* tvars, tlisp_value_t* tvals, tlisp_value_t* tbase_env);

tlisp_value_t* lookup_variable_value(tlisp_value_t* tvar, tlisp_value_t* tenv);

void set_variable_value(tlisp_value_t* tvar, tlisp_value_t* tval, tlisp_value_t* tenv);

void define_variable(tlisp_value_t* tvar, tlisp_value_t* tval, tlisp_value_t* tenv);

#endif
