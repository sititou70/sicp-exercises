#ifndef ENVIRONMENT_H_INCLUDED
#define ENVIRONMENT_H_INCLUDED

#include "../machine/primitives.h"

lisp_value_t* enclosing_environment(lisp_value_t* env);
lisp_value_t* first_frame(lisp_value_t* env);

lisp_value_t* make_frame(lisp_value_t* variables, lisp_value_t* values);
lisp_value_t* frame_variables(lisp_value_t* frame);
lisp_value_t* frame_values(lisp_value_t* frame);
void add_binding_to_frame(lisp_value_t* var, lisp_value_t* val, lisp_value_t* frame);

lisp_value_t* extend_environment(lisp_value_t* vars, lisp_value_t* vals, lisp_value_t* base_env);

lisp_value_t* lookup_variable_value(lisp_value_t* var, lisp_value_t* env);

void set_variable_value(lisp_value_t* var, lisp_value_t* val, lisp_value_t* env);

void define_variable(lisp_value_t* var, lisp_value_t* val, lisp_value_t* env);

#endif
