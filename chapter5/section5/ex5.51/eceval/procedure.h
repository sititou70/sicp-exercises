#ifndef PROCEDURE_H_INCLUDED
#define PROCEDURE_H_INCLUDED

#include "../machine/primitives.h"

// primitive procedure
bool is_primitive_procedure(lisp_value_t* proc);
lisp_value_t* primitive_implementation(lisp_value_t* proc);

lisp_value_t* get_primitive_procedures();

lisp_value_t* get_primitive_procedure_names();
lisp_value_t* get_primitive_procedure_objects_lambda(lisp_value_t* proc);
lisp_value_t* get_primitive_procedure_objects();

lisp_value_t* apply_primitive_procedure(lisp_value_t* proc, lisp_value_t* args);

// compound_procedure
lisp_value_t* make_procedure(lisp_value_t* parameters, lisp_value_t* body, lisp_value_t* env);
bool is_compound_procedure(lisp_value_t* p);
lisp_value_t* procedure_parameters(lisp_value_t* p);
lisp_value_t* procedure_body(lisp_value_t* p);
lisp_value_t* procedure_environment(lisp_value_t* p);

#endif
