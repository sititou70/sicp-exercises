#ifndef STACK_H_INCLUDED
#define STACK_H_INCLUDED

#include "primitives.h"

#define STACK_SIZE 128

extern lisp_value_t* stack[];
extern size_t stack_index;

void push_stack(lisp_value_t* value);
lisp_value_t* pop_stack();
void init_stack();

#endif
