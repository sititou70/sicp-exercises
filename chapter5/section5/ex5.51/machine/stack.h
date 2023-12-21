#ifndef STACK_H_INCLUDED
#define STACK_H_INCLUDED

#include "primitives.h"

#define STACK_SIZE 256

extern tlisp_value_t* stack[];
extern size_t stack_index;

void push_stack(tlisp_value_t* tvalue);
tlisp_value_t* pop_stack();
void init_stack();

#endif
