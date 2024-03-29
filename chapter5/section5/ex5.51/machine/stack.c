#include "stack.h"

#include <stdio.h>

tlisp_value_t* stack[STACK_SIZE] = {};
// 空き領域、または空き領域がない（STACK_SIZE）ことを表す
size_t stack_index = 0;

void push_stack(tlisp_value_t* tvalue) {
  if (stack_index == STACK_SIZE) {
    fprintf(stderr, "stack: overflow\n");
    exit(1);
  }

  stack[stack_index] = tvalue;
  stack_index++;
}

tlisp_value_t* pop_stack() {
  if (stack_index == 0) {
    fprintf(stderr, "stack: no content\n");
    exit(1);
  }

  stack_index--;
  return stack[stack_index];
}

void init_stack() { stack_index = 0; }
