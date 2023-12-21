#include "printer.h"

#include <stdbool.h>
#include <stdio.h>

// printed stack
#define PRINTED_STACK_SIZE 1024
tlisp_value_t *printed_stack[PRINTED_STACK_SIZE] = {};
// 次の空き領域、または空きがない状態を表す
size_t printed_stack_index = 0;
void push_printed(tlisp_value_t *tval) {
  if (printed_stack_index == PRINTED_STACK_SIZE) {
    fprintf(stderr, "push_printed: no free workspaces. gc failed.\n");
    exit(1);
  }

  printed_stack[printed_stack_index] = tval;
  printed_stack_index++;
  return;
}
void reset_printed_stack() {
  printed_stack_index = 0;
  return;
}
bool is_printed(tlisp_value_t *tval) {
  for (size_t i = 0; i < printed_stack_index; i++)
    if (printed_stack[i] == tval) return true;
  return false;
}

// print
int print_pair(tlisp_value_t *tvalue, char *buf) {
  push_printed(tvalue);

  char *orig_buf = buf;

  buf += sprintf(buf, "(");

  buf += print_lisp_value(car(tvalue), buf);

  while (GET_TAG(cdr(tvalue)) == LISP_PAIR_TYPE) {
    tvalue = cdr(tvalue);

    buf += sprintf(buf, " ");
    buf += print_lisp_value(car(tvalue), buf);
  }

  if (GET_TAG(cdr(tvalue)) != LISP_NULL_TYPE) {
    buf += sprintf(buf, " . ");
    buf += print_lisp_value(cdr(tvalue), buf);
  }

  buf += sprintf(buf, ")");

  return buf - orig_buf;
}

int print_number(tlisp_value_t *tvalue, char *buf) {
  push_printed(tvalue);
  return sprintf(buf, "%lf", REM_TAG(tvalue)->number);
}

int print_symbol(tlisp_value_t *tvalue, char *buf) {
  push_printed(tvalue);
  return sprintf(buf, "%s", REM_TAG(tvalue)->symbol);
}

int print_null(tlisp_value_t *tvalue, char *buf) {
  push_printed(tvalue);
  return sprintf(buf, "#NULL#");
}

int print_loop(char *buf) { return sprintf(buf, "<loop>"); }

int print_lisp_value(tlisp_value_t *tvalue, char *buf) {
  if (is_printed(tvalue)) return print_loop(buf);

  switch (GET_TAG(tvalue)) {
    case LISP_PAIR_TYPE:
      return print_pair(tvalue, buf);
    case LISP_NUMBER_TYPE:
      return print_number(tvalue, buf);
    case LISP_SYMBOL_TYPE:
      return print_symbol(tvalue, buf);
    case LISP_NULL_TYPE:
      return print_null(tvalue, buf);
    default:
      break;
  }

  return 0;
}

void printf_lisp_value(tlisp_value_t *tvalue) {
  char buffer[2048] = {};
  reset_printed_stack();
  print_lisp_value(tvalue, buffer);
  printf("%s\n", buffer);
  return;
}
