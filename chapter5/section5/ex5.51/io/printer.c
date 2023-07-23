#include "printer.h"

#include <stdbool.h>
#include <stdio.h>

// printed stack
#define PRINTED_STACK_SIZE 1024
lisp_value_t *printed_stack[PRINTED_STACK_SIZE] = {};
// 次の空き領域、または空きがない状態を表す
size_t printed_stack_index = 0;
void push_printed(lisp_value_t *val) {
  if (printed_stack_index == PRINTED_STACK_SIZE) {
    fprintf(stderr, "push_printed: no free workspaces. gc failed.\n");
    exit(1);
  }

  printed_stack[printed_stack_index] = val;
  printed_stack_index++;
  return;
}
void reset_printed_stack() {
  printed_stack_index = 0;
  return;
}
bool is_printed(lisp_value_t *val) {
  for (size_t i = 0; i < printed_stack_index; i++)
    if (printed_stack[i] == val) return true;
  return false;
}

// print
int print_pair(lisp_value_t *value, char *buf) {
  push_printed(value);

  char *orig_buf = buf;

  buf += sprintf(buf, "(");

  buf += print_lisp_value(car(value), buf);

  while (cdr(value)->type == lisp_pair_type) {
    value = cdr(value);

    buf += sprintf(buf, " ");
    buf += print_lisp_value(car(value), buf);
  }

  if (cdr(value)->type != lisp_null_type) {
    buf += sprintf(buf, " . ");
    buf += print_lisp_value(cdr(value), buf);
  }

  buf += sprintf(buf, ")");

  return buf - orig_buf;
}

int print_number(lisp_value_t *value, char *buf) {
  push_printed(value);
  return sprintf(buf, "%lf", value->number);
}

int print_symbol(lisp_value_t *value, char *buf) {
  push_printed(value);
  return sprintf(buf, "%s", value->symbol);
}

int print_null(lisp_value_t *value, char *buf) {
  push_printed(value);
  return sprintf(buf, "#NULL#");
}

int print_loop(char *buf) { return sprintf(buf, "<loop>"); }

int print_lisp_value(lisp_value_t *value, char *buf) {
  if (is_printed(value)) return print_loop(buf);

  switch (value->type) {
    case lisp_pair_type:
      return print_pair(value, buf);
    case lisp_number_type:
      return print_number(value, buf);
    case lisp_symbol_type:
      return print_symbol(value, buf);
    case lisp_null_type:
      return print_null(value, buf);
    default:
      break;
  }

  return 0;
}

void printf_lisp_value(lisp_value_t *value) {
  char buffer[2048] = {};
  reset_printed_stack();
  print_lisp_value(value, buffer);
  printf("%s\n", buffer);
  return;
}
