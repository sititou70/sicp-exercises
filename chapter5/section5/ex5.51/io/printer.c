#include "printer.h"

#include <stdio.h>

// to string
int print_pair(lisp_value_t *value, char *buf) {
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

int print_number(lisp_value_t *value, char *buf) { return sprintf(buf, "%lf", value->number); }

int print_symbol(lisp_value_t *value, char *buf) { return sprintf(buf, "%s", value->symbol); }

int print_null(lisp_value_t *value, char *buf) { return sprintf(buf, "#NULL#"); }

int print_lisp_value(lisp_value_t *value, char *buf) {
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
  char buffer[1024] = {};
  print_lisp_value(value, buffer);
  printf("%s\n", buffer);
  return;
}
