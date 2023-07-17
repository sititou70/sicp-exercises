#ifndef PRINTER_H_INCLUDED
#define PRINTER_H_INCLUDED

#include "../machine/primitives.h"

int print_lisp_value(lisp_value_t *value, char *buf);
void printf_lisp_value(lisp_value_t *value);

#endif
