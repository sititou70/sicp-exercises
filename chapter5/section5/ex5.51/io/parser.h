#ifndef PARSER_H_INCLUDED
#define PARSER_H_INCLUDED

#include "../machine/primitives.h"

char* parse_lisp_value(char* s, tlisp_value_t** result);

#endif
