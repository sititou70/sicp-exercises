#ifndef REGISTER_H_INCLUDED
#define REGISTER_H_INCLUDED

#include <stdbool.h>

#include "primitives.h"

extern lisp_value_t *reg_exp;
extern lisp_value_t *reg_env;
extern lisp_value_t *reg_val;
extern lisp_value_t *reg_proc;
extern lisp_value_t *reg_argl;
extern lisp_value_t *reg_continue;
extern lisp_value_t *reg_unev;
extern bool reg_flag;

#endif
