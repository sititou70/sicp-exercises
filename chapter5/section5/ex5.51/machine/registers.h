#ifndef REGISTER_H_INCLUDED
#define REGISTER_H_INCLUDED

#include <stdbool.h>

#include "primitives.h"

extern tlisp_value_t *reg_exp;
extern tlisp_value_t *reg_env;
extern tlisp_value_t *reg_val;
extern tlisp_value_t *reg_proc;
extern tlisp_value_t *reg_argl;
extern tlisp_value_t *reg_continue;
extern tlisp_value_t *reg_unev;
extern bool reg_flag;

#endif
