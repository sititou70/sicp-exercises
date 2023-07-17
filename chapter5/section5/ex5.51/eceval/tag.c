#include "tag.h"

#include <stdbool.h>
#include <string.h>

bool tagged_list(lisp_value_t* exp, char* tag) {
  if (exp->type != lisp_pair_type) return false;
  if (car(exp)->type != lisp_symbol_type) return false;
  if (strcmp(car(exp)->symbol, tag) != 0) return false;
  return true;
}
