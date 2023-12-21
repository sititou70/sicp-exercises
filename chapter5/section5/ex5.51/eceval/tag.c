#include "tag.h"

#include <stdbool.h>
#include <string.h>

bool tagged_list(tlisp_value_t* texp, char* tag) {
  if (GET_TAG(texp) != LISP_PAIR_TYPE) return false;
  if (GET_TAG(car(texp)) != LISP_SYMBOL_TYPE) return false;
  if (strcmp(REM_TAG(car(texp))->symbol, tag) != 0) return false;
  return true;
}
