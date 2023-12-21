#include "utils.h"

#include <string.h>

tlisp_value_t *list_from_values(tlisp_value_t **tvalues) {
  size_t values_len = 0;
  while (tvalues[values_len] != NULL) {
    values_len++;
  }

  tlisp_value_t *tlist = make_null();
  for (int i = values_len - 1; i >= 0; i--) {
    tlist = cons(tvalues[i], tlist);
  }
  return tlist;
}

bool is_eq(tlisp_value_t *tv1, tlisp_value_t *tv2) {
  lisp_value_t *v1 = REM_TAG(tv1);
  lisp_value_t *v2 = REM_TAG(tv2);

  if (GET_TAG(tv1) != GET_TAG(tv2)) return false;

  switch (GET_TAG(tv1)) {
    case LISP_PAIR_TYPE:
      return is_eq(v1->car, v2->car) && is_eq(v1->cdr, v2->cdr);
    case LISP_NUMBER_TYPE:
      return v1->number == v2->number;
    case LISP_SYMBOL_TYPE:
      return strcmp(v1->symbol, v2->symbol) == 0;
    case LISP_NULL_TYPE:
      return true;
    default:
      return true;
  }
}

size_t length(tlisp_value_t *tlist) {
  size_t len = 0;
  while (GET_TAG(tlist) == LISP_PAIR_TYPE) {
    len++;
    tlist = cdr(tlist);
  }
  return len;
}

tlisp_value_t *map(tlisp_value_t *(*tproc)(tlisp_value_t *), tlisp_value_t *tlist) {
  size_t len = length(tlist);

  tlisp_value_t **new_tvals = malloc(sizeof(lisp_value_t *) * (len + 1));
  for (size_t i = 0; i < len; i++) {
    new_tvals[i] = tproc(car(tlist));
    tlist = cdr(tlist);
  }
  new_tvals[len] = NULL;

  return list_from_values(new_tvals);
}

tlisp_value_t *append(tlisp_value_t *tlist1, tlisp_value_t *tlist2) {
  size_t len1 = length(tlist1);
  size_t len2 = length(tlist2);

  tlisp_value_t **new_tvals = malloc(sizeof(lisp_value_t *) * (len1 + len2 + 1));
  for (size_t i = 0; i < len1; i++) {
    new_tvals[i] = car(tlist1);
    tlist1 = cdr(tlist1);
  }
  for (size_t i = 0; i < len2; i++) {
    new_tvals[len1 + i] = car(tlist2);
    tlist2 = cdr(tlist2);
  }
  new_tvals[len1 + len2] = NULL;

  return list_from_values(new_tvals);
}

// carcdring
tlisp_value_t *caar(tlisp_value_t *tpair) { return car(car(tpair)); }
tlisp_value_t *cadr(tlisp_value_t *tpair) { return car(cdr(tpair)); }
tlisp_value_t *cdar(tlisp_value_t *tpair) { return cdr(car(tpair)); }
tlisp_value_t *cddr(tlisp_value_t *tpair) { return cdr(cdr(tpair)); }

tlisp_value_t *caaar(tlisp_value_t *tpair) { return car(car(car(tpair))); }
tlisp_value_t *caadr(tlisp_value_t *tpair) { return car(car(cdr(tpair))); }
tlisp_value_t *cadar(tlisp_value_t *tpair) { return car(cdr(car(tpair))); }
tlisp_value_t *caddr(tlisp_value_t *tpair) { return car(cdr(cdr(tpair))); }
tlisp_value_t *cdaar(tlisp_value_t *tpair) { return cdr(car(car(tpair))); }
tlisp_value_t *cdadr(tlisp_value_t *tpair) { return cdr(car(cdr(tpair))); }
tlisp_value_t *cddar(tlisp_value_t *tpair) { return cdr(cdr(car(tpair))); }
tlisp_value_t *cdddr(tlisp_value_t *tpair) { return cdr(cdr(cdr(tpair))); }

tlisp_value_t *caaaar(tlisp_value_t *tpair) { return car(car(car(car(tpair)))); }
tlisp_value_t *caaadr(tlisp_value_t *tpair) { return car(car(car(cdr(tpair)))); }
tlisp_value_t *caadar(tlisp_value_t *tpair) { return car(car(cdr(car(tpair)))); }
tlisp_value_t *caaddr(tlisp_value_t *tpair) { return car(car(cdr(cdr(tpair)))); }
tlisp_value_t *cadaar(tlisp_value_t *tpair) { return car(cdr(car(car(tpair)))); }
tlisp_value_t *cadadr(tlisp_value_t *tpair) { return car(cdr(car(cdr(tpair)))); }
tlisp_value_t *caddar(tlisp_value_t *tpair) { return car(cdr(cdr(car(tpair)))); }
tlisp_value_t *cadddr(tlisp_value_t *tpair) { return car(cdr(cdr(cdr(tpair)))); }
tlisp_value_t *cdaaar(tlisp_value_t *tpair) { return cdr(car(car(car(tpair)))); }
tlisp_value_t *cdaadr(tlisp_value_t *tpair) { return cdr(car(car(cdr(tpair)))); }
tlisp_value_t *cdadar(tlisp_value_t *tpair) { return cdr(car(cdr(car(tpair)))); }
tlisp_value_t *cdaddr(tlisp_value_t *tpair) { return cdr(car(cdr(cdr(tpair)))); }
tlisp_value_t *cddaar(tlisp_value_t *tpair) { return cdr(cdr(car(car(tpair)))); }
tlisp_value_t *cddadr(tlisp_value_t *tpair) { return cdr(cdr(car(cdr(tpair)))); }
tlisp_value_t *cdddar(tlisp_value_t *tpair) { return cdr(cdr(cdr(car(tpair)))); }
tlisp_value_t *cddddr(tlisp_value_t *tpair) { return cdr(cdr(cdr(cdr(tpair)))); }
