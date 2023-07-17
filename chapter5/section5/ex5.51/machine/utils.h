#ifndef UTILS_H_INCLUDED
#define UTILS_H_INCLUDED

#include <stdbool.h>

#include "primitives.h"

#define list(...) list_from_values((lisp_value_t *[]){__VA_ARGS__, NULL})
lisp_value_t *list_from_values(lisp_value_t **values);
bool is_eq(lisp_value_t *v1, lisp_value_t *v2);
size_t length(lisp_value_t *list);
lisp_value_t *map(lisp_value_t *(*proc)(lisp_value_t *), lisp_value_t *list);
lisp_value_t *append(lisp_value_t *list1, lisp_value_t *list2);

// carcdring
lisp_value_t *caar(lisp_value_t *pair);
lisp_value_t *cadr(lisp_value_t *pair);
lisp_value_t *cdar(lisp_value_t *pair);
lisp_value_t *cddr(lisp_value_t *pair);

lisp_value_t *caaar(lisp_value_t *pair);
lisp_value_t *caadr(lisp_value_t *pair);
lisp_value_t *cadar(lisp_value_t *pair);
lisp_value_t *caddr(lisp_value_t *pair);
lisp_value_t *cdaar(lisp_value_t *pair);
lisp_value_t *cdadr(lisp_value_t *pair);
lisp_value_t *cddar(lisp_value_t *pair);
lisp_value_t *cdddr(lisp_value_t *pair);

lisp_value_t *caaaar(lisp_value_t *pair);
lisp_value_t *caaadr(lisp_value_t *pair);
lisp_value_t *caadar(lisp_value_t *pair);
lisp_value_t *caaddr(lisp_value_t *pair);
lisp_value_t *cadaar(lisp_value_t *pair);
lisp_value_t *cadadr(lisp_value_t *pair);
lisp_value_t *caddar(lisp_value_t *pair);
lisp_value_t *cadddr(lisp_value_t *pair);
lisp_value_t *cdaaar(lisp_value_t *pair);
lisp_value_t *cdaadr(lisp_value_t *pair);
lisp_value_t *cdadar(lisp_value_t *pair);
lisp_value_t *cdaddr(lisp_value_t *pair);
lisp_value_t *cddaar(lisp_value_t *pair);
lisp_value_t *cddadr(lisp_value_t *pair);
lisp_value_t *cdddar(lisp_value_t *pair);
lisp_value_t *cddddr(lisp_value_t *pair);

#endif
