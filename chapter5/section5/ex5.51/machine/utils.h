#ifndef UTILS_H_INCLUDED
#define UTILS_H_INCLUDED

#include <stdbool.h>

#include "primitives.h"

#define list(...) list_from_values((tlisp_value_t *[]){__VA_ARGS__, NULL})
tlisp_value_t *list_from_values(tlisp_value_t **tvalues);
bool is_eq(tlisp_value_t *tv1, tlisp_value_t *tv2);
size_t length(tlisp_value_t *tlist);
tlisp_value_t *map(tlisp_value_t *(*tproc)(tlisp_value_t *), tlisp_value_t *tlist);
tlisp_value_t *append(tlisp_value_t *tlist1, tlisp_value_t *tlist2);

// carcdring
tlisp_value_t *caar(tlisp_value_t *tpair);
tlisp_value_t *cadr(tlisp_value_t *tpair);
tlisp_value_t *cdar(tlisp_value_t *tpair);
tlisp_value_t *cddr(tlisp_value_t *tpair);

tlisp_value_t *caaar(tlisp_value_t *tpair);
tlisp_value_t *caadr(tlisp_value_t *tpair);
tlisp_value_t *cadar(tlisp_value_t *tpair);
tlisp_value_t *caddr(tlisp_value_t *tpair);
tlisp_value_t *cdaar(tlisp_value_t *tpair);
tlisp_value_t *cdadr(tlisp_value_t *tpair);
tlisp_value_t *cddar(tlisp_value_t *tpair);
tlisp_value_t *cdddr(tlisp_value_t *tpair);

tlisp_value_t *caaaar(tlisp_value_t *tpair);
tlisp_value_t *caaadr(tlisp_value_t *tpair);
tlisp_value_t *caadar(tlisp_value_t *tpair);
tlisp_value_t *caaddr(tlisp_value_t *tpair);
tlisp_value_t *cadaar(tlisp_value_t *tpair);
tlisp_value_t *cadadr(tlisp_value_t *tpair);
tlisp_value_t *caddar(tlisp_value_t *tpair);
tlisp_value_t *cadddr(tlisp_value_t *tpair);
tlisp_value_t *cdaaar(tlisp_value_t *tpair);
tlisp_value_t *cdaadr(tlisp_value_t *tpair);
tlisp_value_t *cdadar(tlisp_value_t *tpair);
tlisp_value_t *cdaddr(tlisp_value_t *tpair);
tlisp_value_t *cddaar(tlisp_value_t *tpair);
tlisp_value_t *cddadr(tlisp_value_t *tpair);
tlisp_value_t *cdddar(tlisp_value_t *tpair);
tlisp_value_t *cddddr(tlisp_value_t *tpair);

#endif
