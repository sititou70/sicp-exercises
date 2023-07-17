#include "utils.h"

#include <string.h>

lisp_value_t *list_from_values(lisp_value_t **values) {
  size_t values_len = 0;
  while (values[values_len] != NULL) {
    values_len++;
  }

  lisp_value_t *list = make_null();
  for (int i = values_len - 1; i >= 0; i--) {
    list = cons(values[i], list);
  }
  return list;
}

bool is_eq(lisp_value_t *v1, lisp_value_t *v2) {
  if (v1->type != v2->type) return false;

  switch (v1->type) {
    case lisp_pair_type:
      return is_eq(v1->car, v2->car) && is_eq(v1->cdr, v2->cdr);
    case lisp_number_type:
      return v1->number == v2->number;
    case lisp_symbol_type:
      return strcmp(v1->symbol, v2->symbol) == 0;
    case lisp_null_type:
      return true;
    default:
      return true;
  }
}

size_t length(lisp_value_t *list) {
  size_t len = 0;
  while (list->type == lisp_pair_type) {
    len++;
    list = cdr(list);
  }
  return len;
}

lisp_value_t *map(lisp_value_t *(*proc)(lisp_value_t *), lisp_value_t *list) {
  size_t len = length(list);

  lisp_value_t **new_list = malloc(sizeof(lisp_value_t *) * (len + 1));
  for (size_t i = 0; i < len; i++) {
    new_list[i] = proc(car(list));
    list = cdr(list);
  }
  new_list[len] = NULL;

  return list_from_values(new_list);
}

lisp_value_t *append(lisp_value_t *list1, lisp_value_t *list2) {
  size_t len1 = length(list1);
  size_t len2 = length(list2);

  lisp_value_t **new_list = malloc(sizeof(lisp_value_t *) * (len1 + len2 + 1));
  for (size_t i = 0; i < len1; i++) {
    new_list[i] = car(list1);
    list1 = cdr(list1);
  }
  for (size_t i = 0; i < len2; i++) {
    new_list[len1 + i] = car(list2);
    list2 = cdr(list2);
  }
  new_list[len1 + len2] = NULL;

  return list_from_values(new_list);
}

// carcdring
lisp_value_t *caar(lisp_value_t *pair) { return car(car(pair)); }
lisp_value_t *cadr(lisp_value_t *pair) { return car(cdr(pair)); }
lisp_value_t *cdar(lisp_value_t *pair) { return cdr(car(pair)); }
lisp_value_t *cddr(lisp_value_t *pair) { return cdr(cdr(pair)); }

lisp_value_t *caaar(lisp_value_t *pair) { return car(car(car(pair))); }
lisp_value_t *caadr(lisp_value_t *pair) { return car(car(cdr(pair))); }
lisp_value_t *cadar(lisp_value_t *pair) { return car(cdr(car(pair))); }
lisp_value_t *caddr(lisp_value_t *pair) { return car(cdr(cdr(pair))); }
lisp_value_t *cdaar(lisp_value_t *pair) { return cdr(car(car(pair))); }
lisp_value_t *cdadr(lisp_value_t *pair) { return cdr(car(cdr(pair))); }
lisp_value_t *cddar(lisp_value_t *pair) { return cdr(cdr(car(pair))); }
lisp_value_t *cdddr(lisp_value_t *pair) { return cdr(cdr(cdr(pair))); }

lisp_value_t *caaaar(lisp_value_t *pair) { return car(car(car(car(pair)))); }
lisp_value_t *caaadr(lisp_value_t *pair) { return car(car(car(cdr(pair)))); }
lisp_value_t *caadar(lisp_value_t *pair) { return car(car(cdr(car(pair)))); }
lisp_value_t *caaddr(lisp_value_t *pair) { return car(car(cdr(cdr(pair)))); }
lisp_value_t *cadaar(lisp_value_t *pair) { return car(cdr(car(car(pair)))); }
lisp_value_t *cadadr(lisp_value_t *pair) { return car(cdr(car(cdr(pair)))); }
lisp_value_t *caddar(lisp_value_t *pair) { return car(cdr(cdr(car(pair)))); }
lisp_value_t *cadddr(lisp_value_t *pair) { return car(cdr(cdr(cdr(pair)))); }
lisp_value_t *cdaaar(lisp_value_t *pair) { return cdr(car(car(car(pair)))); }
lisp_value_t *cdaadr(lisp_value_t *pair) { return cdr(car(car(cdr(pair)))); }
lisp_value_t *cdadar(lisp_value_t *pair) { return cdr(car(cdr(car(pair)))); }
lisp_value_t *cdaddr(lisp_value_t *pair) { return cdr(car(cdr(cdr(pair)))); }
lisp_value_t *cddaar(lisp_value_t *pair) { return cdr(cdr(car(car(pair)))); }
lisp_value_t *cddadr(lisp_value_t *pair) { return cdr(cdr(car(cdr(pair)))); }
lisp_value_t *cdddar(lisp_value_t *pair) { return cdr(cdr(cdr(car(pair)))); }
lisp_value_t *cddddr(lisp_value_t *pair) { return cdr(cdr(cdr(cdr(pair)))); }
