#include "procedure.h"

#include <stdbool.h>

#include "../io/printer.h"
#include "../machine/utils.h"
#include "tag.h"

// primitive procedure
bool is_primitive_procedure(lisp_value_t* proc) { return tagged_list(proc, "primitive"); }
lisp_value_t* primitive_implementation(lisp_value_t* proc) { return cadr(proc); }

lisp_value_t* primitive_car(lisp_value_t* args) { return car(car(args)); }
lisp_value_t* primitive_cdr(lisp_value_t* args) { return cdr(car(args)); }
lisp_value_t* primitive_cons(lisp_value_t* args) { return cons(car(args), cadr(args)); }
lisp_value_t* primitive_is_null(lisp_value_t* args) {
  if (car(args)->type == lisp_null_type) return make_symbol("true");
  return make_null();
}
lisp_value_t* primitive_is_eq(lisp_value_t* args) {
  if (is_eq(car(args), cadr(args))) return make_symbol("true");
  return make_null();
}
lisp_value_t* primitive_add(lisp_value_t* args) { return make_number(car(args)->number + cadr(args)->number); }
lisp_value_t* primitive_sub(lisp_value_t* args) { return make_number(car(args)->number - cadr(args)->number); }
lisp_value_t* primitive_is_lessthan(lisp_value_t* args) {
  if (car(args)->number < cadr(args)->number) return make_symbol("true");
  return make_null();
}
lisp_value_t* primitive_displayln(lisp_value_t* args) {
  printf_lisp_value(car(args));
  return make_null();
}

lisp_value_t* get_primitive_procedures() {
  lisp_value_t* primitive_procedures = list(                                                  //
      list(make_symbol("car"), make_internal_primitive_procedure(primitive_car)),             //
      list(make_symbol("cdr"), make_internal_primitive_procedure(primitive_cdr)),             //
      list(make_symbol("cons"), make_internal_primitive_procedure(primitive_cons)),           //
      list(make_symbol("null?"), make_internal_primitive_procedure(primitive_is_null)),       //
      list(make_symbol("="), make_internal_primitive_procedure(primitive_is_eq)),             //
      list(make_symbol("<"), make_internal_primitive_procedure(primitive_is_lessthan)),       //
      list(make_symbol("+"), make_internal_primitive_procedure(primitive_add)),               //
      list(make_symbol("-"), make_internal_primitive_procedure(primitive_sub)),               //
      list(make_symbol("displayln"), make_internal_primitive_procedure(primitive_displayln))  //
  );
  return primitive_procedures;
}

lisp_value_t* get_primitive_procedure_names() { return map(car, get_primitive_procedures()); }
lisp_value_t* get_primitive_procedure_objects_lambda(lisp_value_t* proc) {
  return list(make_symbol("primitive"), cadr(proc));
}
lisp_value_t* get_primitive_procedure_objects() {
  return map(get_primitive_procedure_objects_lambda, get_primitive_procedures());
}

lisp_value_t* apply_primitive_procedure(lisp_value_t* proc, lisp_value_t* args) {
  return primitive_implementation(proc)->internal_primitive_procedure(args);
}

// compound procedure
lisp_value_t* make_procedure(lisp_value_t* parameters, lisp_value_t* body, lisp_value_t* env) {
  return list(make_symbol("procedure"), parameters, body, env);
}
bool is_compound_procedure(lisp_value_t* p) { return tagged_list(p, "procedure"); }
lisp_value_t* procedure_parameters(lisp_value_t* p) { return cadr(p); }
lisp_value_t* procedure_body(lisp_value_t* p) { return caddr(p); }
lisp_value_t* procedure_environment(lisp_value_t* p) { return cadddr(p); }
