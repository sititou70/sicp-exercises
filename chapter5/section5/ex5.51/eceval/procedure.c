#include "procedure.h"

#include <stdbool.h>
#include <stdio.h>
#include <sys/stat.h>

#include "../io/parser.h"
#include "../io/printer.h"
#include "../machine/utils.h"
#include "tag.h"

// primitive procedure
bool is_primitive_procedure(tlisp_value_t* tproc) { return tagged_list(tproc, "primitive"); }
tlisp_value_t* primitive_implementation(tlisp_value_t* tproc) { return cadr(tproc); }

//// primitive base apply
tlisp_value_t* primitive_base_apply(tlisp_value_t* targs) {
  return REM_TAG(car(targs))->internal_primitive_procedure(cadr(targs));
}

//// pair
tlisp_value_t* primitive_car(tlisp_value_t* targs) { return car(car(targs)); }
tlisp_value_t* primitive_cdr(tlisp_value_t* targs) { return cdr(car(targs)); }
tlisp_value_t* primitive_cons(tlisp_value_t* targs) { return cons(car(targs), cadr(targs)); }
tlisp_value_t* primitive_set_car(tlisp_value_t* targs) {
  set_car(car(targs), cadr(targs));
  return make_null();
}
tlisp_value_t* primitive_set_cdr(tlisp_value_t* targs) {
  set_cdr(car(targs), cadr(targs));
  return make_null();
}
tlisp_value_t* primitive_is_pair(tlisp_value_t* targs) {
  if (GET_TAG(car(targs)) == LISP_PAIR_TYPE) return make_symbol("true");
  return make_null();
}
////// carcdring
tlisp_value_t* primitive_caar(tlisp_value_t* targs) { return caar(car(targs)); }
tlisp_value_t* primitive_cadr(tlisp_value_t* targs) { return cadr(car(targs)); }
tlisp_value_t* primitive_cdar(tlisp_value_t* targs) { return cdar(car(targs)); }
tlisp_value_t* primitive_cddr(tlisp_value_t* targs) { return cddr(car(targs)); }
tlisp_value_t* primitive_caaar(tlisp_value_t* targs) { return caaar(car(targs)); }
tlisp_value_t* primitive_caadr(tlisp_value_t* targs) { return caadr(car(targs)); }
tlisp_value_t* primitive_cadar(tlisp_value_t* targs) { return cadar(car(targs)); }
tlisp_value_t* primitive_caddr(tlisp_value_t* targs) { return caddr(car(targs)); }
tlisp_value_t* primitive_cdaar(tlisp_value_t* targs) { return cdaar(car(targs)); }
tlisp_value_t* primitive_cdadr(tlisp_value_t* targs) { return cdadr(car(targs)); }
tlisp_value_t* primitive_cddar(tlisp_value_t* targs) { return cddar(car(targs)); }
tlisp_value_t* primitive_cdddr(tlisp_value_t* targs) { return cdddr(car(targs)); }
tlisp_value_t* primitive_caaaar(tlisp_value_t* targs) { return caaaar(car(targs)); }
tlisp_value_t* primitive_caaadr(tlisp_value_t* targs) { return caaadr(car(targs)); }
tlisp_value_t* primitive_caadar(tlisp_value_t* targs) { return caadar(car(targs)); }
tlisp_value_t* primitive_caaddr(tlisp_value_t* targs) { return caaddr(car(targs)); }
tlisp_value_t* primitive_cadaar(tlisp_value_t* targs) { return cadaar(car(targs)); }
tlisp_value_t* primitive_cadadr(tlisp_value_t* targs) { return cadadr(car(targs)); }
tlisp_value_t* primitive_caddar(tlisp_value_t* targs) { return caddar(car(targs)); }
tlisp_value_t* primitive_cadddr(tlisp_value_t* targs) { return cadddr(car(targs)); }
tlisp_value_t* primitive_cdaaar(tlisp_value_t* targs) { return cdaaar(car(targs)); }
tlisp_value_t* primitive_cdaadr(tlisp_value_t* targs) { return cdaadr(car(targs)); }
tlisp_value_t* primitive_cdadar(tlisp_value_t* targs) { return cdadar(car(targs)); }
tlisp_value_t* primitive_cdaddr(tlisp_value_t* targs) { return cdaddr(car(targs)); }
tlisp_value_t* primitive_cddaar(tlisp_value_t* targs) { return cddaar(car(targs)); }
tlisp_value_t* primitive_cddadr(tlisp_value_t* targs) { return cddadr(car(targs)); }
tlisp_value_t* primitive_cdddar(tlisp_value_t* targs) { return cdddar(car(targs)); }
tlisp_value_t* primitive_cddddr(tlisp_value_t* targs) { return cddddr(car(targs)); }

//// 算術演算
tlisp_value_t* primitive_add(tlisp_value_t* targs) {
  return make_number(REM_TAG(car(targs))->number + REM_TAG(cadr(targs))->number);
}
tlisp_value_t* primitive_sub(tlisp_value_t* targs) {
  return make_number(REM_TAG(car(targs))->number - REM_TAG(cadr(targs))->number);
}
tlisp_value_t* primitive_is_lessthan(tlisp_value_t* targs) {
  if (REM_TAG(car(targs))->number < REM_TAG(cadr(targs))->number) return make_symbol("true");
  return make_null();
}

//// 論理演算
tlisp_value_t* primitive_not(tlisp_value_t* targs) {
  if (GET_TAG(car(targs)) == LISP_NULL_TYPE) return make_symbol("true");
  return make_null();
}

//// types
tlisp_value_t* primitive_is_number(tlisp_value_t* targs) {
  if (GET_TAG(car(targs)) == LISP_NUMBER_TYPE) return make_symbol("true");
  return make_null();
}
tlisp_value_t* primitive_is_symbol(tlisp_value_t* targs) {
  if (GET_TAG(car(targs)) == LISP_SYMBOL_TYPE) return make_symbol("true");
  return make_null();
}
tlisp_value_t* primitive_is_null(tlisp_value_t* targs) {
  if (GET_TAG(car(targs)) == LISP_NULL_TYPE) return make_symbol("true");
  return make_null();
}

//// utils
tlisp_value_t* primitive_list(tlisp_value_t* targs) { return targs; }
tlisp_value_t* primitive_length(tlisp_value_t* targs) { return make_number(length(car(targs))); }
tlisp_value_t* primitive_is_eq(tlisp_value_t* targs) {
  if (is_eq(car(targs), cadr(targs))) return make_symbol("true");
  return make_null();
}
tlisp_value_t* primitive_displayln(tlisp_value_t* targs) {
  printf_lisp_value(car(targs));
  return make_null();
}
tlisp_value_t* primitive_error(tlisp_value_t* targs) {
  printf_lisp_value(car(targs));
  exit(1);
}
char** argv = NULL;
void register_argv(char** v) { argv = v; }
tlisp_value_t* primitive_read_from_file(tlisp_value_t* targs) {
  struct stat statBuf;
  stat(argv[1], &statBuf);
  FILE* file = fopen(argv[1], "r");

  char* input_buffer = malloc(sizeof(char) * (statBuf.st_size + 1));
  input_buffer[statBuf.st_size] = '\0';
  fread(input_buffer, sizeof(char), statBuf.st_size, file);

  tlisp_value_t* parse_result = NULL;
  char* input = input_buffer;

  // racketの#langショートハンドを飛ばす
  while (*input != '\n') {
    input++;
  }
  input++;

  parse_lisp_value(input, &parse_result);

  fclose(file);
  free(input_buffer);

  return parse_result;
}

tlisp_value_t* get_primitive_procedures() {
  tlisp_value_t* primitive_procedures = list(                                                              //
      list(make_symbol("primitive-base-apply"), make_internal_primitive_procedure(primitive_base_apply)),  //
      // pair
      list(make_symbol("car"), make_internal_primitive_procedure(primitive_car)),           //
      list(make_symbol("cdr"), make_internal_primitive_procedure(primitive_cdr)),           //
      list(make_symbol("cons"), make_internal_primitive_procedure(primitive_cons)),         //
      list(make_symbol("set-car!"), make_internal_primitive_procedure(primitive_set_car)),  //
      list(make_symbol("set-cdr!"), make_internal_primitive_procedure(primitive_set_cdr)),  //
      list(make_symbol("pair?"), make_internal_primitive_procedure(primitive_is_pair)),     //
      //// carcdring
      list(make_symbol("caar"), make_internal_primitive_procedure(primitive_caar)),      //
      list(make_symbol("cadr"), make_internal_primitive_procedure(primitive_cadr)),      //
      list(make_symbol("cdar"), make_internal_primitive_procedure(primitive_cdar)),      //
      list(make_symbol("cddr"), make_internal_primitive_procedure(primitive_cddr)),      //
      list(make_symbol("caaar"), make_internal_primitive_procedure(primitive_caaar)),    //
      list(make_symbol("caadr"), make_internal_primitive_procedure(primitive_caadr)),    //
      list(make_symbol("cadar"), make_internal_primitive_procedure(primitive_cadar)),    //
      list(make_symbol("caddr"), make_internal_primitive_procedure(primitive_caddr)),    //
      list(make_symbol("cdaar"), make_internal_primitive_procedure(primitive_cdaar)),    //
      list(make_symbol("cdadr"), make_internal_primitive_procedure(primitive_cdadr)),    //
      list(make_symbol("cddar"), make_internal_primitive_procedure(primitive_cddar)),    //
      list(make_symbol("cdddr"), make_internal_primitive_procedure(primitive_cdddr)),    //
      list(make_symbol("caaaar"), make_internal_primitive_procedure(primitive_caaaar)),  //
      list(make_symbol("caaadr"), make_internal_primitive_procedure(primitive_caaadr)),  //
      list(make_symbol("caadar"), make_internal_primitive_procedure(primitive_caadar)),  //
      list(make_symbol("caaddr"), make_internal_primitive_procedure(primitive_caaddr)),  //
      list(make_symbol("cadaar"), make_internal_primitive_procedure(primitive_cadaar)),  //
      list(make_symbol("cadadr"), make_internal_primitive_procedure(primitive_cadadr)),  //
      list(make_symbol("caddar"), make_internal_primitive_procedure(primitive_caddar)),  //
      list(make_symbol("cadddr"), make_internal_primitive_procedure(primitive_cadddr)),  //
      list(make_symbol("cdaaar"), make_internal_primitive_procedure(primitive_cdaaar)),  //
      list(make_symbol("cdaadr"), make_internal_primitive_procedure(primitive_cdaadr)),  //
      list(make_symbol("cdadar"), make_internal_primitive_procedure(primitive_cdadar)),  //
      list(make_symbol("cdaddr"), make_internal_primitive_procedure(primitive_cdaddr)),  //
      list(make_symbol("cddaar"), make_internal_primitive_procedure(primitive_cddaar)),  //
      list(make_symbol("cddadr"), make_internal_primitive_procedure(primitive_cddadr)),  //
      list(make_symbol("cdddar"), make_internal_primitive_procedure(primitive_cdddar)),  //
      list(make_symbol("cddddr"), make_internal_primitive_procedure(primitive_cddddr)),  //

      // 算術演算
      list(make_symbol("="), make_internal_primitive_procedure(primitive_is_eq)),        //
      list(make_symbol("<"), make_internal_primitive_procedure(primitive_is_lessthan)),  //
      list(make_symbol("+"), make_internal_primitive_procedure(primitive_add)),          //
      list(make_symbol("-"), make_internal_primitive_procedure(primitive_sub)),          //

      //// 論理演算
      list(make_symbol("not"), make_internal_primitive_procedure(primitive_not)),  //

      // types
      list(make_symbol("number?"), make_internal_primitive_procedure(primitive_is_number)),  //
      list(make_symbol("symbol?"), make_internal_primitive_procedure(primitive_is_symbol)),  //
      list(make_symbol("null?"), make_internal_primitive_procedure(primitive_is_null)),      //

      // utils
      list(make_symbol("list"), make_internal_primitive_procedure(primitive_list)),                     //
      list(make_symbol("length"), make_internal_primitive_procedure(primitive_length)),                 //
      list(make_symbol("eq?"), make_internal_primitive_procedure(primitive_is_eq)),                     //
      list(make_symbol("displayln"), make_internal_primitive_procedure(primitive_displayln)),           //
      list(make_symbol("error"), make_internal_primitive_procedure(primitive_error)),                   //
      list(make_symbol("read-from-file"), make_internal_primitive_procedure(primitive_read_from_file))  //
  );
  return primitive_procedures;
}

tlisp_value_t* get_primitive_procedure_names() { return map(car, get_primitive_procedures()); }
tlisp_value_t* get_primitive_procedure_objects_lambda(tlisp_value_t* tproc) {
  return list(make_symbol("primitive"), cadr(tproc));
}
tlisp_value_t* get_primitive_procedure_objects() {
  return map(get_primitive_procedure_objects_lambda, get_primitive_procedures());
}

tlisp_value_t* apply_primitive_procedure(tlisp_value_t* tproc, tlisp_value_t* targs) {
  return REM_TAG(primitive_implementation(tproc))->internal_primitive_procedure(targs);
}

// compound procedure
tlisp_value_t* make_procedure(tlisp_value_t* tparameters, tlisp_value_t* tbody, tlisp_value_t* tenv) {
  return list(make_symbol("procedure"), tparameters, tbody, tenv);
}
bool is_compound_procedure(tlisp_value_t* tproc) { return tagged_list(tproc, "procedure"); }
tlisp_value_t* procedure_parameters(tlisp_value_t* tproc) { return cadr(tproc); }
tlisp_value_t* procedure_body(tlisp_value_t* tproc) { return caddr(tproc); }
tlisp_value_t* procedure_environment(tlisp_value_t* tproc) { return cadddr(tproc); }

// compiled procedure: ex5.52で必要
tlisp_value_t* make_compiled_procedure(tlisp_value_t* tentry, tlisp_value_t* tenv) {
  return list(make_symbol("compiled-procedure"), tentry, tenv);
}
bool is_compiled_procedure(tlisp_value_t* tproc) { return tagged_list(tproc, "compiled-procedure"); }
tlisp_value_t* compiled_procedure_entry(tlisp_value_t* tcproc) { return cadr(tcproc); }
tlisp_value_t* compiled_procedure_env(tlisp_value_t* tcproc) { return caddr(tcproc); }
