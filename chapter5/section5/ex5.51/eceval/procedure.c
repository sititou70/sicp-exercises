#include "procedure.h"

#include <stdbool.h>
#include <stdio.h>
#include <sys/stat.h>

#include "../io/parser.h"
#include "../io/printer.h"
#include "../machine/utils.h"
#include "tag.h"

// primitive procedure
bool is_primitive_procedure(lisp_value_t* proc) { return tagged_list(proc, "primitive"); }
lisp_value_t* primitive_implementation(lisp_value_t* proc) { return cadr(proc); }

//// primitive base apply
lisp_value_t* primitive_base_apply(lisp_value_t* args) { return car(args)->internal_primitive_procedure(cadr(args)); }

//// pair
lisp_value_t* primitive_car(lisp_value_t* args) { return car(car(args)); }
lisp_value_t* primitive_cdr(lisp_value_t* args) { return cdr(car(args)); }
lisp_value_t* primitive_cons(lisp_value_t* args) { return cons(car(args), cadr(args)); }
lisp_value_t* primitive_set_car(lisp_value_t* args) {
  set_car(car(args), cadr(args));
  return make_null();
}
lisp_value_t* primitive_set_cdr(lisp_value_t* args) {
  set_cdr(car(args), cadr(args));
  return make_null();
}
lisp_value_t* primitive_is_pair(lisp_value_t* args) {
  if (car(args)->type == lisp_pair_type) return make_symbol("true");
  return make_null();
}
////// carcdring
lisp_value_t* primitive_caar(lisp_value_t* args) { return caar(car(args)); }
lisp_value_t* primitive_cadr(lisp_value_t* args) { return cadr(car(args)); }
lisp_value_t* primitive_cdar(lisp_value_t* args) { return cdar(car(args)); }
lisp_value_t* primitive_cddr(lisp_value_t* args) { return cddr(car(args)); }
lisp_value_t* primitive_caaar(lisp_value_t* args) { return caaar(car(args)); }
lisp_value_t* primitive_caadr(lisp_value_t* args) { return caadr(car(args)); }
lisp_value_t* primitive_cadar(lisp_value_t* args) { return cadar(car(args)); }
lisp_value_t* primitive_caddr(lisp_value_t* args) { return caddr(car(args)); }
lisp_value_t* primitive_cdaar(lisp_value_t* args) { return cdaar(car(args)); }
lisp_value_t* primitive_cdadr(lisp_value_t* args) { return cdadr(car(args)); }
lisp_value_t* primitive_cddar(lisp_value_t* args) { return cddar(car(args)); }
lisp_value_t* primitive_cdddr(lisp_value_t* args) { return cdddr(car(args)); }
lisp_value_t* primitive_caaaar(lisp_value_t* args) { return caaaar(car(args)); }
lisp_value_t* primitive_caaadr(lisp_value_t* args) { return caaadr(car(args)); }
lisp_value_t* primitive_caadar(lisp_value_t* args) { return caadar(car(args)); }
lisp_value_t* primitive_caaddr(lisp_value_t* args) { return caaddr(car(args)); }
lisp_value_t* primitive_cadaar(lisp_value_t* args) { return cadaar(car(args)); }
lisp_value_t* primitive_cadadr(lisp_value_t* args) { return cadadr(car(args)); }
lisp_value_t* primitive_caddar(lisp_value_t* args) { return caddar(car(args)); }
lisp_value_t* primitive_cadddr(lisp_value_t* args) { return cadddr(car(args)); }
lisp_value_t* primitive_cdaaar(lisp_value_t* args) { return cdaaar(car(args)); }
lisp_value_t* primitive_cdaadr(lisp_value_t* args) { return cdaadr(car(args)); }
lisp_value_t* primitive_cdadar(lisp_value_t* args) { return cdadar(car(args)); }
lisp_value_t* primitive_cdaddr(lisp_value_t* args) { return cdaddr(car(args)); }
lisp_value_t* primitive_cddaar(lisp_value_t* args) { return cddaar(car(args)); }
lisp_value_t* primitive_cddadr(lisp_value_t* args) { return cddadr(car(args)); }
lisp_value_t* primitive_cdddar(lisp_value_t* args) { return cdddar(car(args)); }
lisp_value_t* primitive_cddddr(lisp_value_t* args) { return cddddr(car(args)); }

//// 算術演算
lisp_value_t* primitive_add(lisp_value_t* args) { return make_number(car(args)->number + cadr(args)->number); }
lisp_value_t* primitive_sub(lisp_value_t* args) { return make_number(car(args)->number - cadr(args)->number); }
lisp_value_t* primitive_is_lessthan(lisp_value_t* args) {
  if (car(args)->number < cadr(args)->number) return make_symbol("true");
  return make_null();
}

//// 論理演算
lisp_value_t* primitive_not(lisp_value_t* args) {
  if (car(args)->type == lisp_null_type) return make_symbol("true");
  return make_null();
}

//// types
lisp_value_t* primitive_is_number(lisp_value_t* args) {
  if (car(args)->type == lisp_number_type) return make_symbol("true");
  return make_null();
}
lisp_value_t* primitive_is_symbol(lisp_value_t* args) {
  if (car(args)->type == lisp_symbol_type) return make_symbol("true");
  return make_null();
}
lisp_value_t* primitive_is_null(lisp_value_t* args) {
  if (car(args)->type == lisp_null_type) return make_symbol("true");
  return make_null();
}

//// utils
lisp_value_t* primitive_list(lisp_value_t* args) { return args; }
lisp_value_t* primitive_length(lisp_value_t* args) { return make_number(length(car(args))); }
lisp_value_t* primitive_is_eq(lisp_value_t* args) {
  if (is_eq(car(args), cadr(args))) return make_symbol("true");
  return make_null();
}
lisp_value_t* primitive_displayln(lisp_value_t* args) {
  printf_lisp_value(car(args));
  return make_null();
}
lisp_value_t* primitive_error(lisp_value_t* args) {
  printf_lisp_value(car(args));
  exit(1);
}
char** argv = NULL;
void register_argv(char** v) { argv = v; }
lisp_value_t* primitive_read_from_file(lisp_value_t* args) {
  struct stat statBuf;
  stat(argv[1], &statBuf);
  FILE* file = fopen(argv[1], "r");

  char* input_buffer = malloc(sizeof(char) * (statBuf.st_size + 1));
  input_buffer[statBuf.st_size] = '\0';
  fread(input_buffer, sizeof(char), statBuf.st_size, file);

  lisp_value_t* parse_result = NULL;
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

lisp_value_t* get_primitive_procedures() {
  lisp_value_t* primitive_procedures = list(                                                               //
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

// compiled procedure: ex5.52で必要
lisp_value_t* make_compiled_procedure(lisp_value_t* entry, lisp_value_t* env) {
  return list(make_symbol("compiled-procedure"), entry, env);
}
bool is_compiled_procedure(lisp_value_t* proc) { return tagged_list(proc, "compiled-procedure"); }
lisp_value_t* compiled_procedure_entry(lisp_value_t* c_proc) { return cadr(c_proc); }
lisp_value_t* compiled_procedure_env(lisp_value_t* c_proc) { return caddr(c_proc); }
