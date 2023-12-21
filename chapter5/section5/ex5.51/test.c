#include <stdbool.h>

#include "CUnit/Basic.h"
#include "eceval/eval-apply.h"
#include "io/parser.h"
#include "io/printer.h"
#include "machine/registers.h"
#include "machine/utils.h"

#define BUF_SIZE 128

// utils
int reset_and_print_lisp_value(tlisp_value_t *tvalue, char *buf) {
  reset_printed_stack();
  return print_lisp_value(tvalue, buf);
}

// test cases
void test_cons_car_cdr(void) {
  tlisp_value_t *tlist = list(make_number(3.141592), make_symbol("hello"), make_number(1.0));

  CU_ASSERT(REM_TAG(car(tlist))->number == 3.141592);
  CU_ASSERT_STRING_EQUAL(REM_TAG(cadr(tlist))->symbol, "hello");
  CU_ASSERT(REM_TAG(caddr(tlist))->number == 1.0);
  CU_ASSERT(GET_TAG(cdddr(tlist)) == LISP_NULL_TYPE);
}

void test_set_car_cdr(void) {
  tlisp_value_t *tpair = cons(make_number(0), make_number(0));
  set_car(tpair, make_number(123));
  set_cdr(tpair, make_number(456));

  CU_ASSERT(REM_TAG(car(tpair))->number == 123);
  CU_ASSERT(REM_TAG(cdr(tpair))->number == 456);
}

tlisp_value_t *double_number(tlisp_value_t *tnum) { return make_number(REM_TAG(tnum)->number * 2); }
void test_utils(void) {
  // is_eq
  CU_ASSERT(is_eq(cons(make_null(), make_number(1)), cons(make_null(), make_number(1))) == true);
  CU_ASSERT(is_eq(cons(make_null(), make_number(1)), cons(make_null(), make_number(1.1))) == false);
  CU_ASSERT(is_eq(make_number(2), make_number(2)) == true);
  CU_ASSERT(is_eq(make_number(1), make_number(2)) == false);
  CU_ASSERT(is_eq(make_symbol("abc"), make_symbol("abc")) == true);
  CU_ASSERT(is_eq(make_symbol("abc"), make_symbol("hello")) == false);
  CU_ASSERT(is_eq(make_null(), make_null()) == true);

  // length
  CU_ASSERT(length(make_null()) == 0);
  CU_ASSERT(length(list(make_symbol("a"))) == 1);
  CU_ASSERT(length(list(make_symbol("a"), make_number(1), make_null())) == 3);

  // map
  tlisp_value_t *tdoubled = map(double_number, list(make_number(1), make_number(2), make_number(3)));
  CU_ASSERT(length(tdoubled) == 3);
  CU_ASSERT(REM_TAG(car(tdoubled))->number == 2);
  CU_ASSERT(REM_TAG(cadr(tdoubled))->number == 4);
  CU_ASSERT(REM_TAG(caddr(tdoubled))->number == 6);

  // append
  tlisp_value_t *tappended =
      append(list(make_number(1), make_number(2)), list(make_number(3), make_number(4), make_number(5)));
  CU_ASSERT(length(tappended) == 5);
  CU_ASSERT(REM_TAG(car(tappended))->number == 1);
  CU_ASSERT(REM_TAG(cadr(tappended))->number == 2);
  CU_ASSERT(REM_TAG(caddr(tappended))->number == 3);
  CU_ASSERT(REM_TAG(cadddr(tappended))->number == 4);
  CU_ASSERT(REM_TAG(car(cddddr(tappended)))->number == 5);
}

void test_parser_printer(void) {
  char output_buffer[BUF_SIZE] = {};
  tlisp_value_t *tresult = NULL;

  // comment
  tresult = NULL;
  parse_lisp_value("  ; comment  ", &tresult);
  CU_ASSERT_PTR_NULL(tresult);

  // number
  memset(output_buffer, '\0', BUF_SIZE);
  tresult = NULL;
  parse_lisp_value("   123.456  ", &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "123.456000");

  // quote
  memset(output_buffer, '\0', BUF_SIZE);
  tresult = NULL;
  parse_lisp_value("  'a  ", &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "(quote a)");

  // list
  memset(output_buffer, '\0', BUF_SIZE);
  tresult = NULL;
  parse_lisp_value("  ( a  b   c   )  ", &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "(a b c)");

  // nested list
  memset(output_buffer, '\0', BUF_SIZE);
  tresult = NULL;
  parse_lisp_value("(a(b c)  d)", &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "(a (b c) d)");

  // symbol
  memset(output_buffer, '\0', BUF_SIZE);
  tresult = NULL;
  parse_lisp_value("  symbol  ", &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "symbol");

  // complex
  memset(output_buffer, '\0', BUF_SIZE);
  char *complex_input =
      "(define \n"
      "  (fib n)\n"
      "  (if (< n 2) \n"
      "    n\n"
      "    (+ \n"
      "      (fib (- n 1))\n"
      "      (fib (- n 2))\n"
      "    )\n"
      "  )\n"
      ")\n";
  tresult = NULL;
  parse_lisp_value(complex_input, &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer,
                         "(define (fib n) (if (< n 2.000000) n (+ (fib (- n 1.000000)) (fib (- n 2.000000)))))");

  // multiple
  memset(output_buffer, '\0', BUF_SIZE);
  tresult = NULL;

  char *next = NULL;
  next = parse_lisp_value("    1   sym    \n (list) ", &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "1.000000");

  next = parse_lisp_value(next, &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "sym");

  next = parse_lisp_value(next, &tresult);
  reset_and_print_lisp_value(tresult, output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "(list)");
}

void test_gc(void) {
  set_gc_silent_mode(true);

  reg_exp = list(make_number(1), make_number(2), make_number(3), make_number(4));

  for (int i = 0; i < GC_TABLE_SIZE; i++) {
    cons(make_number(123), make_symbol("test"));
    make_number(123);
    make_symbol("test");
    make_null();
    gc_check();
  }

  gc((tlisp_value_t *[]){reg_exp}, 1, NULL, 0);
  CU_ASSERT(REM_TAG(car(reg_exp))->number == 1);
  CU_ASSERT(REM_TAG(cadr(reg_exp))->number == 2);
  CU_ASSERT(REM_TAG(caddr(reg_exp))->number == 3);
  CU_ASSERT(REM_TAG(cadddr(reg_exp))->number == 4);
  CU_ASSERT(GET_TAG(cddddr(reg_exp)) == LISP_NULL_TYPE);
  // 9 = 4（リストの要素） + 4（consセル） + 1（最後のnull値）
  CU_ASSERT(get_gc_obj_count() == 9);

  set_gc_silent_mode(false);
}

void test_eceval(void) {
  set_gc_silent_mode(true);

  char output_buffer[BUF_SIZE] = {};
  tlisp_value_t *tresult = NULL;

  // self-evaluating
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("1", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "1.000000");

  // quote
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("'quote_value", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "quote_value");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("'2", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "2.000000");

  // definition, variable assignment
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(define a 3)", &tresult);
  eval(tresult);
  parse_lisp_value("a", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "3.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(set! a 4)", &tresult);
  eval(tresult);
  parse_lisp_value("a", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "4.000000");

  // if
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(if true 5)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "5.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(if false 999 6)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "6.000000");

  // lambda, compound procedure application
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("((lambda (a) a) 7)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "7.000000");

  parse_lisp_value("(define (eight) 8)", &tresult);
  eval(tresult);
  parse_lisp_value("(eight)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "8.000000");

  // begin
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(begin (set! a 9) a)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "9.000000");

  // cond
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(cond (false 999) ((= a 9) 10))", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "10.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(cond (false 999) (else 11))", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "11.000000");

  // primitive procedure application
  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(+ 6 6)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "12.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(cdr (cons 999 13))", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "13.000000");

  // complex
  memset(output_buffer, '\0', BUF_SIZE);
  char *append =
      "(define \n"
      "  (append x y)\n"
      "  (if (null? x) \n"
      "    y\n"
      "    (cons \n"
      "      (car x)\n"
      "      (append (cdr x) y)\n"
      "    )\n"
      "  )\n"
      ")\n";
  parse_lisp_value(append, &tresult);
  eval(tresult);
  parse_lisp_value("(append '(a b c) '(d e f))", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "(a b c d e f)");

  // fib
  char *fib =
      "(define \n"
      "  (fib n)\n"
      "  (if (< n 2) \n"
      "    n\n"
      "    (+ \n"
      "      (fib (- n 1))\n"
      "      (fib (- n 2))\n"
      "    )\n"
      "  )\n"
      ")\n";
  parse_lisp_value(fib, &tresult);
  eval(tresult);

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 1)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "1.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 2)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "1.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 3)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "2.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 4)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "3.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 5)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "5.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 6)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "8.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 7)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "13.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 8)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "21.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 9)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "34.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 10)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "55.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 20)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "6765.000000");

  memset(output_buffer, '\0', BUF_SIZE);
  parse_lisp_value("(fib 25)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "75025.000000");

  set_gc_silent_mode(false);
}

void test_tail_recursion(void) {
  set_gc_silent_mode(true);

  char output_buffer[BUF_SIZE] = {};
  tlisp_value_t *tresult = NULL;

  parse_lisp_value("(define (sum x res) (if (< 100000 x) res (sum (+ x 1) (+ res x))))", &tresult);
  eval(tresult);
  parse_lisp_value("(sum 0 0)", &tresult);
  reset_and_print_lisp_value(eval(tresult), output_buffer);
  CU_ASSERT_STRING_EQUAL(output_buffer, "5000050000.000000");

  set_gc_silent_mode(false);
}

int noop(void) { return 0; }
int main() {
  CU_pSuite pSuite = NULL;

  /* initialize the CUnit test registry */
  if (CUE_SUCCESS != CU_initialize_registry()) return CU_get_error();

  /* add a suite to the registry */
  pSuite = CU_add_suite("sicp 5.51", noop, noop);
  if (NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }

  /* add the tests to the suite */
  /* NOTE - ORDER IS IMPORTANT - MUST TEST fread() AFTER fprintf() */
  if (
      //
      (NULL == CU_add_test(pSuite, "test_cons_car_cdr", test_cons_car_cdr)) ||      //
      (NULL == CU_add_test(pSuite, "test_set_car_cdr", test_set_car_cdr)) ||        //
      (NULL == CU_add_test(pSuite, "test_utils", test_utils)) ||                    //
      (NULL == CU_add_test(pSuite, "test_parser_printer", test_parser_printer)) ||  //
      (NULL == CU_add_test(pSuite, "test_gc", test_gc)) ||                          //
      (NULL == CU_add_test(pSuite, "test_eceval", test_eceval)) ||                  //
      (NULL == CU_add_test(pSuite, "test_tail_recursion", test_tail_recursion))     //
  ) {
    CU_cleanup_registry();
    return CU_get_error();
  }

  /* Run all tests using the CUnit Basic interface */
  CU_basic_set_mode(CU_BRM_VERBOSE);
  CU_basic_run_tests();
  CU_cleanup_registry();
  return CU_get_error();
}
