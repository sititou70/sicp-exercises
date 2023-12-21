#include "primitives.h"

#include <stdio.h>
#include <string.h>

#include "registers.h"
#include "stack.h"

tlisp_value_t *cons(tlisp_value_t *car_tval, tlisp_value_t *cdr_tval) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->car = car_tval;
  val->cdr = cdr_tval;

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_PAIR_TYPE);

  gc_register(tagged_val);

  return tagged_val;
}
tlisp_value_t *car(tlisp_value_t *tval) {
  if (GET_TAG(tval) != LISP_PAIR_TYPE) {
    fprintf(stderr, "car: val must be pair.\n");
    exit(1);
  }

  return REM_TAG(tval)->car;
}
tlisp_value_t *cdr(tlisp_value_t *tval) {
  if (GET_TAG(tval) != LISP_PAIR_TYPE) {
    fprintf(stderr, "cdr: val must be pair.\n");
    exit(1);
  }

  return REM_TAG(tval)->cdr;
}
void set_car(tlisp_value_t *tpair, tlisp_value_t *tval) {
  if (GET_TAG(tpair) != LISP_PAIR_TYPE) {
    fprintf(stderr, "set_car: target must be pair.\n");
    exit(1);
  }

  REM_TAG(tpair)->car = tval;

  return;
}
void set_cdr(tlisp_value_t *tpair, tlisp_value_t *tval) {
  if (GET_TAG(tpair) != LISP_PAIR_TYPE) {
    fprintf(stderr, "set_cdr: target must be pair.\n");
    exit(1);
  }

  REM_TAG(tpair)->cdr = tval;

  return;
}

tlisp_value_t *make_number(double num) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->number = num;

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_NUMBER_TYPE);
  gc_register(tagged_val);

  return tagged_val;
}
tlisp_value_t *make_symbol(char *str) {
  char *symbol = malloc(sizeof(char) * (strlen(str) + 1));
  strcpy(symbol, str);

  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->symbol = symbol;

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_SYMBOL_TYPE);
  gc_register(tagged_val);

  return tagged_val;
}
// len: null文字を含まない文字列の長さ
tlisp_value_t *make_symbol_with_len(char *str, size_t len) {
  char *symbol = malloc(sizeof(char) * (len + 1));
  memcpy(symbol, str, len);
  symbol[len] = '\0';

  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->symbol = symbol;

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_SYMBOL_TYPE);
  gc_register(tagged_val);

  return tagged_val;
}
tlisp_value_t *make_null() {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_NULL_TYPE);
  gc_register(tagged_val);

  return tagged_val;
}
tlisp_value_t *make_internal_primitive_procedure(tlisp_value_t *(*proc)(tlisp_value_t *)) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->internal_primitive_procedure = proc;

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_INTERNAL_PRIMITIVE_PROCEDURE_TYPE);
  gc_register(tagged_val);

  return tagged_val;
}
tlisp_value_t *make_internal_label(void *(*label)(void)) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->internal_label = label;

  tlisp_value_t *tagged_val = ADD_TAG(val, LISP_INTERNAL_LABEL_TYPE);
  gc_register(tagged_val);

  return tagged_val;
}

// ################
// # gc
// ################
tlisp_value_t *gc_table[GC_TABLE_SIZE] = {};
// 次の空き領域を表す
size_t gc_table_index = 0;
int gc_obj_count = 0;
bool gc_silent_mode = false;

// gcにlisp_valueを登録する
void gc_register(tlisp_value_t *tval) {
  gc_table[gc_table_index] = tval;
  move_gc_index_to_next();
  gc_obj_count++;
}

// gcに登録されているオブジェクト数が一定の割合に達していたらgcを実行する
// レジスタへの代入のあとなど、gcを実行可能なタイミングで呼び出される
// 例えばマシン演算の最中にgcが実行されると、演算途中の値をレジスタやスタックから辿れないため、この関数を呼び出すことはできない。
#define GC_MEMORY_HIGH 0.7
void gc_check() {
  if (gc_obj_count / (double)GC_TABLE_SIZE > GC_MEMORY_HIGH) {
    gc((tlisp_value_t *[]){reg_exp, reg_env, reg_val, reg_proc, reg_argl, reg_continue, reg_unev}, 7, stack,
       stack_index);
    if (gc_table_index == GC_TABLE_SIZE) {
      fprintf(stderr, "gc_check: gc tried but could not allocate free memory.\n");
      exit(1);
    }
  }
}

void gc(tlisp_value_t **registers, size_t registers_len, tlisp_value_t **stack, size_t stack_len) {
  if (!gc_silent_mode) printf("gc: start...");

  reset_should_mark();
  for (size_t i = 0; i < GC_TABLE_SIZE; i++) {
    if (gc_table[i] != NULL) REM_TAG(gc_table[i])->gc_mark = false;
  }

  // mark
  for (size_t i = 0; i < registers_len; i++) {
    if (registers[i] != NULL) push_should_mark(registers[i]);
  }
  for (size_t i = 0; i < stack_len; i++) {
    if (stack[i] != NULL) push_should_mark(stack[i]);
  }
  while (1) {
    tlisp_value_t *tmarking_target = pop_should_mark();
    if (tmarking_target == NULL) break;

    lisp_value_t *marking_target = REM_TAG(tmarking_target);
    if (!marking_target->gc_mark) {
      marking_target->gc_mark = true;
      if (GET_TAG(tmarking_target) == LISP_PAIR_TYPE) {
        push_should_mark(marking_target->car);
        push_should_mark(marking_target->cdr);
      }
    }
  }

  // sweep
  for (size_t i = 0; i < GC_TABLE_SIZE; i++) {
    if (gc_table[i] != NULL && !REM_TAG(gc_table[i])->gc_mark) {
      free_lisp_value(gc_table[i]);
      gc_table[i] = NULL;
      gc_obj_count--;
    }
  }

  gc_table_index = 0;
  move_gc_index_to_next();

  if (!gc_silent_mode) printf("done\n");
  return;
}

// should mark stack
tlisp_value_t *should_mark[GC_TABLE_SIZE + STACK_SIZE] = {};
// 次の空き領域、または空きがない（= GC_TABLE_SIZE + STACK_SIZE）状態を表す
size_t should_mark_index = 0;
void push_should_mark(tlisp_value_t *tval) {
  if (should_mark_index == GC_TABLE_SIZE + STACK_SIZE) {
    fprintf(stderr, "push_should_mark: no free workspaces. gc failed.\n");
    exit(1);
  }

  should_mark[should_mark_index] = tval;
  should_mark_index++;
  return;
}
tlisp_value_t *pop_should_mark() {
  if (should_mark_index == 0) {
    return NULL;
  }

  should_mark_index--;
  return should_mark[should_mark_index];
}
void reset_should_mark() {
  should_mark_index = 0;
  return;
}

// 単一のlisp_valueをfreeする。それがpairであっても再帰的にはfreeしない
void free_lisp_value(tlisp_value_t *tval) {
  lisp_value_t *val = REM_TAG(tval);
  switch (GET_TAG(tval)) {
    case LISP_SYMBOL_TYPE:
      free(val->symbol);
      free(val);
      break;
    default:
      free(val);
      break;
  }

  return;
}

// gc_table_indexを次へ移動する
void move_gc_index_to_next() {
  size_t orig_index = gc_table_index;
  do {
    gc_table_index++;
    if (gc_table_index == GC_TABLE_SIZE) gc_table_index = 0;
  } while (gc_table[gc_table_index] != NULL && gc_table_index != orig_index);

  if (orig_index == gc_table_index) {
    fprintf(stderr, "move_gc_index_to_next: no free memory.\n");
    exit(1);
  }

  return;
}

// 統計情報
int get_gc_obj_count() { return gc_obj_count; }

void set_gc_silent_mode(bool silent) { gc_silent_mode = silent; }
