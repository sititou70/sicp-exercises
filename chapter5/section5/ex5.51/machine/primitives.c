#include "primitives.h"

#include <stdio.h>
#include <string.h>

#include "registers.h"
#include "stack.h"

lisp_value_t *cons(lisp_value_t *car_val, lisp_value_t *cdr_val) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_pair_type;
  val->car = car_val;
  val->cdr = cdr_val;

  gc_register(val);

  return val;
}
lisp_value_t *car(lisp_value_t *val) {
  if (val->type != lisp_pair_type) {
    fprintf(stderr, "car: val must be pair.\n");
    exit(1);
  }

  return val->car;
}
lisp_value_t *cdr(lisp_value_t *val) {
  if (val->type != lisp_pair_type) {
    fprintf(stderr, "cdr: val must be pair.\n");
    exit(1);
  }

  return val->cdr;
}
void set_car(lisp_value_t *target, lisp_value_t *val) {
  if (target->type != lisp_pair_type) {
    fprintf(stderr, "set_car: target must be pair.\n");
    exit(1);
  }

  target->car = val;

  return;
}
void set_cdr(lisp_value_t *target, lisp_value_t *val) {
  if (target->type != lisp_pair_type) {
    fprintf(stderr, "set_cdr: target must be pair.\n");
    exit(1);
  }

  target->cdr = val;

  return;
}

lisp_value_t *make_number(double num) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_number_type;
  val->number = num;

  gc_register(val);

  return val;
}
lisp_value_t *make_symbol(char *str) {
  char *symbol = malloc(sizeof(char) * (strlen(str) + 1));
  strcpy(symbol, str);

  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_symbol_type;
  val->symbol = symbol;

  gc_register(val);

  return val;
}
// len: null文字を含まない文字列の長さ
lisp_value_t *make_symbol_with_len(char *str, size_t len) {
  char *symbol = malloc(sizeof(char) * (len + 1));
  memcpy(symbol, str, len);
  symbol[len] = '\0';

  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_symbol_type;
  val->symbol = symbol;

  gc_register(val);

  return val;
}
lisp_value_t *make_null() {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_null_type;

  gc_register(val);

  return val;
}
lisp_value_t *make_internal_primitive_procedure(lisp_value_t *(*proc)(lisp_value_t *)) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_internal_primitive_procedure_type;
  val->internal_primitive_procedure = proc;

  gc_register(val);

  return val;
}
lisp_value_t *make_internal_label(void *(*label)(void)) {
  lisp_value_t *val = (lisp_value_t *)malloc(sizeof(lisp_value_t));
  val->type = lisp_internal_label_type;
  val->internal_label = label;

  gc_register(val);

  return val;
}

// ################
// # gc
// ################
lisp_value_t *gc_table[GC_TABLE_SIZE] = {};
// 次の空き領域を表す
size_t gc_table_index = 0;
int gc_obj_count = 0;
bool gc_silent_mode = false;

// gcにlisp_valueを登録する
void gc_register(lisp_value_t *val) {
  gc_table[gc_table_index] = val;
  move_gc_index_to_next();
  gc_obj_count++;
}

// gcに登録されているオブジェクト数が一定の割合に達していたらgcを実行する
// レジスタへの代入のあとなど、gcを実行可能なタイミングで呼び出される
// 例えばマシン演算の最中にgcが実行されると、演算途中の値をレジスタやスタックから辿れないため、この関数を呼び出すことはできない。
#define GC_MEMORY_HIGH 0.7
void gc_check() {
  if (gc_obj_count / (double)GC_TABLE_SIZE > GC_MEMORY_HIGH) {
    gc((lisp_value_t *[]){reg_exp, reg_env, reg_val, reg_proc, reg_argl, reg_continue, reg_unev}, 7, stack,
       stack_index);
    if (gc_table_index == GC_TABLE_SIZE) {
      fprintf(stderr, "gc_check: gc tried but could not allocate free memory.\n");
      exit(1);
    }
  }
}

void gc(lisp_value_t **registers, size_t registers_len, lisp_value_t **stack, size_t stack_len) {
  if (!gc_silent_mode) printf("gc: start...");

  reset_should_mark();
  for (size_t i = 0; i < GC_TABLE_SIZE; i++) {
    if (gc_table[i] != NULL) gc_table[i]->gc_mark = false;
  }

  // mark
  for (size_t i = 0; i < registers_len; i++) {
    if (registers[i] != NULL) push_should_mark(registers[i]);
  }
  for (size_t i = 0; i < stack_len; i++) {
    if (stack[i] != NULL) push_should_mark(stack[i]);
  }
  while (1) {
    lisp_value_t *target = pop_should_mark();
    if (target == NULL) break;

    if (!target->gc_mark) {
      target->gc_mark = true;
      if (target->type == lisp_pair_type) {
        push_should_mark(target->car);
        push_should_mark(target->cdr);
      }
    }
  }

  // sweep
  for (size_t i = 0; i < GC_TABLE_SIZE; i++) {
    if (gc_table[i] != NULL && !gc_table[i]->gc_mark) {
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
lisp_value_t *should_mark[GC_TABLE_SIZE + STACK_SIZE] = {};
// 次の空き領域、または空きがない（= GC_TABLE_SIZE + STACK_SIZE）状態を表す
size_t should_mark_index = 0;
void push_should_mark(lisp_value_t *val) {
  if (should_mark_index == GC_TABLE_SIZE + STACK_SIZE) {
    fprintf(stderr, "push_should_mark: no free workspaces. gc failed.\n");
    exit(1);
  }

  should_mark[should_mark_index] = val;
  should_mark_index++;
  return;
}
lisp_value_t *pop_should_mark() {
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
void free_lisp_value(lisp_value_t *val) {
  switch (val->type) {
    case lisp_symbol_type:
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
