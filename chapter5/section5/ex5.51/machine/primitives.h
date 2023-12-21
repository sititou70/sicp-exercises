#ifndef PRIMITIVE_H_INCLUDED
#define PRIMITIVE_H_INCLUDED

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// tlisp_value: tagged lisp value
typedef struct {
} tlisp_value_t;

struct lisp_value {
  bool gc_mark;
  union {
    struct {
      tlisp_value_t *car;
      tlisp_value_t *cdr;
    };
    double number;
    char *symbol;
    void *null;
    tlisp_value_t *(*internal_primitive_procedure)(tlisp_value_t *);
    void *(*internal_label)(void);
  };
};
typedef struct lisp_value lisp_value_t;

// GCCのmallocは8または16バイトにアラインされるため、下位3ビットを型タグに使用する
// see: https://www.gnu.org/software/libc/manual/html_node/Aligned-Memory-Blocks.html
#define TAG_MASK ((1 << 3) - 1)
#define ADD_TAG(p, type) ((tlisp_value_t *)((uintptr_t)REM_TAG(p) | type))
#define REM_TAG(p) ((lisp_value_t *)((uintptr_t)p & (~TAG_MASK)))
#define GET_TAG(p) ((uintptr_t)p & TAG_MASK)

#define LISP_PAIR_TYPE 1
#define LISP_NUMBER_TYPE 2
#define LISP_SYMBOL_TYPE 3
#define LISP_NULL_TYPE 4
#define LISP_INTERNAL_PRIMITIVE_PROCEDURE_TYPE 5
#define LISP_INTERNAL_LABEL_TYPE 6

tlisp_value_t *cons(tlisp_value_t *car_tval, tlisp_value_t *cdr_tval);
tlisp_value_t *car(tlisp_value_t *tval);
tlisp_value_t *cdr(tlisp_value_t *tval);
void set_car(tlisp_value_t *tpair, tlisp_value_t *tval);
void set_cdr(tlisp_value_t *tpair, tlisp_value_t *tval);

tlisp_value_t *make_number(double num);
tlisp_value_t *make_symbol(char *str);
tlisp_value_t *make_symbol_with_len(char *str, size_t len);
tlisp_value_t *make_null();
tlisp_value_t *make_internal_primitive_procedure(tlisp_value_t *(*proc)(tlisp_value_t *));
tlisp_value_t *make_internal_label(void *(*label)(void));

// ################
// # gc
// ################

#define GC_TABLE_SIZE 65536

void gc_register(tlisp_value_t *tval);
void gc_check();
void gc(tlisp_value_t **registers, size_t registers_len, tlisp_value_t **stack, size_t stack_len);

void push_should_mark(tlisp_value_t *tval);
tlisp_value_t *pop_should_mark();
void reset_should_mark();

void free_lisp_value(tlisp_value_t *tval);

void move_gc_index_to_next();

int get_gc_obj_count();

void set_gc_silent_mode(bool silent);

#endif
