#ifndef PRIMITIVE_H_INCLUDED
#define PRIMITIVE_H_INCLUDED

#include <stdbool.h>
#include <stdlib.h>

typedef enum {
  lisp_pair_type,
  lisp_number_type,
  lisp_symbol_type,
  lisp_null_type,
  lisp_internal_primitive_procedure_type,
  lisp_internal_label_type,
} lisp_value_type_t;

struct lisp_value {
  lisp_value_type_t type;
  bool gc_mark;
  union {
    struct {
      struct lisp_value *car;
      struct lisp_value *cdr;
    };
    double number;
    char *symbol;
    void *null;
    struct lisp_value *(*internal_primitive_procedure)(struct lisp_value *);
    void *(*internal_label)(void);
  };
};
typedef struct lisp_value lisp_value_t;

lisp_value_t *cons(lisp_value_t *car_val, lisp_value_t *cdr_val);
lisp_value_t *car(lisp_value_t *val);
lisp_value_t *cdr(lisp_value_t *val);
void set_car(lisp_value_t *target, lisp_value_t *val);
void set_cdr(lisp_value_t *target, lisp_value_t *val);

lisp_value_t *make_number(double num);
lisp_value_t *make_symbol(char *str);
lisp_value_t *make_symbol_with_len(char *str, size_t len);
lisp_value_t *make_null();
lisp_value_t *make_internal_primitive_procedure(lisp_value_t *(*proc)(lisp_value_t *));
lisp_value_t *make_internal_label(void *(*label)(void));

// ################
// # gc
// ################

#define GC_TABLE_SIZE 65536

void gc_register(lisp_value_t *val);
void gc_check();
void gc(lisp_value_t **registers, size_t registers_len, lisp_value_t **stack, size_t stack_len);

void push_should_mark(lisp_value_t *val);
lisp_value_t *pop_should_mark();
void reset_should_mark();

void free_lisp_value(lisp_value_t *val);

void move_gc_index_to_next();

int get_gc_obj_count();

void set_gc_silent_mode(bool silent);

#endif
