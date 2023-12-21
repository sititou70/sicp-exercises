#include <stdio.h>

#include "../ex5.51/eceval/environment.h"
#include "../ex5.51/eceval/eval-apply.h"
#include "../ex5.51/eceval/expression.h"
#include "../ex5.51/eceval/global-environment.h"
#include "../ex5.51/eceval/procedure.h"
#include "../ex5.51/machine/registers.h"
#include "../ex5.51/machine/stack.h"
#include "../ex5.51/machine/utils.h"

#define goto(a) return a
#define assign(a, b) \
  a = b;             \
  gc_check()
#define perform(a) a
#define test(a)   \
  reg_flag = (a); \
  gc_check()
#define branch(a) \
  if (reg_flag) return a
#define save(e) push_stack(e)
#define restore(e) e = pop_stack()

// [compiled code here]

// utils
bool is_true(tlisp_value_t* tval) { return !is_false(tval); }
bool is_false(tlisp_value_t* tval) { return GET_TAG(tval) == LISP_NULL_TYPE; }

tlisp_value_t* get_empty_arglist() { return make_null(); }
tlisp_value_t* adjoin_arg(tlisp_value_t* targ, tlisp_value_t* targlist) { return append(targlist, list(targ)); }

bool is_last_operand(tlisp_value_t* tops) { return GET_TAG(cdr(tops)) == LISP_NULL_TYPE; }

int main(int argc, char* argv[]) {
  register_argv(argv);
  init_stack();
  reg_env = get_global_environment();
  reg_continue = make_internal_label(NULL);

  void* (*next_label)() = compiled_entry;

  while (next_label != NULL) {
    next_label = next_label();
  }

  return 0;
}
