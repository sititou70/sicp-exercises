#include "global-environment.h"

#include "environment.h"
#include "procedure.h"

tlisp_value_t* setup_environment() {
  tlisp_value_t* initial_env =
      extend_environment(get_primitive_procedure_names(), get_primitive_procedure_objects(), make_null());

  define_variable(make_symbol("true"), make_symbol("true"), initial_env);
  define_variable(make_symbol("false"), make_null(), initial_env);

  return initial_env;
}

tlisp_value_t* the_global_environment = NULL;
tlisp_value_t* get_global_environment() {
  if (the_global_environment == NULL) the_global_environment = setup_environment();

  return the_global_environment;
}
