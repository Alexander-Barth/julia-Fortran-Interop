#include <julia.h>

jl_module_t *get_jl_main_module(void) {
    return jl_main_module;
}

jl_module_t *get_jl_base_module(void) {
    return jl_base_module;
}

jl_datatype_t *get_jl_float64_type(void) {
    return jl_float64_type;
}
