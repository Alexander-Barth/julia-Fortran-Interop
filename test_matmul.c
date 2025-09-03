#include <julia.h>
#include <stdio.h>
#include <stdlib.h>

JULIA_DEFINE_FAST_TLS

jl_function_t *tuple;
jl_function_t *push_;
jl_value_t *load_path;

void julia_add_load_path(const char* path) {
  jl_call2(push_, load_path, jl_cstr_to_string(path));
}

// Build a Julia tuple (x1, x2, ..., xn) with n long values
static jl_value_t *make_dims_tuple(int n, ...) {
    jl_value_t **args = (jl_value_t**)alloca(n * sizeof(jl_value_t*));

    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < n; i++) {
        long v = va_arg(ap, long);
        args[i] = jl_box_long(v);
    }
    va_end(ap);

    return jl_call(tuple, args, n);
}


int main(int argc, char *argv[])
{
    jl_init();
    tuple = jl_get_function(jl_base_module, "tuple");
    push_ = jl_get_function(jl_base_module, "push!");
    load_path = jl_get_global(jl_main_module, jl_symbol("LOAD_PATH"));

    julia_add_load_path(".");

    jl_eval_string("using MyLinAlg");

    // Get MyLinAlg module
    jl_module_t *main_mod = jl_main_module;
    jl_value_t *mylinalg_val = jl_get_global(main_mod, jl_symbol("MyLinAlg"));
    jl_module_t *mylinalg = (jl_module_t*)mylinalg_val;

    // Get mul! function
    jl_function_t *mulbang = jl_get_function(mylinalg, "mul!");

    // Matrix size
    int n = 2;

    // Column-major arrays
    double A_data[4] = {1.0, 3.0,
                        2.0, 4.0};  // A = [1 2; 3 4]
    double B_data[4] = {5.0, 7.0,
                        6.0, 8.0};  // B = [5 6; 7 8]
    double C_data[4] = {0.0, 0.0,
                        0.0, 0.0};

    // Julia Array{Float64,2} type
    jl_value_t *array2d_type = jl_apply_array_type((jl_value_t*)jl_float64_type, 2);

    // Build dims tuple (n,n)
    jl_value_t *dims = make_dims_tuple(n,n);

    // Wrap C arrays (no copy, Julia does not own memory)
    jl_array_t *A = jl_ptr_to_array(array2d_type, A_data, dims, 0);
    jl_array_t *B = jl_ptr_to_array(array2d_type, B_data, dims, 0);
    jl_array_t *C = jl_ptr_to_array(array2d_type, C_data, dims, 0);

    // Call mul!(C, A, B)
    jl_call3(mulbang, (jl_value_t*)C, (jl_value_t*)A, (jl_value_t*)B);

    // Print result
    printf("Result C = A*B:\n");
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf("%6.2f ", C_data[i + j*n]);  // column-major indexing
        }
        printf("\n");
    }

    jl_atexit_hook(0);
    return 0;
}
