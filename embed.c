#include <stdio.h>
#include <julia.h>

JULIA_DEFINE_FAST_TLS // only define this once, in an executable (not in a shared library) if you want fast code.

int main(int argc, char *argv[])
{
    /* required: setup the Julia context */
    jl_init();


    jl_function_t *func = jl_get_function(jl_base_module, "sqrt");
    jl_value_t *argument = jl_box_float64(2.0);
    jl_value_t *ret = jl_call1(func, argument);
    double result = jl_unbox_float64(ret);

    printf("From C: %f\n", result);


    double data[5] = {1.0, 2.0, 3.0, 4.0, 5.0};
    jl_value_t *array_type = jl_apply_array_type((jl_value_t*)jl_float64_type, 1);
    jl_array_t *jarray = jl_ptr_to_array_1d(array_type, data, 5, 0);

    // Call sum
    jl_function_t *sum_func = jl_get_function(jl_base_module, "sum");
    jl_value_t *ret_sum = jl_call1(sum_func, (jl_value_t*)jarray);

    // Unbox result
    double result_sum = jl_unbox_float64(ret_sum);

    // Print result
    printf("Sum result: %f\n", result_sum);

    jl_atexit_hook(0);
    return 0;
}
