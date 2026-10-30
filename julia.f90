module julia
 use iso_c_binding
 implicit none

 ! global variables
 ! note since julia 1.13 the following does not work any more:
 ! type(c_ptr),   bind(C, name="jl_main_module")  :: jl_main_module
 ! we need a C function including julia.h and returning the jl_main_module
 ! pointer. This is done in jl_setup().
 type(c_ptr) :: jl_main_module
 type(c_ptr) :: jl_base_module
 ! add all types that you need
 type(c_ptr) :: jl_float64_type

 ! function handlers
 type(c_ptr) :: fun_tuple
 type(c_ptr) :: fun_push_b

 interface
   subroutine jl_init() bind(C, name="jl_init")
   end subroutine jl_init

   function get_jl_main_module() result(p) &
        bind(C, name="get_jl_main_module")
    import :: c_ptr
    type(c_ptr) :: p
   end function get_jl_main_module

   function get_jl_base_module() result(p) &
        bind(C, name="get_jl_base_module")
    import :: c_ptr
    type(c_ptr) :: p
   end function get_jl_base_module

   function get_jl_float64_type() result(p) &
        bind(C, name="get_jl_float64_type")
    import :: c_ptr
    type(c_ptr) :: p
   end function get_jl_float64_type

   subroutine jl_atexit_hook(status) bind(C, name="jl_atexit_hook")
    import :: c_int
    integer(c_int), value :: status
   end subroutine jl_atexit_hook

   function jl_eval_string(str) result(res) bind(C, name="jl_eval_string")
    import :: c_ptr, c_char
    character(kind=c_char), dimension(*) :: str
    type(c_ptr) :: res
   end function jl_eval_string

   function jl_cstr_to_string(str) result(res) bind(C, name="jl_cstr_to_string")
    import :: c_ptr, c_char
    character(kind=c_char), dimension(*) :: str
    type(c_ptr) :: res
   end function jl_cstr_to_string

   function jl_get_global(mod, name) result(res) bind(C, name="jl_get_global")
    import :: c_ptr
    type(c_ptr), value :: mod
    type(c_ptr), value :: name
    type(c_ptr) :: res
   end function jl_get_global

   function jl_symbol(name) result(res) bind(C, name="jl_symbol")
    import :: c_ptr, c_char
    character(kind=c_char), dimension(*) :: name
    type(c_ptr) :: res
   end function jl_symbol

   function jl_apply_array_type(t, dim) result(res) bind(C, name="jl_apply_array_type")
    import :: c_ptr, c_int
    type(c_ptr), value :: t
    integer(c_int), value :: dim
    type(c_ptr) :: res
   end function jl_apply_array_type

   function jl_box_int64(x) bind(C, name="jl_box_int64")
    import :: c_ptr, c_long
    integer(c_long), value :: x
    type(c_ptr) :: jl_box_int64
   end function jl_box_int64

   function jl_ptr_to_array(array_type, data, dims, own_buffer) result(res) bind(C,name="jl_ptr_to_array")
    import :: c_ptr, c_int
    type(c_ptr), value :: array_type
    type(c_ptr), value :: data
    type(c_ptr), value :: dims
    integer(c_int), value :: own_buffer
    type(c_ptr) :: res
   end function jl_ptr_to_array

   function jl_call(fun, args, nargs) result(res) bind(C, name="jl_call")
    import :: c_ptr, c_int
    type(c_ptr), value :: fun
    type(c_ptr), dimension(*) :: args
    integer(c_int), value :: nargs
    type(c_ptr) :: res
   end function jl_call

   function jl_call2(f, arg1, arg2) result(res) bind(C, name="jl_call2")
    import :: c_ptr
    type(c_ptr), value :: f, arg1, arg2
    type(c_ptr) :: res
   end function jl_call2

   function jl_call3(f, arg1, arg2, arg3) result(res) bind(C, name="jl_call3")
    import :: c_ptr
    type(c_ptr), value :: f, arg1, arg2, arg3
    type(c_ptr) :: res
   end function jl_call3

 end interface



contains

 subroutine jl_setup()
  call jl_init()
  jl_main_module = get_jl_main_module()
  jl_base_module = get_jl_base_module()
  jl_float64_type = get_jl_float64_type()

  fun_tuple = jl_get_function(jl_base_module, "tuple"//C_NULL_CHAR)
  fun_push_b = jl_get_function(jl_base_module, "push!"//C_NULL_CHAR)
 end subroutine jl_setup

 function jl_get_function(mod, name) result(res)
  type(c_ptr), value :: mod
  character(len=*) :: name
  type(c_ptr) :: res

  res = jl_get_global(mod,jl_symbol(trim(name)//C_NULL_CHAR ))
 end function jl_get_function


 function jl_ntuple_int64(x) result(res)
  implicit none
  integer(8) :: x(:)
  type(c_ptr) :: res

  type(c_ptr) :: arg(size(x))
  integer :: i

  do i = 1,size(x)
    arg(i) = jl_box_int64(x(i))
  end do

  res = jl_call(fun_tuple, arg, size(x))
 end function jl_ntuple_int64

 function jl_to_array_f64_2(A_data) result(res)
   real(8), target :: A_data(:,:)
   type(c_ptr) :: dims, res
   type(c_ptr) :: array2d_type

   dims = jl_ntuple_int64(int(shape(A_data),8))
   array2d_type = jl_apply_array_type(jl_float64_type, size(shape(A_data)))
   res = jl_ptr_to_array(array2d_type, c_loc(A_data), dims, 0)
 end function jl_to_array_f64_2

 subroutine jl_add_load_path(path)
  character(len=*) :: path
  type(c_ptr) :: tmp, load_path

  load_path = jl_get_global(jl_main_module, jl_symbol("LOAD_PATH"//C_NULL_CHAR))

  if (.not.c_associated(load_path)) then
    write(0,*) "ERROR: the julia variable LOAD_PATH was not found."
    stop
  end if

  tmp = jl_call2(fun_push_b, load_path, jl_cstr_to_string(trim(path)//C_NULL_CHAR));

  if (.not.c_associated(tmp)) then
    write(0,*) "ERROR: jl_add_load_path failed"
    stop
  end if

 end subroutine jl_add_load_path

 function jl_using(modname) result(res)
  character(len=*) :: modname
  type(c_ptr) :: tmp, res

  res = c_null_ptr
  tmp = jl_eval_string('using ' // modname // C_NULL_CHAR)

  if (.not.c_associated(tmp)) then
    return
  else
    res = jl_get_global(jl_main_module, jl_symbol(modname//C_NULL_CHAR))
  end if
 end function jl_using


end module julia
