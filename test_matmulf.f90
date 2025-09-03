module julia
 use iso_c_binding
 implicit none


 ! global variables
 type(c_ptr),   bind(C, name="jl_main_module")  :: jl_main_module
 type(c_ptr),   bind(C, name="jl_float64_type") :: jl_float64_type

 ! function handlers
 type(c_ptr) :: fun_tuple
 type(c_ptr) :: fun_push_b

 interface
   subroutine ll_jl_init() bind(C, name="jl_init")
   end subroutine ll_jl_init

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

 subroutine jl_init()
  call ll_jl_init()
  fun_tuple = jl_get_function(jl_main_module, "tuple"//C_NULL_CHAR)
  fun_push_b = jl_get_function(jl_main_module, "push!"//C_NULL_CHAR)
 end subroutine jl_init

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
    write(0,*) "julia LOAD_PATH not found"
    stop
  end if

  tmp = jl_call2(fun_push_b, load_path, jl_cstr_to_string(trim(path)//C_NULL_CHAR));

  if (.not.c_associated(tmp)) then
    write(0,*) "jl_add_load_path failed"
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


program test_matmul
 use iso_c_binding
 use julia
 implicit none

 type(c_ptr) :: jl_array_type, mylinalg_val, mylinalg
 type(c_ptr) :: fun_mul_b, dims, A, B, C
 type(c_ptr) :: array2d_type

 integer(c_int), parameter :: n = 2
 real(c_double), dimension(n,n), target :: A_data, B_data
 real(c_double), dimension(n,n), target :: C_data = 0.0

 integer :: i, j
 type(c_ptr) :: tmp

 A_data = reshape([1.0, 3.0, 2.0, 4.0],shape(A_data))
 B_data = reshape([5.0, 7.0, 6.0, 8.0],shape(B_data))
 C_data = 0

 call jl_init()
 call jl_add_load_path('.')
 mylinalg = jl_using('MyLinAlg')

 if (.not.c_associated(mylinalg)) then
   write(0,*) "julia module MyLinAlg not found"
   stop
 end if

 fun_mul_b = jl_get_function(mylinalg, "mul!")

 if (.not.c_associated(fun_mul_b)) then
   write(0,*) "function mul! not found"
   stop
 end if

 A = jl_to_array_f64_2(A_data)
 B = jl_to_array_f64_2(B_data)
 C = jl_to_array_f64_2(C_data)

 ! Call mul!(C, A, B)
 tmp = jl_call3(fun_mul_b, C, A, B)

 ! Print result
 print *, "Result C = A*B:"
 do i = 1, n
   do j = 1, n
     write(*,'(F6.2)', advance="no") C_data(i,j)
     write(*,'(A)', advance="no") " "
   end do
   print *
 end do

 call jl_atexit_hook(0)

end program test_matmul
