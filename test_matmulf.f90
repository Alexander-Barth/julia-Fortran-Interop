module julia
  use iso_c_binding
  implicit none

  type(c_ptr),   bind(C, name="jl_main_module")  :: jl_main_module
  type(c_ptr),   bind(C, name="jl_float64_type") :: jl_float64_type

  interface
    ! --- Julia C API functions ---
    subroutine jl_init() bind(C, name="jl_init")
    end subroutine jl_init

    subroutine jl_atexit_hook(status) bind(C, name="jl_atexit_hook")
      import :: c_int
      integer(c_int), value :: status
    end subroutine

    function jl_eval_string(str) result(res) bind(C, name="jl_eval_string")
      import :: c_ptr, c_char
      character(kind=c_char), dimension(*) :: str
      type(c_ptr) :: res
    end function

    function jl_get_global(mod, name) result(res) bind(C, name="jl_get_global")
      import :: c_ptr
      type(c_ptr), value :: mod
      type(c_ptr), value :: name
      type(c_ptr) :: res
    end function

    function jl_symbol(name) result(res) bind(C, name="jl_symbol")
      import :: c_ptr, c_char
      character(kind=c_char), dimension(*) :: name
      type(c_ptr) :: res
    end function

    function jl_apply_array_type(t, dim) result(res) bind(C, name="jl_apply_array_type")
      import :: c_ptr, c_int
      type(c_ptr), value :: t
      integer(c_int), value :: dim
      type(c_ptr) :: res
    end function

    function jl_ptr_to_array(array_type, data, dims, own_buffer) result(res) bind(C,name="jl_ptr_to_array")
      import :: c_ptr, c_int
      type(c_ptr), value :: array_type
      type(c_ptr), value :: data
      type(c_ptr), value :: dims
      integer(c_int), value :: own_buffer
      type(c_ptr) :: res
    end function

    function jl_call3(f, arg1, arg2, arg3) result(res) bind(C, name="jl_call3")
      import :: c_ptr
      type(c_ptr), value :: f, arg1, arg2, arg3
      type(c_ptr) :: res
    end function
  end interface

  ! --- Helper function: build tuple of integers ---
  interface
    function jl_tuple2(x1, x2) bind(C,name="jl_call_tuple2")
      import :: c_ptr
      type(c_ptr), value :: x1, x2
      type(c_ptr) :: jl_tuple2
    end function
  end interface

  contains

  function jl_get_function(mod, name) result(res)
   type(c_ptr), value :: mod
   character(kind=c_char), dimension(*) :: name
   type(c_ptr) :: res
   ! return (jl_function_t*)jl_get_global(m, jl_symbol(name));

   res = jl_get_global(mod,jl_symbol(name))

  end function jl_get_function

 end module julia


! test_matmul.f95
program test_matmul
 use iso_c_binding
 use julia
  implicit none

  ! Julia types
  type(c_ptr) :: jl_array_type, jl_main_mod, mylinalg_val, mylinalg
  type(c_ptr) :: mulbang, dims, A, B, C
  type(c_ptr) :: array2d_type

  ! Matrix size
  integer(c_int), parameter :: n = 2
  real(c_double), dimension(n*n), target :: A_data = [1.0_c_double, 3.0_c_double, &
                                               2.0_c_double, 4.0_c_double]
  real(c_double), dimension(n*n), target :: B_data = [5.0_c_double, 7.0_c_double, &
                                               6.0_c_double, 8.0_c_double]
  real(c_double), dimension(n*n), target :: C_data = 0.0_c_double

  integer :: i, j
  type(c_ptr) :: tmp



  ! --- Initialize Julia ---
  call jl_init()

  ! Add current directory to LOAD_PATH and load MyLinAlg
  tmp = jl_eval_string('push!(LOAD_PATH,".")'//C_NULL_CHAR)
  tmp = jl_eval_string('using MyLinAlg'//C_NULL_CHAR)

  ! Get MyLinAlg module and mul! function
  jl_main_mod = jl_main_module
  mylinalg_val = jl_get_global(jl_main_mod, jl_symbol("MyLinAlg"//C_NULL_CHAR))
  mylinalg = mylinalg_val
  mulbang = jl_get_function(mylinalg, "mul!"//C_NULL_CHAR)

  ! Array type for Array{Float64,2}
  array2d_type = jl_apply_array_type(jl_float64_type, 2)

  ! Build dims tuple (n,n) by calling Julia's tuple function
  dims = jl_eval_string("(2,2)"//C_NULL_CHAR)  ! simple way in Fortran 95

  ! Wrap C arrays (no copy)
  A = jl_ptr_to_array(array2d_type, c_loc(A_data), dims, 0)
  B = jl_ptr_to_array(array2d_type, c_loc(B_data), dims, 0)
  C = jl_ptr_to_array(array2d_type, c_loc(C_data), dims, 0)

  ! Call mul!(C, A, B)
  tmp = jl_call3(mulbang, C, A, B)

  ! Print result
  print *, "Result C = A*B:"
  do i = 1, n
    do j = 1, n
      write(*,'(F6.2)', advance="no") C_data(i + (j-1)*n)
      write(*,'(A)', advance="no") " "
    end do
    print *
  end do

  call jl_atexit_hook(0)

end program test_matmul
