program test_matmul
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

 call jl_setup()
 ! julia code is in the file MyLinAlg.jl assumed to be in the current directory
 call jl_add_load_path('.')
 mylinalg = jl_using('MyLinAlg')

 if (.not.c_associated(mylinalg)) then
   write(0,*) "ERROR: julia module MyLinAlg not found"
   stop
 end if

 ! get the julia function MyLinAlg.mul!
 fun_mul_b = jl_get_function(mylinalg, "mul!")

 if (.not.c_associated(fun_mul_b)) then
   write(0,*) "ERROR: function mul! not found"
   stop
 end if

 A = jl_to_array(A_data)
 B = jl_to_array(B_data)
 C = jl_to_array(C_data)

 ! Call the 3-argument function mul!(C, A, B)
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
