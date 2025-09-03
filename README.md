
This is an example how to call julia from Fortran.


Prerequesites (installed and in your PATH):
* julia (tested with version 1.11.6).
* cmake (tested vesion 3.28.3)
* C and Fortran compiler (tested with gcc/gfortran version 13.3.0)


The code can be compiled with:

```bash
cmake .
make
```

Before calling julia from Fortran it is instructive to call first Julia from C.


* `embed.c`: simple C example calling build-in functions in julia
* `test_matmul.c`: C example calling a function in the module `MyLinAlg.jl`
* `test_matmulf.c`: Fortran example calling a function in the module `MyLinAlg.jl`
