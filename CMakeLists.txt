cmake_minimum_required(VERSION 3.10)
project(JuliaEmbedding C Fortran)

# julia is assumed to be in PATH
# use julia-config.jl for compilation flags
execute_process(
    COMMAND julia -e "print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, \"julia\", \"julia-config.jl\"))"
    OUTPUT_VARIABLE JULIA_CONFIG_PATH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND "${JULIA_CONFIG_PATH}" --cflags
    OUTPUT_VARIABLE JULIA_CFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND "${JULIA_CONFIG_PATH}" --ldflags
    OUTPUT_VARIABLE JULIA_LDFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND "${JULIA_CONFIG_PATH}" --ldlibs
    OUTPUT_VARIABLE JULIA_LDLIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)


set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${JULIA_CFLAGS}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -C -fPIC")


add_executable(embed embed.c)
add_executable(test_matmul test_matmul.c)
add_executable(test_matmulf test_matmulf.f90)


target_link_libraries(embed PRIVATE ${JULIA_LDFLAGS} ${JULIA_LDLIBS})
target_link_libraries(test_matmul PRIVATE ${JULIA_LDFLAGS} ${JULIA_LDLIBS})
target_link_libraries(test_matmulf PRIVATE ${JULIA_LDFLAGS} ${JULIA_LDLIBS})
