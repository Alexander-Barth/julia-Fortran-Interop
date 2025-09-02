#JL_SHARE = $(shell julia -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))')
#CFLAGS   += $(shell $(JL_SHARE)/julia-config.jl --cflags)
#CXXFLAGS += $(shell $(JL_SHARE)/julia-config.jl --cflags)
#LDFLAGS  += $(shell $(JL_SHARE)/julia-config.jl --ldflags)
#LDLIBS   += $(shell $(JL_SHARE)/julia-config.jl --ldlibs)


JULIA_BASE = /mnt/data1/abarth/.julia/juliaup/julia-1.11.6+0.x64.linux.gnu/
CFLAGS = -g -C -std=gnu11 -I$(JULIA_BASE)include/julia -fPIC
CXXFLAGS = -std=gnu11 -I$(JULIA_BASE)include/julia -fPIC
FFLAGS= -g -C  -fPIC
LDFLAGS = -L$(JULIA_BASE)lib -Wl,--export-dynamic
LDLIBS = -Wl,-rpath,$(JULIA_BASE)lib -Wl,-rpath,$(JULIA_BASE)lib/julia -ljulia

all: embed test_matmul test_matmulf
embed: embed.c
#test_matmul: test_matmul.c

test_matmulf:
	gfortran test_matmulf.f90 $(FFLAGS) $(LDFLAGS) $(LDLIBS) -o test_matmulf

echo:
	echo $(CFLAGS)
	echo $(CXXFLAGS)
	echo $(LDFLAGS)
	echo $(LDLIBS)

clean:
	rm -f embed test_matmul
