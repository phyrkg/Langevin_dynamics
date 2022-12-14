#
# If you can use C/C++/NTL/GF2X define USE_NTL := yes
# else USE_NTL := no
#
#USE_NTL := yes
USE_NTL := no


# fortran90/95 compiler
FC  := ifort
#FC := gfortran

# c++ compiler
CXX = icpc
#CXX := g++

# fortran compiler options
FFLAGS := -O3 -g -traceback

# c++ compiler options
CXXFLAGS := -O3 -g

# put include path to NTL, gf2x libraries.
NTL_INCLUDES := -I$(HOME)/include

# put library path to NTL, gf2x and stdC++ libraries.
NTL_LIBS :=  -L$(HOME)/lib -lntl -lgf2x -lstdc++

# linker flag (use fortran)
LDFLAGS := -L/usr/lib/static

export FC
export FFLAGS
export CXX
export CXXFLAGS
export INCLUDES
export LIBS
export LDFLAGS

ifeq ($(USE_NTL),yes)
FFLAGS := $(FFLAGS) -D_NTL_
JUMPLIBDIR := jump_ahead_coeff
JUMPOBJ    := $(JUMPLIBDIR)/get_coeff.o
INCLUDES := $(NTL_INCLUDES)
LIBS := $(NTL_LIBS)
else
FFLAGS := $(FFLAGS) -UD_NTL_
JUMPLIBDIR := f_jump_ahead_coeff
JUMPOBJ    := $(JUMPLIBDIR)/f_get_coeff.o $(JUMPLIBDIR)/gf2xe.o
INCLUDES :=
LIBS :=
endif

.F90.o :

%.o : %.F90
	$(FC) $(FFLAGS) $(INCLUDES) -c $< -o $@

all : check mt_stream.o

check : check_stream_1.out check_stream_2.out check_stream_3.out

check_stream_1.out : check_stream_1
	@sh ./check_1.sh $@

check_stream_2.out : check_stream_2
	@sh ./check_2.sh $@

check_stream_3.out : check_stream_3
	@sh ./check_3.sh $@

check_stream_1 : check_stream_1.o mt_stream_debug.o $(JUMPOBJ)
	$(FC) $(LDFLAGS) -L/usr/lib/static $^ -o $@ $(LIBS)

check_stream_2 : check_stream_2.o mt_stream.o $(JUMPOBJ)
	$(FC) $(LDFLAGS) -L/usr/lib/static $^ -o $@ $(LIBS)

check_stream_3 : check_stream_3.o mt_stream_debug.o $(JUMPOBJ)
	$(FC) $(LDFLAGS) -L/usr/lib/static $^ -o $@ $(LIBS)

check_stream_1.o : check_stream.F90 mt_stream_debug.o
	$(FC) $(FFLAGS) -D_DEBUG_ -D_CHECK_STREAM_ $(INCLUDES) -c $< -o $@

check_stream_2.o : check_stream.F90 mt_stream.o
	$(FC) $(FFLAGS) $(INCLUDES) -c $< -o $@

check_stream_3.o : check_stream.F90 mt_stream_debug.o
	$(FC) $(FFLAGS) -D_DEBUG_ -D_CHECK_SAVEREAD_ $(INCLUDES) -c $< -o $@

mt_stream_debug.o : mt_stream.F90 mt_kind_defs.o
	$(FC) $(FFLAGS) -D_DEBUG_ $(INCLUDES) -c $< -o $@

mt_stream.o : mt_stream.F90 mt_kind_defs.o
	$(FC) $(FFLAGS) $(INCLUDES) -c $< -o $@

mt_kind_defs.o : mt_kind_defs.F90
	$(FC) $(FFLAGS) $(INCLUDES) -c $< -o $@

$(JUMPOBJ) :
	cd $(JUMPLIBDIR) && $(MAKE)

clean :
	rm -f *.o *.mod *~ *back check_stream_1 check_stream_2 check_stream_3 \
	check_stream_1.out check_stream_2.out check_stream_3.out \
	save.dat read.dat mt_state_save.dat
	(cd $(JUMPLIBDIR); make clean)
