#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/sh
            EXEC          = fsklearn_test

#--------------------------------------------------------------------------
#        PRECISION          DEFAULT PRECISION: SINGLE                     
#                           UNCOMMENT TO SELECT DOUBLE PRECISION
  # FLAG_1 = -DDOUBLE_PRECISION 
#--------------------------------------------------------------------------
#        Used to choose two cases for test
#        Not necessary for the implementation
  FLAG_2 = -DMETHOD_ONE
  # FLAG_2 = -DMETHOD_TWO
  FLAG_3 = -DGEN_TRAINING
  FLAG_4 = -DTRAINING

#--------------------fortran compiler and flags----------------------------
  CPP      = /usr/bin/cpp 
  CPPFLAGS = -P -traditional 
  CPPARGS  = $(CPPFLAGS) $(FLAG_1) $(FLAG_2) $(FLAG_3) $(FLAG_4) $(FLAG_5) 
  
  FC       = ifort
  OPT      = -fast
  FCFLAGS  = $(OPT) -free -Tf 
  FFLAGS   = $(OPT) 
#==========================================================================


.SUFFIXES: .o .f90 

.f90.o:
	$(CPP) $(CPPARGS) $*.f90 > $*.f95
	$(FC)  -c $(FCFLAGS) $(INCS) $*.f95
#	\rm $*.f90
#--------------------------------------------------------------------------
#  SUBGRID Source Code.
#--------------------------------------------------------------------------

MODS  = mod_fsklearn.f90
MAIN  = test_fsklearn.f90
SRCS = $(MODS)  $(MAIN)
OBJS = $(SRCS:.f90=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(FFLAGS) -o $(EXEC) $(OBJS) $(LIBS)
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#  Tar Up Code                           
#--------------------------------------------------------------------------

tarfile:
	tar cvf fsklearn_test.tar *.F  Makefile

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod

clobber:	clean
		/bin/rm -f *.f95 *.o fsklearn_test








