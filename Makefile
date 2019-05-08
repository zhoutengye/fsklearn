#-----------BEGIN MAKEFILE-------------------------------------------------
	SHELL = /bin/sh
  EXEC  = fsklearn_test
  MAIN = test_fsklearn.f90

#--------------------------------------------------------------------------
#        PRECISION          DEFAULT PRECISION: SINGLE                     
#                           UNCOMMENT TO SELECT DOUBLE PRECISION
  # FLAG_1 = -DDOUBLE_PRECISION 
#--------------------------------------------------------------------------
#        Used to choose two cases for test
#        Not necessary for the implementation
  # FLAG_2 = -DPARALLEL
  FLAG_3 = -DFSKLEARN_TRAINING
  # FLAG_4 = -DFSKLEARN_PREDICTION

#--------------------fortran compiler and flags----------------------------
  CPP      = /usr/bin/cpp 
  CPPFLAGS = -P -traditional 
  CPPARGS  = $(CPPFLAGS) $(FLAG_1) $(FLAG_2) $(FLAG_3) $(FLAG_4) $(FLAG_5) 

	FC       = ifort
	# FC       = mpiifort
	# FC       = gfortran
	# FC       = mpif90
  OPT      =  -O3
  FCFLAGS  = $(OPT)
  FFLAGS   = $(OPT)
#==========================================================================

BUILD_DIR = build
SRC_DIR = src
EXEC_F = ${BUILD_DIR}/${EXEC}
MAIN_F = ${SRC_DIR}/${MAIN}
OBJ_DIR = $(BUILD_DIR)/obj
PRE_DIR = $(BUILD_DIR)/src_pre
MOD_DIR = $(BUILD_DIR)/mod
SRCS := $(wildcard $(SRC_DIR)/mod_*.f90) $(SRC_DIR)/${MAIN}
PRES := $(patsubst $(SRC_DIR)/%.f90,$(PRE_DIR)/%.f90,$(SRCS))
OBJS := $(patsubst $(PRE_DIR)/%.f90,$(OBJ_DIR)/%.o,$(PRES))

# keep the *.f90 files in $(PRE_DIR) after make
.PRECIOUS: $(PRE_DIR)/%.f90


# pre-processors
$(PRE_DIR)/%.f90 : $(SRC_DIR)/%.f90
	$(CPP) $(CPPARGS) $< $@

# compile objective files
$(OBJ_DIR)/%.o : $(PRE_DIR)/%.f90
ifeq ($(FC),$(filter $(FC), ifort mpiifort))
	$(FC) -c $(FCFLAGS) $(OPT) $< -o $@ -module $(MOD_DIR)
else ifeq ($(FC), $(filter $(FC), gfortran mpif90))
	$(FC) -c $(FCFLAGS) $< -o $@ -J $(MOD_DIR)
endif

# link and generate the exec file
$(EXEC_F): $(OBJS)
	$(FC) $(FCLAGS) -o $(EXEC_F) $(OBJS)

# Create all directories needed, pre-processor comes first
$(PRES): | $(PRE_DIR)

$(PRE_DIR):
	mkdir -p $(BUILD_DIR)
	mkdir -p $(PRE_DIR)
	mkdir -p $(OBJ_DIR)
	mkdir -p $(MOD_DIR)


#--------------------------------------------------------------------------
#  Tar Up Code
#--------------------------------------------------------------------------

tarfile:
	tar cvf fsklearn_test.tar src/*.f90  Makefile

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

# just run
run:
ifeq ($(FC),$(filter $(FC), ifort gfortran))
	./$(BUILD_DIR)/$(EXEC)
else
	mpirun -np 4 ./$(BUILD_DIR)/$(EXEC)
endif

# compile and run
crun:
	$(EXEC_F)
ifeq ($(FC),$(filter $(FC), ifort gfortran))
	./$(BUILD_DIR)/$(EXEC)
else
	mpirun -np 4 ./$(BUILD_DIR)/$(EXEC)
endif

# re-compile 
rc:
	make clean
	$(EXEC_F)
ifeq ($(FC),$(filter $(FC), ifort gfortran))
	./$(BUILD_DIR)/$(EXEC)
else
	mpirun -np 4 ./$(BUILD_DIR)/$(EXEC)
endif

clean:
		/bin/rm -f ${OBJ_DIR}/*.o
		/bin/rm -f ${MOD_DIR}/*.mod
		/bin/rm -f ${PRE_DIR}/*.f90

clobber:
		/bin/rm -rf build

