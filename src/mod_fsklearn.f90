!---------------------------------------------------------
! mod_fsklearn
!---------------------------------------------------------
!
! - Some modification needs to be done if you do not want
!   to change much to your code, including:
!   + Initialization
!   + Define the input and output vector and interface
!
! The Fsklearn_Initialization Do the following things:
! 1. read the coefficients from namelist file
! 2. write the coefficients to the json file (if TRAINING
!    flag is defined)
! 3. determine the machine learning method and assign
!    the procedure pointer to the corresponding
!    Subroutines.
!
!---------------------------------------------------------
! Flow chart for TRAINING:
! (Search the keyword TR_* to see the corresponding location)
!---------------------------------------------------------
!  - TR_1: Read from *.namelist file
!  - TR_2: Choose the training type
!    + Write to the .json file for python
!  - PR_3: Determine the training type
!    + the pointer procedure are assigned
!  - TR_4: Get training data
!    + May occur more than once
!  - TR_5: Write training data to data file
!    + May occur more than once
!  - TR_6: Call the *.py file for training
!    + [TO_UPDATE]
!    + Call from system command
!    + Execute after the program
!    + The coefficients for training comes from the .json file
!  - TR_6: Training to write *.dat file
!    + *.dat files are coefficients for prediction
!
!---------------------------------------------------------
! Flow chart for PREDICTION:
! (Search the keyword PR_* to see the corresponding location)
!---------------------------------------------------------
!  - PR_1: Read from *.namelist file
!  - PR_2: Determine the training type
!    + the pointer procedure are assigned
!  - PR_3: Read coefficients
!    + Read from the corresponding .dat
!  - PR_4: Predict
!    + May occur more than once
!
!--------------------------------------------------------
! Module variables:
!--------------------------------------------------------
!
! PS:
!    - Precision for float number.
!    - PS = 4: single precision
!    - PS = 8: double precision
!
! Fsklearn_Define:
!    - Derived type that contains the machine learning
!       variables. It can do training and prediction
!       with a uniform way.
!    - %n_inputs:
!       + length of the input vector
!       + It is necessary to provide the length of the
!         input vector during initialization.
!    - %n_outputs:
!       + length of the output vector
!       + It is necessary to provide the length of the
!         output vector during initialization.
!    - %Inputs:
!       + Input vector, not necessary
!    - %Outputs:
!       + Output vector, not necessary
!    - %Coef_Read:
!       + Read the Coefficients and coefficients during
!         initialization for PREDICTION.
!    - %Predict:
!       + Predict procedure
!    - %Gen_Training:
!       + Procedure for generate training data, can be
!         point to other subroutines.
!    - %Predict:
!       + Procedure for initialization, can be point to
!         other subroutines.
! F_sklearn:
!    - Derived type variable for Fsklearn_Define
!
!--------------------------------------------------------
!
! by Zhouteng Ye
! Last update: 04/17/2019
!---------------------------------------------------------
Module Mod_Fsklearn

  Use Mod_Fsklearn_Essential
  Private
# if defined(DOUBLE_PRECISION)
  Integer, Private, Parameter :: PS = 8
# else
  Integer, Private, Parameter :: PS = 4
# endif

  Type, Public, Extends(Neural_Network) :: Fsklearn_Example
    Contains
      Procedure :: Initialization
      Procedure :: Gen_Training => Generate_Training_Data
      Procedure :: Py_Training => Training
  End type Fsklearn_Example

Contains

  Subroutine Initialization(self)
    Implicit None
    Class(Fsklearn_Example) :: self

  End Subroutine Initialization

  Subroutine Generate_Training_Data &
      (self, T_data, data_num)

# if defined (PARALLEL)
    Use mpi
# endif
    Implicit None
    Class(Fsklearn_Example) :: self
# if defined (PARALLEL)
    Integer  :: myid, n_proc, ier
# endif
    Integer  :: data_num
    Integer  :: num_total
    Real(PS), Dimension(data_num, self%n_inputs+self%n_outputs) :: T_data
    Integer   :: i

    num_total = self%n_inputs + self%n_outputs

# if defined (PARALLEL)
    Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
    Do i = 1, data_num
      Call Write_Line(2000+myid,T_data(i,1:self%n_inputs),self%n_inputs)
      Call Write_Line(3000+myid,T_data(i,self%n_inputs+1:num_total),self%n_outputs)
    End Do
# else
    Do i = 1, data_num
      Call Write_Line(2000,T_data(i,1:self%n_inputs),self%n_inputs)
      Call Write_Line(3000,T_data(i,self%n_inputs+1:num_total),self%n_outputs)
    End Do
# endif

  End Subroutine Generate_Training_Data

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Call python to train↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !---------------------------------------------------------------
  ! If train_after_run is set as .True. in the .namelist file,
  ! run the python file after training.
  !---------------------------------------------------------------
  Subroutine Training(self)
# if defined (PARALLEL)
    Use mpi
    Implicit None
    Class(Fsklearn_Example) :: self

    Integer :: ier, myid

    If (self%train_after_run .eqv. .True.) Then
      Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)

      If (myid .eq. 0) then
        Call Execute_Command_Line('python '//Adjustl(Trim(self%coef_files_path)) &
            //Adjustl(Trim(self%training_py)) )
      End If
    End If

    Call MPI_BARRIER(MPI_COMM_WORLD, ier)
# else
    Implicit None
    Class(Fsklearn_Example) :: self
    If (self%train_after_run .eqv. .True.) Then
      Call Execute_Command_Line('python '//Adjustl(Trim(self%coef_files_path)) &
          //Adjustl(Trim(self%training_py)) )
    End If
# endif

  End Subroutine Training
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑end training↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

End Module Mod_Fsklearn
