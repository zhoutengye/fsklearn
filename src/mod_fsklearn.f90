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
! 1. read the parameters from namelist file
! 2. write the parameters to the json file (if TRAINING
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
!    + The parameters for training comes from the .json file
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
!    - %Para_Read:
!       + Read the parameters and coefficients during
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
# if defined(DOUBLE_PRECISION)
  Integer, Private, Parameter :: PS = 8
# else
  Integer, Private, Parameter :: PS = 4
# endif

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Fsklearn Type Variables↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  Type :: Fsklearn_Define
    Character(20) :: training_type
    Integer :: n_inputs
    Integer :: n_outputs
    Real(PS), Allocatable :: Inputs(:)
    Real(PS), Allocatable :: Outputs(:)
    Procedure(Choose_Read),    Pointer, NoPass :: Para_Read => NULL()
    Procedure(Choose_Predict), Pointer, NoPass :: Predict => NULL()
  Contains
    Procedure, Nopass :: Gen_Training   => Generate_Training_Data
    Procedure, Nopass :: Initialization => fsklearn_Initialization
  End Type Fsklearn_Define

  Interface
    Function Choose_Predict(input, n_input, n_output)
      Import PS
      Implicit None
      Integer  :: n_input
      Integer  :: n_output
      Real(PS) :: Input(n_input)
      Real(PS) :: Choose_Predict(n_output)
    End Function Choose_Predict
  End Interface

  Interface
    Subroutine Choose_Read
    End Subroutine Choose_Read
  End Interface

  ! Interface for the write line function
  !    dealing with different data type
  Interface Write_Line
    Module Procedure Write_Line_Integer
    Module Procedure Write_Line_Real
  End Interface Write_Line
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Fsklearn Variables↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


  ! Predefined type variables.
  ! One may use one's own declaration
  Type(Fsklearn_Define) :: F_Sklearn

Contains

  Subroutine Fsklearn_Initialization

# if defined(PARALLEL)
    Use mpi
# endif
    Implicit None

    Character(20)  :: training_type
    Integer        :: n_inputs
    Integer        :: n_outputs
    Character(100) :: tmp_name
    Character(100) :: str_id
    Logical        :: train_after_run
# if defined(PARALLEL)
    Integer :: myid, ier, ierr, n_proc
# endif

    Namelist /sizes/ n_inputs, n_outputs
    Namelist /train_type/ training_type, train_after_run

    ! set path
    training_data_path = 'build/training/'
    para_files_path ='build/fsklearn_files/'
    Call Execute_Command_Line('mkdir -p '//training_data_path)
    Call Execute_Command_Line('mkdir -p '//para_files_path)

# if defined (PARALLEL)
! mpi version
    ! Read from namelist file
    Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
    Call MPI_COMM_SIZE(MPI_COMM_WORLD, n_proc, ier)
    If (myid .eq. 0) Then
      tmp_name = Trim(Adjustl(para_files_path))// &
          Trim(Adjustl(set_ML_file))
      Open(79, file = tmp_name) ! TR_1 &PR_1

      Read(79, nml = sizes)
      Read(79, nml = train_type)

    End If

    Call MPI_BCAST(n_inputs, 1, MPI_INT, 0, &
        MPI_COMM_WORLD, ier)
    Call MPI_BCAST(n_outputs, 1, MPI_INT, 0, &
        MPI_COMM_WORLD, ier)
    Call MPI_BCAST(training_type, 20, MPI_CHARACTER, 0, &
        MPI_COMM_WORLD, ier)

    F_Sklearn%training_type = training_type
    F_Sklearn%n_inputs      = n_inputs
    F_Sklearn%n_outputs     = n_outputs

    Write(str_id,"(I10)") myid
    TMP_NAME = TRIM(adjustl(training_data_path))// &
        TRIM(adjustl(traing_input_name))// &
        Trim(adjustl(str_id))// &
        '.dat'
    Open(2000+myid,file=tmp_name,status='unknown')
    TMP_NAME = TRIM(adjustl(training_data_path))// &
        TRIM(adjustl(traing_output_name))// &
        Trim(adjustl(str_id))// &
        '.dat'
    Open(3000+myid,file=tmp_name,status='unknown')

# if defined (FSKLEARN_TRAINING)
    ! TR_3: Write parameters to json file
    If (myid .eq. 0) Then
      tmp_name = Trim(adjustl(para_files_path))// &
          Trim(adjustl(f2py_training_param))
      Open(78, file = tmp_name)
      Write(78, *) '{'
      Write(78, *) '    "num_mpi": ', n_proc, ','
      Write(78, *) '    "data_path": "', TRIM((adjustl(training_data_path))), '",'
      Write(78, *) '    "para_path": "', TRIM((adjustl(para_files_path))), '",'
      Write(78, *) '    "training_type": "', TRIM((adjustl(training_type))),'"'
      Write(78, *) '}'
    End If
# endif

# else
! sequential version
    ! Read from namelist file
    tmp_name = Trim(Adjustl(para_files_path))// &
        Trim(Adjustl(set_ML_file))
    Open(79, file = tmp_name)

    Read(79, nml = sizes)
    Read(79, nml = train_type)

    F_Sklearn%training_type = training_type
    F_Sklearn%n_inputs      = n_inputs
    F_Sklearn%n_outputs     = n_outputs

    tmp_name = Trim(Adjustl(training_data_path))// &
        Trim(Adjustl(traing_input_name))// &
        '.dat'
    Open(2000, file=tmp_name,status='unknown')
    tmp_name = Trim(Adjustl(training_data_path))// &
        Trim(Adjustl(traing_output_name))// &
        '.dat'
    Open(3000, file=tmp_name,status='unknown')


# if defined (FSKLEARN_TRAINING)
    ! TR_2: Write parameters to json file
      tmp_name = Trim(Adjustl(para_files_path))// &
          Trim(Adjustl(f2py_training_param))
      Open(78, file = tmp_name)
      Write(78, *) '{'
      Write(78, *) '    "data_path": "', Trim((Adjustl(training_data_path))), '",'
      Write(78, *) '    "para_path": "', TRIM((adjustl(para_files_path))), '",'
      Write(78, *) '    "training_type": "', TRIM((adjustl(training_type))),'"'
      Write(78, *) '}'
# endif

# endif

    ! TR_3 & PR_3: Assign the procedure pointer to the type defined
    ! in the namelist file
    If ( Trim(F_Sklearn%training_type) .eq. 'Neural_Network' ) Then
      F_Sklearn%Para_Read => Read_Neural_Network
      F_Sklearn%Predict => Predict_Neural_Network
    ElseIf ( Trim(F_Sklearn%Training_type) .eq. 'Decision_Tree' ) Then
      F_Sklearn%Para_Read => Read_Decision_Tree
      F_Sklearn%Predict => Predict_Decision_Tree
    ElseIf ( Trim(F_Sklearn%Training_type) .eq. 'Random_Forest' ) Then
      F_Sklearn%Para_Read => Read_Random_Forest
      F_Sklearn%Predict => Predict_Random_Forest
    Else
      write(*,*) 'Wrong training type!'
      stop
    End If

    ! Read the parameters from the .dat file
# if defined (FSKLEARN_PREDICTION)
    ! Coefficients
    call F_Sklearn%Para_Read
# endif

  End Subroutine Fsklearn_Initialization


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Generate_Training_Data↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !
  !-----------------------------------------------------
  ! This part need to be modified according to the
  !   specific needs.
  ! I this parts,
  !-----------------------------------------------------
  !
  !-----------------------------------------------------
  ! I/O
  !-----------------------------------------------------
  !
  ! Defined by user depending on the problem
  !-----------------------------------------------------
  Subroutine Generate_Training_Data &
      (T_data, input_len, output_len, data_num)

# if defined (PARALLEL)
    Use mpi
# endif
    Implicit None
# if defined (PARALLEL)
    Integer  :: myid, n_proc, ier
# endif
    Integer  :: input_len, output_len, data_num
    Integer  :: num_total
    Real(PS), Dimension(data_num, input_len+output_len) :: T_data
    Integer   :: i

    num_total = input_len + output_len

# if defined (PARALLEL)
    Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
    Do i = 1, data_num
      Call Write_Line(2000+myid,T_data(i,1:input_len),input_len)
      Call Write_Line(3000+myid,T_data(i,input_len+1:num_total),output_len)
    End Do
# else
    Do i = 1, data_num
      Call Write_Line(2000,T_data(i,1:input_len),input_len)
      Call Write_Line(3000,T_data(i,input_len+1:num_total),output_len)
    End Do
# endif

  End Subroutine Generate_Training_Data
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Generate_Training_Data↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Write_Line↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  ! TR_4 & TR_5
  Subroutine Training
    Implicit None
    if (myid .eq. 0) then
      CALL Execute_Command_Line('python '//Adjustl(Trim(para_files_path)) &
          //'fsklearn_training.py')
    end if
    Call MPI_BARRIER(MPI_COMM_WORLD, ier)
  End Subroutine Training

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Write_Line↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  ! Subroutine that writes a line of data to the object file
  Subroutine Write_Line_Integer(file_num, vector, length)
    Implicit None

    Integer :: file_num
    Integer :: length
    Integer :: vector(length)

    Write(file_num,*) vector

  End Subroutine Write_Line_Integer

  Subroutine Write_Line_Real(file_num, vector, length)
    Implicit None

    Integer :: file_num
    Integer :: length
    Real(PS) :: vector(length)

    Write(file_num,'(*(F14.6))') vector

  End Subroutine Write_Line_Real
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Write_Line↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

End Module Mod_Fsklearn
