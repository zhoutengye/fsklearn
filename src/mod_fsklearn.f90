!--------------------------------------------------------
! Module for user to buld their own API to the specific
! computational models
!--------------------------------------------------------
!
! by Zhouteng Ye
! Last update: 05/12/2019
!---------------------------------------------------------

# if defined (PARALLEL)
Module Mod_Fsklearn

  Use Mod_Fsklearn_Essential ! Necessary to use this module
  Private
  Public Fsklearn_Example
# if defined(DOUBLE_PRECISION)
  Integer, Private, Parameter :: PS = 8
# else
  Integer, Private, Parameter :: PS = 4
# endif

  ! The pre-processor flags is not necessary, usually the type
  ! is determined while building the API to the computational model
# if(NeuralNetwork)
  Type, Extends(Neural_Network) :: Fsklearn_Example
# elif defined (DecisionTree)
  Type, Extends(Decision_Tree) :: Fsklearn_Example
# else
  Type, Extends(Random_Forest) :: Fsklearn_Example
# endif
  ! define some variables specifically for the computational model
    Integer :: num_data_sets
  Contains
    ! define some procedures for the computational model
    Procedure :: Initialization => Example_Initialization
    Procedure :: Gen_Training   => Example_Generate_Training_Data
    Procedure :: Prediction   => Example_Prediction
    Procedure :: Py_Training    => Example_Training
  End type Fsklearn_Example

Contains

  Subroutine Example_Initialization(self)
    Implicit None
    Class(Fsklearn_Example) :: self

    ! Set up parameters if possible
    self%training_data_path   = 'build/training/'
    self%coef_files_path      = 'build/fsklearn_files/'
    self%set_ML_file          = 'fsklearn_coef.namelist' ! default value
    self%training_input_name  = 'training_input'         ! default value
    self%training_output_name = 'training_output'        ! default value
    self%training_py          = 'training.py'            ! default value

    Call self%Common_Initialization ! Necessary

    self%num_data_sets = 250 ! assgin value to user defined variable

  End Subroutine Example_Initialization


  Subroutine Example_Generate_Training_Data(self, T_data)

    Use mpi
    Implicit None
    Class(Fsklearn_Example) :: self
    Integer  :: myid, n_proc, ier
    Integer  :: num_total
    Real(PS), Dimension(self%num_data_sets, self%n_inputs+self%n_outputs) :: T_data
    Real(PS), Dimension(self%n_inputs) :: input
    Real(PS), Dimension(self%n_outputs) :: output
    Integer   :: i

    num_total = self%n_inputs + self%n_outputs

    Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
    If (myid .eq. 0) Then
      Do i = 1, self%num_data_sets
        input = T_data(i, 1:self%n_inputs)
        output = T_data(i, self%n_inputs+1:self%n_outputs)
        ! Write line function is public in module mod_fsklearn_essential
        ! Call with
        !   Write_Line(file_number, input_vecrot, output_vector)
        Call Write_Line(2000+myid, input, self%n_outputs)
        Call Write_Line(3000+myid, output, self%n_outputs)
      End Do
    End If

  End Subroutine Example_Generate_Training_Data


  Function Example_Prediction(self, input)
    Implicit None
    Class(Fsklearn_Example) :: self
    Real(PS), Dimension(self%n_inputs) :: input
    Real(PS), Dimension(self%n_outputs) :: Example_Prediction

    Example_Prediction = self%predict_one(input)

  End Function Example_Prediction


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Call python to train↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !---------------------------------------------------------------
  ! If train_after_run is set as .True. in the .namelist file,
  ! run the python file after training.
  !---------------------------------------------------------------
  Subroutine Example_Training(self)
    Use mpi
    Implicit None
    Class(Fsklearn_Example) :: self

    Integer :: ier, myid

    print *, self%train_after_run 
    If (self%train_after_run .eqv. .True.) Then
      Call Execute_Command_Line('python '//Adjustl(Trim(self%coef_files_path)) &
            //Adjustl(Trim(self%training_py)) )
    End If

  End Subroutine Example_Training
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑end training↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑



End Module Mod_Fsklearn
#else
Module Mod_Fsklearn

  Use Mod_Fsklearn_Essential ! Necessary to use this module
  Private
  Public Fsklearn_Example
# if defined(DOUBLE_PRECISION)
  Integer, Private, Parameter :: PS = 8
# else
  Integer, Private, Parameter :: PS = 4
# endif

  ! The pre-processor flags is not necessary, usually the type
  ! is determined while building the API to the computational model
# if(NeuralNetwork)
  Type, Extends(Neural_Network) :: Fsklearn_Example
# elif defined (DecisionTree)
  Type, Extends(Decision_Tree) :: Fsklearn_Example
# else
  Type, Extends(Random_Forest) :: Fsklearn_Example
# endif
  ! define some variables specifically for the computational model
    Integer :: num_data_sets
  Contains
    ! define some procedures for the computational model
    Procedure :: Initialization => Example_Initialization
    Procedure :: Gen_Training   => Example_Generate_Training_Data
    Procedure :: Prediction   => Example_Prediction
    Procedure :: Py_Training    => Example_Training
  End type Fsklearn_Example

Contains

  Subroutine Example_Initialization(self)
    Implicit None
    Class(Fsklearn_Example) :: self

    ! Set up parameters if possible
    self%training_data_path   = 'build/training/'
    self%coef_files_path      = 'build/fsklearn_files/'
    self%set_ML_file          = 'fsklearn_coef.namelist' ! default value
    self%training_input_name  = 'training_input'         ! default value
    self%training_output_name = 'training_output'        ! default value
    self%training_py          = 'training.py'            ! default value

    Call self%Common_Initialization ! Necessary

    self%num_data_sets = 1000 ! assgin value to user defined variable

  End Subroutine Example_Initialization


  Subroutine Example_Generate_Training_Data(self, T_data)

    Implicit None
    Class(Fsklearn_Example) :: self
    Integer  :: num_total
    Real(PS), Dimension(self%num_data_sets, self%n_inputs+self%n_outputs) :: T_data
    Real(PS), Dimension(self%n_inputs) :: input
    Real(PS), Dimension(self%n_outputs) :: output
    Integer   :: i

    num_total = self%n_inputs + self%n_outputs

    Do i = 1, self%num_data_sets
      input = T_data(i, 1:self%n_inputs)
      output = T_data(i, self%n_inputs+1:self%n_outputs)
      ! Write line function is public in module mod_fsklearn_essential
      ! Call with
      !   Write_Line(file_number, input_vecrot, output_vector)
      Call Write_Line(2000, input, self%n_outputs)
      Call Write_Line(3000, output, self%n_outputs)
    End Do

  End Subroutine Example_Generate_Training_Data


  Function Example_Prediction(self, input)
    Implicit None
    Class(Fsklearn_Example) :: self
    Real(PS), Dimension(self%n_inputs) :: input
    Real(PS), Dimension(self%n_outputs) :: Example_Prediction

    Example_Prediction = self%predict_one(input)

  End Function Example_Prediction


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Call python to train↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !---------------------------------------------------------------
  ! If train_after_run is set as .True. in the .namelist file,
  ! run the python file after training.
  !---------------------------------------------------------------
  Subroutine Example_Training(self)
    Implicit None
    Class(Fsklearn_Example) :: self


    Call Execute_Command_Line('python '//Adjustl(Trim(self%coef_files_path)) &
        //Adjustl(Trim(self%training_py)) )

  End Subroutine Example_Training
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑end training↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑



End Module Mod_Fsklearn
# endif
