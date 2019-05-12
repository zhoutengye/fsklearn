!   ! self%training_data_path = 'build/training/'
!   ! self%coef_files_path ='build/fsklearn_files/'

! # if defined (FSKLEARN_TRAINING)
!   ! TR_3: Write Coefficients to python and json file 
!   If (myid .eq. 0) Then
!     tmp_name = Trim(adjustl(self%coef_files_path))// &
!         Trim(adjustl(self%training_py))
!     Open(77, file = tmp_name)
!     Call self%Read_Training_Param(79,self%Training_type)
!     Call self%Generate_Training_Py(77)
!   End If
! # endif

! # if defined (FSKLEARN_TRAINING)
!   ! TR_2: Write coefficients to python and json file 
!   tmp_name = Trim(adjustl(self%coef_files_path))// &
!       Trim(adjustl(self%training_py))
!   Open(77, file = tmp_name)
!   Call self%Read_Training_Param(79,self%Training_type)
!   Call self%Generate_Training_Py(77)
! # endif

  ! TR_3 & PR_3: Assign the procedure pointer to the type defined
  ! in the namelist file


module test

  Integer, Parameter :: PS = 4

  Type String
    Character(100) :: str
  End type String

  Type, Public :: Fsklearn_IO
    Character(20) :: training_type
    Character(1000) :: Training_script
    Integer :: n_inputs
    Integer :: n_outputs
    Logical :: train_after_run
    Real(PS), Allocatable :: Inputs(:)
    Real(PS), Allocatable :: Outputs(:)
    Integer :: num_para
    Type(String) , Allocatable :: key(:)
    Type(String) , Allocatable :: value(:)
    Character(100) :: Coef_files_path      = ''
    Character(100) :: Coef_file_Name       = ''
    Character(100) :: set_ML_file          = 'fsklearn_coef.namelist'
    Character(100) :: training_py          = 'training.py'
    Character(100) :: training_data_path   = ''
    Character(100) :: training_input_name  = 'training_input'
    Character(100) :: training_output_name = 'training_output'
  Contains
    Procedure :: Common_Initialization
    Procedure :: Read_Coef => Common_Read_Coef
    Procedure :: Gen_PY => Generate_Training_Python
    Procedure :: Read_Training_Param => Common_Read_and_Update_Para
    Procedure :: Gen_Para_Script => Generate_Parameter_Script
    Procedure :: Py_Import
    Procedure :: PY_main
    Procedure :: PY_sk2f
  End Type Fsklearn_IO

  Type Ragged_Vector
    Real(PS), Allocatable :: Vec(:)
  End Type Ragged_vector
  Type Ragged_Matrix
    Real(PS), Allocatable :: Mat(:,:)
  End Type Ragged_Matrix
  Type :: NN_activation
    Procedure(Sub_Interface), Pointer, NoPass :: activate => NULL()
  End Type NN_activation

  Interface
    Function Sub_Interface(n, X)
      Import :: PS
      Integer,  Intent(in) :: n
      Real(PS), Intent(in), Dimension(n) :: X
      Real(PS), Dimension(n) :: Sub_Interface
    End Function Sub_Interface
  End Interface

  Type, Extends(Fsklearn_IO) :: Neural_Network
    Integer :: layers
    Integer, Allocatable :: Layer_Size(:)
    Type(Ragged_Vector), Allocatable :: Activations(:)
    Type(Ragged_Vector), Allocatable :: Intercepts(:)
    Type(Ragged_Matrix), Allocatable :: Coefs(:)
    Character(10) :: Activation_type
    Character(10) :: out_Activation_type
    Type(NN_Activation) :: Activation
    Type(NN_Activation) :: Out_Activation
    Contains
      Procedure :: Read_Coef => NN_Read_Coef
      Procedure :: Predict_One => NN_Predict_One
      Procedure :: Predict_Vec => NN_Predict_Vec
      Procedure :: Predict_Mat => NN_Predict_Mat
      Procedure :: Read_Training_Param => NN_Read_and_Update_Para
  End Type Neural_Network

  Type Nodes
    Integer  :: children_left
    Integer  :: children_right
    Integer  :: feature
    Real(PS) :: threshold
    Real(PS), Allocatable :: Values(:)
  ! Contains
  End Type Nodes

  type Trees
    Integer :: node_count
    Integer :: max_depth
    Type(Nodes), Allocatable :: Node(:)
  End type Trees

  Type, Extends(Fsklearn_IO) :: Decision_Tree
    Type(Trees) :: Tree
  Contains
    Procedure :: Read_Coef => DT_Read_Coef
    Procedure :: Predict_One => DT_Predict_One
    Procedure :: Predict_Vec => DT_Predict_Vec
    Procedure :: Predict_Mat => DT_Predict_Mat
    Procedure :: Read_Training_Param => DT_Read_and_Update_Para
  End Type Decision_Tree

  Type, Extends(Fsklearn_IO) :: Random_Forest
    Integer :: tree_count
    Type(Trees), Allocatable :: Trees(:)
  Contains
    Procedure :: Read_Coef => RF_Read_Coef
    Procedure :: Predict_One => RF_Predict_One
    Procedure :: Predict_Vec => RF_Predict_Vec
    Procedure :: Predict_Mat => RF_Predict_Mat
    Procedure :: Read_Training_Param => RF_Read_and_Update_Para
  End Type Random_Forest

   interface Read_Coef
     Module Procedure DT_Read_Coef
     Module Procedure NN_Read_Coef
     Module Procedure RF_Read_Coef
   end Interface Read_Coef

   interface Predict
     Module Procedure DT_Predict_One
     Module Procedure NN_Predict_One
     Module Procedure RF_Predict_One
     Module Procedure DT_Predict_Vec
     Module Procedure NN_Predict_Vec
     Module Procedure RF_Predict_Vec
     Module Procedure DT_Predict_Mat
     Module Procedure NN_Predict_Mat
     Module Procedure RF_Predict_Mat
   end Interface Predict

 Contains
   Subroutine Common_Initialization(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     Print *,'Common_Initialization'

     select type (self)
     type is (Fsklearn_IO)
       ! no further initialization required
     class is (Neural_Network)
       self%Training_Type = 'Neural_Network'
       self%num_para = 21
       Allocate(self%key(self%num_para))
       Allocate(self%value(self%num_para))
     class is (Decision_Tree)
       self%Training_Type = 'Decision_Tree'
       self%num_para = 12
       Allocate(self%key(self%num_para))
       Allocate(self%value(self%num_para))
     class is (Random_Forest)
       self%Training_Type = 'Random_Forest'
       self%num_para = 16
       Allocate(self%key(self%num_para))
       Allocate(self%value(self%num_para))
     end select
     print *, self%training_type

     Call self%Read_Coef

     Call self%Gen_Py

   End Subroutine Common_Initialization

   Subroutine Common_Read_Coef(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     print *, 'No training type is assigned, no coefficient will be loaded'
   end Subroutine Common_Read_Coef

   Subroutine NN_Read_Coef(self)
     Implicit None
     Class(Neural_Network) :: self
     print *, 'NN_READ_COEF'
   end Subroutine NN_Read_Coef

   Subroutine DT_Read_Coef(self)
     Implicit None
     Class(Decision_Tree) :: self
     print *, 'DT_READ_CEOF'
   end Subroutine DT_Read_Coef

   Subroutine RF_Read_Coef(self)
     Implicit None
     Class(Random_Forest) :: self
     print *, 'RF_READ_COEF'
   end Subroutine RF_Read_Coef

   Function NN_Predict_One(self, input)
     Implicit None
     Class(Neural_Network) :: self
     Real(PS) :: input(self%n_inputs)
     Real(PS) :: NN_Predict_One(self%n_outputs)
     print *, 'NN_Predict_One'
     NN_Predict_One = 1.0
   end Function NN_Predict_One

   Function DT_Predict_One(self, input)
     Implicit None
     Class(Decision_Tree) :: self
     Real(PS) :: input(self%n_inputs)
     Real(PS) :: DT_Predict_One(self%n_outputs)
     print *, 'DT_Predict_One'
     DT_Predict_One = 1.0
   end Function DT_Predict_One

   Function RF_Predict_One(self, input)
     Implicit None
     Class(Random_Forest) :: self
     Real(PS) :: input(self%n_inputs)
     Real(PS) :: RF_Predict_One(self%n_outputs)
     print *, 'RF_Predict_One'
     RF_Predict_One = 1.0
   end Function RF_Predict_One

   Function NN_Predict_Vec(self, input, n_data)
     Implicit None
     Class(Neural_Network) :: self
     Integer  :: n_data
     Real(PS) :: input(self%n_inputs,n_data)
     Real(PS) :: NN_Predict_Vec(self%n_outputs,n_data)
     print *, 'NN_Predict_Vec'
     NN_Predict_Vec = 1.0
   End Function NN_Predict_Vec

   Function DT_Predict_Vec(self, input, n_data)
     Implicit None
     Class(Decision_Tree) :: self
     Integer  :: n_data
     Real(PS) :: input(self%n_inputs,n_data)
     Real(PS) :: DT_Predict_Vec(self%n_outputs,n_data)
     print *, 'DT_Predict_Vec'
     DT_Predict_Vec = 1.0
   End Function DT_Predict_Vec

   Function RF_Predict_Vec(self, input, n_data)
     Implicit None
     Class(Random_Forest) :: self
     Integer  :: n_data
     Real(PS) :: input(self%n_inputs,n_data)
     Real(PS) :: RF_Predict_Vec(self%n_outputs,n_data)
     print *, 'RF_Predict_Vec'
     RF_Predict_Vec = 1.0
   End Function RF_Predict_Vec

   Function NN_Predict_Mat(self, input, nx_data, ny_data)
     Implicit None
     Class(Neural_Network) :: self
     Integer  :: nx_data, ny_data
     Real(PS) :: input(self%n_inputs,ny_data, ny_data)
     Real(PS) :: NN_Predict_Mat(self%n_outputs, nx_data, ny_data)
     print *, 'NN_Predict_Mat'
     NN_Predict_Mat = 1.0
   End Function NN_Predict_Mat

   Function DT_Predict_Mat(self, input, nx_data, ny_data)
     Implicit None
     Class(Decision_Tree) :: self
     Integer  :: nx_data, ny_data
     Real(PS) :: input(self%n_inputs,ny_data, ny_data)
     Real(PS) :: DT_Predict_Mat(self%n_outputs, nx_data, ny_data)
     print *, 'DT_Predict_Mat'
     DT_Predict_Mat = 1.0
   End Function DT_Predict_Mat

   Function RF_Predict_Mat(self, input, nx_data, ny_data)
     Implicit None
     Class(Random_Forest) :: self
     Integer  :: nx_data, ny_data
     Real(PS) :: input(self%n_inputs,ny_data, ny_data)
     Real(PS) :: RF_Predict_Mat(self%n_outputs, nx_data, ny_data)
     print *, 'RF_Predict_Mat'
     RF_Predict_Mat = 1.0
   End Function RF_Predict_Mat

   Subroutine Common_Generate_Parameter_Script(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     print *, 'No training type is assigned, no coefficient will be loaded'
   end Subroutine Common_Generate_Parameter_Script

   Subroutine NN_Generate_Parameter_Script(self)
     Implicit None
     Class(Neural_Network) :: self
     print *,'NN_generate_parameter_script'
   end Subroutine NN_Generate_Parameter_Script

   Subroutine DT_Generate_Parameter_Script(self)
     Implicit None
     Class(Decision_Tree) :: self
     print *,'DT_generate_parameter_script'
   end Subroutine DT_Generate_Parameter_Script

   Subroutine RF_Generate_Parameter_Script(self)
     Implicit None
     Class(Random_Forest) :: self
     print *,'RF_generate_parameter_script'
   end Subroutine RF_Generate_Parameter_Script

   Subroutine Common_Read_and_Update_Para(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     print *, 'No training type is assigned, no coefficient will be loaded'
   end Subroutine Common_Read_and_Update_Para

   Subroutine NN_Read_and_Update_Para(self)
     Implicit None
     Class(Neural_Network) :: self
     print *,'Read Training Para NN'
   end Subroutine NN_Read_And_Update_Para

   Subroutine DT_Read_and_Update_Para(self)
     Implicit None
     Class(Decision_Tree) :: self
     print *,'Read Training Para DT'
   end Subroutine DT_Read_And_Update_Para

   Subroutine RF_Read_and_Update_Para(self)
     Implicit None
     Class(Random_Forest) :: self
     print *,'Read Training Para RF'
   end Subroutine RF_Read_And_Update_Para

   Subroutine Generate_Training_Python(self)
     Implicit None
     Class(FSKLEARN_IO) :: self
     Print *,'Gen_Py'
     Call self%Read_Training_Param
     Call self%Gen_Para_script
     Call self%Py_Import
     Call self%Py_Main
     Call self%Py_sk2f
   End Subroutine Generate_Training_Python

   Subroutine PY_sk2f(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     print *,'sk2f'
   end Subroutine PY_sk2f

   Subroutine Py_Import(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     print *,'py_import'
   end Subroutine Py_Import

   Subroutine Py_Main(self)
     Implicit None
     Class(Fsklearn_IO) :: self
     print *,'py_main'
   end Subroutine Py_Main

end module test

program main

  use Test
  Implicit None
  Real(4) :: a1(2)
  Real(4) :: a2(3)
  Real(4) :: a3(2,10)
  Real(4) :: a4(3,10)
  Real(4) :: a5(2,10,10)
  Real(4) :: a6(3,10,10)
  Integer :: b, c
  ! Type(Neural_Network) :: learn
  Type(Decision_Tree) :: learn
  ! Type(Random_Forest) :: learn

  learn%n_inputs = 2
  learn%n_outputs = 3


  Call learn%Common_Initialization
  ! print *,''
  ! Call learn%Gen_Py
  ! print *,''
  ! Call learn%Read_Coef
  ! print *,''
  ! a2 = learn%Predict_One(a1)

end program main
