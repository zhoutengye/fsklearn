Module Mod_Fsklearn

  ! Subject to the choice of precision.
  ! Single precision by default
# if defined(DOUBLE_PRECISION)
  Integer, Private, Parameter :: PS = 8
# else
  Integer, Private, Parameter :: PS = 4
# endif

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Ragged vector and array ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !
  ! Declearation of ragged vector and ragged matrix
  ! Used to build neural nwtworks in this case
  !
  ! Ragged vectors for 2-D array consists of vectors with
  !   different length
  ! Ragged vectors for 3-D array consists of matrice with
  !   different sized

  Type Ragged_Vector
    Real(PS), Allocatable :: Vec(:)
  End Type Ragged_vector
  Type Ragged_Matrix
    Real(PS), Allocatable :: Mat(:,:)
  End Type Ragged_Matrix

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Ragged vector and array ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Neural Networks variables↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !
  ! NN_activation used to choose activation function
  !     at the beginning.
  ! Neural_Network for choosing
  Type :: NN_activation
    Procedure(Sub_Interface), Pointer, NoPass :: &
                                   Activate =>NULL()
  End Type NN_activation

  ! interface for choose activation type
  Interface
    Function Sub_Interface(n, x)
      Import :: PS
      Integer,  Intent(in) :: n
      Real(PS), Intent(in), Dimension(n) :: x
      Real(PS), Dimension(n) :: Sub_Interface
    End Function Sub_Interface
  End Interface

  ! neural network parameters
  Type :: Neural_Network
    Integer :: input_len
    Integer :: output_len
    Integer :: layers
    Integer, Allocatable :: Layer_Size(:)
    Type(Ragged_Vector), Allocatable :: Activations(:)
    Type(Ragged_Vector), Allocatable :: Intercepts(:)
    Type(Ragged_Matrix), Allocatable :: Coefs(:)
    Character(10) :: activation_type
    Character(10) :: out_activation_type
    Type(NN_Activation) :: Activation
    Type(NN_Activation) :: Out_Activation
  Contains
    ! procedure training  => training_Neural_Network
    Procedure , Nopass :: Para_read => Read_Neural_Network
    Procedure , Nopass :: Predict   => Predict_Neural_Network
  End Type Neural_Network

  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Neural Network Variables↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Decision Tree Variables↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  Type Nodes
    Integer  :: children_left
    Integer  :: children_right
    Integer  :: feature
    Real(PS) :: threshold
    Real(PS), Allocatable :: Values(:)
  End Type Nodes

  Type:: Decision_Tree
    Integer :: node_count
    Integer :: n_inputs
    Integer :: n_outputs
    Integer :: max_depth
    Type(Nodes), allocatable :: Node(:)
  Contains
    Procedure , nopass :: Para_Read => Read_Decision_Tree
    Procedure , Nopass :: Predict => Predict_Decision_Tree
  End Type Decision_Tree
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Decision Tree Variables↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Random Forest Variables↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  Type:: Random_Forest
    Integer :: tree_count
    Integer :: n_inputs
    Integer :: n_outputs
    Type(Decision_Tree), allocatable :: Trees(:)
  Contains
    Procedure , NoPass :: Para_Read => Read_Random_Forest
    Procedure , NoPass :: Predict => Predict_Random_Forest
  End Type Random_Forest
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Random Forest Variables↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


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
    Procedure, Nopass :: Gen_Training => Generate_Training_Data
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
    Subroutine Choose_read
    End Subroutine Choose_read
  End Interface

  ! Interface for the write line function
  !    dealing with different data type
  Interface Write_Line
    Module Procedure Write_Line_Integer
    Module Procedure Write_Line_Real
  End Interface Write_Line
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Fsklearn Variables↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓Files↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !
  ! Files associate with Fsklearn. If not set, the path
  ! of the training data and the training parameters
  ! will be "fsklearn_data" and "fsklearn_para" respectively
  ! in the current directory by default.
  !
  ! "training_data_path":
  !    - the path for training datas
  ! "training_input_name":
  !    - File name for input data
  !    - Located in the path "training_data_path"
  !    - The length of the data in each row is n_inputs
  !    - If mpi with N processot is defined, there will be
  !         N input files, then all files will be gathered
  !         automatically by the Python code.
  ! "training_output_name":
  !    - Located in the path "training_data_path"
  !    - file name for put put data
  !    - The length of the data in each row is n_outputs
  !         N input files, then all files will be gathered
  !         automatically by the Python code.
  ! "":
  !    - the path for training datas
  ! "set_ML_file":
  !    - File name for ata
  !    - Located in the path "training_data_path"

  Character(50) :: training_data_path
  Character(50) :: para_files_path
  Character(50) :: f2py_training_param
  Character(50) :: set_ML_file
  Character(50) :: traing_input_name
  Character(50) :: traing_output_name
  Character(50) :: nn_param_name
  Character(50) :: dt_param_name
  Character(50) :: rf_param_name

  !↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓End Files↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓


  ! Predefined type variables.
  ! One may use one's own decleration
  Type(Neural_Network) :: N_Work
  Type(Decision_Tree) :: D_Tree
  Type(Random_Forest) :: R_Forest
  Type(Fsklearn_Define) :: F_Sklearn

  !↓↓↓↓↓↓↓↓↓↓↓↓↓Customized variables↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
  !
  ! some variables specificially for the object model,
  ! for example, some variables used for calculating the
  ! input and output vectors.
  !
  !↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑End Fsklearn Variables↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

Contains

  Subroutine Fsklearn_Initialization

    implicit none

    character(20) :: training_type
    integer :: n_inputs
    integer :: n_outputs
    CHARACTER(LEN=80)::TMP_NAME
    CHARACTER(LEN=80):: str_id

    namelist /sizes/ n_inputs, n_outputs
    namelist /train_type/ training_type

    TMP_NAME = trim(adjustl(para_files_path))// &
        trim(adjustl(f2py_training_param))
    open(78,file = TMP_NAME)
    TMP_NAME = trim(adjustl(para_files_path))// &
        trim(adjustl(set_ML_file))
    open(79,file = TMP_NAME)

    Read(79,nml=sizes)
    Read(79,nml=train_type)

    training_data_path = ''
    para_files_path ='fsklearn_ML/'
    f2py_training_param = 'training_param.json'
    set_ML_file = 'fsklearn_param.namelist'
    traing_input_name = 'training_input'
    traing_output_name = 'training_output'
    nn_param_name = 'nn_param.dat'
    dt_param_name = 'dt_param.dat'
    rf_param_name = 'rf_param.dat'

    F_Sklearn%training_type = training_type
    F_Sklearn%n_inputs  = n_inputs
    F_Sklearn%n_outputs = n_outputs


    if ( trim(F_Sklearn%training_type) .eq. 'Neural_Network' ) then
      F_Sklearn%para_read => read_Neural_Network
      F_Sklearn%predict => predict_Neural_Network
    elseif ( trim(F_Sklearn%training_type) .eq. 'Decision_Tree' ) then
      F_Sklearn%para_read => read_Decision_Tree
      F_Sklearn%predict => predict_Decision_Tree
    elseif ( trim(F_Sklearn%training_type) .eq. 'Random_Forest' ) then
      F_Sklearn%para_read => read_Random_Forest
      F_Sklearn%predict => predict_Random_Forest
    else
      write(*,*) 'wrong training type!'
    end if

    call F_Sklearn%para_read

  end subroutine fsklearn_initialization


  subroutine read_Neural_Network
    implicit none

    integer :: i,j
    integer :: error
    character(len=20) :: string

    character :: activation
    character :: out_activation

    open(81,file='nn_output.dat',status='unknown')

    read(81,*,iostat=error) string
    read(81,*) N_Work%layers
    allocate(N_Work%layer_size(N_Work%layers))

    ! read 
    read(81,*,iostat=error) string
    read(81,*) N_Work%layer_size
    N_Work%input_len = N_Work%layer_size(1)
    N_Work%output_len = N_Work%layer_size(N_Work%layers)


    allocate(N_Work%activations(N_Work%layers))
    do i = 1,N_Work%layers
      allocate(N_Work%activations(i)%vec(N_Work%layer_size(i)))
    end do

    read(81,*,iostat=error) string
    allocate(N_Work%intercepts(N_Work%layers-1))
    do i = 1,N_Work%layers-1
      allocate(N_Work%intercepts(i)%vec(N_Work%layer_size(i+1)))
      read(81,*) N_Work%intercepts(i)%vec
    end do


    read(81,*,iostat=error) string
    allocate(N_Work%coefs(N_Work%layers-1))
    do i = 1,N_Work%layers-1
      allocate(N_Work%coefs(i)%mat(N_Work%layer_size(i+1),N_Work%layer_size(i)))
      read(81,*,iostat=error) string
      do j = 1,N_Work%layer_size(i+1)
        read(81,*) N_Work%coefs(i)%mat(j,:)
      end do
    end do

    read(81,*,iostat=error) string
    read(81,*) N_Work%activation_type
    read(81,*,iostat=error) string
    read(81,*) N_Work%out_activation_type

    close(81)

    if (trim(N_Work%activation_type).eq.'logistic') then
      N_Work%activation%activate => activation_logistic
    else if (trim(N_Work%activation_type).eq.'tanh') then
      N_Work%activation%activate => activation_tanh
    else if (trim(N_Work%activation_type).eq.'softmax') then
      N_Work%activation%activate => activation_softmax
    else if (trim(N_Work%activation_type).eq.'relu') then
      N_Work%activation%activate => activation_ReLU
    else if (trim(N_Work%activation_type).eq.'identity') then
      N_Work%activation%activate => activation_identity
    else
      write(*,*) 'invalid activation type'
    end if

    if (trim(N_Work%out_activation_type).eq.'logistic') then
      N_Work%out_activation%activate => activation_logistic
    else if (trim(N_Work%out_activation_type).eq.'tanh') then
      N_Work%out_activation%activate => activation_tanh
    else if (trim(N_Work%out_activation_type).eq.'softmax') then
      N_Work%out_activation%activate => activation_softmax
    else if (trim(N_Work%out_activation_type).eq.'relu') then
      N_Work%out_activation%activate => activation_ReLU
    else if (trim(N_Work%out_activation_type).eq.'identity') then
      N_Work%out_activation%activate => activation_identity
    else
      write(*,*) 'invalid output activation type'
    end if

  end subroutine read_Neural_Network

  subroutine read_Decision_Tree
    implicit none 
    integer :: i,j
    integer :: error
    character(len=20) :: string

    open(82,file='dt_output.dat',status='unknown')
    read(82,*,iostat=error) string
    read(82,*) D_TREE%node_count
    read(82,*,iostat=error) string
    read(82,*) D_TREE%n_inputs
    read(82,*,iostat=error) string
    read(82,*) D_TREE%n_outputs
    read(82,*,iostat=error) string
    read(82,*) D_TREE%max_depth

    allocate(D_TREE%node(D_TREE%node_count))

    do i = 1,D_TREE%node_count
      allocate(D_TREE%node(i)%values(D_TREE%n_outputs))
      read(82,*,iostat=error) string
      read(82,*) D_TREE%node(i)%children_left
      read(82,*) D_TREE%node(i)%children_right
      read(82,*) D_TREE%node(i)%feature
      read(82,*) D_TREE%node(i)%threshold
      read(82,*) D_TREE%node(i)%values
    end do

    close(82)

  end subroutine read_Decision_Tree

  subroutine read_Random_Forest
    implicit none
    integer :: i,j
    integer :: error
    character(len=20) :: string
    type(Decision_Tree) :: tree1

    open(83,file='rf_output.dat',status='unknown')
    read(83,*,iostat=error) string
    read(83,*) R_FOREST%tree_count

    allocate(R_FOREST%trees(R_FOREST%tree_count))

    do j = 1, R_FOREST%tree_count
      read(83,*,iostat=error) string
      read(83,*) R_FOREST%trees(j)%node_count
      read(83,*,iostat=error) string
      read(83,*) R_FOREST%trees(j)%n_inputs
      read(83,*,iostat=error) string
      read(83,*) R_FOREST%trees(j)%n_outputs
      read(83,*,iostat=error) string
      read(83,*) R_FOREST%trees(j)%max_depth

      allocate(R_FOREST%trees(j)%node(R_FOREST%trees(j)%node_count))

      do i = 1,R_FOREST%trees(j)%node_count
        allocate(R_FOREST%trees(j)%node(i)%values(R_FOREST%trees(j)%n_outputs))
        read(83,*,iostat=error) string
        read(83,*) R_FOREST%trees(j)%node(i)%children_left
        read(83,*) R_FOREST%trees(j)%node(i)%children_right
        read(83,*) R_FOREST%trees(j)%node(i)%feature
        read(83,*) R_FOREST%trees(j)%node(i)%threshold
        read(83,*) R_FOREST%trees(j)%node(i)%values
      end do
    end do

    R_FOREST%n_inputs  = R_FOREST%trees(1)%n_inputs
    R_FOREST%n_outputs = R_FOREST%trees(1)%n_outputs

    close(83)

  end subroutine read_Random_Forest

  function predict_Neural_Network(input, n_input, n_output)
    implicit none
    integer :: n_input
    integer :: n_output
    real(PS) :: input(n_input)
    real(PS) :: predict_Neural_Network(n_output) 
    integer :: i

    N_Work%activations(1)%vec = input

    do i = 1, N_Work%layers-2
      N_Work%activations(i+1)%vec = matmul(N_Work%coefs(i)%mat,N_Work%activations(i)%vec) + N_Work%intercepts(i)%vec
      N_Work%activations(i+1)%vec =N_Work%activation%activate(N_Work%layer_size(i+1),N_Work%activations(i+1)%vec)
    end do
    N_Work%activations(N_Work%layers)%vec = &
        matmul(N_Work%coefs(N_Work%layers-1)%mat,N_Work%activations(N_Work%layers-1)%vec) + N_Work%intercepts(N_Work%layers-1)%vec
    N_Work%activations(N_Work%layers)%vec = &
    N_Work%out_activation%activate(N_Work%output_len,N_Work%activations(N_Work%layers)%vec)

    predict_Neural_Network = N_Work%activations(N_Work%layers)%vec

  end function predict_Neural_Network

  function predict_Decision_Tree(input,n_input,n_output)
    implicit none
    integer :: n_input
    integer :: n_output
    real(PS) :: input(n_input)
    real(PS) :: predict_Decision_Tree(n_output)

    integer :: i,n

    n = 1
    do i = 1, D_TREE%max_depth
      if (D_TREE%node(n)%feature .eq. -1) Exit
      if (input(D_TREE%node(n)%feature) .le. D_TREE%node(n)%threshold) then
        n = D_TREE%node(n)%children_left
      else
        n = D_TREE%node(n)%children_right
      end if
    end do

    predict_Decision_Tree = D_TREE%node(n)%values

  end function predict_Decision_Tree

  function predict_Random_Forest(input,n_input,n_output)
    implicit none
    integer :: n_input
    integer :: n_output
    real(PS) :: input(n_input)
    real(PS) :: predict_Random_Forest(n_output)

    integer :: i, j, n

    predict_Random_Forest = 0.0_PS
    do j = 1, R_FOREST%tree_count
      n=1
      do i = 1, R_FOREST%trees(j)%max_depth
        if (R_FOREST%trees(j)%node(n)%feature .eq. -1) Exit
        if (input(R_FOREST%trees(j)%node(n)%feature) .le. R_FOREST%trees(j)%node(n)%threshold) then
          n = R_FOREST%trees(j)%node(n)%children_left
        else
          n = R_FOREST%trees(j)%node(n)%children_right
        end if
      end do
      predict_Random_Forest = predict_Random_Forest + R_FOREST%trees(j)%node(n)%values
    end do

    predict_Random_Forest = predict_Random_Forest / R_FOREST%tree_count

  end function predict_Random_Forest

  function activation_logistic(n,X)
    implicit none
    integer, intent(in) :: n
    real(PS), intent(in), dimension(n) :: X
    real(PS), dimension(n) :: activation_logistic
    activation_logistic = 1.0 / (1.0+exp(-X))
  end function activation_logistic

  function activation_tanh(n,X)
    implicit none
    integer, intent(in) :: n
    real(PS), intent(in), dimension(n) :: X
    real(PS), dimension(n) :: activation_tanh
    activation_tanh = tanh(X)
  end function activation_tanh

  function activation_ReLU(n,X)
    implicit none
    integer, intent(in) :: n
    real(PS), intent(in), dimension(n) :: X
    real(PS), dimension(n) :: activation_ReLU
      activation_ReLU = max(X,0.d0)
    end function activation_ReLU

  function activation_identity(n,X)
    implicit none
    integer, intent(in) :: n
    real(PS), intent(in), dimension(n) :: X
    real(PS), dimension(n) :: activation_identity
    activation_identity = X
  end function activation_identity

  function activation_softmax(n,X)
    implicit none
    integer, intent(in) :: n
    real(PS), intent(in), dimension(n) :: X
    real(PS), dimension(n) :: tmp
    real(PS), dimension(n) :: activation_softmax
    tmp = exp(X - maxval(X))/sum(tmp)
    activation_softmax = 1.0 / (1.0+exp(-X))
  end function activation_softmax

  subroutine generate_training_data
    implicit none
    character(20) :: file_name
    integer  :: a = 1
    real(PS) :: b = 2.0_PS
    file_name = 'training.dat'
    open(75,file=file_name, status = 'unknown')
    write(75,*)

  end subroutine generate_training_data

  subroutine training
    implicit none
  end subroutine training

  Subroutine Write_Line_Integer(file_num, vector, length)
    implicit none

    Integer :: file_num
    Integer :: length
    Integer :: vector(length)

    write(file_num,*) vector

  end Subroutine Write_Line_Integer

  Subroutine Write_Line_Real(file_num, vector, length)
    implicit none

    Integer :: file_num
    Integer :: length
    real(PS) :: vector(length)

    write(file_num,'(*(F14.6))') vector

  end Subroutine Write_Line_Real

end Module mod_fsklearn
