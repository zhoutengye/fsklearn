Module mod_fsklearn

# if defined(Double_Presion) 
  integer , private, parameter :: SP = 8
# else
integer , private, parameter :: SP = 4  
# endif
  !---------------ragged---------------
  ! ragged vectors for 2-D array consists of vectors with different length
  type ragged_vector
    real(SP),allocatable::vec(:)
  end type ragged_vector

  ! ragged array for 3-D array consists of 2-D matrices with difference size
  type ragged_matrix
    real(SP),allocatable::mat(:,:)
  end type ragged_matrix

  !---------------Neural Networks variables---------------
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||
  !
  ! NN_activation used to choose activation function 
  ! at the beginning.
  ! Neural_Network for choosing 
  type :: NN_activation
    procedure(sub_interface), pointer, nopass :: activate =>NULL()
  end type NN_activation

  ! interface for choose activation type
  interface
    function sub_interface(n, x)
      import :: SP
      integer, intent(in) :: n
      real(SP), intent(in), dimension(n) :: x
      real(SP), dimension(n) :: sub_interface
    end function sub_interface
  end interface

  ! neural network parameters
  type :: Neural_Network
    integer  :: input_len
    integer  :: output_len
    integer  :: layers
    integer  , allocatable :: layer_size(:)
    type(ragged_vector) , allocatable :: activations(:)
    type(ragged_vector) , allocatable :: intercepts(:)
    type(ragged_matrix) , allocatable :: coefs(:)
    character(10) :: activation_type
    character(10) :: out_activation_type
    type(NN_activation) :: activation
    type(NN_activation) :: out_activation
  contains
    ! procedure training  => training_Neural_Network
    procedure , nopass :: para_read  => read_Neural_Network
    procedure , nopass :: predict   => predict_Neural_Network
  end type Neural_Network

  !---------------Decision tree  variables----------------
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||
  Type Nodes
    integer :: children_left
    Integer :: children_right
    Integer :: feature
    Real(SP) :: threshold
    Real(SP), allocatable :: values(:)
  end type Nodes

  Type:: Decision_Tree
    integer :: node_count
    integer :: n_inputs
    integer :: n_outputs
    integer :: max_depth
    type(Nodes), allocatable :: node(:)
  contains
    procedure , nopass :: para_read => read_Decision_Tree
    procedure , nopass :: predict => predict_Decision_Tree
  end type Decision_Tree
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||=>NULL()
  !-----------End Decision tree  variables----------------=>NULL()
  
  !---------------Random forest variables-----------------
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||
  Type:: Random_Forest
    integer :: tree_count
    integer :: n_inputs
    integer :: n_outputs
    type(Decision_Tree), allocatable :: trees(:)
  contains
    procedure , nopass :: para_read => read_Random_Forest
    procedure , nopass :: predict => predict_Random_Forest
  end type Random_Forest
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||
  !-----------End Random forest variables-----------------
  
  type(Neural_Network) :: nn
  type(Decision_Tree) :: dt
  type(Random_Forest) :: rf

  type :: fsklearn_define
    character(20) :: training_type
    integer :: n_inputs
    integer :: n_outputs
    real(SP), allocatable :: inputs(:)
    real(SP), allocatable :: outputs(:)
    procedure(choose_read), pointer, nopass :: para_read =>NULL()
    procedure(choose_predict), pointer, nopass :: predict =>NULL()
  end type fsklearn_define

  interface

  function choose_predict(input,n_input,n_output)
      import sp
      implicit none
      integer :: n_input
      integer :: n_output
      real(SP) :: input(n_input)
      real(SP) :: choose_predict(n_output)
    end function choose_predict
  end interface

  interface
    subroutine choose_read
    end subroutine choose_read
  end interface

  type(fsklearn_define) :: fsklearn

contains

  subroutine fsklearn_initialization

    implicit none

    character(20) :: training_type
    integer :: n_inputs
    integer :: n_outputs

    open(79,file ='test.namelist')

    namelist /sizes/ n_inputs, n_outputs
    namelist /train_type/ training_type

    Read(79,nml=sizes) 
    Read(79,nml=train_type) 

    fsklearn%training_type = training_type
    fsklearn%n_inputs  = n_inputs
    fsklearn%n_outputs = n_outputs

    if ( trim(fsklearn%training_type) .eq. 'Neural_Network' ) then
      fsklearn%para_read => read_Neural_Network
      fsklearn%predict => predict_Neural_Network
    elseif ( trim(fsklearn%training_type) .eq. 'Decision_Tree' ) then
      fsklearn%para_read => read_Decision_Tree
      fsklearn%predict => predict_Decision_Tree
    elseif ( trim(fsklearn%training_type) .eq. 'Random_Forest' ) then
      fsklearn%para_read => read_Random_Forest
      fsklearn%predict => predict_Random_Forest
    else
      write(*,*) 'wrong training type!'
    end if

    call fsklearn%para_read

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
    read(81,*) NN%layers
    allocate(NN%layer_size(NN%layers))

    ! read 
    read(81,*,iostat=error) string
    read(81,*) NN%layer_size
    NN%input_len = NN%layer_size(1)
    NN%output_len = NN%layer_size(NN%layers)


    allocate(NN%activations(NN%layers))
    do i = 1,NN%layers
      allocate(NN%activations(i)%vec(NN%layer_size(i)))
    end do

    read(81,*,iostat=error) string
    allocate(NN%intercepts(NN%layers-1))
    do i = 1,NN%layers-1
      allocate(NN%intercepts(i)%vec(NN%layer_size(i+1)))
      read(81,*) NN%intercepts(i)%vec
    end do


    read(81,*,iostat=error) string
    allocate(NN%coefs(NN%layers-1))
    do i = 1,NN%layers-1
      allocate(NN%coefs(i)%mat(NN%layer_size(i+1),NN%layer_size(i)))
      read(81,*,iostat=error) string
      do j = 1,NN%layer_size(i+1)
        read(81,*) NN%coefs(i)%mat(j,:)
      end do
    end do

    read(81,*,iostat=error) string
    read(81,*) NN%activation_type
    read(81,*,iostat=error) string
    read(81,*) NN%out_activation_type

    close(81)

    if (trim(NN%activation_type).eq.'logistic') then
      NN%activation%activate => activation_logistic
    else if (trim(NN%activation_type).eq.'tanh') then
      NN%activation%activate => activation_tanh
    else if (trim(NN%activation_type).eq.'softmax') then
      NN%activation%activate => activation_softmax
    else if (trim(NN%activation_type).eq.'relu') then
      NN%activation%activate => activation_ReLU
    else if (trim(NN%activation_type).eq.'identity') then
      NN%activation%activate => activation_identity
    else
      write(*,*) 'invalid activation type'
    end if

    if (trim(NN%out_activation_type).eq.'logistic') then
      NN%out_activation%activate => activation_logistic
    else if (trim(NN%out_activation_type).eq.'tanh') then
      NN%out_activation%activate => activation_tanh
    else if (trim(NN%out_activation_type).eq.'softmax') then
      NN%out_activation%activate => activation_softmax
    else if (trim(NN%out_activation_type).eq.'relu') then
      NN%out_activation%activate => activation_ReLU
    else if (trim(NN%out_activation_type).eq.'identity') then
      NN%out_activation%activate => activation_identity
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
    read(82,*) DT%node_count
    read(82,*,iostat=error) string
    read(82,*) DT%n_inputs
    read(82,*,iostat=error) string
    read(82,*) DT%n_outputs
    read(82,*,iostat=error) string
    read(82,*) DT%max_depth

    allocate(DT%node(DT%node_count))

    do i = 1,DT%node_count
      allocate(DT%node(i)%values(DT%n_outputs))
      read(82,*,iostat=error) string
      read(82,*) DT%node(i)%children_left
      read(82,*) DT%node(i)%children_right
      read(82,*) DT%node(i)%feature
      read(82,*) DT%node(i)%threshold
      read(82,*) DT%node(i)%values
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
    read(83,*) RF%tree_count

    allocate(RF%trees(RF%tree_count))

    do j = 1, RF%tree_count
      read(83,*,iostat=error) string
      read(83,*) RF%trees(j)%node_count
      read(83,*,iostat=error) string
      read(83,*) RF%trees(j)%n_inputs
      read(83,*,iostat=error) string
      read(83,*) RF%trees(j)%n_outputs
      read(83,*,iostat=error) string
      read(83,*) RF%trees(j)%max_depth

      allocate(RF%trees(j)%node(RF%trees(j)%node_count))

      do i = 1,RF%trees(j)%node_count
        allocate(RF%trees(j)%node(i)%values(RF%trees(j)%n_outputs))
        read(83,*,iostat=error) string
        read(83,*) RF%trees(j)%node(i)%children_left
        read(83,*) RF%trees(j)%node(i)%children_right
        read(83,*) RF%trees(j)%node(i)%feature
        read(83,*) RF%trees(j)%node(i)%threshold
        read(83,*) RF%trees(j)%node(i)%values
      end do
    end do

    RF%n_inputs  = RF%trees(1)%n_inputs
    RF%n_outputs = RF%trees(1)%n_outputs

    close(83)

  end subroutine read_Random_Forest

  function predict_Neural_Network(input,n_input,n_output)
    implicit none    
    integer :: n_input
    integer :: n_output
    real(SP) :: input(n_input)
    real(SP) :: predict_Neural_Network(n_output) 
    integer :: i

    NN%activations(1)%vec = input

    do i = 1, NN%layers-2
      NN%activations(i+1)%vec = matmul(NN%coefs(i)%mat,NN%activations(i)%vec) + NN%intercepts(i)%vec
      NN%activations(i+1)%vec =NN%activation%activate(NN%layer_size(i+1),NN%activations(i+1)%vec)
    end do
    NN%activations(NN%layers)%vec = &
        matmul(NN%coefs(NN%layers-1)%mat,NN%activations(NN%layers-1)%vec) + NN%intercepts(NN%layers-1)%vec
    NN%activations(NN%layers)%vec = &
    NN%out_activation%activate(nn%output_len,NN%activations(NN%layers)%vec)

    predict_Neural_Network = NN%activations(NN%layers)%vec

  end function predict_Neural_Network

  function predict_Decision_Tree(input,n_input,n_output)
    implicit none
    integer :: n_input
    integer :: n_output
    real(SP) :: input(n_input)
    real(SP) :: predict_Decision_Tree(n_output)

    integer :: i,n

    n = 1
    do i = 1, DT%max_depth
      if (DT%node(n)%feature .eq. -1) Exit
      if (input(DT%node(n)%feature) .le. DT%node(n)%threshold) then
        n = DT%node(n)%children_left
      else
        n = DT%node(n)%children_right
      end if
    end do

    predict_Decision_Tree = DT%node(n)%values

  end function predict_Decision_Tree
  
  function predict_Random_Forest(input,n_input,n_output)
    implicit none
    integer :: n_input
    integer :: n_output
    real(SP) :: input(n_input)
    real(SP) :: predict_Random_Forest(n_output)

    integer :: i, j, n

    predict_Random_Forest = 0.0_SP
    do j = 1, RF%tree_count
      n=1
      do i = 1, RF%trees(j)%max_depth
        if (RF%trees(j)%node(n)%feature .eq. -1) Exit
        if (input(RF%trees(j)%node(n)%feature) .le. RF%trees(j)%node(n)%threshold) then
          n = RF%trees(j)%node(n)%children_left
        else
          n = RF%trees(j)%node(n)%children_right
        end if
      end do
      predict_Random_Forest = predict_Random_Forest + RF%trees(j)%node(n)%values
    end do

    predict_Random_Forest = predict_Random_Forest / RF%tree_count

  end function predict_Random_Forest

  function activation_logistic(n,X)
    integer, intent(in) :: n
    real(SP), intent(in), dimension(n) :: X
    real(SP), dimension(n) :: activation_logistic
    activation_logistic = 1.0 / (1.0+exp(-X))
  end function activation_logistic
  
  function activation_tanh(n,X)
    integer, intent(in) :: n
    real(SP), intent(in), dimension(n) :: X
    real(SP), dimension(n) :: activation_tanh
    activation_tanh = tanh(X)
  end function activation_tanh

  function activation_ReLU(n,X)
    integer, intent(in) :: n
    real(SP), intent(in), dimension(n) :: X
    real(SP), dimension(n) :: activation_ReLU
    ! do i = 1,n
      activation_ReLU = max(X,0.d0)
      ! end dendo
    end function activation_ReLU

  function activation_identity(n,X)
    integer, intent(in) :: n
    real(SP), intent(in), dimension(n) :: X
    real(SP), dimension(n) :: activation_identity
    activation_identity = X
  end function activation_identity

  function activation_softmax(n,X)
    integer, intent(in) :: n
    real(SP), intent(in), dimension(n) :: X
    real(SP), dimension(n) :: tmp
    real(SP), dimension(n) :: activation_softmax
    tmp = exp(X - maxval(X))/sum(tmp)
    activation_softmax = 1.0 / (1.0+exp(-X))
  end function activation_softmax

end Module mod_fsklearn

