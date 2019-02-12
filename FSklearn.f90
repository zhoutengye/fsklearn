Module FSklearn

  integer , parameter :: SP = 8

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
    real(SP) , allocatable :: input(:)
    real(SP) , allocatable :: output(:)
    type(ragged_vector) , allocatable :: activations(:)
    type(ragged_vector) , allocatable :: intercepts(:)
    type(ragged_matrix) , allocatable :: coefs(:)
    character(10) :: activation_type
    character(10) :: out_activation_type
    type(NN_activation) :: activation
    type(NN_activation) :: out_activation
  contains
    ! procedure training  => training_Neural_Network
    procedure para_read => read_Neural_Network
    procedure predict   => predict_Neural_Network
  end type Neural_Network
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||
  !-----------End Neural Networks variables---------------

  
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
    real(SP), allocatable :: input(:)
    real(SP), allocatable :: output(:)
    type(Nodes), allocatable :: node(:)
  contains
    procedure para_read => read_Decision_Tree
    procedure predict => predict_Decision_Tree
  end type Decision_Tree
  !|||||||||||||||||||||||||||||||||||||||||||||||||||||||
  !-----------End Decision tree  variables----------------

contains

  subroutine read_Neural_Network(NN)
    implicit none

    integer :: i,j
    integer :: error
    character(len=20) :: string
    class(Neural_Network) :: NN

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
    NN%output_len = NN%layer_size(NN.layers)
    allocate(NN%input(NN%input_len))
    allocate(NN%output(NN%output_len))


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
    nn%activations(1)%vec=NN%input

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

  subroutine read_Decision_Tree(DT)
    implicit none 
    class(Decision_Tree) :: DT
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

    allocate(DT%input(DT%n_inputs))
    allocate(DT%output(DT%n_outputs))
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

  end subroutine read_Decision_Tree

  function predict_Neural_Network(NN,input,n_input,n_output)
    implicit none
    class(Neural_Network) :: NN
    integer :: n_input
    integer :: n_output
    real(SP) :: input(n_input)
    real(SP) :: predict_Neural_Network(n_output) 
    integer :: i

    NN%input = input
    NN%activations(1)%vec = NN%input

    do i = 1, NN%layers-2
      NN%activations(i+1)%vec = matmul(NN%coefs(i)%mat,NN%activations(i)%vec) + NN%intercepts(i)%vec
      NN%activations(i+1).vec =NN%activation%activate(NN%layer_size(i+1),NN%activations(i+1)%vec)
      write(*,*) NN%activations(i+1)%vec
    end do
    NN%activations(NN.layers)%vec = matmul(NN%coefs(NN.layers-1)%mat,NN%activations(NN.layers-1)%vec) + NN%intercepts(NN.layers-1)%vec
    NN%activations(NN.layers)%vec =  NN%out_activation%activate(nn%output_len,NN%activations(NN%layers)%vec)
    NN%output = NN%activations(NN.layers)%vec

    predict_Neural_Network = NN%output

  end function predict_Neural_Network

  function predict_Decision_Tree(DT,input,n_input,n_output)
    implicit none
    class(Decision_Tree) :: DT
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

end Module FSklearn

