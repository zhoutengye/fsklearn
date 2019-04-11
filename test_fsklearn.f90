program main
  use mod_FSklearn
  implicit none 
  character(20) :: test_data
  real(4) :: input_line(3) 
  real(4) :: output_line(3) 
  real(4) :: inputs(3)
  integer :: n_inputs = 3
  integer :: n_outputs = 3
  integer :: i

# if defined(GEN_TRAINING)
  test_data = 'sample.dat'
  open(1,file = test_data)
  open(2,file = 'training_inputs.dat', status = 'unknown')
  open(3,file = 'training_outputs.dat', status = 'unknown')
  do i = 1,1000
    read(1,*) input_line, output_line
    write(2,*) input_line
    write(3,*) output_line
  end do
# endif

# if defined(TRAINING)
  CALL system('python fsklearn_training.py') 
# endif

# if defined(METHOD_ONE)
! method one, read from file
! in this example will load 'test.namelist' 
! juat call fsklearn_initialization as initialization
  call fsklearn_initialization
# elif defined(METHOD_TWO)
! initialize the training parameters by assiging ONE of the training types
! nn: neural network
! dt: decision tree
! rf: random forest
  call N_Work%para_read
  call D_Tree%para_read
  call R_Forest%para_read
# endif

! example for data input, 
! The length of the input should be consistant with n_inputs
  F_Sklearn%inputs=[-0.99,0.141067, -0.54]

# if defined(METHOD_ONE)
! Use the predict function from fsklearn class for prediction
! One can replace the fsklearn%outputs by any variables 
!   as long as the length is consistant with n_outputs
  F_Sklearn%outputs = F_Sklearn%predict(F_Sklearn%inputs,F_Sklearn%n_inputs,F_Sklearn%n_outputs)
  write(*,*) F_Sklearn%outputs 
# elif defined(METHOD_TWO)  
! Use ONE of the following sentences according to the choice in para_read
  write(*,*) N_Work%predict(inputs,N_Work%input_len,N_Work%output_len)
  write(*,*) D_Tree%predict(inputs,D_Tree%n_inputs,D_Tree%n_outputs)
  write(*,*) R_Forest%predict(inputs,R_Forest%n_inputs,R_Forest%n_outputs)

  ! write(*,*) nn%predict(inputs,n_inputs,n_outputs)
  ! write(*,*) dt%predict(inputs,n_inputs,n_outputs)
  ! write(*,*) rf%predict(inputs,n_inputs,n_outputs)
# endif

end program main
