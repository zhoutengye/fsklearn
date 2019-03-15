program main
  use mod_FSklearn
  implicit none 
  real(4) :: inputs(3)
  integer :: n_inputs = 3
  integer :: n_outputs = 2

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
  call nn%para_read
  call dt%para_read
  call rf%para_read
# endif

! example for data input, 
! The length of the input should be consistant with n_inputs
  fsklearn%inputs=[-0.99,0.141067, -0.54]

# if defined(METHOD_ONE)
! Use the predict function from fsklearn class for prediction
! One can replace the fsklearn%outputs by any variables 
!   as long as the length is consistant with n_outputs
  fsklearn%outputs = fsklearn%predict(fsklearn%inputs,fsklearn%n_inputs,fsklearn%n_outputs)
  write(*,*) fsklearn%outputs 
# elif defined(METHOD_TWO)  
! Use ONE of the following sentences according to the choice in para_read
  write(*,*) nn%predict(inputs,nn%input_len,nn%output_len)
  write(*,*) dt%predict(inputs,dt%n_inputs,dt%n_outputs)
  write(*,*) rf%predict(inputs,rf%n_inputs,rf%n_outputs)

  ! write(*,*) nn%predict(inputs,n_inputs,n_outputs)
  ! write(*,*) dt%predict(inputs,n_inputs,n_outputs)
  ! write(*,*) rf%predict(inputs,n_inputs,n_outputs)
# endif

end program main
