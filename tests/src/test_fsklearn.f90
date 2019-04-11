program main
  use mod_FSklearn
  implicit none
  character(20) :: test_data
  real(4) :: input_line(3)
  real(4) :: output_line(3)
  real(4) :: inputs(3)
  integer :: i

  Call fsklearn_initialization

# if defined(FSKLEARN_TRAINING)
  test_data = 'sample.dat'
  open(1,file = test_data)
  do i = 1,1000
    read(1,*) input_line, output_line
  end do
# endif

# if defined(TRAINING)
  CALL system('python fsklearn_training.py') 
# endif

# if defined(FSKLEARN_PREDICTION)
  F_Sklearn%inputs=[-0.99,0.141067, -0.54]
  F_Sklearn%outputs = F_Sklearn%predict(F_Sklearn%inputs,F_Sklearn%n_inputs,F_Sklearn%n_outputs)
  write(*,*) F_Sklearn%outputs
# endif

end program main
