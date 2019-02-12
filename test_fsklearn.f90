program main
  use FSklearn
  implicit none 

  type(Neural_Network) :: nn
  type(Decision_Tree) :: dt

  integer :: i

  ! call nn%para_read
  ! call nn%predict
  ! call dt%para_read
  ! call dt%predict

  nn.input_len = 3
  nn.output_len = 2
  nn.layers = 5
  nn.activation_type = 1
  nn.out_activation_type = 5


  call nn%para_read
  call dt%para_read

  write(*,*) dt%max_depth

  nn%input=[-0.99,0.141067, -0.54]
  dt%input=[-0.99,0.141067, -0.54]
  nn%activations(1)%vec = nn%input

  call nn%predict
  write(*,*) dt.predict(dt%input,3,2)

end program main
