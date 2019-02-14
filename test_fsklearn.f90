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

  call nn%para_read
  call dt%para_read

  nn%input=[-0.99,0.141067, -0.54]

  write(*,*) nn%predict(nn%input,nn%input_len,nn%output_len)
  write(*,*) dt%predict(nn%input,nn%input_len,nn%output_len)

end program main
