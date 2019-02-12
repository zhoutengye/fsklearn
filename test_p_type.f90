Program scratch
  type :: test(n)
    integer, len :: n
    real(8) :: b(n)
  end type test

  type(test(n=:)), allocatable  :: c(:)

  ! allocate(c(1)) 

  namelist /tes/ c

  open(1,file='test.namelist',status='unknown')

  allocate(test(n=4) :: c(1))
allocate(test(n=3) :: c(2))
  c(1).b = 3
  c(2).b = 4
   write(*,*) c(1).b
   write(*,*) c(2).b

  
  ! read(1,nml=tes,iostat=ios)
  ! read(1,nml=tes2,iostat=ios)
  ! write(*,*) c%a,c%b

End Program scratch
