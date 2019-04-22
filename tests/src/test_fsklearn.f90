# if defined (PARALLEL)
Program main_MPI
  Use mpi
  use mod_FSklearn
  implicit none
  character(100) :: test_data
  real(4) :: sample_data(1000,6)
  real(4) :: sample_data_mpi(250,6)
  integer :: i
  integer :: myid, ier

  Call MPI_INIT(ier)

  Call F_Sklearn%initialization

  Write(*,*) '============================='
# if defined(FSKLEARN_TRAINING)
  Write(*,*) 'Mode: Training'
# elif defined (FSKLEARN_PREDICTION)
  Write(*,*) 'Mode: Prediction'
# endif

  Write(*,*) 'MPI: YES'
  Write(*,*) ' '
  Write(*,*) 'Initialization complete!'

# if defined(FSKLEARN_TRAINING)
  Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
  if (myid .eq. 0) then !
    test_data = Trim(para_files_path)//'sample.dat'
    open(1,file = test_data)
    do i = 1,1000
      read(1,*) sample_data(i,:)
    end do
  end If

  Call MPI_BCAST(sample_data, 6000, MPI_REAL, 0, &
      MPI_COMM_WORLD, ier)

  do i = 1,250
    sample_data_mpi(i,:) = sample_data(i+250*myid,:)
  end do

  Call F_Sklearn%Gen_Training(sample_data_mpi, 3, 3, 250)
  write(*,*) 'Complete Training'
  write(*,*) ''

  Call F_Sklearn%PY_Training

# endif

# if defined(FSKLEARN_PREDICTION)
  F_Sklearn%inputs=[-0.99,0.141067, -0.54]
  F_Sklearn%outputs = F_Sklearn%predict(F_Sklearn%inputs,F_Sklearn%n_inputs,F_Sklearn%n_outputs)
  write(*,*) 'Complete prediction'
  write(*,*) "results are:", F_Sklearn%outputs
# endif


End Program main_MPI

# else
! sequential version
Program main_sequential
  use mod_FSklearn
  implicit none
  character(100) :: test_data
  real(4) :: sample_data(1000,6)
  integer :: i
  integer :: myid, ier

  Write(*,*) '============================='
# if defined(FSKLEARN_TRAINING)
  Write(*,*) 'Mode: Training'
# elif defined (FSKLEARN_PREDICTION)
  Write(*,*) 'Mode: Prediction'
# endif

  Write(*,*) 'MPI: YES'
  Write(*,*) ' '
  Write(*,*) 'Initialization complete!'

  Call F_Sklearn%initialization

# if defined(FSKLEARN_TRAINING)
  test_data = Trim(para_files_path)//'sample.dat'
  open(1,file = test_data)
  do i = 1,1000
    read(1,*) sample_data(i,:)
  end do

  Call F_Sklearn%Gen_Training(sample_data, 3, 3, 1000)
  write(*,*) 'Complete Generate Training Data'
  write(*,*) ' '

  Call F_Sklearn%PY_Training
# endif

# if defined(FSKLEARN_PREDICTION)
  F_Sklearn%inputs=[-0.99,0.141067, -0.54]
  F_Sklearn%outputs = F_Sklearn%predict(F_Sklearn%inputs,F_Sklearn%n_inputs,F_Sklearn%n_outputs)
  write(*,*) 'Complete prediction'
  write(*,*) "results are:", F_Sklearn%outputs
# endif

  Write(*,*) '============================='

End Program main_sequential
# endif
