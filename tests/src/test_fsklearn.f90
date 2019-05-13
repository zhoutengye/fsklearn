# if defined (PARALLEL)
Program main_MPI
  Use mpi
  use mod_FSklearn
  implicit none
  Type(Fsklearn_Example) :: F_sklearn
  character(100) :: test_data
  real(4) :: sample_data(1000,6)
  real(4) :: sample_data_mpi(250,6)
  integer :: i
  integer :: myid, ier

  Call MPI_INIT(ier)
  Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)

  Call F_Sklearn%Initialization

  if (myid .eq. 0) then
    Write(*,*) '============================='
# if defined(FSKLEARN_TRAINING)
    Write(*,*) 'Mode: Training'
# elif defined (FSKLEARN_PREDICTION)
    Write(*,*) 'Mode: Prediction'
# endif

    Write(*,*) 'MPI: YES'
    Write(*,*) ' '
    Write(*,*) 'Initialization complete!'
  endif

# if defined(FSKLEARN_TRAINING)
  ! Read data
  Call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ier)
  if (myid .eq. 0) then !
    test_data = Trim(F_Sklearn%coef_files_path)//'sample.dat'
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

  Call F_Sklearn%Gen_Training(sample_data_mpi)

  If (myid .eq.0) Then
    write(*,*) 'Complete Generating training data'
    write(*,*) 'Calling training.py for training'

    Call F_Sklearn%PY_Training
    Write(*,*) '============================='
  End If

# endif

# if defined(FSKLEARN_PREDICTION)
  Print *, F_sklearn%Prediction([-0.990000, 0.141067, -0.560000])
  write(*,*) 'Complete prediction'
  write(*,*) "results are:", F_Sklearn%outputs
# endif

  Call MPI_Barrier(MPI_COMM_WORLD, ier)


End Program main_MPI

# else
! sequential version
Program main
  use mod_FSklearn
  implicit none
  Type(Fsklearn_Example) :: F_sklearn
  character(100) :: test_data
  real(4) :: sample_data(1000,6)
  integer :: i

  Call F_Sklearn%Initialization

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
  ! Read data
  test_data = Trim(F_Sklearn%coef_files_path)//'sample.dat'
  open(1,file = test_data)
  do i = 1,1000
    read(1,*) sample_data(i,:)
  end do

  Call F_Sklearn%Gen_Training(sample_data)

  write(*,*) 'Complete Generating training data'
  write(*,*) 'Calling training.py for training'

  Call F_Sklearn%PY_Training
  Write(*,*) '============================='

# endif

# if defined(FSKLEARN_PREDICTION)
  Print *, F_sklearn%Prediction([-0.990000, 0.141067, -0.560000])
  write(*,*) 'Complete prediction'
  write(*,*) "results are:", F_Sklearn%outputs
# endif


End Program main
# endif
