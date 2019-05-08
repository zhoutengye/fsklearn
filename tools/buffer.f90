
Module NN
  Type :: NN_dict
    Integer :: num_para = 21
    Character(50) :: key(21)
    Character(50) :: value(21)
  Contains
    Procedure :: NN_Load_Default_Para
    Procedure :: NN_Read_Get_Para
    Procedure :: NN_Update_Para
  End type NN_dict
Contains
  Subroutine NN_Load_Default_Para(self)
    Implicit None
    Class(NN_Dict) :: self

    self%key(1)= 'hidden_layer_sizes(:)'
    self%value(1)= "(100,)"

    self%key(2) = 'activation'
    self%value(2) = "'relu'"

    self%key(3) = 'solver'
    self%value(3) = "'adam'"

    self%key(4) = 'alpha'
    self%value(4) = "0.0001"

    self%key(5) = 'batch_size'
    self%value(5) = "'auto'"

    self%key(6) = 'learning_rate'
    self%value(6) = "'constant'"

    self%key(7) = 'learning_rate_init'
    self%value(7) = "0.001"

    self%key(8) = 'power_t'
    self%value(8) = "0.5"

    self%key(9) = 'max_iter'
    self%value(9) = "200"

    self%key(10) = 'shuffle'
    self%value(10) = "True"

    self%key(11) = 'random_state'
    self%value(11) = "None"

    self%key(12) = 'tol'
    self%value(12) = "0.0001"

    self%key(13) = 'verbose'
    self%value(13) = "False"

    self%key(14) = 'warm_start'
    self%value(14) = "False"

    self%key(15) = 'momentum'
    self%value(15) = 'True'

    self%key(16) = 'nesterovs_momentum'
    self%value(16) = 'True'

    self%key(17) = 'early_stopping'
    self%value(17) = "False"

    self%key(18) = "validation_fraction"
    self%value(18) = "0.1"

    self%key(19) = 'beta_1'
    self%value(19) = "0.9"

    self%key(20) = 'beta_2'
    self%value(20) = "0.999"

    self%key(21) = 'epsilon'
    self%value(21) = "1e-08"

  end Subroutine NN_Load_Default_Para

end Module NN

program main
  Use NN
  Implicit None
  Type (NN_Dict) :: ND
  Integer :: i

  Character(50) :: hidden_layer_sizes = "NULL"
  Character(50) :: activation = "NULL"
  Character(50) :: solver = "NULL"
  Character(50) :: alpha = "NULL"
  Character(50) :: batch_size = "NULL"
  Character(50) :: learning_rate = "NULL"
  Character(50) :: learning_rate_init = "NULL"
  Character(50) :: power_t = "NULL"
  Character(50) :: max_iter = "NULL"
  Character(50) :: shuffle = "NULL"
  Character(50) :: random_state = "NULL"
  Character(50) :: tol = "NULL"
  Character(50) :: verbose = "NULL"
  Character(50) :: warm_start
  Character(50) :: momentum = "NULL"
  Character(50) :: nesterovs_momentum = "NULL"
  Character(50) :: early_stopping = "NULL"
  Character(50) :: validation_fraction = "NULL"
  Character(50) :: beta_1 = "NULL"
  Character(50) :: beta_2 = "NULL"
  Character(50) :: epsilon = "NULL"

  Character(50) , Dimension(21) :: NNP

  Namelist /NN_Parameter/ hidden_layer_sizes, activation, &
      solver, alpha, batch_size, learning_rate, learning_rate_init, &
      power_t, max_iter, shuffle, random_state, tol, verbose, &
      warm_start, momentum, nesterovs_momentum, early_stopping, &
      validation_fraction, beta_1, beta_2, epsilon


  Call ND%NN_Load_Default_Para
  open(1,file = 'buffer.namelist',status='unknown')
  read(1,nml=NN_Parameter)
  
  do i = 1, ND%num_para
    ! print *, trim(ND%key(i))//'='//trim(ND%value(i))
    print *, epsilon
  end do


End Program main
