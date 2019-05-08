Character(50) :: n_estimators= 'NULL'
Character(50) :: criterion= 'NULL'
Character(50) :: max_depth= 'NULL'
Character(50) :: min_samples_split= 'NULL'
Character(50) :: min_samples_leaf= 'NULL'
Character(50) :: min_weight_fraction_leaf= 'NULL'
Character(50) :: max_features= 'NULL'
Character(50) :: max_leaf_nodes= 'NULL'
Character(50) :: min_impurity_decrease= 'NULL'
Character(50) :: min_impurity_split= 'NULL'
Character(50) :: bootstrap= 'NULL'
Character(50) :: oob_score= 'NULL'
Character(50) :: n_jobs= 'NULL'
Character(50) :: random_state= 'NULL'
Character(50) :: verbose= 'NULL'
Character(50) :: warm_start= 'NULL'

Namelist /RF_Parameter/ n_estimators, &
criterion, &
max_depth, &
min_samples_split, &
min_samples_leaf, &
min_weight_fraction_leaf, &
max_features, &
max_leaf_nodes, &
min_impurity_decrease, &
min_impurity_split, &
bootstrap, &
oob_score, &
n_jobs, &
random_state, &
verbose, &
warm_start

If (n_estimators .ne. 'NULL') Then
self%value(1) = n_estimators
End If

If (criterion .ne. 'NULL') Then
self%value(2) = criterion
End If

If (max_depth .ne. 'NULL') Then
self%value(3) = max_depth
End If

If (min_samples_split .ne. 'NULL') Then
self%value(4) = min_samples_split
End If

If (min_samples_leaf .ne. 'NULL') Then
self%value(5) = min_samples_leaf
End If

If (min_weight_fraction_leaf .ne. 'NULL') Then
self%value(6) = min_weight_fraction_leaf
End If

If (max_features .ne. 'NULL') Then
self%value(7) = max_features
End If

If (max_leaf_nodes .ne. 'NULL') Then
self%value(8) = max_leaf_nodes
End If

If (min_impurity_decrease .ne. 'NULL') Then
self%value(9) = min_impurity_decrease
End If

If (min_impurity_split .ne. 'NULL') Then
self%value(10) = min_impurity_split
End If

If (bootstrap .ne. 'NULL') Then
self%value(11) = bootstrap
End If

If (oob_score .ne. 'NULL') Then
self%value(12) = oob_score
End If

If (n_jobs .ne. 'NULL') Then
self%value(13) = n_jobs
End If

If (random_state .ne. 'NULL') Then
self%value(14) = random_state
End If

If (verbose .ne. 'NULL') Then
self%value(15) = verbose
End If

If (warm_start .ne. 'NULL') Then
self%value(16) = warm_start
End If

self%key(1) = "n_estimators"
self%value(1) = "'warn' "

self%key(2) = "criterion"
self%value(2) = "'mse' "

self%key(3) = "max_depth"
self%value(3) = "None"

self%key(4) = "min_samples_split"
self%value(4) = "2 "

self%key(5) = "min_samples_leaf"
self%value(5) = "1"

self%key(6) = "min_weight_fraction_leaf"
self%value(6) = "0.0 "

self%key(7) = "max_features"
self%value(7) = "'auto' "

self%key(8) = "max_leaf_nodes"
self%value(8) = "None"

self%key(9) = "min_impurity_decrease"
self%value(9) = "0.0 "

self%key(10) = "min_impurity_split"
self%value(10) = "None"

self%key(11) = "bootstrap"
self%value(11) = "True "

self%key(12) = "oob_score"
self%value(12) = "False "

self%key(13) = "n_jobs"
self%value(13) = "None"

self%key(14) = "random_state"
self%value(14) = "None "

self%key(15) = "verbose"
self%value(15) = "0 "

self%key(16) = "warm_start"
self%value(16) = "False"

