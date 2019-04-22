

# fsklearn

A simple module for machine learning in Fortran using [scikit-learn](https://github.com/scikit-learn/scikit-learn).


## Overview

Currently the module can be used to do training and prediction in FORTRAN. 

The training part uses [scikit-learn](<https://github.com/scikit-learn/scikit-learn>) library by calling Python from FORTRAN. 

The prediction part, as it may be called frequent by a FORTRAN code, is written with FORTRAN. 

\## Supported machine learning method
Currently, the following methods are supported for
****regression**** (not classification) problem:

-   Neual Networks
-   Decision Tree
-   Random Forest

\## Compile the code
\### Supported FORTRAN compiler

-   Currently the module can be used to do training and prediction in FORTRAN.

-   The training part uses [scikit-learn](https://github.com/scikit-learn/scikit-learn) library by calling Python from FORTRAN.

-   The prediction part, as it may be called frequent by a FORTRAN code, is written with FORTRAN 2003.


## Supported FORTRAN compiler

The following compiler are tested. 

-   Intel - Tested with 2019.0.2.187
-   GNU - Tested with 8.1.0

Since derived type is used in the module, it is recommended to used GNU > 5.0 or Intel > 14.0 (2013 SP1).

## Required packages for **Python3**

-   numpy
-   json
-   [scikit-learn](https://github.com/scikit-learn/scikit-learn)


## build tool

-   Makefile
-   [FoBis.py](https://github.com/szaghi/FoBiS) (testing)


## Run the test

```bash
    git clone https://github.com/Yeldon/fsklearn.git
    cd fsklearn/
    cp -r tests/src ./
    cp -r tests/build ./
    make 
    ./build/fsklearn_test
```

## Example

Assume you have set up the file path and the correct input and output
interface for your data, a simple main program (sequential version) could be:


-   training

```fortran
    program main
    use mod_fsklearn
    implicit none
    integer :: input_len = 3
    integer :: input_len = 3
    integer :: num_datalen = 500
    integer :: sample_data(500,6)
    
    Call fsklearn_initialization ! Initialization
    ! Assume you have defined your interface in mod_fsklearn
    Call F_Sklearn%Fen_Training(sample_data,3, 3, 500)
    Call F_Sklearn%PY_Training ! Call python to train
    
    end program main
```


-   prediction

```
    program main
    use mod_fsklearn
    implicit none
    integer :: inputs = 3
    
    Call fsklearn_initialization ! Initialization
    inputs=[-0.99,0.141067, -0.54]
    ! If you have setup the mod_fsklearn and input.namelist, this should be working
    F_Sklearn%outputs = F_Sklearn%predict(inputs,F_Sklearn%n_inputs,F_Sklearn%n_outputs)
``` 

## Progress<code>[4/8]</code>

-   [X] Glue FORTRAN and Python
-   [X] Basic training and prediction interface
-   [X] MPI version
-   [X] First step tests
-   [ ] A more smart Python code generator
-   [ ] Parameters for training function
-   [ ] Prediction for data, vector and matrix
-   [ ] Second step tests


## License

This project is licensed under the BSD3 - see the [LICENSE.md](LICENSE.md) file for details

