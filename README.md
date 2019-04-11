

# fsklearn

A simple module for machine learning in Fortran using [scikit-learn](https://github.com/scikit-learn/scikit-learn). 

## Overview

Currently the module can be used to do training and prediction in FORTRAN. 

The training part uses [scikit-learn](https://github.com/scikit-learn/scikit-learn) library by calling Python from FORTRAN. 

The prediction part, as it may be called frequent by a FORTRAN code, is written with FORTRAN. 


## Supported machine learning method
Currently, the following methods are supported for
**regression** (not classification) problem:

* Neual Networks
* Decision Tree
* Random Forest

## Compile the code
### Supported FORTRAN compiler
* Intel - Tested with 2019.0.2.187
* GNU - Tested with 8.1.0

### Required packages for **Python3**
* [scikit-learn](https://github.com/scikit-learn/scikit-learn)
* json

### Compile the code
Some pre-processor directives are used in the code. To build the code, 
* [FoBis.py](https://github.com/szaghi/FoBiS) - The web framework used
* Makefile

## Example

```fortran
program main
implicit none
waiting to be added
end program main
```
## License

This project is licensed under the BSD3 - see the [LICENSE.md](LICENSE.md) file for details
