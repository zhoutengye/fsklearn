# fsklearn
A FORTRAN interface to scikit-learn library. 
Training with Python and predict in Fortran.

## Motivations
Python has been widely used in machine learning areas. 
However, there is few Fortran package for machine learning machine learning. 
While many scientific computational code are peogrammed with FORTRAN. wrapping the existing machine learning library to the FORTRAN will bring the FORTRAN model more power without doing too much extra work.
One can call Python from system call or from embedding Python in C in Fortran. 
However, the former way bring much more extra CPU cost and the later may be a nightmire for working among Python, C and Fortran.
Or one may consider about compiling Fortran code as a Python code via f2py. 
Personally I think it would be fine for smaller Fortran projects, but for larger Fortran projects, 
again, it would be a nightmire.

In most cases, 
we only need to do the training before the start of the simulation, 
and the coefficients can be used to predict some variables in the computational model. 
An alternative painless way of wrapping between Python and Fortran is to come up with simple Python code for training and save the coefficients to a data file. 
Then a Fortran module will read the coefficients from the data file at the beginning and predict the results.
The prediction part is much simplier than the training part for programming.

