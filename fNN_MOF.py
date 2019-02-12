import math

import time
from matplotlib import cm
from matplotlib import gridspec
from matplotlib import pyplot as plt
import numpy as np
import sklearn
from sklearn import metrics
from sklearn import linear_model
from sklearn.ensemble import RandomForestRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.neural_network import MLPRegressor


# Load Training Date and test data
f = np.loadtxt('DATA/MOF_Training4.dat',unpack='true')

X = np.transpose(f[0:3,:])
y = np.transpose(f[4:6,:])
# to change the range of test data, sinply change the subscripts or import other data.
n_test = 1
L = np.zeros((n_test,7))
X2 = np.transpose(f[0:3,1:2])
Y3 = np.transpose(f[4:6,1:2])
n_test = X2.shape[0]

regr = MLPRegressor(hidden_layer_sizes=(2,3,5), activation='logistic', max_iter=30, alpha=0.0,\
                     verbose=False, tol=1e-4, random_state=0,\
                     learning_rate_init=.1)

dt = DecisionTreeRegressor()

regr.fit(X,y)
dt.fit(X,y)
y2 = regr.predict(X2) 
y2dt = dt.predict(X2)

print(X2)
print(y2)
print(y2dt)
print(Y3)

# # for i in range(regr.n_layers_-1):
# np.savetxt('DATA/out.dat',regr.coefs_[0])


ww = regr.coefs_

nn_output  = open('nn_output.dat', 'w')

nn_output.write('! n_layers\n%d\n'%(regr.n_layers_))
nn_output.write('! layer_sizes\n')
nn_output.write('%d '%np.shape(regr.coefs_[0])[0])
for i in range(len(regr.hidden_layer_sizes)):
    nn_output.write('%d '%regr.hidden_layer_sizes[i])
nn_output.write('%d\n'%regr.n_outputs_)
nn_output.write('! intercepts\n')
for i in range(len(regr.intercepts_)):
    # for j in range(len(regr.intercepts_[i])):
        # nn_output.write('%f '%np.transpose(regr.intercepts_[i][j]))
    np.savetxt(nn_output,regr.intercepts_[i],newline=' ')
    nn_output.write('\n')
nn_output.write('! coefs\n')
for i in range(len(regr.coefs_)):
    nn_output.write('!! layer%d\n'%i)
    np.savetxt(nn_output, np.transpose(regr.coefs_[i]))
nn_output.write('! activations\n')
nn_output.write(regr.activation)
nn_output.write('\n')
nn_output.write('! out_activations\n')
nn_output.write(regr.out_activation_)


dt_output = open('dt_output.dat','w')

dt_output.write('! node_count\n')
dt_output.write('%d\n'%dt.tree_.node_count)
dt_output.write('! n_features\n')
dt_output.write('%d\n'%dt.tree_.n_features)
dt_output.write('! n_outputs\n')
dt_output.write('%d\n'%dt.tree_.n_outputs)
dt_output.write('! max_depth\n')
dt_output.write('%d\n'%dt.tree_.max_depth)

for i in range(dt.tree_.node_count):
    dt_output.write('! node %d\n'%(i+1))
    dt_output.write('%d\n'%(dt.tree_.children_left[i]+1))
    dt_output.write('%d\n'%(dt.tree_.children_right[i]+1))
    dt_output.write('%d\n'%(dt.tree_.feature[i]+1))
    dt_output.write('%f\n'%dt.tree_.threshold[i])
    for j in range(dt.tree_.n_outputs):
        dt_output.write('%f '%dt.tree_.value[i,j])
    dt_output.write('\n')





# nn_output.write('\n')
# nn_output.write(regr.hidden_layer_sizes)


