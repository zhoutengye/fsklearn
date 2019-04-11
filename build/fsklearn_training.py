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
from sk2f import sk2f

# Load Training Date and test data
inputs = np.loadtxt('training_inputs.dat', unpack='true')
outputs = np.loadtxt('training_outputs.dat', unpack='true')

X = np.transpose(inputs)
y = np.transpose(outputs)
n_test = 1
L = np.zeros((n_test, 7))
X2 = X[1:2,:]
y3 = y[1:2,:]
n_test = X2.shape[0]

regr = MLPRegressor(hidden_layer_sizes=(2,3,5), activation='logistic', max_iter=30, alpha=0.0,\
                     verbose=False, tol=1e-4, random_state=0,\
                     learning_rate_init=.1)

dt = DecisionTreeRegressor()

rf = RandomForestRegressor()

regr.fit(X,y)
dt.fit(X,y)
rf.fit(X,y)
y2 = regr.predict(X2) 
y2dt = dt.predict(X2)
y2rf = rf.predict(X2)

print(X2)
print(y2)
print(y2dt)
print(y2rf)
print(y3)

regr.type = 'neural_network'
dt.type   = 'decision_tree'
rf.type   = 'random_forest'

sk2f(regr)
sk2f(dt)
sk2f(rf)

