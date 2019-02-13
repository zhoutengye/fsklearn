import keras
from keras.models import Sequential
from keras.layers import Dense
import numpy as np
import pandas as pd 



f = np.loadtxt('DATA/MOF_Training4.dat', unpack='true')
X = np.transpose(f[0:3, :])
y = np.transpose(f[4:6, :])
n_test = 1
L = np.zeros((n_test, 7))
X2 = np.transpose(f[0:3, 1:2])
Y3 = np.transpose(f[4:6, 1:2])
n_test = X2.shape[0]

model = Sequential()
model.add(Dense(units=64, activation='relu', input_dim=100))
model.add(Dense(units=10, activation='linear'))

model.compile(loss='categorical_crossentropy',
              optimizer='sgd',
              metrics=['accuracy'])

X = keras.backend.batch_set_value(X)
y = keras.backend.batch_set_value(y)
model.fit(X, y)
