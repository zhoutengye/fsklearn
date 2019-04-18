import json
import numpy as np
from sklearn.neural_network import MLPRegressor
from sklearn.ensemble import AdaBoostRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn import tree
from sklearn.multioutput import MultiOutputRegressor
from sk2f import sk2f
import os

for root, dirs, files in os.walk("build"):
    for file in files:
        if file.endswith(".json"):
             json_f = os.path.join(root, file)

print(json_f)

with open(json_f) as json_file:
    params = json.load(json_file)

T_data_path = params['data_path']
param_path = params['para_path']
training_type = params['training_type']

if 'num_mpi' in params.keys():
    T_input  = np.loadtxt(T_data_path+'training_input0.dat')
    T_output  = np.loadtxt(T_data_path+'training_output0.dat')
    for i in range(params['num_mpi']-1):
        file_name = T_data_path+'training_input' + str(i+1)+'.dat'
        file_name1 = T_data_path+'training_output' + str(i+1)+'.dat'
        T_input = np.append(T_input,np.loadtxt(file_name), axis=0)
        T_output = np.append(T_output,np.loadtxt(file_name1), axis=0)
else:
    T_input  = np.loadtxt(T_data_path+'training_input.dat')
    T_output  = np.loadtxt(T_data_path+'training_output.dat')

if (training_type == 'Neural_Network'):
    ml = MLPRegressor()
elif (training_type == 'Random_Forest'):
    ml = RandomForestRegressor()
elif (training_type == 'Decision_Tree'):
    ml = tree.DecisionTreeRegressor()

ml.fit(T_input, T_output)

ml.type = training_type
ml.output_param_path = param_path
sk2f(ml)

print('-------------------------------\n')
print('training_completed\n\n')
print('training_type:\n')
print(training_type+'\n')
print('-------------------------------\n')
