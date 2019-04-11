def sk2f(write_paras):
    if (write_paras.type == 'neural_network'):
        nn_sk2f(write_paras)
    if (write_paras.type == 'random_forest'):
        rf_sk2f(write_paras)
    elif (write_paras.type == 'decision_tree'):
        dt_sk2f(write_paras)

def nn_sk2f(ml):

    file_name = 'nn_output.dat'
    nn_output  = open(file_name, 'w')
    nn_output.write('! n_layers\n%d\n'%(ml.n_layers_))
    nn_output.write('! layer_sizes\n')
    nn_output.write('%d \n'%np.shape(ml.coefs_[0])[0])
    for i in range(len(ml.hidden_layer_sizes)):
        nn_output.write('%d '%ml.hidden_layer_sizes[i])
    nn_output.write('%d\n'%ml.n_outputs_)
    nn_output.write('! intercepts\n')

    for i in range(len(ml.intercepts_)):
        # somehow numpy seems to have some problem with 3x version in def function
        # np.savetxt(nn_output,np.intercepts_[i])
        for j in range(len(ml.intercepts_[i])):
            nn_output.write('%f '%ml.intercepts_[i][j])
            print(ml.intercepts_[i][j])
        nn_output.write('\n')
    nn_output.write('! coefs\n')
    for i in range(len(ml.coefs_)):
        nn_output.write('!! layer%d\n'%i)
        # same numpy savetxt issue
        # np.savetxt(nn_output, np.transpose(ml.coefs_[i]))
        coef = np.transpose(ml.coefs_[i])
        for j in range(len(coef)):
            for k in range(len(coef[j])):
                nn_output.write('%f '%coef[j][k])
            nn_output.write('\n')
    nn_output.write('! activations\n')
    nn_output.write(ml.activation)
    nn_output.write('\n')
    nn_output.write('! out_activations\n')
    nn_output.write(ml.out_activation_)
    nn_output.write('\n')

def dt_sk2f(dt):

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

def rf_sk2f(rf):

    rf_output = open('rf_output.dat','w')

    rf_output.write('! tree_count\n')
    rf_output.write('%d\n'%len(rf.estimators_))

    trees = rf.estimators_

    for j in range(len(rf.estimators_)):
        tree1 = trees[j].tree_
        rf_output.write('! node_count\n')
        rf_output.write('%d\n'%tree1.node_count)
        rf_output.write('! n_features\n')
        rf_output.write('%d\n'%tree1.n_features)
        rf_output.write('! n_outputs\n')
        rf_output.write('%d\n'%tree1.n_outputs)
        rf_output.write('! max_depth\n')
        rf_output.write('%d\n'%tree1.max_depth)

        for i in range(tree1.node_count):
            rf_output.write('! node %d\n'%(i+1))
            rf_output.write('%d\n'%(tree1.children_left[i]+1))
            rf_output.write('%d\n'%(tree1.children_right[i]+1))
            rf_output.write('%d\n'%(tree1.feature[i]+1))
            rf_output.write('%f\n'%tree1.threshold[i])
            for j in range(tree1.n_outputs):
                rf_output.write('%f '%tree1.value[i,j])
            rf_output.write('\n')

import numpy as np
