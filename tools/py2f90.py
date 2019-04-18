file_in  ='fsklearn_training.py'
file_out = 'pyf.f90'
py_out_test = 'pyf.py'
file_num = 'file_num'
complete_f = True
line_begin = '    write('+file_num+',\'(A)\') "'
line_end = '"'

py_in = open(file_in)
f_out = open(file_out,'w')

if complete_f == True:
    f_out.write('Program pyf\n')
    f_out.write('    Implicit None\n')
    f_out.write('    Integer :: file_num = 7\n\n')
    f_out.write('    Open(7,file=\''+py_out_test+'\')\n')


for line in py_in:
    line2 = line_begin + line.replace('"','""').replace('\n','"\n')
    f_out.write(line2)

if complete_f == True:
    f_out.write('End Program pyf\n')