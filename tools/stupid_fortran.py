import numpy as np

f = open('buffe1r.namelist')
f2 = open('stupid_fortran.f90', 'w')


i = 1
vars = []
values = []
for line in f:
        var = line[0:line.rindex('=')]
        value = line[line.rindex('=')+1:-1]
        var.strip()
        value.strip()
        vars.append(var)
        values.append(value)

for i in range(len(vars)):
    f2.write('Character(50) :: ' + vars[i] + '= \'NULL\'\n')
f2.write('\n')

f2.write('Namelist /RF_Parameter/ ')
for i in range(len(vars)-1):
    f2.write(vars[i]+', &\n')
f2.write(vars[-1] + '\n\n')

for i in range(len(vars)):
    f2.write('If ('+ vars[i] + ' .ne. \'NULL\') Then\n')
    f2.write('self%value(' + str(i+1) + ') = ' + vars[i] +'\n')
    f2.write('End If\n\n')

for i in range(len(vars)):
    f2.write('self%key('+str(i+1)+') = ' + '\"'+vars[i] + '\"\n')
    f2.write('self%value('+str(i+1)+') = ' + '\"'+values[i] + '\"\n\n')
