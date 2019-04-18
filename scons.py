env = Environment(tools=['default','gfortran'],F90='gfortran',LINK='gfortran',LINKFLAGS='-g',F90FLAGS='-g')
sources = ['src/test_fsklearn.f90']
env.Library('TestMod',sources)
env.Program('test.exe','main.f90',LIBS=['TestMod'],LIBPATH='.')