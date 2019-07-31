# -*- coding: utf-8 -*-
"""
A simply python program for =f90= module file auto documentation. The
auto-generated documentation is Emacs =org-mode= file.  

By using some simple tool, such as =pandoc=, it can be converted to many other
formats. 
"""
import numpy as np
import sys

def get_start_end(file_name):
    """
    **Step 1**: Get the block information from the Module code.
    Including: starting of the block, end of the block,
    starting of the variables, end of the variables, location of **implicit none**.

    **Step 2**: Serach through the file to find the information of code blocks

    The ith serach starts with the end line number of the (i-1)th block

    The ith search ends when find end statement
    """

    # step 1
    f = open(file_name)
    code_blocks = []
    code_blocks.append(code_block())
    code_blocks[0].line_start = 1
    code_blocks[0].block_type = 'module'
    line_num = 0
    for line in f:
        total_line_num = line_num
        line_num = line_num + 1
    total_line_num = line_num
    f.close()

    line_num = 0
    var_start_flag = 1
    var_end_flag = 1
    type_contain_flag = 0
    f = open(file_name)
    for line in f:
        line_num = line_num + 1
        line = line.lstrip()
        line_lower = line.lower()
        # find the start of the variables
        if var_start_flag == 1:
            if line_lower[0:4] == '!!>>':
                var_start_flag = 0
                code_blocks[0].var_beg = line_num+1

        # find the end of the variables
        if var_end_flag == 1:
            if line_lower[0:4] == '!!<<':
                var_end_flag = 0
                code_blocks[0].var_end = line_num-1

        if type_contain_flag == 0:
            if line_lower[0:5] == 'type ' or line_lower[0:5] == 'type,':
                type_contain_flag = 1

        if type_contain_flag == 1:
            if line_lower[0:8] == 'end type':
                type_contain_flag = 0

        # find the implicit none
        if line_lower[0:13] == 'implicit none':
            code_blocks[0].var_beg = line_num + 1
            code_blocks[0].imnone = line_num

        # For module files, if there is 'contains', then there is no subroutine or function.
        if line_lower[0:8] == 'contains' and type_contain_flag == 0:
            code_blocks[0].line_end = line_num
            code_blocks[0].var_end = line_num - 1
            break
        # find the end of the module file 
        if line_lower[0:10] == 'end module':
            code_blocks[0].line_end = line_num
            code_blocks[0].var_end = line_num - 1
            return code_blocks
    f.close()

    end_mark = line_num
    block_num = 0
    line_num = 0
    line_beg = 0

    # step 2
    stop_flag = 0
    for i in range(100000):
        if stop_flag == 1:
            break
        code_blocks.append(code_block())
        line_beg = code_blocks[block_num].line_end
        block_num = block_num + 1
        line_num = 0
        start_flag = 1
        var_start_flag = 1
        var_end_flag = 1
        f = open(file_name)
        for line in f:
            line_num = line_num + 1
            if line_lower[0:10] == 'end module':
                stop_flag = 1
                break

            if line_num == total_line_num:
                stop_flag = 1
                break

            if line_num > line_beg:
                line = line.lstrip()
                line_lower = line.lower()

                if line_lower[0:13] == 'implicit none':
                    code_blocks[block_num].imnone = line_num

                # Start searching when seeing subroutine or function
                if start_flag == 1:
                    if line_lower[0:1] == '!' or line_lower[0:8] == 'function':
                        start_flag = 0
                        code_blocks[block_num].line_beg = line_num
                    if line_lower[0:1] == '!' or line_lower[0:10] == 'subroutine':
                        start_flag = 0
                        code_blocks[block_num].line_beg = line_num

                # start of the variable list
                if var_start_flag == 1:
                    if line_lower[0:4] == '!!>>':
                        var_start_flag = 0
                        code_blocks[block_num].var_beg = line_num +1

                # end of the variable list
                if var_end_flag == 1:
                    if line_lower[0:4] == '!!<<':
                        var_end_flag = 0
                        code_blocks[block_num].var_end = line_num -1

                # end searching when see end
                if line_lower[0:12] == 'end function':
                    end_flag = 0
                    code_blocks[block_num].line_end = line_num
                    line_beg = line_num
                    f.close()
                    break
                if line_lower[0:14] == 'end subroutine':
                    end_flag = 0
                    code_blocks[block_num].line_end = line_num
                    line_beg = line_num
                    f.close()
                    break

                if 'function' in line_lower and '!' not in line_lower and "end" not in line_lower:
                    code_blocks[block_num].block_type = 'function'

                if line_lower[0:10] == 'subroutine':
                    code_blocks[block_num].block_type = 'subroutine'

    return code_blocks



class code_block():
    """
    This class contains the information and operation for one cde block
    """
    def __init__(self):
        """
        Ths variables will be determined in =get_start_end=
        """
        self.line_beg = 0
        self.line_end = 0
        self.block_type = 'unknown'
        self.var_beg = 0
        self.var_end = 0
        self.imnone = 0
        self.scripts = []

    def get_comments(self, file_name):
        """
        Determine the type of the Block.
        Current only support **module**, **subroutine** and **function**.
        """

        if self.block_type == 'module':
            self.module_header(file_name)
        elif self.block_type == 'subroutine':
            self.subroutine_header(file_name)
        elif self.block_type == 'function':
            self.function_header(file_name)

        self.body_comments(file_name)

    # body comments
    def body_comments(self, file_name):
        """ Extract the documentation in the code body,
        includeing, normal lines, src blocks. Normallines will start a new paragraph if there is **...** in the previous line.
        """
        scb = []
        cb1 = []
        cb2 = []
        cbs = []
        scripts = []
        f = open(file_name)
        line_num = 0
        for line in f:
            line_num = line_num + 1
            line = line.lstrip()
            line_lower = line.lower()
            # determine the src blocks
            if self.imnone <= line_num <= self.line_end:
                if '!->>' in line_lower:
                    cb1.append(line_num)
                if '!-<<' in line_lower:
                    cb2.append(line_num)
                if '!-><' in line_lower:
                    scb.append(line_num)

        if cb1:
            for i in range(len(cb1)):
                for j in range(cb1[i]+1,cb2[i]):
                    cbs.append(j)

        f.close()
        f = open(file_name)
        line_num = 0
        cb_flag = 0
        num_block = 0
        for line in f:
            line_num = line_num + 1
            num_block= num_block + 1
            line = line.lstrip()
            line_lower = line.lower()
            if self.imnone <= line_num <= self.line_end:
                # add lines for src blocks
                if line_num in scb:
                    self.scripts.append('#+begin_src fortran\n')
                    ss = [i for i in range(len(line)) if line[i] == "<"][-1]
                    self.scripts.append(line)
                    self.scripts.append('#+end_src\n\n')
                elif line_num in cb1:
                    self.scripts.append('#+begin_src fortran\n')
                    ss = [i for i in range(len(line)) if line[i] == ">"][-1]
                    self.scripts.append(line)
                elif line_num in cb2:
                    ss = [i for i in range(len(line)) if line[i] == "<"][-1]
                    self.scripts.append(line)
                    self.scripts.append('#+end_src\n\n')
                elif line_num in cbs:
                    self.scripts.append(line)
                else:
                    # add normal lines. if "..." at the end of the line,
                    # new line will start with a new paragraph
                    if '!>' in line_lower:
                        ss = [i for i in range(len(line)) if line[i] == ">"][-1]
                        if '...' in line_lower:
                            self.scripts.append(line[ss+2:-4]+'\n\n')
                        else:
                            self.scripts.append(line[ss+2:])

    def module_header(self, file_name):
        """ Extract the documentation from the module header.
        The **module name** will be automatically extracted from the file and the documentation before the starting of the module will come after the title of the module. Then the list of the variables will be extracted automatically as the src block.
        """
        hscripts = []
        ohscripts = []
        use = []
        var = []
        HHflag = 0
        Uflag = 0

        f = open(file_name)
        for line in f:
            line = line.lstrip()
            line_lower = line.lower()
            # Get the org options
            if line_lower[0:4] == '!=oh':
                ohscripts.append(line[5:])
            if line_lower[0:6] == 'module':
                mod_name = line[7:]
                self.scripts.append('* ' + mod_name)
                f.close()
                break;

        f = open(file_name)
        line_num = 0
        for line in f:
            line_num = line_num + 1
            if line_num > self.line_end:
                break
            line_o = line
            line = line.lstrip()
            line_lower = line.lower()

            # Get the documentation before the begining of the module
            if line_lower[0:4] == '!=>>':
                HHflag = 1
            if line_lower[0:4] == '!=<<':
                HHflag = 0
            if HHflag == 1 and line[0:2] == '!>':
                if '...' in line_lower:
                    hscripts.append(line[3:-4]+'\n\n')
                else:
                    hscripts.append(line[3:])

            if line_num > self.imnone and line_num < self.line_end:
                var.append(line_o)


            # Get the use module
            if line_lower[0:3] == 'use':
                use.append(line_o)

        f.close()

        for i in range(len(ohscripts)):
            self.scripts.append(ohscripts[i])
        self.scripts.append('\n')

        for i in range(len(hscripts)):
            self.scripts.append(hscripts[i])
        self.scripts.append('\n')

        # export the list of use module
        self.scripts.append('- Use of modules:\n\n')
        if use:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(use)):
                self.scripts.append(use[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        # export the list of variables
        self.scripts.append('- list of variables:\n\n')
        if var:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(var)):
                self.scripts.append(var[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

    def subroutine_header(self, file_name):
        """ Extract the documentation from the subroutien header.
        The **subroutine name** will be automatically extracted from the file and the documentation before the starting of the module will come after the title of the module.
        The =input= and =output= variables will be detected by the =intent= syntax. 
        Then the list of the variables will be extracted automatically as the src block.
        """
        hscripts = []
        ohscripts = []
        use = []
        var = []
        interface_names = []
        inputs = []
        outputs = []

        f = open(file_name)
        line_num = 0
        for line in f:
            line_num = line_num + 1
            line = line.lstrip()
            line_lower = line.lower()
            if line_lower[0:10] == 'subroutine' and self.line_beg <= line_num <= self.line_end:
                subroutine_name = line.split('(')[0]
                subroutine_name = subroutine_name[11:]
                self.scripts.append('** ' + subroutine_name)
                ss1 = [i for i in range(len(line)) if line[i] == "("]
                ss2 = [i for i in range(len(line)) if line[i] == ")"]
                if ss1:
                    interface_string = line[ss1[0]+1:ss2[0]]
                    interface_names = interface_string.split(',')
                    for i in range(len(interface_names)):
                        interface_names[i] = interface_names[i].strip()
                f.close()
                break;

        f = open(file_name)
        line_num = 0
        HHflag = 0
        Uflag = 0
        for line in f:
            line_num = line_num + 1
            if line_num < self.line_beg:
                continue
            if line_num > self.line_end:
                break
            line_o = line
            line = line.lstrip()
            line_lower = line.lower()

            # get the documentation for the before the beginning of the subrouine
            if line_lower[0:4] == '!=>>':
                HHflag = 1
            if line_lower[0:4] == '!=<<':
                HHflag = 0
            if HHflag == 1 and line[0:2] == '!>':
                hscripts.append(line[2:])

            # get the org options.
            if line_lower[0:4] == '!=oh':
                ohscripts.append(line[5:])

            # detect the use variables
            if line_lower[0:3] == 'use':
                use.append(line_o)

            if line_num >= self.var_beg and line_num <= self.var_end:
                var.append(line_o)

            # determine the input and output
            if ss1:
                if ('intent(in)' in line_lower):
                    inputs.append(line_o)
                if ('intent(out)' in line_lower):
                    outputs.append(line_o)
                if ('intent(inout)' in line_lower):
                    inputs.append(line_o)
                    outputs.append(line_o)

        f.close()

        # export documentation
        for i in range(len(ohscripts)):
            self.scripts.append(ohscripts[i])
        self.scripts.append('\n')

        for i in range(len(hscripts)):
            self.scripts.append(hscripts[i])
        self.scripts.append('\n')

        self.scripts.append('- Use of modules:\n')
        if use:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(use)):
                self.scripts.append(use[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        self.scripts.append('input variables\n\n')
        if inputs:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(inputs)):
                self.scripts.append(inputs[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        self.scripts.append('output variables\n\n')
        if outputs:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(outputs)):
                self.scripts.append(outputs[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        self.scripts.append('- list of local variables:\n\n')
        if var:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(var)):
                self.scripts.append(var[i])
            self.scripts.append('#+end_src\n\n')
 


    def function_header(self, file_name):
        """ Extract the documentation from the function header.
        The **function name** will be automatically extracted from the file and the documentation before the starting of the module will come after the title of the module.
        The =input= and =output= variables will be detected by function line.
        Then the list of the variables will be extracted automatically as the src block.
        """
        hscripts = []
        ohscripts = []
        use = []
        var = []
        input_names = []
        output_names = []
        inputs = []
        outputs = []

        f = open(file_name)
        line_num = 0
        for line in f:
            line_num = line_num + 1
            line = line.lstrip()
            line_lower = line.lower()
            # determine the input and output variables
            if line_lower[0:8] == 'function' and self.line_beg <= line_num  <= self.line_end:
                function_name = line.split('(')[0]
                function_name = function_name[9:]
                self.scripts.append('** ' + function_name)
                ss1 = [i for i in range(len(line)) if line[i] == "("]
                ss2 = [i for i in range(len(line)) if line[i] == ")"]
                input_string = line[ss1[0]+1:ss2[0]]
                if 'output' in line_lower:
                    output_string = line[ss1[1]+1:ss2[1]]
                else:
                    output_string = function_name
                input_names = input_string.split(',')
                output_names = output_string.split(',')
                for i in range(len(inputs)):
                    input_names[i] = input_names[i].strip()
                for i in range(len(output_names)):
                    output_names[i] = output_names[i].strip()
                f.close()
                break;

        # determine the types of export script
        f = open(file_name)
        line_num = 0
        HHflag = 0
        Uflag = 0
        for line in f:
            line_num = line_num + 1
            if line_num < self.line_beg:
                continue
            if line_num > self.line_end:
                break
            line_o = line
            line = line.lstrip()
            line_lower = line.lower()

            if line_lower[0:4] == '!=>>':
                HHflag = 1
            if line_lower[0:4] == '!=<<':
                HHflag = 0
            if HHflag == 1 and line[0:2] == '!>':
                hscripts.append(line[2:])

            if line_lower[0:4] == '!=oh':
                ohscripts.append(line[5:])

            if line_lower[0:3] == 'use':
                use.append(line_o)

            if line_num >= self.var_beg and line_num <= self.var_end:
                var.append(line_o)

            for i in range(len(input_names)):
                if input_names[i] in line and '::' in line:
                    inputs.append(line_o)
            for i in range(len(output_names)):
                if output_names[i] in line and '::' in line:
                    outputs.append(line_o)

        f.close()

        # export the documentation
        for i in range(len(ohscripts)):
            self.scripts.append(ohscripts[i])
        self.scripts.append('\n')

        for i in range(len(hscripts)):
            self.scripts.append(hscripts[i])
        self.scripts.append('\n')

        self.scripts.append('- Use of modules:\n')
        if use:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(use)):
                self.scripts.append(use[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        self.scripts.append('input variables\n\n')
        if inputs:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(inputs)):
                self.scripts.append(inputs[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        self.scripts.append('output variables\n\n')
        if outputs:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(outputs)):
                self.scripts.append(outputs[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

        self.scripts.append('- list of local variables:\n\n')
        if var:
            self.scripts.append('#+begin_src fortran\n')
            for i in range(len(var)):
                self.scripts.append(var[i])
            self.scripts.append('#+end_src\n\n')
        else:
            self.scripts.append('*None*\n\n')

    # only used for debug purpose
    def printblock(self):
        for i in range(len(self.scripts)):
            print(self.scripts[i])

    # only used for debug purpose
    def printvar(self):
        print('line_beg', self.line_beg)
        print('line_end', self.line_end)
        print('line_block_type', self.block_type)
        print('var_beg', self.var_beg)
        print('var_end', self.var_end)
        print('scripts', self.scripts)

    def write_2_file(self, file_name):
        """write the documentation to file"""
        if self.block_type == 'module':
            f = open(file_name, 'w')
        else:
            f = open(file_name, 'a')
        for i in range(len(self.scripts)):
            f.write(self.scripts[i])


# # main function
file = sys.argv[1]
out_file = sys.argv[2]

a = get_start_end(file)


for i in range(len(a)):
    a[i].get_comments(file)
    a[i].write_2_file(out_file)
