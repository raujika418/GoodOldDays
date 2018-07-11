##############################################################################
# 
#                     The LLVM Compiler Infrastructure
#
# This file was developed by the LLVM research group and is distributed under
# the University of Illinois Open Source License. See LICENSE.TXT for details.
# 
##############################################################################
#
# File: llvm.py
#
# Description:
#	This file contains python classes that define resources and tests
#	performed by the LLVM test suite.
#
##############################################################################

import qm
import qm.test
import qm.test.test
import qm.fields

import os
import filecmp
import resource

#
# Function: ExecProgram ()
#
# Description:
#	Execute the specified program with the specified environment.
#
# Inputs:
#	args - An array of arguments to the program.
#	env  - The environment passed to the new program.
#
# Outputs:
#	None.
#
# Return value:
#	0 - The program executed successfully.
#	1 - The program failed.
#
def ExecProgram (args, env = os.environ, outfile=''):

	#
	# Create a pipe to the new program.
	#
	if (outfile == ''):
		(parent_in, child_out) = os.pipe ()
	else:
		child_out = os.open (outfile, os.O_WRONLY | os.O_CREAT, 0700)

	#
	# Create a new child process.
	#
	child = os.fork()
	if (child == 0):
		#
		# Construct new stdout, and stderr.
		#
		os.dup2 (child_out, 1);
		os.dup2 (child_out, 2);

		#
		# Execute the specified program.
		#
		os.execvpe (args[0], args, env)

		#
		# Exit with failure if the exec failed for any reason.
		#
		os._exit (1)

	#
	# Parent process.
	#

	#
	# Wait for the child process to exit.
	#
	(pid, status) = os.waitpid (child, 0)

	#
	# Close our pipes to the child.
	#
	os.close (child_out)
	if (outfile == ''):
		os.close (parent_in)

	#
	# Return true if the program crashed or exited with a non-zero
	# exit status.
	#
	return (not ((os.WIFEXITED(status)) and ((os.WEXITSTATUS(status)) == 0)))

##############################################################################
#
# Class: AssembleTest
#
# Description:
#	This class verifies that a specified LLVM assembly file can be
#	assembled into bytecode using the `llvm-as' utility.
#
##############################################################################
class AssembleTest(qm.test.test.Test):

	#
	# Description
	#
	description="Verifies that LLVM can assemble a file"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Assembly File',
		                    description='LLVM Assembly File to Assemble'),
	]

	devnull = ' > /dev/null 2>&1'

	def Run (self, context, result):
		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']
		buildtype=context['buildtype']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile=srcroot + '/' + self.srcfile
		bcpath=tmpdir + '/' + 'temp.bc'

		#
		# Construct the pathnames to the various utilities.
		#
		as  = buildroot + '/tools/' + buildtype + '/llvm-as'

		#
		# Assume we will succeed
		#
		fail = 0

		#
		# Use the LLVM assembler to assemble the program.
		#
		if (ExecProgram ((as, srcfile, '-d', '-f', '-o', bcpath))):
			fail = 1
			result.Fail('Failed to assemble ' + srcfile)

		#
		# Cleanup the bytecode file.
		#
		if ((os.access(bcpath,os.F_OK)) == 1):
			os.remove (bcpath)
		else:
			if (fail != 1):
				result.Fail('Output file not generated')

		return

##############################################################################
#
# Class: ConvertToCTest
#
# Description:
#	This test verifies that the specified bytecode file can be converted
#	back into C code.
#
##############################################################################
class ConvertToCTest(qm.test.test.Test):

	description="Verifies that LLVM can convert a file into C code"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Bytecode File',
		                    description='LLVM Bytecode File to Convert to C'),
	]


	devnull = ' > /dev/null 2>&1'

	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile=srcroot + '/' + self.srcfile
		bcfile=tmpdir + '/feature-cc-' + os.path.basename (self.srcfile)
		objfile=tmpdir + '/feature-cc-' + os.path.basename (self.srcfile) + '.tc'

		#
		# Construct the pathnames to the various utilities.
		#
		as  = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'
		dis = buildroot + '/tools/' + context['buildtype'] + '/llvm-dis'

		#
		# Use the LLVM assembler to assemble the program.
		#
		if (ExecProgram ((as, srcfile, '-f', '-o', bcfile))):
			result.Fail ('Failed to assemble ' + srcfile)
			return

		#
		# Use the LLVM disassembler to convert the program to C code.
		#
		if (ExecProgram ((dis, bcfile, '-f', '-c', '-o', objfile))):
			fail = 1
			result.Fail ('Failed to convert ' + bcfile + ' to C code.')
		else:
			fail = 0

		#
		# Cleanup the file.
		#
		if ((os.access(objfile,os.F_OK)) == 1):
			os.remove (objfile)
		else:
			if (fail == 0):
				result.Fail ('Object file not generated')

		return

##############################################################################
#
# Class: LLToCTest
#
# Description:
#	This test verifies that the specified LLVM source file can be converted
#	into C code which can then be compiled.
#
##############################################################################
class LLToCTest(qm.test.test.Test):

	description="Verifies that LLVM can convert a file into C code"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Bytecode File',
		                    description='LLVM Bytecode File to Convert to C'),
	]


	devnull = ' > /dev/null 2>&1'

	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile=srcroot + '/' + self.srcfile
		csrcfile=tmpdir + '/' + os.path.basename (self.srcfile) + '.c'
		cobjfile=tmpdir + '/' + os.path.basename (self.srcfile) + '.to'

		#
		# Construct the pathnames to the various utilities.
		#
		cc   = context['cc']
		as   = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'
		dis  = buildroot + '/tools/' + context['buildtype'] + '/llvm-dis'

		#
		# Construct the command to generate the C source and object
		# file.
		#
		ccmd = as + ' < ' + srcfile + ' | ' + dis + ' -f -c -o=' + csrcfile
		lcmd = cc + ' -c -Werror ' + csrcfile + ' -o ' + cobjfile + ' >  ' + tmpdir + '/' + os.path.basename (self.srcfile) + '.out 2>&1'

		#
		# Assemble the program into C code and then compile it.
		#
		estatus=os.system (ccmd)
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			fail = 1
			result.Fail('Converting to C code failed')
		else:
			estatus=os.system (lcmd)
			if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
				fail = 1
				result.Fail('Compiling code failed')
			else:
				fail = 0

		#
		# Cleanup the files.
		#
		if ((os.access(cobjfile,os.F_OK)) == 1):
			os.remove (cobjfile)
		else:
			if (fail == 0):
				result.Fail ('Object file not generated')

		if ((os.access(csrcfile,os.F_OK)) == 1):
			os.remove (csrcfile)
		else:
			if (fail == 0):
				result.Fail ('Source file not generated')

		return

##############################################################################
#
# Class: AssemblyCodeTest
#
# Description:
#	This test verifies that the specified bytecode file can be converted
#	into native assembly code.
#
##############################################################################
class MachineCodeTest(qm.test.test.Test):

	description="Verifies that LLVM can convert a file into native assembly code"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Assembly Code File',
		                    description='LLVM Assembly Code File to Convert to Native Assembly Code'),
	]

	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile=srcroot + '/' + self.srcfile
		bcfile=tmpdir + '/feature-mc-' + os.path.basename (self.srcfile)
		objfile=tmpdir + '/feature-mc-' + os.path.basename (self.srcfile) + '.s'

		#
		# Construct the pathnames to the various utilities.
		#
		as   = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'
		llc  = buildroot + '/tools/' + context['buildtype'] + '/llc'

		#
		# Assemble the bytecode file.
		#
		if (ExecProgram ((as, srcfile, '-f', '-o', bcfile))):
			result.Fail ('Failed to assemble ' + srcfile + ' to file ' + bcfile)
			return

		#
		# Use the LLVM assembler to assemble the program.
		#
		if (ExecProgram ((llc, bcfile, '-f', '-o', objfile))):
			fail = 1
			result.Fail('Failed to compile ' + bcfile + ' to native asm code.')
		else:
			fail = 0

		#
		# Cleanup the file if it exists.
		#
		if ((os.access(objfile,os.F_OK)) == 1):
			os.remove (objfile)
		else:
			if (fail == 0):
				result.Fail ('Native assembly file ' + objfile + ' was not created')

		return

##############################################################################
#
# Class: TestOptimizer
#
# Description:
#	This class runs two optimizer tests on the specified program.
#
##############################################################################
class TestOptimizer(qm.test.test.Test):

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Assembly File',
		                    description='LLVM Assembly File to run through the Optimizer'),
	]

	#
	# LLVM Utilities
	#
	as='llvm-as'
	dis='llvm-dis'
	opt='opt'

	#
	# LLVM Directories
	#
	tmpdir=''

	#
	# Method: Run
	#
	# Description:
	#	This method is called when the test case is invoked by QMTest.
	#
	def Run (self, context, result):
		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		self.tmpdir = context['tmpdir']

		#
		# Construct the pathname of the source file.
		#
		sourcefile=srcroot + '/' + self.srcfile

		#
		# Construct the pathnames to the various utilities.
		#
		self.as  = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'
		self.dis = buildroot + '/tools/' + context['buildtype'] + '/llvm-dis'
		self.opt = buildroot + '/tools/' + context['buildtype'] + '/opt'

		#
		# Run the optimizer.
		#
		flags = '-constprop -dce'
		self.Opt (flags, sourcefile, result)
		flags = '-sccp -dce'
		self.Opt (flags, sourcefile, result)

		return

	#
	# Method: Opt()
	#
	# Description:
	#	This method attempts to assemble and optimize the program
	#	with the specified flags.  If the resultant program can be
	#	further reduced, then the optimization has failed.
	#
	# Inputs:
	#	flags - The list of optimizer flags to use.
	#	file - The assembly file to assemble and optimize.
	#	result - The result object used for the test.
	#
	# Notes:
	#	This function will probably not be portable the first time
	#	around.
	#
	def Opt(self, flags, file, result):
		#
		# Create local versions of the global class variables
		#
		as = self.as
		dis=self.dis
		opt=self.opt

		#
		# The pipe symbole
		#
		p=' | '

		estatus = os.system (as + ' < ' + file + p + opt + ' -q -inline -dce ' + flags + p + dis + p + as + ' > ' + 'bc.1')
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			result.Fail()
			return;

		#
		# Now, attempt to optimize the the program again.
		#
		estatus=os.system (opt + ' -q ' + flags + ' < ' + 'bc.1 > ' + 'bc.2')
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			result.Fail()
			return

		#
		# If the two programs are identical, then we have failed!
		#
		# This should provide a bytecode file that cannot be optimized
		# any further.
		#
		status1 = os.spawnl (os.P_WAIT, dis, dis, 'bc.1', '-f', '-o=ll.1');
		status2 = os.spawnl (os.P_WAIT, dis, dis, 'bc.2', '-f', '-o=ll.2');
		if ((status1 != 0) or (status2 != 0)):
			result.Fail()
			return

		status = filecmp.cmp ('ll.1', 'll.2', 'false');
		if (status == 0):
			result.Fail()
			return

		#
		# Cleanup any temporary files.
		#
		os.remove ('bc.1')
		os.remove ('bc.2')
		os.remove ('ll.1')
		os.remove ('ll.2')
		return

##############################################################################
#
# Class: TestAsmDisasm
#
# Description:
#	This class defines a qmtest test class.  It will assemble and
#	disassemble a given LLVM assembly file to ensure that the assembler
#	and disassembler work properly.
#
##############################################################################
class TestAsmDisasm(qm.test.test.Test):

	# List of information that is passed into each test
	arguments = [
		# Name of the file to assemble and disassemble
		qm.fields.TextField(name="srcfile",
		                    title="LLVM Assembly File",
		                    description="Name of the LLVM Assembly language file to assemble and disassemble"),
	]

	#
	# Method: Run
	#
	# Description:
	#	This method is called by qmtest to start the test.  It will
	#	complete two full cycles of assembling/disassembling since
	#	that is what is needed for bitwise stability.
	#
	def Run(self,context,result):
		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Determine where the build directory is located.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']

		#
		# Determine the path to the assembler and disassembler
		#
		as  = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'
		dis = buildroot + '/tools/' + context['buildtype'] + '/llvm-dis'

		#
		# Find the name of the file to assemble.
		#
		input=srcroot + '/' + self.srcfile

		#
		# Construct output filenames.
		#
		bc1 = context['tmpdir'] + '/bc.1'
		bc2 = context['tmpdir'] + '/bc.2'
		ll1 = context['tmpdir'] + '/ll.1'
		ll2 = context['tmpdir'] + '/ll.2'

		#
		# Assemble the file.
		#
		if (ExecProgram ((as, input, '-f', '-o', bc1))):
			result.Fail ()

		#
		# Disassemble the output.
		#
		if (ExecProgram ((dis, bc1, '-f', '-o', ll1))):
			result.Fail()

		#
		# Assemble the program again.
		#
		if (ExecProgram ((as, ll1, '-f', '-o', bc2))):
			result.Fail()

		#
		# Disassemble the program one last time.
		#
		if (ExecProgram ((dis, bc2, '-f', '-o', ll2))):
			result.Fail()

		#
		# Compare the LLVM assembly output to see if it is the same.
		#
		exit_status = filecmp.cmp (ll1, ll2, 'false')
		if exit_status == 0:
			result.Fail()
			return

		#
		# Cleanup the test.
		#
		os.remove (bc1)
		os.remove (bc2)
		os.remove (ll1)
		os.remove (ll2)

		return


##############################################################################
#
# Class: VerifierTest
#
# Description:
#	This class is designed to test the verifier pass of LLVM.  It attempts
#	to assemble the specified file.
#
##############################################################################
class VerifierTest(qm.test.test.Test):

	#
	# Description
	#
	description="Verifies that LLVM can assemble a file"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Assembly File',
		                    description='LLVM Assembly File to Verify'),
	]

	devnull = ' > /dev/null 2>&1'

	def Run (self, context, result):
		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file.
		#
		srcfile=srcroot + '/' + self.srcfile

		#
		# Construct the pathnames to the various utilities.
		#
		as  = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'

		#
		# Use the LLVM assembler to assemble the program.
		#
		cmd = as + ' ' + srcfile + ' -f -o /dev/null ' + self.devnull
		estatus=os.system (cmd)
		if (os.WIFEXITED(estatus)):
			if (os.WEXITSTATUS(estatus) == 0):
				result.Fail(srcfile + ' assembled')
		else:
			result.Fail ('Assembler Terminated Abnormally')

		return


##############################################################################
#
# Class: LLITest
#
# Description:
#	This test verifies that the specified bytecode file can be run through
#	the JIT.
#
##############################################################################
class LLITest(qm.test.test.Test):

	description="Verifies that LLVM can use LLI on the specified program"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='LLVM Bytecode File',
		                    description='LLVM Bytecode File to Convert to Machine Code'),
	]

	devnull = ' > /dev/null 2>&1'

	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile = srcroot + '/' + self.srcfile
		bcfile  = tmpdir + '/llitest-' + os.path.basename (self.srcfile)

		#
		# Construct the pathnames to the various utilities.
		#
		as  = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'
		lli = buildroot + '/tools/' + context['buildtype'] + '/lli -force-interpreter=false '

		#
		# Assemble the program.
		#
		if (ExecProgram ((as, '-f', '-o', bcfile, srcfile))):
			result.Fail('Failed to assemble LLVM code')

		#
		# Execute the program.
		#
		estatus=os.system (lli + ' ' + bcfile + ' < /dev/null')
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			result.Fail('LLI failed to execute bytecode')

		return

##############################################################################
#
# Class: TestRunner
#
# Description:
#	This test executes the specified program using the TestRunner script
#	from days of yore.
#
##############################################################################
class TestRunner(qm.test.test.Test):

	description="Execute a test via the TestRunner Script"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='TestRunner script file',
		                    description='Name of script to execute via TestRunner'),
	]


	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']
		buildtype=context['buildtype']

		#
		# Create a new directory based upon the test's name.
		#
		tmpdir = tmpdir + '/tr' + os.path.basename (self.srcfile)
		if ((os.access(tmpdir,os.F_OK)) == 0):
			try:
				os.mkdir (tmpdir)
			except OSError:
				result.Fail ('Failed to make ' + tmpdir)
				return

		#
		# Construct the pathname of the source file and output script
		# file.
		#
		srcfile=srcroot + '/' + self.srcfile
		scriptfile = tmpdir + '/testscript.' + os.path.basename (srcfile)
		outputfile = tmpdir + '/testscript.' + os.path.basename (srcfile) + '.out'

		#
		# Construct a new path that includes the LLVM tools.
		#
		environment=os.environ
		oldpath=environment['PATH']
		environment['PATH'] = buildroot + '/tools/' + buildtype + ':' + srcroot + '/test/Scripts:' + environment['PATH']
		environment['QMV_llvmgcc'] = context['llvmgcc']

		#
		# Create the script that will run the test.
		#
		exstatus=os.spawnlp (os.P_WAIT, 'sed',
		                                'sed',
		                                '-n',
		                                '-e',
		                                '/RUN:/!d;',
		                                '-e',
		                                '/RUN:/s|^.*RUN:\(.*\)$|\\1|g;',
		                                '-e',
		                                's|%s|' + srcfile + '|g;',
		                                '-e',
		                                's|%t|' + scriptfile + '.tmp|g;',
		                                '-e',
		                                's|%llvmgcc|' + context['llvmgcc'] + '|g;',
		                                '-e',
		                                's|%llvmgxx|' + context['llvmgxx'] + '|g;',
		                                '-e',
		                                'w ' + scriptfile,
		                                srcfile)

		if (exstatus != 0):
			result.Fail('The sed script failed')
			environment['PATH'] = oldpath
			return

		#
		# Execute the script using TestRunner.
		#
		mypath = os.getcwd ()
		os.chdir (tmpdir)
		if (ExecProgram (('/bin/sh', scriptfile), environment, outputfile)):
			result.Fail('Script: ' + scriptfile + '\n    Output: ' + outputfile)

		os.chdir (mypath)

		#
		# Restore the PATH environment variable
		#
		environment['PATH'] = oldpath

		#
		# Return to the caller.
		#
		return


##############################################################################
#
# Class: CTest
#
# Description:
#	This test verifies that the specified C program can be compiled
#	into LLVM assembly code which can then be assembled into LLVM byte
#	code.
#
##############################################################################
class CTest(qm.test.test.Test):

	description="Verifies that LLVM can compile C code into LLVM bytecode"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='C Source Code File',
		                    description='C Source Code File to Convert to LLVM Bytecode'),
	]


	#devnull = ' > /dev/null 2>&1'
	devnull = ''

	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile=srcroot + '/' + self.srcfile
		llvmsrc=tmpdir + '/' + os.path.basename (self.srcfile) + '.ll'
		llvmobj=tmpdir + '/' + os.path.basename (self.srcfile) + '.bc'

		#
		# Construct the pathnames to the various utilities.
		#
		cc   = context['llvmgcc']
		as   = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'

		#
		# Construct the command to generate the LLVM assembly and byte
		# code.
		#
		ccmd = cc + ' -S ' + srcfile + ' -o ' + llvmsrc + self.devnull
		acmd = as + ' -f ' + llvmsrc + ' -o /dev/null 2>&1 /dev/null'

		#
		# Assemble the program into C code and then compile it.
		#
		estatus=os.system (ccmd)
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			fail = 1
			result.Fail('Compiling C code failed')
		else:
			estatus=os.system (acmd)
			if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
				fail = 1
				result.Fail('Compiling code failed')
			else:
				fail = 0

		#
		# Cleanup the files.
		#
		if ((os.access(llvmsrc,os.F_OK)) == 1):
			os.remove (llvmsrc)
		else:
			if (fail == 0):
				result.Fail ('Object file not generated')

		return

##############################################################################
#
# Class: CXXTest
#
# Description:
#	This test verifies that the specified C++ program can be compiled
#	into LLVM assembly code which can then be assembled into LLVM byte
#	code.
#
##############################################################################
class CXXTest(qm.test.test.Test):

	description="Verifies that LLVM can compile C++ code into LLVM bytecode"

	#
	# List of arguments that the objects accepts for test configuration.
	#
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='C Source Code File',
		                    description='C Source Code File to Convert to LLVM Bytecode'),
	]


	#devnull = ' > /dev/null 2>&1'
	devnull = ''

	def Run (self, context, result):

		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Fetch the source and build root directories from the context.
		#
		srcroot=context['srcroot']
		buildroot=context['buildroot']
		tmpdir=context['tmpdir']

		#
		# Construct the pathname of the source file and object file.
		#
		srcfile=srcroot + '/' + self.srcfile
		llvmsrc=tmpdir + '/' + os.path.basename (self.srcfile) + '.ll'
		llvmobj=tmpdir + '/' + os.path.basename (self.srcfile) + '.bc'

		#
		# Construct the pathnames to the various utilities.
		#
		cc   = context['llvmgxx']
		as   = buildroot + '/tools/' + context['buildtype'] + '/llvm-as'

		#
		# Construct the command to generate the LLVM assembly and byte
		# code.
		#
		ccmd = cc + ' -S ' + srcfile + ' -o ' + llvmsrc + self.devnull
		acmd = as + ' -f ' + llvmsrc + ' -o /dev/null 2>&1 /dev/null'

		#
		# Assemble the program into C code and then compile it.
		#
		estatus=os.system (ccmd)
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			fail = 1
			result.Fail('Compiling C++ code failed')
		else:
			estatus=os.system (acmd)
			if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
				fail = 1
				result.Fail('Compiling code failed')
			else:
				fail = 0

		#
		# Cleanup the files.
		#
		if ((os.access(llvmsrc,os.F_OK)) == 1):
			os.remove (llvmsrc)
		else:
			if (fail == 0):
				result.Fail ('Object file not generated')

		return


##############################################################################
# RESOURCES
##############################################################################

##############################################################################
#
# Class: MakeResource
#
# Description:
#	This resource builds the specified directory.
#
##############################################################################
class MakeResource(qm.test.resource.Resource):
	arguments = [
		qm.fields.TextField(name='srcdir',
		                    title='Source Directory',
		                    description='Name of the source directory to build.'),
		qm.fields.TextField(name='bcfilename',
		                    title='Bytecode Filename',
		                    description='Name of the bytecode file to be generated.'),
	]

	srcroot=''
	buildroot=''
	tmpdir=''

	#
	# Method: SetUp()
	#
	# Description:
	#	This method creates an instance of this resource.
	#	Specifically, it will use make to build the specified
	#	directory.
	#
	def SetUp(self, context, result):
		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Determine the pathnames to the source code and object code.
		#
		self.srcroot     = context['srcroot']
		self.buildroot   = context['buildroot']
		self.tmpdir      = context['tmpdir']
		self.make = make = context['make']

		#
		# Create the complete pathname of the source directory.
		#
		srcdir = self.srcroot + '/' + self.srcdir

		#
		# Get the current directory.
		#
		curdir = os.getcwd()

		#
		# Change to the directory where the makefile will be.
		#
		os.chdir (self.buildroot + '/' + self.srcdir)

		#
		# Execute make to build the programs.
		#
		estatus=os.system (make)
		if (not ((os.WIFEXITED(estatus)) and ((os.WEXITSTATUS(estatus)) == 0))):
			result.Fail()

		#
		# Change our working directory back to where we were.
		#
		os.chdir (curdir)

		return

	#
	# Method: CleanUp()
	#
	# Description:
	#	This method removes the resource once it is no longer needed.
	#
	def CleanUp(self, result):
		return


##############################################################################
#
# Class: BytecodeResource
#
# Description:
#	This resource takes the specified source file and builds a bytecode
#	file from it.
#
##############################################################################

class BytecodeResource(qm.test.resource.Resource):
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='Source File',
		                    description='Name of the file to assemble into bytecode.'),
		qm.fields.TextField(name='bcfilename',
		                    title='Bytecode Filename',
		                    description='Name of the bytecode file to be generated.'),
	]

	srcroot=''
	buildroot=''
	tmpdir=''

	#
	# Method: SetUp()
	#
	# Description:
	#	This method creates an instance of this resource.
	#	Specifically, it will create the necessary bytecode file from
	#	the specified source file.
	#
	def SetUp(self, context, result):
		#
		# Set the core dump size
		#
		coresize=int(context['coresize'])
		resource.setrlimit (resource.RLIMIT_CORE, (coresize,coresize))

		#
		# Determine the pathnames to the source code and object code.
		#
		self.srcroot   = context['srcroot']
		self.buildroot = context['buildroot']
		self.tmpdir    = context['tmpdir']

		#
		# Now create the source file pathname.
		#
		srcpath = self.srcroot + '/' + self.srcfile
		bcpath  = self.tmpdir  + '/' + self.bcfilename

		#
		# Find the pathname of the assembler.
		#
		as = self.buildroot + '/tools/' + context['buildtype'] + '/llvm-as'

		#
		# Use the LLVM assembler to create the bytecode file.
		#
		exstatus=os.spawnl (os.P_WAIT, as, as, srcpath, '-o=' + bcpath)
		if (exstatus != 0):
			result.Fail('Failed to assemble ' + srcpath)
			return
		return

	#
	# Method: CleanUp()
	#
	# Description:
	#	This method removes the resource once it is no longer needed.
	#
	def CleanUp(self, result):
		os.remove (self.tmpdir + '/' + self.bcfilename)
		return


##############################################################################
#
# Class: NativecodeResource
#
# Description:
#	This resource takes the specified source file and compiles it into
#	a native program using the standard C or C++ compiler.
#
##############################################################################

class NativecodeResource(qm.test.resource.Resource):
	arguments = [
		qm.fields.TextField(name='srcfile',
		                    title='Source File',
		                    description='Name of the file to assemble into bytecode.'),
		qm.fields.TextField(name='libs',
		                    title='Libraries',
		                    description='Libraries to link into the program.'),
	]

	srcroot=''
	buildroot=''
	tmpdir=''

	#
	# Method: SetUp()
	#
	# Description:
	#	This method creates an instance of this resource.
	#	Specifically, it will create the compile the program and link
	#	it with the specified libraries.
	#
	def SetUp(self, context, result):

		#
		# Determine the pathnames to the source code and object code.
		#
		self.srcroot   = context['srcroot']
		self.buildroot = context['buildroot']
		self.tmpdir    = context['tmpdir']

		#
		# Split the pathname into it's basename and suffix.
		#
		(base, suffix) = os.path.splitext (self.srcfile)

		#
		# Now create the source file pathname.
		#
		srcpath = self.srcroot + '/' + self.srcfile
		objpath = self.tmpdir  + '/' + base

		#
		# Determine which compiler to use.
		#
		if (suffix == '.c'):
			cc = context['cc']
			cflags = context['cflags']
		else:
			cc = context['cpp']
			cflags = context['cppflags']

		#
		# Use the LLVM assembler to create the bytecode file.
		#
		exstatus=os.spawnl (os.P_WAIT, as, as, srcpath, '-o=' + bcpath)
		if (exstatus != 0):
			result.Fail('Failed to assemble ' + srcpath)
			return
		return

	#
	# Method: CleanUp()
	#
	# Description:
	#	This method removes the resource once it is no longer needed.
	#
	def CleanUp(self, result):
		os.remove (self.tmpdir + '/' + self.bcfilename)
		return

