##############################################################################
# 
#                     The LLVM Compiler Infrastructure
#
# This file was developed by the LLVM research group and is distributed under
# the University of Illinois Open Source License. See LICENSE.TXT for details.
# 
##############################################################################
#
# File: llvmdb.py
#
# Description:
#	This file contains python classes that define a new test database
#	for the LLVM test suite.
#
##############################################################################

import qm
import qm.test
import qm.test.test
import qm.fields
import qm.test.database
import qm.test.suite
import qm.attachment

import os
import stat
import filecmp
import resource

#
# Mapping between test directories and test names
#
RegressionMap={'Assembler':'llvm.AssembleTest',
               'Analysis':'llvm.TestRunner',
               'BugPoint':'llvm.TestRunner',
               'C++Frontend':'llvm.CXXTest',
               'CBackend':'llvm.LLToCTest',
               'CFrontend':'llvm.CTest',	
               'Jello':'llvm.LLITest',	
               'LLC':'llvm.MachineCodeTest',	
               'Linker':'llvm.TestRunner',	
               'Other':'llvm.TestRunner',	
               'TableGen':'llvm.TestRunner',	
               'Transforms':'llvm.TestRunner',	
               'Verifier':'llvm.VerifierTest'}

class llvmdb (qm.test.database.Database):

	# Pathname to the test database
	dbpath=''

	def __init__ (self, path, arguments):
		#
		# Record the location of the test database.
		#
		self.dbpath = path
		qm.test.database.Database.__init__ (self, path, arguments)
		return

	#
	# Method: GetTest ()
	#
	# Description:
	#	Retrieve a test from the test database.
	#
	def GetTest (self, test_id):
		#
		# Determine if this is a feature test.  If so, massage the pathname
		# a little bit.
		#
		(headlabel, label) = self.SplitLabelLeft (test_id)
		if (headlabel == 'Feature'):
			(featuretypelabel, testlabel) = self.SplitLabelLeft (label)
			testlabel = headlabel + '.' + testlabel
		else:
			testlabel = test_id

		#
		# Try to figure out whether this test exists or not.
		#
		exts=['ll', 'llx', 'c', 'cpp', 'td', 'c.tr', 'cpp.tr']

		testpath = self.dbpath + '/' + self.LabelToPath (testlabel)
		for ext in exts:
			if (os.path.exists (testpath + '.' + ext)):
				break
		else:
			raise qm.test.database.NoSuchTestError(self)

		#
		# Construct the pathname of the test relative to the LLVM source tree.
		#
		testpath = 'test/' + self.LabelToPath (testlabel) + '.' + ext

		#
		# If the test is a feature test, create the appropraite test for it.
		#
		if (headlabel == 'Feature'):
			if (featuretypelabel == 'opt'):
				return qm.test.database.TestDescriptor(self, test_id, 'llvm.TestOptimizer', {'srcfile':testpath})
			if (featuretypelabel == 'mc'):
				return qm.test.database.TestDescriptor(self, test_id, 'llvm.MachineCodeTest', {'srcfile':testpath})
			if (featuretypelabel == 'cc'):
				return qm.test.database.TestDescriptor(self, test_id, 'llvm.ConvertToCTest', {'srcfile':testpath})
			if (featuretypelabel == 'ad'):
				return qm.test.database.TestDescriptor(self, test_id, 'llvm.TestAsmDisasm', {'srcfile':testpath})

		#
		# If the file ends in .llx or .tr, then it is a TestRunner (as opposed
		# to HomeStar Runner) test.
		#
		if ((ext == 'llx') or (ext == 'c.tr') or (ext == 'cpp.tr')):
			return qm.test.database.TestDescriptor(self, test_id, 'llvm.TestRunner', {'srcfile':testpath})

		#
		# Get the first part of the test name.
		#
		(firstsuite,suite) = self.SplitLabelLeft (test_id)
		if (firstsuite == 'Regression'):
			(testtype, suite) = self.SplitLabelLeft (suite)
			testtype = RegressionMap[testtype]
			testargs = {'srcfile':testpath}

		return qm.test.database.TestDescriptor(self, test_id, testtype, testargs)

	#
	# Method: GetDirsAndFiles
	#
	# Description:
	#	This method will take a given directory and find all of the test
	#	directories and tests inside of it.
	#
	# Inputs:
	#	dirpath - The pathname to the directory.
	#
	# Return value:
	#	(dirs, files)
	#		dirs  = A list of directories inside this directory.
	#		files = A list of files within this directory.
	#
	def GetDirsAndFiles (self, dirpath):

		# The list of feature directories
		featuredirs = ['opt', 'mc', 'cc', 'ad']

		#
		# To perform magic on the Feature tests, adjust the pathname.
		#
		if (os.path.basename (dirpath) == 'Feature'):
			return (featuredirs, [])

		for x in featuredirs:
			if (x == os.path.basename (dirpath)):
				dirpath = os.path.dirname (dirpath)

		#
		# Get a list of the tests located in this directory.
		#
		tests=os.listdir (dirpath)

		#
		# Record names of invalid directories and files.
		#
		invalid_dirs = ['CVS', 'QMTest', 'QMTestDB', 'Scripts', 'Programs',
		                'Fragments', 'Reoptimizer']

		invalid_files = ['Makefile', 'README.txt', '.cvsignore', 'opaquetypes.ll']

		#
		# Start with an empty list of files and directories.
		#
		dirs = []
		files = []

		#
		# Process each file inside the directory.
		#
		for path in tests:
			#
			# Determine the file's type.
			#
			fileinfo = os.stat (dirpath + '/' + path)

			#
			# If the file is a directory, add it to the directory list, unless
			# it is one of the ignored directories.
			#
			if (stat.S_ISDIR(fileinfo.st_mode)):
				for x in invalid_dirs:
					if (x == path):
						break
				else:
					dirs = dirs + [path]
			else:
				for x in invalid_files:
					if (x == path):
						break
				else:
					files = files + [path]

		return (dirs, files)

	#
	# Method: GetSuite ()
	#
	# Description:
	#	Retrieve a test suite from the database.
	#
	def GetSuite (self, suite_id):
		#
		# Determine what we should prepend to every suite and test ID we
		# return.
		#
		if (suite_id == ''):
			suite_prefix = ''
		else:
			suite_prefix = suite_id + '.'

		#
		# Convert the suite name into a pathname.
		#
		suitepath = self.dbpath + '/' + self.LabelToPath (suite_id)

		#
		# Get a list of the tests located in this directory.
		#
		(dirs, files) = self.GetDirsAndFiles (suitepath)

		#
		# Convert the list of directories and files into labels.
		#
		dirlabels  = []
		filelabels = []
		for path in dirs:
			dirlabels = dirlabels + [suite_prefix + path]

		if (suite_id != ''):
			for path in files:
				(filebase, fileext) = os.path.splitext(path)
				if (fileext == '.tr'):
					(filebase, fileext) = os.path.splitext(filebase)
				filelabels = filelabels + [suite_prefix + filebase]

		#
		# Load this suite
		#
		suite = qm.test.suite.Suite (self, suite_id, 0, filelabels, dirlabels)
		return suite
		#raise qm.test.database.NoSuchSuiteError (suite_id)

	def GetAttachmentStore (self):
		return qm.attachment.FileAttachmentStore (self)

	def GetSubdirectories (self, pathlabel):
		#
		# Convert the directory label into a full pathname.
		#
		pathname = self.dbpath + '/' + self.LabelToPath (pathlabel)

		#
		# Retrieve all of the directories within this directory.
		#
		(dirs, files) = self.GetDirsAndFiles (pathname)
		return dirs

	def GetResourceIds(self, directory="", scan_subdirs=1):
		return []

	def GetSuiteIds(self, directory="", scan_subdirs=1):
		#
		# Convert the directory label into a full pathname.
		#
		if (directory == ''):
			dirpath = self.dbpath
		else:
			dirpath = self.dbpath + '/' + self.LabelToPath (directory)

		#
		# Get a list of the tests located in this directory.
		#
		(dirs, files) = self.GetDirsAndFiles (dirpath)

		#
		# Add the top suite to the list of suites in this directory, and then
		# convert the rest of the directories into suite ID labels.
		#
		dirlabels = [directory]

		for path in dirs:
			if (directory == ''):
				dirlabels = dirlabels + [path]
			else:
				dirlabels = dirlabels + [directory + '.' + path]

		#
		# If we're asked to scan subdirectories, recurse on ourselves.
		#
		if (scan_subdirs == 1):
			for label in dirlabels:
				dirlabels = dirlabels + self.GetTestIds (label, 1)

		return dirlabels

	def GetTestIds(self, directory="", scan_subdirs=1):
		#
		# Convert the directory label into a directory name.
		#
		if (directory == ''):
			dirpath = self.dbpath
		else:
			dirpath = self.dbpath + '/' + self.LabelToPath (directory)

		#
		# Get a list of the tests located in this directory.
		#
		(dirs, files) = self.GetDirsAndFiles (dirpath)

		#
		# Convert all of the directories into labels.
		#
		dirlabels = []
		for path in dirs:
			if (directory == ''):
				dirlabels = dirlabels + [path]
			else:
				dirlabels = dirlabels + [directory + '.' + path]


		#
		# Convert all of the file names into labels.
		#
		filelabels = []
		for path in files:
			if (directory == ''):
				continue
			else:
				(filebase, fileext) = os.path.splitext(path)
				if (fileext == '.tr'):
					(filebase, fileext) = os.path.splitext(filebase)
				filelabels = filelabels + [directory + '.' + filebase]

		#
		# Recurse through subdirectories using recursion if necessary.
		#
		if (scan_subdirs == 1):
			for label in dirlabels:
				filelabels = filelabels + self.GetTestIds (label, 1)

		return filelabels

