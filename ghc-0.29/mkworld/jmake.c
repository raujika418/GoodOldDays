/*****************************************************************************\
 BASED ON:
	$XConsortium: imake.c,v 1.65 91/07/25 17:50:17 rws Exp $
 *                                                                           *
 *                                Porting Note                               *
 *                                                                           *
 * Add the value of BOOTSTRAPCFLAGS to the cpp_argv table (in jmakemdep.h)   *
 * so that it will be passed to the template file.			     *
 *                                                                           *
\*****************************************************************************/

/*
 * 
 * Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology
 * 
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in
 * advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 * M.I.T. makes no representations about the suitability of
 * this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 * 
 * Original Author:
 *	Todd Brunhoff
 *	Tektronix, inc.
 *	While a guest engineer at Project Athena, MIT
 *
 * jmake: the include-make program.
 *
 * Usage: jmake [-Idir] [-Ddefine] [-T] [-f jmakefile ] [-s] [-e] [-v] [make flags]
 *
 * Jmake takes a template makefile (TEMPLATE.jm) and runs cpp on it
 * producing a temporary makefile in /tmp.  It then runs make on
 * this pre-processed makefile.
 * Options:
 *		-D	define.  Same as cpp -D argument.
 *		-I	Include directory.  Same as cpp -I argument.
 *		-T	template.  Designate a template other
 * 			than TEMPLATE.jm
 *		-s[F]	show.  Show the produced makefile on the standard
 *			output.  Make is not run is this case.  If a file
 *			argument is provided, the output is placed there.
 *              -e[F]   execute instead of show; optionally name Makefile F
 *		-v	verbose.  Show the make command line executed.
 *
 * Environment variables:
 *		
 *		JMAKEINCLUDE	Include directory to use in addition to "."
 *		JMAKECPP	Cpp to use instead of /lib/cpp
 *		JMAKEMAKE	make program to use other than what is
 *				found by searching the $PATH variable.
 * Other features:
 *	jmake reads the entire cpp output into memory and then scans it
 *	for occurences of "@@".  If it encounters them, it replaces it with
 *	a newline.  It also trims any trailing white space on output lines
 *	(because make gets upset at them).  This helps when cpp expands
 *	multi-line macros but you want them to appear on multiple lines.
 *
 *	The macros MAKEFILE and MAKE are provided as macros
 *	to make.  MAKEFILE is set to jmake's makefile (not the constructed,
 *	preprocessed one) and MAKE is set to argv[0], i.e. the name of
 *	the jmake program.
 *
 * Theory of operation:
 *   1. Determine the name of the jmakefile from the command line (-f)
 *	or from the content of the current directory (Jmakefile or jmakefile).
 *	Call this <jmakefile>.  This gets added to the arguments for
 *	make as MAKEFILE=<jmakefile>.
 *   2. Determine the name of the template from the command line (-T)
 *	or the default, TEMPLATE.jm.  Call this <template>
 *   3. Start up cpp an provide it with three lines of input:
 *		#define JMAKE_TEMPLATE		" <template> "
 *		#define INCLUDE_JMAKEFILE	< <jmakefile> >
 *		#include JMAKE_TEMPLATE
 *	Note that the define for INCLUDE_JMAKEFILE is intended for
 *	use in the template file.  This implies that the jmake is
 *	useless unless the template file contains at least the line
 *		#include INCLUDE_JMAKEFILE
 *   4. Gather the output from cpp, and clean it up, expanding @@ to
 *	newlines, stripping trailing white space, cpp control lines,
 *	and extra blank lines.  This cleaned output is placed in a
 *	temporary file.  Call this <makefile>.
 *   5. Start up make specifying <makefile> as its input.
 *
 * The design of the template makefile should therefore be:
 *	<set global macros like CFLAGS, etc.>
 *	<include machine dependent additions>
 *	#include INCLUDE_JMAKEFILE
 *	<add any global targets like 'clean' and long dependencies>
 */

#ifdef __STDC__
#define PROTO(x)	x
#define STG_NO_ARGS	void
#else
#define PROTO(x)	()
#define STG_NO_ARGS	/* no such thing either */
#endif /* ! __STDC__ */

#include <stdio.h>
#if (defined(SVR4) || defined(_IBMR2) || defined(SYSV386)) && __STDC__
FILE * fdopen();
#endif
#include <ctype.h>
#include "Xosdefs.h"
#ifndef X_NOT_POSIX
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#endif
#include <sys/types.h>
#include <fcntl.h>
#ifdef X_NOT_POSIX
#include <sys/file.h>
#else
#include <unistd.h>
#endif
#if defined(X_NOT_POSIX) || defined(_POSIX_SOURCE)
#include <signal.h>
#else
#define _POSIX_SOURCE
#include <signal.h>
#undef _POSIX_SOURCE
#endif
#include <sys/stat.h>
#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <sys/wait.h>
#else
#define _POSIX_SOURCE
#include <sys/wait.h>
#undef _POSIX_SOURCE
#endif
#define waitCode(w)	WEXITSTATUS(w)
#define waitSig(w)	WTERMSIG(w)
typedef int		waitType;
#else /* X_NOT_POSIX */
#ifdef SYSV
#define waitCode(w)	(((w) >> 8) & 0x7f)
#define waitSig(w)	((w) & 0xff)
typedef int		waitType;
#else /* SYSV */
#include <sys/wait.h>
#define waitCode(w)	((w).w_T.w_Retcode)
#define waitSig(w)	((w).w_T.w_Termsig)
typedef union wait	waitType;
#endif
#ifndef WIFSIGNALED
#define WIFSIGNALED(w) waitSig(w)
#endif
#ifndef WIFEXITED
#define WIFEXITED(w) waitCode(w)
#endif
#endif /* X_NOT_POSIX */
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc(), *realloc();
void exit();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc();
#endif /* macII */
#ifdef X_NOT_STDC_ENV
extern char	*getenv();
#endif
#include <errno.h>
extern int	errno;

/* This "configure" thingy will tell us where the GNU CPP is.
   Must be included *before* jmakemdep.h.
*/
#include "config.h"

#include "jmakemdep.h"

#ifndef DontDeclareSysErrlist
extern char	*sys_errlist[];
#endif

#define	TRUE		1
#define	FALSE		0

#ifdef FIXUP_CPP_WHITESPACE
int	InRule = FALSE;
#endif

/*
 * Some versions of cpp reduce all tabs in macro expansion to a single
 * space.  In addition, the escaped newline may be replaced with a
 * space instead of being deleted.  Blech.
 */
#ifndef FIXUP_CPP_WHITESPACE
#define KludgeOutputLine(arg)
#define KludgeResetRule()

#else
void KludgeOutputLine PROTO((char **));
void KludgeResetRule PROTO((STG_NO_ARGS));
#endif

typedef	unsigned char	boolean;

#ifdef GnuCppCmd
#undef DEFAULT_CPP
#define DEFAULT_CPP GnuCppCmd
#else /* the hard way */
#ifndef DEFAULT_CPP
#ifdef USE_CC_E
#define DEFAULT_CPP "/bin/cc"
#else
#ifdef CPP_PROGRAM
#define DEFAULT_CPP CPP_PROGRAM
#else
#define DEFAULT_CPP "/lib/cpp"
#endif
#endif
#endif
#endif

char *cpp = DEFAULT_CPP;

char	*tmpMakefile    = "/tmp/Imf.XXXXXX";
char	*tmpJmakefile    = "/tmp/IIf.XXXXXX";
char	*make_argv[ ARGUMENTS ] = { "make" };

int	make_argindex;
int	cpp_argindex;
char	*make = NULL;
char	*Jmakefile = NULL;
char	*Makefile = "Makefile";
char	*Template = "TEMPLATE.jm";
/* partain: SetupLabel and ProjectLabel are my additions */
char	*SetupLabel  = "std";
char	*ProjectLabel = "none";
char	*ProjectMkworldDir = "";
/* partain: net patch from dubois; orig:
char	*program;
*/
char	*program = "jmake";

/* forward declarations */
char	*FindJmakefile PROTO((char *));
char	*ReadLine PROTO((FILE *, char *));
char	*CleanCppInput PROTO((char *));
char	*Strdup PROTO((register char *));
char	*Emalloc PROTO((int));

void init	PROTO((STG_NO_ARGS));
void AddCppArg PROTO((char *));
void SetOpts PROTO((int, char **));
void AddMakeArg PROTO((char *));
void LogFatalI PROTO((char *, int));
void LogFatal PROTO((char *, char *));
void cppit PROTO((char *, char *, FILE *, char *));
void showit PROTO((	FILE *));
void makeit PROTO((STG_NO_ARGS));
void wrapup PROTO((STG_NO_ARGS));
void CleanCppOutput PROTO((FILE *, char *));
boolean isempty PROTO((char *));

boolean	verbose = FALSE;
boolean	show = TRUE;

main(argc, argv)
	int	argc;
	char	**argv;
{
	FILE	*tmpfd;
	char	makeMacro[ BUFSIZ ];
	char	makefileMacro[ BUFSIZ ];

	program = argv[0];
	init();
	SetOpts(argc, argv);
#ifdef USE_CC_E
	AddCppArg("-");
#endif

	Jmakefile = FindJmakefile(Jmakefile);
	if (Makefile)
		tmpMakefile = Makefile;
	else {
		tmpMakefile = Strdup(tmpMakefile);
		(void) mktemp(tmpMakefile);
	}
	AddMakeArg("-f");
	AddMakeArg( tmpMakefile );
	sprintf(makeMacro, "MAKE=%s", program);
	AddMakeArg( makeMacro );
	sprintf(makefileMacro, "MAKEFILE=%s", Jmakefile);
	AddMakeArg( makefileMacro );

	if ((tmpfd = fopen(tmpMakefile, "w+")) == NULL)
		LogFatal("Cannot create temporary file %s.", tmpMakefile);

	cppit(Jmakefile, Template, tmpfd, tmpMakefile);

	if (show) {
		if (Makefile == NULL)
			showit(tmpfd);
	} else
		makeit();
	wrapup();
	exit(0);
}

void
showit(fd)
	FILE	*fd;
{
	char	buf[ BUFSIZ ];
	int	red;

	fseek(fd, 0, 0);
	while ((red = fread(buf, 1, BUFSIZ, fd)) > 0)
		fwrite(buf, red, 1, stdout);
	if (red < 0)
		LogFatal("Cannot write stdout.", "");
}

void
wrapup()
{
	if (tmpMakefile != Makefile)
		unlink(tmpMakefile);
	unlink(tmpJmakefile);
}

#ifdef SIGNALRETURNSINT
int
#else
void
#endif
catch(sig)
	int	sig;
{
	errno = 0;
	LogFatalI("Signal %d.", sig);
}

/*
 * Initialize some variables.
 */
void
init()
{
	char	*p;

	make_argindex=0;
	while (make_argv[ make_argindex ] != NULL)
		make_argindex++;
	cpp_argindex = 0;
	while (cpp_argv[ cpp_argindex ] != NULL)
		cpp_argindex++;

	/*
	 * See if the standard include directory is different than
	 * the default.  Or if cpp is not the default.  Or if the make
	 * found by the PATH variable is not the default.
	 */
	if (p = getenv("JMAKEINCLUDE")) {
		if (*p != '-' || *(p+1) != 'I')
			LogFatal("Environment var JMAKEINCLUDE %s\n",
				"must begin with -I");
		AddCppArg(p);
		for (; *p; p++)
			if (*p == ' ') {
				*p++ = '\0';
				AddCppArg(p);
			}
	}
	if (p = getenv("JMAKECPP"))
		cpp = p;
	if (p = getenv("JMAKEMAKE"))
		make = p;

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, catch);
}

void
AddMakeArg(arg)
	char	*arg;
{
	errno = 0;
	if (make_argindex >= ARGUMENTS-1)
		LogFatal("Out of internal storage.", "");
	make_argv[ make_argindex++ ] = arg;
	make_argv[ make_argindex ] = NULL;
}

void
AddCppArg(arg)
	char	*arg;
{
	errno = 0;
	if (cpp_argindex >= ARGUMENTS-1)
		LogFatal("Out of internal storage.", "");
	cpp_argv[ cpp_argindex++ ] = arg;
	cpp_argv[ cpp_argindex ] = NULL;
}

void
SetOpts(argc, argv)
	int	argc;
	char	**argv;
{
	errno = 0;
	/*
	 * Now gather the arguments for make
	 */
	for(argc--, argv++; argc; argc--, argv++) {
	    /*
	     * We intercept these flags.
	     */
	    if (argv[0][0] == '-') {
		if (argv[0][1] == 'D') {
		    AddCppArg(argv[0]);
		} else if (argv[0][1] == 'I') {
		    AddCppArg(argv[0]);
		} else if (argv[0][1] == 'f') {
		    if (argv[0][2])
			Jmakefile = argv[0]+2;
		    else {
			argc--, argv++;
			if (! argc)
			    LogFatal("No description arg after -f flag\n", "");
			Jmakefile = argv[0];
		    }
		} else if (argv[0][1] == 's') {
		    if (argv[0][2])
			Makefile = ((argv[0][2] == '-') && !argv[0][3]) ?
			    NULL : argv[0]+2;
		    else {
			argc--, argv++;
			if (!argc)
			    LogFatal("No description arg after -s flag\n", "");
			Makefile = ((argv[0][0] == '-') && !argv[0][1]) ?
			    NULL : argv[0];
		    }
		    show = TRUE;
		} else if (argv[0][1] == 'e') {
		   Makefile = (argv[0][2] ? argv[0]+2 : NULL);
		   show = FALSE;
		} else if (argv[0][1] == 'T') {
		    if (argv[0][2])
			Template = argv[0]+2;
		    else {
			argc--, argv++;
			if (! argc)
			    LogFatal("No description arg after -T flag\n", "");
			Template = argv[0];
		    }
/* partain added -P <project>, -S <setup>, and -C <dir> (project mkworld dir) options */
		} else if (argv[0][1] == 'P') {
		    if (argv[0][2])
			ProjectLabel = argv[0]+2;
		    else {
			argc--, argv++;
			if (! argc)
			    LogFatal("No description arg after -P flag\n", "");
			ProjectLabel = argv[0];
		    }
		} else if (argv[0][1] == 'S') {
		    if (argv[0][2])
			SetupLabel = argv[0]+2;
		    else {
			argc--, argv++;
			if (! argc)
			    LogFatal("No description arg after -S flag\n", "");
			SetupLabel = argv[0];
		    }
		} else if (argv[0][1] == 'C') {
		    if (argv[0][2])
			ProjectMkworldDir = argv[0]+2;
		    else {
			argc--, argv++;
			if (! argc)
			    LogFatal("No description arg after -C flag\n", "");
			ProjectMkworldDir = argv[0];
		    }
		    { char *temp = Emalloc(4096); /* hack */
		      strcpy(temp, "-I");
		      strcat(temp, ProjectMkworldDir);
		      AddCppArg(temp);
		    }
/* end of addition */
		} else if (argv[0][1] == 'v') {
		    verbose = TRUE;
		} else
		    AddMakeArg(argv[0]);
	    } else
		AddMakeArg(argv[0]);
	}
}

char *
FindJmakefile(Jmakefile)
	char	*Jmakefile;
{
	int	fd;

	if (Jmakefile) {
		if ((fd = open(Jmakefile, O_RDONLY)) < 0)
			LogFatal("Cannot open %s.", Jmakefile);
	} else {
		if ((fd = open("Jmakefile", O_RDONLY)) < 0)
			LogFatal("No description file (Jmakefile).", "");
		else
			Jmakefile = "Jmakefile";
	}
	close (fd);
	return(Jmakefile);
}

void
LogFatalI(s, i)
	char *s;
	int i;
{
	/*NOSTRICT*/
	LogFatal(s, (char *)i);
}

void
LogFatal(x0,x1)
	char *x0, *x1;
{
	static boolean	entered = FALSE;

	if (entered)
		return;
	entered = TRUE;

	fprintf(stderr, "%s: ", program);
	if (errno)
		fprintf(stderr, "%s: ", sys_errlist[ errno ]);
	fprintf(stderr, x0,x1);
	fprintf(stderr, "  Stop.\n");
/*partain sleep(30); */
	wrapup();
	exit(1);
}

void
showargs(argv)
	char	**argv;
{
	for (; *argv; argv++)
		fprintf(stderr, "%s ", *argv);
	fprintf(stderr, "\n");
}

void
cppit(Jmakefile, template, outfd, outfname)
	char	*Jmakefile;
	char	*template;
	FILE	*outfd;
	char	*outfname;
{
	FILE	*pipeFile;
	int	pid, pipefd[2];
	waitType	status;
	char	*cleanedJmakefile;

	/*
	 * Get a pipe.
	 */
	if (pipe(pipefd) < 0)
		LogFatal("Cannot make a pipe.", "");

	/*
	 * Fork and exec cpp
	 */
	pid = fork();
	if (pid < 0)
		LogFatal("Cannot fork.", "");
	if (pid) {	/* parent */
		close(pipefd[0]);
		cleanedJmakefile = CleanCppInput(Jmakefile);
		if ((pipeFile = fdopen(pipefd[1], "w")) == NULL)
			LogFatalI("Cannot fdopen fd %d for output.", pipefd[1]);

		fprintf(pipeFile, "#define JMAKE_TEMPLATE\t\"%s\"\n",
			template);
		fprintf(pipeFile, "#define INCLUDE_JMAKEFILE\t<%s>\n",
			cleanedJmakefile);

/* partain added OVERRIDE_FILEs business */
		fprintf(pipeFile, "#define SetupLabel\t%s\n",  SetupLabel);
		fprintf(pipeFile, "#define SetupIsStd\t%d\n",  (! strcmp("std", SetupLabel)));
		fprintf(pipeFile, "#define ProjectLabel\t%s\n", ProjectLabel);
		fprintf(pipeFile, "#define ProjectIsNone\t%d\n", (! strcmp("none", ProjectLabel)));
		if (strcmp("", ProjectMkworldDir) != 0) {
		    fprintf(pipeFile, "#define HaveProjectMkworldDir\t1\n");
		    fprintf(pipeFile, "#define ProjectMkworldDir\t%s\n", ProjectMkworldDir);
		}

		fprintf(pipeFile, "#define SITE_SETUP_FILE\t\"site-%s-%s.jm\"\n",
			ProjectLabel, SetupLabel);
		fprintf(pipeFile, "#define SITE_PROJECT_FILE\t\"site-%s.jm\"\n",
			ProjectLabel);

		fprintf(pipeFile, "#define ONLY4_SETUP_FILE\t\"only4-%s-%s.jm\"\n",
			ProjectLabel, SetupLabel);
		fprintf(pipeFile, "#define ONLY4_PROJECT_FILE\t\"only4-%s.jm\"\n",
			ProjectLabel);

		fprintf(pipeFile, "#define SUFFIXES_SETUP_FILE\t\"suffixes-%s-%s.jm\"\n",
			ProjectLabel, SetupLabel);
		fprintf(pipeFile, "#define SUFFIXES_PROJECT_FILE\t\"suffixes-%s.jm\"\n",
			ProjectLabel);

		fprintf(pipeFile, "#define MACROS_SETUP_FILE\t\"macros-%s-%s.jm\"\n",
			ProjectLabel, SetupLabel);
		fprintf(pipeFile, "#define MACROS_PROJECT_FILE\t\"macros-%s.jm\"\n",
			ProjectLabel);

		fprintf(pipeFile, "#define UTILS_SETUP_FILE\t\"utils-%s-%s.jm\"\n",
			ProjectLabel, SetupLabel);
		fprintf(pipeFile, "#define UTILS_PROJECT_FILE\t\"utils-%s.jm\"\n",
			ProjectLabel);

		fprintf(pipeFile, "#define INSTALLATION_SETUP_FILE\t\"install-%s-%s.jm\"\n",
			ProjectLabel, SetupLabel);
		fprintf(pipeFile, "#define INSTALLATION_PROJECT_FILE\t\"install-%s.jm\"\n",
			ProjectLabel);
/* end of addition */

		fprintf(pipeFile, "#include JMAKE_TEMPLATE\n");
		fclose(pipeFile);
		while (wait(&status) > 0) {
			errno = 0;
			if (WIFSIGNALED(status))
				LogFatalI("Signal %d.", waitSig(status));
			if (WIFEXITED(status) && waitCode(status))
				LogFatalI("Exit code %d.", waitCode(status));
		}
#ifdef linux
		freopen(outfname, "r+", outfd);
#endif
		CleanCppOutput(outfd, outfname);
	} else {	/* child... dup and exec cpp */
		if (verbose)
			showargs(cpp_argv);
		dup2(pipefd[0], 0);
		dup2(fileno(outfd), 1);
		close(pipefd[1]);
		execv(cpp, cpp_argv);
		LogFatal("Cannot exec %s.", cpp);
	}
}

void
makeit()
{
	int	pid;
	waitType	status;

	/*
	 * Fork and exec make
	 */
	pid = fork();
	if (pid < 0)
		LogFatal("Cannot fork.", "");
	if (pid) {	/* parent... simply wait */
		while (wait(&status) > 0) {
			errno = 0;
			if (WIFSIGNALED(status))
				LogFatalI("Signal %d.", waitSig(status));
			if (WIFEXITED(status) && waitCode(status))
				LogFatalI("Exit code %d.", waitCode(status));
		}
	} else {	/* child... dup and exec cpp */
		if (verbose)
			showargs(make_argv);
		if (make)
			execv(make, make_argv);
		else
			execvp("make", make_argv);
		LogFatal("Cannot exec %s.", make);
	}
}

char *
CleanCppInput(Jmakefile)
	char	*Jmakefile;
{
	FILE	*outFile = NULL;
	int	infd;
	char	*buf,		/* buffer for file content */
		*pbuf,		/* walking pointer to buf */
		*punwritten,	/* pointer to unwritten portion of buf */
		*cleanedJmakefile = Jmakefile,	/* return value */
		*ptoken,	/* pointer to # token */
		*pend,		/* pointer to end of # token */
		savec;		/* temporary character holder */
	struct stat	st;

	/*
	 * grab the entire file.
	 */
	if ((infd = open(Jmakefile, O_RDONLY)) < 0)
		LogFatal("Cannot open %s for input.", Jmakefile);
	fstat(infd, &st);
	buf = Emalloc(st.st_size+1);
	if (read(infd, buf, st.st_size) != st.st_size)
		LogFatal("Cannot read all of %s:", Jmakefile);
	close(infd);
	buf[ st.st_size ] = '\0';

	punwritten = pbuf = buf;
	while (*pbuf) {
	    /* pad make comments for cpp */
	    if (*pbuf == '#' && (pbuf == buf || pbuf[-1] == '\n')) {

		ptoken = pbuf+1;
		while (*ptoken == ' ' || *ptoken == '\t')
			ptoken++;
		pend = ptoken;
		while (*pend && *pend != ' ' && *pend != '\t' && *pend != '\n')
			pend++;
		savec = *pend;
		*pend = '\0';
		if (strcmp(ptoken, "include")
		 && strcmp(ptoken, "define")
		 && strcmp(ptoken, "undef")
		 && strcmp(ptoken, "ifdef")
		 && strcmp(ptoken, "ifndef")
		 && strcmp(ptoken, "else")
		 && strcmp(ptoken, "endif")
		 && strcmp(ptoken, "if")) {
		    if (outFile == NULL) {
			tmpJmakefile = Strdup(tmpJmakefile);
			(void) mktemp(tmpJmakefile);
			cleanedJmakefile = tmpJmakefile;
			outFile = fopen(tmpJmakefile, "w");
			if (outFile == NULL)
			    LogFatal("Cannot open %s for write.\n",
				tmpJmakefile);
		    }
		    fwrite(punwritten, sizeof(char), pbuf-punwritten, outFile);
		    fputs("/**/", outFile);
		    punwritten = pbuf;
		}
		*pend = savec;
	    }
	    pbuf++;
	}
	if (outFile) {
	    fwrite(punwritten, sizeof(char), pbuf-punwritten, outFile);
	    fclose(outFile); /* also closes the pipe */
	}

	return(cleanedJmakefile);
}

void
CleanCppOutput(tmpfd, tmpfname)
	FILE	*tmpfd;
	char	*tmpfname;
{
	char	*input;
	int	blankline = 0;

	while(input = ReadLine(tmpfd, tmpfname)) {
		if (isempty(input)) {
			if (blankline++)
				continue;
			KludgeResetRule();
		} else {
			blankline = 0;
			KludgeOutputLine(&input);
			fputs(input, tmpfd);
		}
		putc('\n', tmpfd);
	}
	fflush(tmpfd);
#ifdef NFS_STDOUT_BUG
	/*
	 * On some systems, NFS seems to leave a large number of nulls at
	 * the end of the file.  Ralph Swick says that this kludge makes the
	 * problem go away.
	 */
	ftruncate (fileno(tmpfd), (off_t)ftell(tmpfd));
#endif
}

/*
 * Determine of a line has nothing in it.  As a side effect, we trim white
 * space from the end of the line.  Cpp magic cookies are also thrown away.
 */
boolean
isempty(line)
	char	*line;
{
	char	*pend;

	/*
	 * Check for lines of the form
	 *	# n "...
	 * or
	 *	# line n "...
	 */
	if (*line == '#') {
		pend = line+1;
		if (*pend == ' ')
			pend++;
		if (strncmp(pend, "line ", 5) == 0)
			pend += 5;
		if (isdigit(*pend)) {
			while (isdigit(*pend))
				pend++;
			if (*pend++ == ' ' && *pend == '"')
				return(TRUE);
		}
	}

	/*
	 * Find the end of the line and then walk back.
	 */
	for (pend=line; *pend; pend++) ;

	pend--;
	while (pend >= line && (*pend == ' ' || *pend == '\t'))
		pend--;
	*++pend = '\0';
	return (*line == '\0');
}

/*ARGSUSED*/
char *
ReadLine(tmpfd, tmpfname)
	FILE	*tmpfd;
	char	*tmpfname;
{
	static boolean	initialized = FALSE;
	static char	*buf, *pline, *end;
	char	*p1, *p2;

	if (! initialized) {
		int	total_red;
		struct stat	st;

		/*
		 * Slurp it all up.
		 */
		fseek(tmpfd, 0, 0);
		fstat(fileno(tmpfd), &st);
		pline = buf = Emalloc(st.st_size+1);
#ifdef linux
		total_red = fread(buf, 1, st.st_size, tmpfd);
#else
		total_red = read(fileno(tmpfd), buf, st.st_size);
#endif
		if (total_red != st.st_size)
			LogFatal("cannot read %s\n", tmpMakefile);
		end = buf + st.st_size;
		*end = '\0';
#ifdef linux
		fseek(tmpfd, 0, 0);
#else
		lseek(fileno(tmpfd), 0, 0);
#endif
#ifdef SYSV
		freopen(tmpfname, "w+", tmpfd);
#else	/* !SYSV */
		ftruncate(fileno(tmpfd), (off_t)0);
#endif	/* !SYSV */
		initialized = TRUE;
	    fprintf (tmpfd, "# Makefile generated by jmake -- DO NOT EDIT!!!\n");
	    fprintf (tmpfd, "# (edit the Jmakefile instead)\n");
/* partain: no thanks
	    fprintf (tmpfd, "# %s\n",
		"$XConsortium: jmake.c,v 1.51 89/12/12 12:37:30 jim Exp $");
*/
#ifdef FIXUP_CPP_WHITESPACE
	    {
		static char *cpp_warning[] = {
"#",
"# The cpp used on this machine replaces all newlines and multiple tabs and",
"# spaces in a macro expansion with a single space.  Jmake tries to compensate",
"# for this, but is not always successful.",
"#",
NULL };
		char **cpp;

		for (cpp = cpp_warning; *cpp; cpp++) {
		    fprintf (tmpfd, "%s\n", *cpp);
		}
	    }
#endif /* FIXUP_CPP_WHITESPACE */
	}

	for (p1 = pline; p1 < end; p1++) {
		if (*p1 == '@' && *(p1+1) == '@') { /* soft EOL */
			*p1++ = '\0';
			p1++; /* skip over second @ */
			break;
		}
		else if (*p1 == '\n') { /* real EOL */
			*p1++ = '\0';
			break;
		}
	}

	/*
	 * return NULL at the end of the file.
	 */
	p2 = (pline == p1 ? NULL : pline);
	pline = p1;
	return(p2);
}

void
writetmpfile(fd, buf, cnt)
	FILE	*fd;
	int	cnt;
	char	*buf;
{
	errno = 0;
	if (fwrite(buf, cnt, 1, fd) != 1)
		LogFatal("Cannot write to %s.", tmpMakefile);
}

char *
Emalloc(size)
	int	size;
{
	char	*p;

	if ((p = malloc(size)) == NULL)
		LogFatalI("Cannot allocate %d bytes\n", size);
	return(p);
}

#ifdef FIXUP_CPP_WHITESPACE
void
KludgeOutputLine(pline)
	char	**pline;
{
	char	*p = *pline;

	switch (*p) {
	    case '#':	/*Comment - ignore*/
		break;
	    case '\t':	/*Already tabbed - ignore it*/
	    	break;
	    case ' ':	/*May need a tab*/
	    default:
		for (; *p; p++) if (p[0] == ':' && 
				    p > *pline && p[-1] != '\\') {
		    if (**pline == ' ')
			(*pline)++;
		    InRule = TRUE;
		    break;
		}
		if (InRule && **pline == ' ')
		    **pline = '\t';
		break;
	}
}

void
KludgeResetRule()
{
	InRule = FALSE;
}
#endif /* FIXUP_CPP_WHITESPACE */

char *
Strdup(cp)
	register char *cp;
{
	register char *new = Emalloc(strlen(cp) + 1);

	strcpy(new, cp);
	return new;
}
