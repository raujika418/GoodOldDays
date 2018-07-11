/*
 * Based on:
 * $XConsortium: imakemdep.h,v 1.38 91/08/25 11:36:39 rws Exp $
 * 
 * This file contains machine-dependent constants for the jmake utility.  When
 * porting jmake, read each of the steps below and add in any necessary
 * definitions.  Do *not* edit ccjmake.c or jmake.c!
 */

#ifdef CCJMAKE
/*
 * Step 1:  jmake_ccflags
 *     Define any special flags that will be needed to get jmake.c to compile.
 *     These will be passed to the compile along with the contents of the
 *     make variable BOOTSTRAPCFLAGS.
 */
#ifdef hpux
#ifdef hp9000s800
#define jmake_ccflags "-DSYSV"
#else
#define jmake_ccflags "-Wc,-Nd4000,-Ns3000 -DSYSV"
#endif
#endif

#if defined(macII) || defined(_AUX_SOURCE)
#define jmake_ccflags "-DmacII -DSYSV"
#endif

#ifdef stellar
#define jmake_ccflags "-DSYSV"
#endif

#ifdef att
#define jmake_ccflags "-Xc -DSVR4"
#endif

#ifdef sony
#ifdef SYSTYPE_SYSV
#define jmake_ccflags "-DSVR4"
#else
#include <sys/param.h>
#if NEWSOS < 41
#define jmake_ccflags "-Dbsd43 -DNOSTDHDRS"
#else
#define jmake_ccflags "-Dbsd43"
#endif
#endif
#endif

#ifdef CRAY
/* -DX_NOT_STDC_ENV is just so we can bootstrap with cc instead of scc */
#define jmake_ccflags "-DSYSV -DUSG -DX_NOT_STDC_ENV"
#endif

#ifdef _IBMR2
#define jmake_ccflags "-Daix -DSYSV -D_IBMR2"
#else
#ifdef aix
#define jmake_ccflags "-Daix -DSYSV"
#endif
#endif

#ifdef Mips
#  if defined(SYSTYPE_BSD) || defined(BSD) || defined(BSD43)
#    define jmake_ccflags "-DMips -DBSD43"
#  else 
#    define jmake_ccflags "-DMips -DSYSV"
#  endif
#endif 

/* OLD:
#ifdef mips
#ifdef SYSTYPE_SYSV
#define jmake_ccflags "-DSYSV -I/usr/include/bsd -lbsd"
#endif
#endif
*/

#ifdef is68k
#define jmake_ccflags "-Dluna -Duniosb"
#endif

#ifdef SYSV386
# ifdef SVR4
#  define jmake_ccflags "-Xc -DSVR4"
# else
#  define jmake_ccflags "-DSYSV"
# endif
#endif

#ifdef __convex__
#define jmake_ccflags "-fn -tm c1"
#endif

#ifdef apollo
#define jmake_ccflags "-DX_NOT_POSIX"
#endif

#else /* not CCJMAKE */
#ifndef MAKEDEPEND
/*
 * Step 2:  dup2
 *     If your OS doesn't have a dup2() system call to duplicate one file
 *     descriptor onto another, define such a mechanism here (if you don't
 *     already fall under the existing category(ies).
 */
#if defined(SYSV) && !defined(CRAY) && !defined(Mips)
#define	dup2(fd1,fd2)	((fd1 == fd2) ? fd1 : (close(fd2), \
					       fcntl(fd1, F_DUPFD, fd2)))
#endif


/*
 * Step 3:  FIXUP_CPP_WHITESPACE
 *     If your cpp collapses tabs macro expansions into a single space and
 *     replaces escaped newlines with a space, define this symbol.  This will
 *     cause jmake to attempt to patch up the generated Makefile by looking
 *     for lines that have colons in them (this is why the rules file escapes
 *     all colons).  One way to tell if you need this is to see whether or not
 *     your Makefiles have no tabs in them and lots of @@ strings.
 */
#if defined(sun)	|| defined(__sun) 	|| defined(__sun__)	\
 || defined(linux)	|| defined(__linux)	|| defined(__linux__)	\
 || defined(ultrix)	|| defined(__ultrix)	|| defined(__ultrix__)	\
 || defined(hpux)	|| defined(__hpux)	|| defined(__hpux__)	\
 || defined(alpha)	|| defined(__alpha)	|| defined(__alpha__)	\
 || defined(NeXT)	|| defined(__NeXT)	|| defined(__NeXT__)    \
 || defined(FreeBSD)	|| defined(__FreeBSD)	|| defined(__FreeBSD__)
#define FIXUP_CPP_WHITESPACE
#endif


/*
 * Step 4:  DEFAULT_CPP
 *     If the C preprocessor does not live in /lib/cpp, set this symbol to 
 *     the appropriate location.
 */
#ifdef apollo
#define DEFAULT_CPP "/usr/lib/cpp"
#endif
#if defined(_IBMR2) && !defined(DEFAULT_CPP)
#define DEFAULT_CPP "/usr/lpp/X11/Xamples/util/cpp/cpp"
#endif

/*
 * Step 5:  cpp_argv
 *     The following table contains the cpp flags that should be passed to 
 *     cpp whenever a Makefile is being generated.  If your preprocessor 
 *     doesn't predefine any unique symbols, choose one and add it to the
 *     end of this table.  Then, do the following:
 * 
 *         a.  Use this symbol at the top of Jmake.tmpl when setting MacroFile.
 *         b.  Put this symbol in the definition of BootstrapCFlags in your
 *             <platform>.cf file.
 *         c.  When doing a make World, always add "BOOTSTRAPCFLAGS=-Dsymbol" 
 *             to the end of the command line.
 * 
 *     Note that you may define more than one symbols (useful for platforms 
 *     that support multiple operating systems).
 */

#define	ARGUMENTS 50	/* number of arguments in various arrays */
char *cpp_argv[ARGUMENTS] = {
#ifdef USE_CC_E
	"cc",		/* replaced by the actual cpp program to exec */
	"-E",
#else
	"cpp",		/* replaced by the actual cpp program to exec */
#endif /* USE_CC_E */
	"-I.",		/* add current directory to include path */
#ifdef unix
	"-Uunix",	/* remove unix symbol so that filename unix.c okay */
#endif
#ifdef M4330
	"-DM4330",	/* Tektronix */
#endif
#ifdef M4310
	"-DM4310",	/* Tektronix */
#endif
#if defined(macII) || defined(_AUX_SOURCE)
	"-DmacII",	/* Apple A/UX */
#endif
#ifdef att
	"-Datt",	/* AT&T products */
#endif
#ifdef sony
	"-Dsony",	/* Sony */
#ifndef SYSTYPE_SYSV
	"-Dbsd43",
#endif
#endif
#ifdef _IBMR2
	"-D_IBMR2",	/* IBM RS-6000 (we ensured that aix is defined above) */
#ifndef aix
#define aix		/* allow BOOTSTRAPCFLAGS="-D_IBMR2" */
#endif
#endif /* _IBMR2 */
#ifdef aix
	"-Daix",	/* AIX instead of AOS */
#ifndef ibm
#define ibm		/* allow BOOTSTRAPCFLAGS="-Daix" */
#endif
#endif /* aix */
#ifdef ibm
	"-Dibm",	/* IBM PS/2 and RT under both AOS and AIX */
#endif
#ifdef luna
	"-Dluna",	/* OMRON luna 68K and 88K */
#ifdef luna1
	"-Dluna1",
#endif
#ifdef uniosb
	"-Duniosb",
#endif
#ifdef uniosu
	"-Duniosu",
#endif
#endif /* luna */
#ifdef CRAY		/* Cray */
	"-Ucray",
#endif
#ifdef Mips
	"-DMips",	/* Define and use Mips for Mips Co. OS/mach. */
# if defined(SYSTYPE_BSD) || defined(BSD) || defined(BSD43)
	"-DBSD43",	/* Mips RISCOS supports two environments */
# else
	"-DSYSV",	/* System V environment is the default */
# endif
#endif /* Mips */
#ifdef MOTOROLA
	"-DMOTOROLA",    /* Motorola Delta Systems */
# ifdef SYSV
	"-DSYSV", 
# endif
# ifdef SVR4
	"-DSVR4",
# endif
#endif /* MOTOROLA */
#ifdef SYSV386           /* System V/386 folks */
	"-DSYSV386",
# ifdef SVR4
	"-DSVR4",
# endif
# ifdef ISC
	"-DISC",         /* ISC 2.2.1 */
# endif
# ifdef SCO
	"-DSCO",
# endif
# ifdef ESIX
	"-DESIX",
# endif
# ifdef ATT
	"-DATT",
# endif
# ifdef DELL
	"-DDELL",
# endif
#endif
/*
   Be sure -traditional gets set if we will be using GNU cpp (JSM)
 */
#ifdef GnuCppCmd
	"-traditional",
#endif   
};
#else /* else MAKEDEPEND */
/*
 * Step 6:  predefs
 *     If your compiler and/or preprocessor define any specific symbols, add
 *     them to the the following table.  The definition of struct symtab is
 *     in util/makedepend/def.h.
 */
struct symtab	predefs[] = {
#ifdef apollo
	{"apollo", "1"},
#endif
#ifdef ibm032
	{"ibm032", "1"},
#endif
#ifdef ibm
	{"ibm", "1"},
#endif
#ifdef aix
	{"aix", "1"},
#endif
#ifdef sun
	{"sun", "1"},
#endif
#ifdef hpux
	{"hpux", "1"},
#endif
#ifdef vax
	{"vax", "1"},
#endif
#ifdef VMS
	{"VMS", "1"},
#endif
#ifdef cray
	{"cray", "1"},
#endif
#ifdef CRAY
	{"CRAY", "1"},
#endif
#ifdef att
	{"att", "1"},
#endif
#ifdef mips
	{"mips", "1"},
#endif
#ifdef ultrix
	{"ultrix", "1"},
#endif
#ifdef stellar
	{"stellar", "1"},
#endif
#ifdef mc68000
	{"mc68000", "1"},
#endif
#ifdef mc68020
	{"mc68020", "1"},
#endif
#ifdef __GNUC__
	{"__GNUC__", "1"},
#endif
#if __STDC__
	{"__STDC__", "1"},
#endif
#ifdef __HIGHC__
	{"__HIGHC__", "1"},
#endif
#ifdef CMU
	{"CMU", "1"},
#endif
#ifdef luna
	{"luna", "1"},
#ifdef luna1
	{"luna1", "1"},
#endif
#ifdef luna2
	{"luna2", "1"},
#endif
#ifdef luna88k
	{"luna88k", "1"},
#endif
#ifdef uniosb
	{"uniosb", "1"},
#endif
#ifdef uniosu
	{"uniosu", "1"},
#endif
#endif
#ifdef ieeep754
	{"ieeep754", "1"},
#endif
#ifdef is68k
	{"is68k", "1"},
#endif
#ifdef m68k
        {"m68k", "1"},
#endif
#ifdef m88k
        {"m88k", "1"},
#endif
#ifdef bsd43
	{"bsd43", "1"},
#endif
#ifdef hcx
	{"hcx", 1},
#endif
#if defined(MIPSEL)	\
 || defined(__MIPSEL)	\
 || defined(__MIPSEL__)
	{"__MIPSEL__", 1},
#endif
#if defined(alpha) || defined(__alpha) || defined(__alpha__)
	{"__alpha__", 1},
#endif
#ifdef NeXT
	{"NeXT", "1"},
	{"mc68020", "1"},
#endif
#ifdef __MACH__
	{"__MACH__", "1"},
#endif
	/* add any additional symbols before this line */
	{NULL, NULL}
};

#endif /* MAKEDEPEND */

/* Some systems already declare sys_errlist in stdio.h, and don't like
 * it to be redeclared.
 */

#ifdef __FreeBSD__
#define DontDeclareSysErrlist
#else
#undef  DontDeclareSysErrlist
#endif

#endif /* CCJMAKE */

