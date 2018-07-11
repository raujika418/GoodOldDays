%{
#if 0
\section[De-at-ifier]{Handles `at' and `tr' shorthands}

[Taking over from lit-inputter, minus the LITINPUTS stuff]

Has to know whether \tr{lit2pgm}, \tr{lit2latex} or \tr{lit2texi}
(grabbed from deviously-passed command-line argument).

And makes index entries appear well-behaved (same as \tr{\tr}, really).

And nukes [most] comments (sigh).

\subsection[deatify_Definitions]{Lex definitions}

Here, we use \tr{lex} ``start conditions'' a lot; we keep a stack of
them and manipulate them in a fairly obvious way.  Using start
conditions like this gives you ``mini-lexers'' within the overall
lexer.
\begin{code}
#endif
%}
%{
/* Hacky definition of yywrap: see flex doc.

   If we don't do this, then we'll have to get the default
   yywrap from the flex library, which is often something
   we are not good at locating.  This avoids that difficulty.
   (Besides which, this is the way old flexes (pre 2.4.x) did it.)
   WDP 96/01/15
*/
#define yywrap() 1

#define LEX_STK_SIZE 256

int lex_stk[LEX_STK_SIZE];
int lex_stk_top = 0;

#define PUSH(x)    	   lex_stk[++lex_stk_top] = (x); BEGIN(x)
#define POP        	   BEGIN(lex_stk[--lex_stk_top])
#define LEX_STK_TOP	   lex_stk[lex_stk_top]
#define LEX_NEXT_TO_STK_TOP lex_stk[lex_stk_top - 1]
#define CHG_TOP(x)	   lex_stk[lex_stk_top] = (x); BEGIN(x)
#define CHG_NEXT_TO_TOP(x) lex_stk[lex_stk_top - 1] = (x)
/* not a lot in the old stack {over,under}flow dept, what? */

void print_leading_whitespace(void);

void
print_lex_stk () /* just for debugging */
{
    int i;
    fprintf(stderr,"lex_stk:");
    for (i = lex_stk_top; i >= 0; i--) {
        fprintf(stderr," %d",lex_stk[i]);
    }
    fprintf(stderr,"::%s\n",yytext);
}
#if 0
\end{code}

Things related to handling \tr{\input} commands (not doing this at the
moment):
\begin{code}
#endif
int verbose;	   /* set from first argv */
int follow_inputs; /* set from second argv */

#include "config.h"

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#define index strchr
#define rindex strrchr
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#define INPUTFILE_STK_SIZE 20
struct {
    YY_BUFFER_STATE handle; /* flex-supplied magic */
    char* 	    name;
    int   	    lineno;
} inputfile_stk[INPUTFILE_STK_SIZE];
int   inputfile_stk_top = 0;
void  push_inputfile();
void  pop_inputfile();
int   i; /* temporary */
char* rbrace;

/* ----------------- */

int deatify_debug       = 0;
int unmatched_lbraces   = 0;
int unmatched_lbrackets = 0;

/* like everything, must track srcfile name and lineno */
char srcfile_name[1024]; /* too lazy to do this right */
int  lineno = 1; /* count \n's as they go by */
int  lineno_fiddled = 0; /* boolean: incr if newlines get trashed */

int  exit_status = 0;
void not_OK ();  /* prints an error msg and bumps exit_status */
void warning (); /* same, but no exit-status fiddling */

int lit2what; /* set from ARGV */

#define IS_LIT2PGM_Q	(lit2what == 1)
#define IS_LIT2PGM	(lit2what == 2)
#define IS_LIT2TEXI	(lit2what == 3)
#define IS_LIT2LATEX	(lit2what == 4)
#define IS_LIT2DEPEND	(lit2what == 5)
#define IS_LIT2CHANGELOG (lit2what== 6)

/* the condition for printing to stdout */
#define SHLD_PRINT	(! IS_LIT2PGM_Q || (LEX_STK_TOP == Code || LEX_STK_TOP == Bird))

void myecho();
#define MYECHO myecho()
void myprintstr();

#define YY_USER_ACTION if (deatify_debug) print_lex_stk(); /* debugging */
%}
D		[0-9]+
END_WORD	[-~/!\?(),\.'`A-Za-z0-9]*[ \t]*

%x Norm Verb Code Bird Atting Math Index Typing Linify_braces Linify_brackets
%%
	CHG_TOP(Norm); /* that's where we start */
%{
#if 0
\end{code}

\subsection[deatify_Rules]{Lex rules}

We start as @<Norm>@ and branch off to one of the other
``mini-scanners'' when we see certain things.

First: some magic characters absolutely verboten in the input:
\begin{code}
#endif
%}
[\001\002\003]		{ not_OK("ASCII chars \001, \002, \003 may not appear in literate files\n");
			  exit(1);
			}

<Norm>^"srcfile!_!"(.+)"!_!"([0-9]+)"!_!"\n {
		  sscanf(yytext+10,"%[^!]!_!%d", srcfile_name, &lineno);
		  if (lineno > 0) {
		     ECHO;
		  }
		}
%{
#if 0
\end{code}

Now a bit of stuff to handle \tr{\input} lines:
\begin{code}
#endif
%}
<Norm>^\\input\{[^\}\n]+\}.*\n		{ lineno++;
					  if (follow_inputs) {
					      /* nullify first right brace */
					      rbrace = index(yytext+7,'}');
					      *rbrace = '\0';
					      push_inputfile(yytext+7);
					  } else {
					      MYECHO;
					  }
					}
<Norm,Bird><<EOF>>	{ if ( inputfile_stk_top <= 0 ) {
			     yyterminate();
			  } else {
			     pop_inputfile();
			  }
			  CHG_TOP(Norm); /* revert */
			}
%{
#if 0
\end{code}


Code and verbatim stuff gets copied through absolutely unmangled,
using the @<Verb>@ (or almost equivalently @<Code>@) mini-scanner.
\begin{code}
#endif
%}
<Norm>^\\begin\{verbatim\}.*\n		|
<Norm>^\\begin\{flushverbatim\}.*\n	|
<Norm>^\\begin\{pseudocode\}.*\n	{ lineno++; MYECHO; PUSH(Verb); /* some may have opt arg */ }
<Norm>^\\begin\{code\}.*\n	{ lineno++;
				  if (IS_LIT2PGM_Q) {
				      printf("srcfile!_!%s!_!%d!_!\n",srcfile_name,lineno);
				  }
				  MYECHO;
				  PUSH(Code);
				}
%{
#if 0
\end{code}

Then there is the \tr{>} business; the ugliest way ever invented for
writing programs.  You can only get there from \tr{<Norm>}, because I
said so.

\begin{code}
#endif
%}
<Norm>\n[ \t]+\n\>	{ /* convert to something less weird & try again */
			  unput('>'); unput('\n'); unput('\n');
			}
<Norm>\n\n\>.*\n	{ lineno += 3;
			  /* NB: init > converted to space */
			  if (IS_LIT2PGM_Q) {
			      printf("srcfile!_!%s!_!%d!_!\n",srcfile_name,(lineno-1));
			      printf("%s",yytext+3);
			  } else {
			      ECHO;
			  }
			  PUSH(Bird);
			}
<Norm>\n\>.*\n		{ lineno++; /* so msg will have right line # */
			  if (lineno > 2) /* I feel guilty about this hack */
			      /* to avoid: file is: blank lines, then code */
			      not_OK("Bird-style code not preceded by a blank line\n");
			  lineno++; /* account for other \n */
			  if (IS_LIT2PGM_Q) {
			      printf("srcfile!_!%s!_!%d!_!\n",srcfile_name,(lineno-1));
			      printf("%s",yytext+2);
			  } else {
			      ECHO;
			  }
			  PUSH(Bird);
			}
<Norm>^\>.*\n		{ lineno++; /* first line of file */
			  if (IS_LIT2PGM_Q) {
			      printf("srcfile!_!%s!_!%d!_!\n",srcfile_name,(lineno-1));
			      printf("%s",yytext+1);
			  } else {
			      ECHO;
			  }
			  PUSH(Bird);
			}
%{
#if 0
\end{code}

There are a few macros (\tr{\section}, \tr{\author}, and the others
below) that may have funny \tr{@...@}s, etc., in them (meaning
newlines would be inserted normally); these newlines must be
suppressed!  These macros send us off to \tr{<Linify_braces>} or
\tr{<Linify_brackets>}, which are like \tr{<Norm>} except that the
\tr{unput} newlines (below) get munched.  (This is getting really
horrible...)

\begin{code}
#endif
%}
<Norm>\\author\{		{ MYECHO; PUSH(Linify_braces); }
<Norm>\\caption\{		{ MYECHO; PUSH(Linify_braces); }
<Norm>\\centerline\{		{ MYECHO; PUSH(Linify_braces); }
<Norm>\\date\{			{ MYECHO; PUSH(Linify_braces); }
<Norm>\\define\{		{ MYECHO; PUSH(Linify_braces); }
<Norm>\\heading\{		{ MYECHO; PUSH(Linify_braces); }
<Norm>\\label\{			{ MYECHO; PUSH(Linify_braces); }
<Norm>\\menuentry\{		{ MYECHO; PUSH(Linify_braces); }
<Norm>\\node\{			{ MYECHO; PUSH(Linify_braces); }
<Norm>\\owner\{			{ MYECHO; PUSH(Linify_braces); }
<Norm>\\[sub]*section{D}?\{	{ MYECHO; PUSH(Linify_braces); }
<Norm>\\[sub]*section{D}?\[	{ MYECHO; PUSH(Linify_brackets); }
<Norm>\\standaloneornot\{	{ MYECHO; PUSH(Linify_braces); }
<Norm>\\title\{			{ MYECHO; PUSH(Linify_braces); }
<Norm>\\(tr|pl)\{		{ MYECHO; unmatched_lbraces = 1; PUSH(Typing);
				  /* \tr has braces as literals */
				}
<Norm>\\item\[			{ MYECHO; PUSH(Linify_brackets); }
%{
#if 0
\end{code}

Similarly, an \tr{@} sends us off to @<Atting>@.  Of course, a \tr{\@} does not.
\begin{code}
#endif
%}
<Norm>\\@			{ MYECHO; }
<Norm>@				{ myprintstr("\\codeInText{"); PUSH(Atting); }
%{
#if 0
\end{code}

Simple math mode stuff.

\begin{code}
#endif
%}
<Norm>\\\$			{ MYECHO; }
<Norm>\$			{ myprintstr("\\MathMode{"); PUSH(Math); }
%{
#if 0
\end{code}

For index commands, we want them to be at the beginning of a line (we do?);
hence the @printf@s here.  The internal magic is done by
@<Index>@.
\begin{code}
#endif
%}
<Norm>\\index\{		{ MYECHO;
			  unmatched_lbraces = 1;
                          PUSH(Index);
                        }
%{
#if 0
\end{code}

The rest of the @<Norm>@ mini-scanner handles comments and echoes
everything else.
\begin{code}
#endif
%}
<Norm>\\\{			{ MYECHO; }
<Norm>\\\}			{ MYECHO; }
<Norm,Math>\\%			{ MYECHO; }
<Norm,Math>\%.*\n[ \t]*\n>	{ /* technically this Bird-style code is wrong
				     (no blank line in front of it, but that's
				     not intuitively obvious to the casual observer).
				     Solution? A hack!
				  */
				  unput('>'); unput('\n'); unput('\n');
				}
<Norm,Math>\%.*\n		{ /* throw away */ lineno++;
				  if (! IS_LIT2PGM_Q)
				     printf("\nsrcfile!_!%s!_!%d!_!\n",srcfile_name,lineno);
				}
<Norm>[A-Za-z0-9 \t]+		{ MYECHO; /* slight efficiency */ }
<Norm>.				{ MYECHO; }
<Norm>\n			{ /* handle line-number fiddling */
				  MYECHO;
				  lineno++;
				  if (lineno_fiddled) {
				      if (! IS_LIT2PGM_Q)
				         printf("srcfile!_!%s!_!%d!_!\n",srcfile_name,lineno);
				      lineno_fiddled = 0;
				  }
				}
%{
#if 0
\end{code}

The \tr{<Verb>} and \tr{<Code>} mini-scanners pass everything through until it sees an
\tr{\end <something>}.  As it stands, the \tr{<something>} does not
have to be what was \tr{\begun} (bug).
\begin{code}
#endif
%}
<Verb>^\\end\{verbatim\}	|
<Verb>^\\end\{flushverbatim\}	|
<Verb>^\\end\{pseudocode\}	{ MYECHO; POP; }
<Code>^\\end\{code\}		{ if (! IS_LIT2PGM_Q) ECHO; POP; }
<Verb,Code>^[^\\\n].*\n		{ lineno++; MYECHO; /* a line not started by \ */}
<Verb,Code>.			{ MYECHO; }
<Verb,Code>\n			{ lineno++; MYECHO; }
<Verb,Code><<EOF>>		{ not_OK("unexpected end-of-file in verbatim text or (pseudo)code\n");
				  exit(1);
				}
%{
#if 0
\end{code}

The \tr{<Bird>} mini-scanner is here only to check for
no-blank-line-after errors in uglyly-written code.
\begin{code}
#endif
%}
<Bird>[ \t\n]*\n\>.*\n?	{ /* whitespace/blank-lines do not matter */
			  /* count lines... */
			  for (i = 0; i < strlen(yytext); i++) {
			      if (yytext[i] == '\n')
			      	  lineno++;
			  }
			  if (! IS_LIT2PGM_Q) {
			    ECHO;
			  } else { /* don't print leading whitestuff */
			      for (i = 0; i < strlen(yytext) && yytext[i] != '>'; i++)
				  ; /* no-op */
			      /* report where this got us...*/
			      printf("srcfile!_!%s!_!%d!_!\n",srcfile_name,
				((yytext[yyleng-1] == '\n') ? (lineno-1) : lineno));
			      /* putchar(' '); convert leading > to space */
			      for ( i++ ; i < strlen(yytext); i++)
				  putchar(yytext[i]);
			  }
			}
<Bird>^\>.*\n?		{ if (yytext[yyleng-1] == '\n') { lineno++; }
			  if (IS_LIT2PGM_Q) printf("%s",yytext+1); else ECHO;
			}
<Bird>[ \t]*\n		{ lineno++;
			  if ( ! IS_LIT2PGM_Q) ECHO;
			  POP;
			}
<Bird>.			{ lineno--; /* to get right # on msg */
			  not_OK("Bird-style code not followed by a blank line\n");
			  lineno++; /* put it back */
			  unput(yytext[0]);
			  POP;
			}
%{
#if 0
\end{code}

\tr{<Typing>} and \tr{<Index>} deal with braces the same way: these
rules must come before the default rules for \tr{<Typing>.} and
\tr{<Index>.}.

The rule of the @<Atting>@ game is to send everything through except
right braces and newlines (which break the use of @/[^\}]+/@ regexps).
No longer worry about one per line.

\begin{code}
#endif
%}
<Atting>@@		{ myprintstr("@"); }
<Atting>@\\noindex\{\}	|
<Atting>@\\noindex	{ myprintstr("\001noindex\003}");
			  POP;
			}
<Atting>@		{ myprintstr("}");
			  POP;
			}
<Atting>\{		{ myprintstr("\001lbrace\003"); }
<Atting>\}		{ myprintstr("\001rbrace\003"); }
<Atting>\n		{ warning("I'm turning a newline inside a @...@ into a space\n");
			  myprintstr(" ");
			  lineno++;
			}

<Atting><<EOF>>		{ not_OK("unexpected end-of-file inside an @ ... @\n");
			  exit(1);
			}
<Atting>[A-Za-z0-9 \t]+	{ MYECHO; /* tiny efficiency */ }
<Atting>.		{ MYECHO; }
%{
#if 0
\end{code}

Math mode stuff.  At present, only extra work is to replace some
control sequences with plain text---for \tr{lit2texi} only.

\begin{code}
#endif
%}
<Math>\\\$		{ MYECHO; }
<Math>\$		{ myprintstr("}");
			  POP;
			}
<Math>\{		{ myprintstr("\001lbrace\003"); }
<Math>\}		{ myprintstr("\001rbrace\003"); }
<Math>\n		{ myprintstr("\001newline\003");
			  lineno++;
			}

<Math><<EOF>>		{ not_OK("unexpected end-of-file inside $ ... $ (math mode)\n");
			  exit(1);
			}
<Math>[A-Za-z0-9 \t]+	{ MYECHO; /* tiny efficiency */ }
<Math>.			{ MYECHO; }
%{
#if 0
\end{code}

The braces/brackets stuff isn't too bad.  Well, there's the stuff for
\tr{\tr} and \tr{@..@}'s (no \tr{\index}'s now...) ...
\begin{code}
#endif
%}
<Linify_braces,Linify_brackets>\\(tr|pl)\{ {
			  MYECHO;
			  unmatched_lbraces = 1;
			  PUSH(Typing);
			}

<Index,Linify_braces,Linify_brackets,Typing>\\\\ {
			  MYECHO;
			}
<Index,Linify_braces,Linify_brackets,Typing>\\ {
			  MYECHO;
			}
<Linify_braces,Linify_brackets>\\\{ {
			  MYECHO;
			}

<Linify_braces>\{	{ MYECHO; PUSH(Linify_braces); }
<Linify_brackets>\{	{ MYECHO; }

<Index,Typing>\{	{ unmatched_lbraces++; myprintstr("\001lbrace\003"); }

<Linify_braces,Linify_brackets>\\\} {
			  MYECHO;
			}
<Linify_braces>\}\{	{ /* next arg */ MYECHO; }

<Linify_braces>\}	{ MYECHO; POP; }
<Linify_brackets>\}	{ MYECHO; }

<Index,Typing>\}	{ if (--unmatched_lbraces == 0) {
			      MYECHO;
                              POP;
                          } else {
			      myprintstr("\001rbrace\003");
                          }
                        }

<Index,Linify_braces,Linify_brackets,Typing>\\@ {
			  MYECHO;
			  /* Note: our pseudo-makeindex has two magic chars */
			}
<Linify_braces,Linify_brackets>@ {
			  myprintstr("\\codeInText{"); /* no newline */
			  PUSH(Atting); 
			}
<Typing>@		{ MYECHO; /* NB: magic <Index> chars below */ }

<Index,Linify_braces,Linify_brackets,Typing>\\\$ {
			  MYECHO;
			}
<Linify_braces,Linify_brackets>\$ {
			  myprintstr("\\MathMode{"); /* no newline */
			  PUSH(Math);
			}
<Index,Typing>\$	{ MYECHO; }

<Index,Linify_braces,Linify_brackets,Typing>\\% {
			  MYECHO;
			}
<Linify_braces,Linify_brackets>\%.*\n[ \t]*\n> {
			  /* see earlier comments about crimes against humanity */
			  unput('>'); unput('\n'); unput('\n');
			}
<Linify_braces,Linify_brackets>\%.*\n { /* throw away */
			  lineno++;
			  lineno_fiddled++;
			}
<Index,Typing>%		{ MYECHO; }

<Linify_brackets>\\\[	{ MYECHO; }
<Linify_brackets>\[	{ MYECHO; PUSH(Linify_brackets); }	

<Linify_brackets>\\\]	{ myprintstr("\\\001rbracket\003"); /* cannot have these in the way */ }
<Linify_brackets>\]\{	{ if (LEX_NEXT_TO_STK_TOP == Norm) {
			      MYECHO;
			  } else {
			      myprintstr("\001rbracket\003\{");
			      not_OK("WHAT'S GOING ON? ]{\n");
			      print_lex_stk();
			  }
			  CHG_TOP(Linify_braces);
                        }
<Linify_brackets>\]	{ if (LEX_NEXT_TO_STK_TOP == Norm) {
			      MYECHO;
			  } else {
			      myprintstr("\001rbracket\003");
			  }
			  POP;
                        }

<Index>\@		{ myprintstr("\001idxsort\003"); }
<Index>\\!		{ MYECHO; }
<Index>\!		{ myprintstr("\001idxsubitem\003"); }

<Index,Linify_braces,Linify_brackets,Typing>\n {
			  lineno++;
			  lineno_fiddled++;
			  if (LEX_STK_TOP == Index) {
			     warning("I'm turning a newline inside an \\index{...} into a space\n");
			     myprintstr(" ");
			  } else if (LEX_STK_TOP == Typing) {
			     warning("I'm turning a newline inside a \\tr{...} or \\pl{...} into a space\n");
			     myprintstr(" ");
			  } else {
			     myprintstr("\001newline\003");
			  }
			}

<Index,Linify_braces,Linify_brackets,Typing>[A-Za-z0-9 \t]+ {
			  MYECHO; /* tiny efficiency */
			}

<Index,Linify_braces,Linify_brackets,Typing>. { MYECHO; /* catch all */ }

<Index><<EOF>>		{ not_OK("unexpected end-of-file inside an \\index{...}\n");
			  exit(1);
			}
<Linify_braces><<EOF>>	{ not_OK("unexpected end-of-file inside a macro argument\n");
			  exit(1);
			}
<Linify_brackets><<EOF>> {not_OK("unexpected end-of-file inside a macro optional argument\n");
			  exit(1);
			}
<Typing><<EOF>>		{ not_OK("unexpected end-of-file inside a \\tr{...}\n");
			  exit(1);
			}
%{
#if 0
\end{code}

Finally, we have a non-default @main@ routine because we need to pick
up the (mandatory) argument that says if we are doing pre-processing
for \tr{lit2pgm}, \tr{lit2latex}, or \tr{lit2texi}.
\begin{code}
#endif
%}
%%

main(argc, argv)
int  argc;
char **argv;
{
    if (argc != 5) {
	fprintf(stderr,"Sorry, %s must have exactly 4 arguments\n", argv[0]);
	exit(1);
    }
    verbose	  = argv[1][0] - '0'; /* hacks */
    follow_inputs = argv[2][0] - '0';
    lit2what      = argv[3][0] - '0'; /* used in IS_LIT2xxx tests */
    if (strcmp(argv[4], "-") == 0) {
	yyin = stdin;
    } else if ((yyin = fopen(argv[4], "r")) == NULL) {
	fprintf(stderr,"%s: can't open file %s\n", argv[0], argv[4]);
	exit(1);
    }
    strcpy(srcfile_name, argv[4]);
    inputfile_stk[ inputfile_stk_top /* 0 */ ].name = argv[4];
    if ( ! IS_LIT2PGM_Q ) printf("srcfile!_!%s!_!1!_!\n",argv[4]);
    yylex();
    exit(exit_status);
}

void
push_inputfile (fname)
char* fname;
{
    if ( inputfile_stk_top >= INPUTFILE_STK_SIZE ) {
	fprintf(stderr,"\\input's too deeply nested\n");
	exit(1);
    }

    /* record where we are in current file */
    inputfile_stk[ inputfile_stk_top ].lineno   = lineno;
    inputfile_stk[ inputfile_stk_top ].handle = YY_CURRENT_BUFFER;
    /* actual push */
    inputfile_stk_top++;

    /* open new file fname */
    if ((yyin = fopen(fname, "r")) == NULL) {
	fprintf(stderr,"%s:%d: can't open \\input file %s\n", srcfile_name, lineno, fname);
	exit(1);
    }
    yy_switch_to_buffer( yy_create_buffer( yyin, YY_BUF_SIZE ) );

    /* report that's where we are */
    if (! IS_LIT2PGM_Q) printf("srcfile!_!%s!_!1!_!\n",fname);
    /* set srcfile_name and lineno for new file */

    strcpy(srcfile_name, fname); /* for my own tracking */
    lineno = 1;

    /* push/record everything for new file */
    inputfile_stk[ inputfile_stk_top ].name     = srcfile_name;
    inputfile_stk[ inputfile_stk_top ].lineno   = lineno;
}

void
pop_inputfile ()
{
    inputfile_stk_top--; /* actual pop */

    /* remind where we were */
    strcpy(srcfile_name, inputfile_stk[inputfile_stk_top].name);
    lineno       = inputfile_stk[inputfile_stk_top].lineno;
    /* put in an extra newline or two before the next file;
       otherwise, the cat'ting of files gives things like:

       > last line of code
       \section{Next section}

       which happens to be very bad for lit2stuff.
    */
    if (! IS_LIT2PGM_Q) printf("\n\nsrcfile!_!%s!_!%d!_!\n",srcfile_name,lineno);

    /* return to prev inputfile */
    yy_switch_to_buffer( inputfile_stk[inputfile_stk_top].handle );
}

void
myecho ()
{
    myprintstr(yytext);
}

void
myprintstr (str)
char *str;
{
    if (SHLD_PRINT) {
	printf("%s",str);
    }
}

void
print_leading_whitespace (void)
{
    int i;
    
    for (i = 0; yytext[i] == ' ' || yytext[i] == '\t'; i++) {
    	putchar(yytext[i]);
    }
}

void
not_OK(msg)
char *msg;
{
    /* a good old emacsable error msg */
    fprintf(stderr,"%s:%d: %s", srcfile_name, lineno, msg);
    exit_status++;
}

void
warning(msg) /* exit_status not fiddled */
char *msg;
{
    /* a good old emacsable warning msg */
    fprintf(stderr,"%s:%d: [warning] %s", srcfile_name, lineno, msg);
}
#if 0
\end{code}
#endif
