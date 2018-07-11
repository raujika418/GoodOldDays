/*
 * $XConsortium: ccjmake.c,v 1.12 89/10/16 12:09:23 jim Exp $
 * 
 * Warning:  This file must be kept as simple as posible so that it can 
 * compile without any special flags on all systems.  Do not touch it unless
 * you *really* know what you're doing.  Make changes in jmakemdep.h, not here.
 */

#define CCJMAKE			/* only get jmake_ccflags definitions */
#include "jmakemdep.h"		/* things to set when porting jmake */

#ifndef jmake_ccflags
#define jmake_ccflags "-O"
#endif

main()
{
	write(1, jmake_ccflags, sizeof(jmake_ccflags) - 1);
	exit(0);
}

