/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#ifndef _ERROR_H_INCLUDED
#define _ERROR_H_INCLUDED

#define ERR_INSTANTIATION 1		/* general badly typed argument */
#define ERR_FORMAT 	  2		/* bad format in table */
#define ERR_IO	          3		/* general IO error */

#define error(a,b,c,d) error_func(a,b,c,(long)(d))

extern int	error_func(int type, const char *pred, int argi, long argl);

#endif /*_ERROR_H_INCLUDED*/

