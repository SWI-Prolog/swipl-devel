/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/

:- module(crypt,
	  [ crypt/2
	  ]).

:- load_foreign_library(foreign(crypt), install_crypt).

%	crypt(+Passwd, ?Encripted).
%
%	Used to test an encrypted passwd or create one.  In the latter
%	case, the first 2 letter must be instantiated
