/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(memory_file,
	  [ new_memory_file/1,		% -Handle
	    free_memory_file/1,		% +Handle
	    size_memory_file/2,		% +Handle, -Size
	    open_memory_file/3,		% +Handle, +Mode, -Stream
	    memory_file_to_atom/2,	% +Handle, -Atom
	    memory_file_to_codes/2	% +Handle, -CodeList
	  ]).
:- load_foreign_library(foreign(memfile)).
