/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- use_module(library(unix)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple demo illustrating the combination of   fork/1  and pipe/2 to make
Prolog fork a child to do  some  work   and  get  back  when done. Using
wait_for_input/3 you can make the  main   Prolog  task wait for multiple
childs to return results.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fork_demo(Result) :-
	pipe(Read, Write),
	fork(Pid),
	(   Pid == child
	->  close(Read),
	    format(Write, '~q.~n', [hello(world)]),
	    flush_output(Write),	% stream is fully buffered!
	    halt
	;   close(Write),
	    read(Read, Result),
	    close(Read)
	).
