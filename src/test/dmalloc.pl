:- module(dmalloc,
	  [ dmalloc/1,			% :Goal
	    dmalloc/3,			% :Goal, +Format, +Args
	    dmalloc_mark/1,		% -Mark
	    dmalloc_log_changed/4,	% +Mark, +NotFreed, +Freed, +Details
	    dmalloc_message/2		% +Format, +Args
	  ]).

/** <module> DMALLOC utility predicates

Usage: compile dmalloc.c using the  command   below  before loading this
module.

    ==
    % plld --shared -o dmalloc dmalloc.c
    ==

Prolog must be compiled  with  dmalloc   support  enabled,  which can be
achieved using =|configure --enable-dmalloc|= or by adding =|-DDMALLOC|=
to COFLAGS and =|-ldmalloc|= to LIBS in the generated Makefile.

Now, configure dmalloc for the shell, start Prolog and load this module.
E.g.

    ==
    % dmalloc -l logfile low
    % pl
    % ?- [dmalloc].
    % ?- dmalloc(go).
    ==

Note that typically, you should run the  predicate for which you want to
do leak-testing twice. The first run may  show `leaks' due to autoloaded
predicates, lazily generated clause indices, etc.

@see	The Dmalloc home-page at http://dmalloc.com/
*/

:- use_foreign_library(dmalloc).
:- meta_predicate
	dmalloc(0),
	dmalloc(0, +, +).

%%	dmalloc(:Goal).
%%	dmalloc(:Goal, +Format, +Args).
%
%	Run Goal and  print  incremental   dmalloc  diagnostics  to  the
%	current malloc logfile.

dmalloc(G) :-
	dmalloc(G, '', []).

dmalloc(G, Fmt, Args) :-
	garbage_collect,
	garbage_collect_atoms,
	trim_stacks,
	setup_call_cleanup(dmalloc_mark(Mark),
			   G,
			   end_dmalloc(Mark, Fmt, Args)).

end_dmalloc(Mark, Fmt, Args) :-
	(   Fmt \== ''
	->  dmalloc_message(Fmt, Args)
	;   true
	),
	garbage_collect,
	garbage_collect_atoms,
	trim_stacks,
	dmalloc_log_changed(Mark, true, false, true).

dmalloc_message(Format, Args) :-
	format(string(S), Format, Args),
	'_dmalloc_message'(S).
