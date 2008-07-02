/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(http_dispatch,
	  [ http_dispatch/1,		% +Request
	    http_handler/3,		% +Path, +Predicate, +Options
	    http_delete_handler/1,	% +Path
	    http_reply_file/3,		% +File, +Options, +Request
	    http_current_handler/2	% ?Path, ?Pred
	  ]).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(settings)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_header)).
:- use_module(library(http/thread_httpd)).

/** <module> Dispatch requests in the HTTP server

This module can be placed between   http_wrapper.pl  and the application
code to associate HTTP _locations_ to   predicates that serve the pages.
In addition, it associates parameters  with   locations  that  deal with
timeout handling and user authentication.  The typical setup is:

==
server(Port, Options) :-
	http_server(http_dispatch,
		    [ port(Port),
		    | Options
		    ]).

:- http_handler('/index.html', write_index, []).

write_index(Request) :-
	...
==

@author	Jan Wielemaker
*/

:- setting(http:time_limit, nonneg, 300,
	   'Time limit handling a single query (0=infinite)').

%%	http_handler(+Path, :Pred, +Options) is det.
%
%	Register Pred as a handler for HTTP requests. Path is either the
%	full location of the HTTP  request   or  a term prefix(+Prefix).
%	Pred is either an atom or one of the following reserved terms:
%	
%		* reply_file(+File, +FileOptions)
%		Reply contents of File using the given options.
%		FileOptions include:
%			
%			* mime_type(+Type)
%			Mime-type specified with File.
%			
%			* cache(+Boolean)
%			If =false=, do not pass and process
%			last_modification time.
%
%	Options is a list containing the following options:
%	
%		* time_limit(+Spec)
%		One of =infinite=, =default= or a positive number
%		(seconds)
%		
%		* chunked
%		Use =|Transfer-encoding: chunked|= if the client
%		allows for it.
%		
%		* spawn(+SpawnOptions)
%		Run the handler in a seperate thread.  If SpawnOptions
%		is an atom, it is interpreted as a thread pool name
%		(see create_thread_pool/3).  Otherwise the options
%		are passed to http_spawn/2 and from there to
%		thread_create/3.  These options are typically used to
%		set the stack limits.
%		
%		* authentication(+Type)
%		Demand authentication.  Authentication methods are
%		pluggable.  The library http_authenticate.pl provides
%		a plugin for user/password based =Basic= HTTP
%		authentication.
%		
%		* priority(+Integer)
%		If two handlers handle the same path, the one with the
%		highest priority is used.  If equal, the last registered
%		is used.  Please be aware that the order of clauses in
%		multifile predicates can change due to reloading files.
%		
%	Note that http_handler/3 is normally invoked  as a directive and
%	processed using term-expansion.  Using   term-expansion  ensures
%	proper update through make/0 when the specification is modified.
%	We do not expand when the  cross-referencer is running to ensure
%	proper handling of the meta-call.

:- dynamic handler/3.
:- multifile handler/3.
:- dynamic generation/1.

:- meta_predicate
	http_handler(+, :, +),
	http_current_handler(?, :).

http_handler(Path, Pred, Options) :-
	strip_module(Pred, M, P),
	compile_handler(Path, M:P, Options, Clause),
	next_generation,
	assert(Clause).

:- multifile
	user:term_expansion/2.

user:term_expansion((:- http_handler(Path, Pred, Options)), Clause) :-
	\+ current_prolog_flag(xref, true),
	prolog_load_context(module, M),
	compile_handler(Path, M:Pred, Options, Clause),
	next_generation.


%%	http_delete_handler(+Path) is det.
%
%	Delete handler for Path. Typically, this should only be used for
%	handlers that are registered dynamically.

http_delete_handler(Path) :-
	retractall(handler(Path, _Pred, _Options)),
	next_generation.


%%	next_generation is det.
%%	current_generation(-G) is det.
%
%	Increment the generation count.

next_generation :-
	with_mutex(http_dispatch, next_generation_unlocked).

next_generation_unlocked :-
	retract(generation(G0)), !,
	G is G0	+ 1,
	assert(generation(G)).
next_generation_unlocked :-
	assert(generation(1)).

current_generation(G) :-
	with_mutex(http_dispatch, generation(G)), !.
current_generation(0).


%%	compile_handler(+Path, +Pred, +Options) is det.
%
%	Compile a handler specification. For now we this is a no-op, but
%	in the feature can make this more efficiently, especially in the
%	presence of one or multiple prefix declarations. We can also use
%	this to detect conflicts.

compile_handler(Path, PredSpec, Options,
		http_dispatch:handler(Path, M:Pred, Options)) :-
	strip_module(PredSpec, M, Pred).


%%	http_dispatch(Request) is det.
%
%	Dispatch a Request using http_handler/3 registrations.

http_dispatch(Request) :-
	memberchk(path(Path), Request),
	find_handler(Path, Pred, Options),
	authentication(Request, Options, User),
	(   nonvar(User)
	->  action(Pred, [user(User)|Request], Options)
	;   action(Pred, Request, Options)
	).


%%	http_current_handler(+Location, -Closure) is semidet.
%%	http_current_handler(-Location, +Closure) is nondet.
%
%	True if Location is handled by Closure.

http_current_handler(Path, Closure) :-
	atom(Path), !,
	path_tree(Tree),
	find_handler(Tree, Path, Closure, _).
http_current_handler(Path, M:C) :- !,
	handler(Path, M:C, _).
http_current_handler(Path, Closure) :-
	strip_module(Closure, M, C),
	handler(Path, M:C, _).


%%	authentication(+Request, +Options, -User) is det.
%
%	Verify  authentication  information.   If    authentication   is
%	requested through Options, demand it. The actual verification is
%	done by the multifile   predicate  http_dispatch:authenticate/3.
%	The  library  http_authenticate.pl  provides  an  implementation
%	thereof.
%	
%	@error	permission_error(http_location, access, Location)

:- multifile
	http:authenticate/3.

authentication(Request, Options, User) :-
	memberchk(authentication(Type), Options), !,
	(   http:authenticate(Type, Request, User)
	->  true
	;   memberchk(path(Path), Request),
	    throw(error(permission_error(http_location, access, Path), _))
	).
authentication(_Request, _Options, _User).

%%	find_handler(+Path, -Action, -Options) is det.
%
%	Find the handler to call from Path.  Rules:
%	
%		* If there is a matching handler, use this.
%		* If there are multiple prefix(Path) handlers, use the
%		  longest.
%		  
%	If there is a handler for =|/dir/|=   and  the requested path is
%	=|/dir|=, find_handler/3 throws a  http_reply exception, causing
%	the wrapper to generate a 301 (Moved Permanently) reply.
%		  
%	@error	existence_error(http_location, Location)
%	@throw	http_reply(moved(Dir))
%	@tbd	Introduce automatic redirection to indexes here?

find_handler(Path, Action, Options) :-
	path_tree(Tree),
	(   find_handler(Tree, Path, Action, Options)
	->  true
	;   \+ sub_atom(Path, _, _, 0, /),
	    atom_concat(Path, /, Dir),
	    find_handler(Tree, Dir, Action, Options)
	->  throw(http_reply(moved(Dir)))
	;   throw(error(existence_error(http_location, Path), _))
	).


find_handler([node(prefix(Prefix), PAction, POptions, Children)|_],
	     Path, Action, Options) :-
	sub_atom(Path, 0, _, _, Prefix), !,
	(   find_handler(Children, Path, Action, Options)
	->  true
	;   Action = PAction,
	    Options = POptions
	).
find_handler([node(Path, Action, Options, _)|_], Path, Action, Options) :- !.
find_handler([_|Tree], Path, Action, Options) :-
	find_handler(Tree, Path, Action, Options).


%%	action(+Action, +Request, +Options) is det.
%
%	Execute the action found.  Here we take care of the options
%	=time_limit=, =chunked= and =spawn=.
%
%	@error	goal_failed(Goal)

action(Action, Request, Options) :-
	memberchk(chunked, Options), !,
	format('Transfer-encoding: chunked~n'),
	spawn_action(Action, Request, Options).
action(Action, Request, Options) :-
	spawn_action(Action, Request, Options).

spawn_action(Action, Request, Options) :-
	option(spawn(Spawn), Options), !,
	spawn_options(Spawn, SpawnOption),
	http_spawn(time_limit_action(Action, Request, Options), SpawnOption).
spawn_action(Action, Request, Options) :-
	time_limit_action(Action, Request, Options).

spawn_options([], []) :- !.
spawn_options(Pool, Options) :-
	atom(Pool), !,
	Options = [pool(Pool)].
spawn_options(List, List).

time_limit_action(Action, Request, Options) :-
	(   option(time_limit(TimeLimit), Options),
	    TimeLimit \== default
	->  true
	;   setting(http:time_limit, TimeLimit)
	),
	number(TimeLimit),
	TimeLimit > 0, !,
	call_with_time_limit(TimeLimit, call_action(Action, Request, Options)).
time_limit_action(Action, Request, Options) :-
	call_action(Action, Request, Options).


%%	call_action(+Action, +Request, +Options)
%
%	@tbd	reply_file is normal call?

call_action(reply_file(File, FileOptions), Request, _Options) :- !,
	http_reply_file(File, FileOptions, Request).
call_action(Pred, Request, _Options) :-
	(   call(Pred, Request)
	->  true
	;   Pred =.. List,
	    append(List, [Request], List2),
	    Goal =.. List2,
	    throw(error(goal_failed(Goal), _))
	).


%%	http_reply_file(+FileSpec, +Options, +Request) is det.
%
%	Options is a list of
%	
%		* cache(+Boolean)
%		If =true= (default), handle If-modified-since and send
%		modification time.
%		
%		* mime_type(+Type)
%		Overrule mime-type guessing from the filename as
%		provided by file_mime_type/2.
%
%	@throws	http_reply(not_modified)
%	@throws http_reply(file(MimeType, Path)

http_reply_file(File, Options, Request) :-
	absolute_file_name(File, Path,
			   [ access(read)
			   ]),
	(   option(cache(true), Options, true)
	->  (   memberchk(if_modified_since(Since), Request),
	        time_file(Path, Time),
		catch(http_timestamp(Time, Since), _, fail)
	    ->  throw(http_reply(not_modified))
	    ;	true
	    ),
	    Reply = file(Type, Path)
	;   Reply = tmp_file(Type, Path)
	),
	(   option(mime_type(Type), Options)
	->  true
	;   file_mime_type(Path, Type)
	->  true
	;   Type = text/plain		% fallback type
	),
	throw(http_reply(Reply)).


		 /*******************************
		 *	  PATH COMPILATION	*
		 *******************************/

%%	path_tree(-Tree) is det.
%
%	Compile paths into  a  tree.  The   treee  is  multi-rooted  and
%	represented as a list of nodes, where each node has the form:
%	
%		node(PathOrPrefix, Action, Options, Children)
%		
%	The tree is a potentially complicated structure. It is cached in
%	a global variable. Note that this   cache is per-thread, so each
%	worker thread holds a copy of  the   tree.  If handler facts are
%	changed the _generation_ is  incremented using next_generation/0
%	and each worker thread will  re-compute   the  tree  on the next
%	ocasion.

path_tree(Tree) :-
	current_generation(G),
	nb_current(http_dispatch_tree, G-Tree), !. % Avoid existence error
path_tree(Tree) :-
	findall(Prefix, handler(prefix(Prefix), _, _), Prefixes0),
	sort(Prefixes0, Prefixes),
	prefix_tree(Prefixes, [], PTree),
	prefix_options(PTree, [], OPTree),
	add_paths_tree(OPTree, Tree),
	current_generation(G),
	nb_setval(http_dispatch_tree, G-Tree).


%%	prefix_tree(PrefixList, +Tree0, -Tree)
%
%	@param Tree	list(Prefix-list(Children))

prefix_tree([], Tree, Tree).
prefix_tree([H|T], Tree0, Tree) :-
	insert_prefix(H, Tree0, Tree1),
	prefix_tree(T, Tree1, Tree).

insert_prefix(Prefix, Tree0, Tree) :-
	select(P-T, Tree0, Tree1),
	sub_atom(Prefix, 0, _, _, P), !,
	insert_prefix(Prefix, T, T1),
	Tree = [P-T1|Tree1].
insert_prefix(Prefix, Tree, [Prefix-[]|Tree]).


%%	prefix_options(+PrefixTree, +DefOptions, -OptionTree)
%
%	Generate the option-tree for all prefix declarations.
%	
%	@tbd	What to do if there are more?

prefix_options([], _, []).
prefix_options([P-C|T0], DefOptions,
	       [node(prefix(P), Action, Options, Children)|T]) :-
	once(handler(prefix(P), Action, Options0)),
	merge_options(Options0, DefOptions, Options),
	prefix_options(C, Options, Children),
	prefix_options(T0, DefOptions, T).


%%	add_paths_tree(+OPTree, -Tree) is det.
%
%	Add the plain paths.

add_paths_tree(OPTree, Tree) :-
	findall(path(Path, Action, Options), 
		plain_path(Path, Action, Options),
		Triples),
	add_paths_tree(Triples, OPTree, Tree).

add_paths_tree([], Tree, Tree).
add_paths_tree([path(Path, Action, Options)|T], Tree0, Tree) :-
	add_path_tree(Path, Action, Options, [], Tree0, Tree1),
	add_paths_tree(T, Tree1, Tree).


%%	plain_path(-Path, -Action, -Options) is nondet.
%
%	True if {Path,Action,Options} is registered and  Path is a plain
%	(i.e. not _prefix_) location.

plain_path(Path, Action, Options) :-
	handler(Path, Action, Options),
	atom(Path).


%%	add_path_tree(+Path, +Action, +Options, +Tree0, -Tree) is det.
%
%	Add a path to a tree. If a  handler for the same path is already
%	defined, the one with the highest   priority or the latest takes
%	precedence.

add_path_tree(Path, Action, Options0, DefOptions, [],
	      [node(Path, Action, Options, [])]) :- !,
	merge_options(Options0, DefOptions, Options).
add_path_tree(Path, Action, Options, _,
	      [node(prefix(Prefix), PA, DefOptions, Children0)|RestTree],
	      [node(prefix(Prefix), PA, DefOptions, Children)|RestTree]) :-
	sub_atom(Path, 0, _, _, Prefix), !,
	add_path_tree(Path, Action, Options, DefOptions, Children0, Children).
add_path_tree(Path, Action, Options1, DefOptions, [H0|T], [H|T]) :-
	H0 = node(Path, _, Options2, _),
	option(priority(P1), Options1, 0),
	option(priority(P2), Options2, 0),
	P1 >= P2, !,
	merge_options(Options1, DefOptions, Options),
	H = node(Path, Action, Options, []).
add_path_tree(Path, Action, Options, DefOptions, [H|T0], [H|T]) :-
	add_path_tree(Path, Action, Options, DefOptions, T0, T).


		 /*******************************
		 *	      XREF		*
		 *******************************/

:- multifile
	prolog:meta_goal/2.
:- dynamic
	prolog:meta_goal/2.

prolog:meta_goal(http_handler(_, G, _), [G+1]).
prolog:meta_goal(http_current_handler(_, G), [G+1]).


		 /*******************************
		 *	       EDIT		*
		 *******************************/

% Allow edit(Location) to edit the implementation for an HTTP location.

:- multifile
	prolog_edit:locate/3.

prolog_edit:locate(Path, Spec, Location) :-
	atom(Path),
	http_current_handler(Path, Pred),
	closure_name_arity(Pred, 1, PI),
	prolog_edit:locate(PI, Spec, Location).

closure_name_arity(M:Term, Extra, M:Name/Arity) :- !,
	callable(Term),
	functor(Term, Name, Arity0),
	Arity is Arity0 + Extra.
closure_name_arity(Term, Extra, Name/Arity) :-
	callable(Term),
	functor(Term, Name, Arity0),
	Arity is Arity0 + Extra.

