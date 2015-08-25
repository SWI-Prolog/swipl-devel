/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Richard O'Keefe
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(check_installation,
	  [ check_installation/0
	  ]).

/** <module> Check installation issues and features

This library performs checks on  the   installed  system to verify which
optional components are available and  whether   all  libaries that load
shared objects/DLLs can be loaded.
*/

%%	component(?Component, -Features) is nondet.
%
%	This predicate describes the test components. Features is a dict
%	with the following components:
%
%	  - test:Goal
%	  (Additional) test that must succeed for the component to be
%	  functional.
%	  - url:URL
%	  URL with additional information, relative to
%	  =|http://www.swi-prolog.org/build/issues/|=.  If not provided,
%	  the library file with extension =|.html|= is used.
%	  - optional:true
%	  If the library does not exist, do not complain.
%	  - os:OS
%	  One of =windows=, =unix= or =linux=. If present, the component
%	  is only checked for if we are running on a version of the
%	  specified operating system.
%	  - features:Goal
%	  After successful evaluation that loading and basic operation
%	  of the component succeeds, run this to check additional
%	  features.

% Feature tests
component(gmp,
	  _{ test:current_prolog_flag(bounded, false),
	     url:'gmp.html'
	   }).
component(command_line_editing,
	  _{ test:current_predicate(system:rl_add_history/1),
	     url:'cmdlineedit.html'
	   }).
% Packages that depend on foreign libraries
component(library(archive), _{features:archive_features}).
component(library(cgi), _{}).
component(library(crypt), _{}).
component(library(db), _{optional:true}).
component(library(double_metaphone), _{}).
component(library(filesex), _{}).
component(library(http/http_stream), _{}).
component(library(http/json), _{}).
component(library(isub), _{}).
component(library(jpl), _{}).
component(library(memfile), _{}).
component(library(mime), _{}).
component(library(odbc), _{}).
component(library(pce),
	  _{pre:load_foreign_library(pce_principal:foreign(pl2xpce)),
	    url:'xpce.html'}).
component(library(pdt_console), _{}).
component(library(porter_stem), _{}).
component(library(process), _{}).
component(library(readutil), _{}).
component(library(rlimit), _{os:unix}).
component(library(semweb/rdf_db), _{}).
component(library(semweb/rdf_ntriples), _{}).
component(library(semweb/turtle), _{}).
component(library(sgml), _{}).
component(library(sha), _{}).
component(library(snowball), _{}).
component(library(socket), _{}).
component(library(ssl), _{}).
component(library(syslog), _{os:unix}).
component(library(table), _{}).
component(library(time), _{}).
component(library(tipc/tipc), _{os:linux}).
component(library(unicode), _{}).
component(library(uri), _{}).
component(library(uuid), _{}).
component(library(zlib), _{}).

issue_base('http://www.swi-prolog.org/build/issues/').

:- thread_local
	issue/1.

:- meta_predicate
	run_silent(0, +).

%%	check_installation
%
%	Check features of the installed   system. Performs the following
%	tests:
%
%	  1. Test whether features that depend on optional libraries
%	     are present (e.g., unbounded arithmetic support)
%	  2. Test that all standard libraries that depend on foreign
%	     code are present.
%
%	If issues are found it prints a   diagnostic message with a link
%	to a wiki page with additional information about the issue.

check_installation :-
	print_message(informational, installation(checking)),
	retractall(issue(_)),
	forall(component(Source, _Properties),
	       check_installation(Source)),
	findall(I, retract(issue(I)), Issues),
	(   Issues == []
	->  print_message(informational, installation(perfect))
	;   length(Issues, Count),
	    print_message(warning, installation(imperfect(Count)))
	).

check_installation(Source) :-
	component(Source, Properties), !,
	check_installation(Source, Properties.put(source,Source)).

check_installation(Source, Properties) :-
	compound(Source), !,
	check_source(Source, Properties).
check_installation(Feature, Properties) :-
	print_message(informational, installation(checking(Feature))),
	(   call(Properties.test)
	->  print_message(informational, installation(ok))
	;   print_issue(installation(missing(Properties)))
	).

check_source(_Source, Properties) :-
	OS = Properties.get(os),
	\+ current_os(OS), !.
check_source(Source, Properties) :-
	exists_source(Source), !,
	print_message(informational, installation(loading(Source))),
	(   run_silent(( (   Pre = Properties.get(pre)
			 ->  call(Pre)
			 ;   true
			 ),
			 load_files(Source, [silent(true), if(not_loaded)])
		       ),
		       Properties.put(action, load))
	->  test_component(Properties),
	    print_message(informational, installation(ok)),
	    check_features(Properties)
	;   true
	).
check_source(_Source, Properties) :-
	Properties.get(optional) == true, !,
	print_message(silent,
		      installation(optional_not_found(Properties))).
check_source(_Source, Properties) :-
	print_issue(installation(not_found(Properties))).

current_os(unix)    :- current_prolog_flag(unix, true).
current_os(windows) :- current_prolog_flag(windows, true).
current_os(linux)   :- current_prolog_flag(arch, Arch), sub_atom(Arch, _, _, _, linux).

%%	test_component(+Properties) is semidet.
%
%	Run additional tests to see whether the componnent really works.

test_component(Dict) :-
	Test = Dict.get(test), !,
	call(Test).
test_component(_).

%%	check_features(+Properties) is semidet.
%
%	Check for additional features of the components.
%
%	@see check_installation/1 should be used for checking that the
%	component works.

check_features(Dict) :-
	Test = Dict.get(features), !,
	call(Test).
check_features(_).


%%	run_silent(:Goal, +Properties) is semidet.
%
%	Succeed if Goal succeeds  and  does   not  print  any  errors or
%	warnings.

run_silent(Goal, Properties) :-
	run_collect_messages(Goal, Result, Messages),
	(   Result == true,
	    Messages == []
	->  true
	;   print_issue(installation(failed(Properties, Result, Messages))),
	    fail
	).

%%	run_collect_messages(Goal, Result, Messages) is det.
%
%	Run Goal, unify Result with  =true=, =false= or exception(Error)
%	and  messages  with  a  list  of  generated  error  and  warning
%	messages. Each message is a term:
%
%	    message(Term,Kind,Lines)
%
%	@see message_hook/3.

:- thread_local
	got_message/1.

run_collect_messages(Goal, Result, Messages) :-
	setup_call_cleanup(
	    asserta((user:thread_message_hook(Term,Kind,Lines) :-
		        error_kind(Kind),
		        assertz(got_message(message(Term,Kind,Lines)))), Ref),
	    (	catch(Goal, E, true)
	    ->	(   var(E)
		->  Result0 = true
		;   Result0 = exception(E)
		)
	    ;	Result0 = false
	    ),
	    erase(Ref)),
	findall(Msg, retract(got_message(Msg)), Messages),
	Result = Result0.

error_kind(warning).
error_kind(error).


		 /*******************************
		 *	   SPECIAL TESTS	*
		 *******************************/

%%	archive_features
%
%	Report features supported by library(archive).

archive_features :-
	tmp_file_stream(utf8, Name, Out),
	close(Out),
	findall(F, archive_filter(F, Name), Filters),
	print_message(informational, installation(archive(filters, Filters))),
	findall(F, archive_format(F, Name), Formats),
	print_message(informational, installation(archive(formats, Formats))),
	delete_file(Name).

archive_filter(F, Name) :-
	a_filter(F),
	catch(archive_open(Name, A, [filter(F)]), E, true),
	(   var(E)
	->  archive_close(A)
	;   true
	),
	\+ subsumes_term(error(domain_error(filter, _),_), E).

archive_format(F, Name) :-
	a_format(F),
	catch(archive_open(Name, A, [format(F)]), E, true),
	(   var(E)
	->  archive_close(A)
	;   true
	),
	\+ subsumes_term(error(domain_error(filter, _),_), E).

a_filter(bzip2).
a_filter(compress).
a_filter(gzip).
a_filter(grzip).
a_filter(lrzip).
a_filter(lzip).
a_filter(lzma).
a_filter(lzop).
a_filter(none).
a_filter(rpm).
a_filter(uu).
a_filter(xz).

a_format('7zip').
a_format(ar).
a_format(cab).
a_format(cpio).
a_format(empty).
a_format(gnutar).
a_format(iso9660).
a_format(lha).
a_format(mtree).
a_format(rar).
a_format(raw).
a_format(tar).
a_format(xar).
a_format(zip).

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

print_issue(Term) :-
	assertz(issue(Term)),
	print_message(warning, Term).

issue_url(Properties, URL) :-
	Local = Properties.get(url), !,
	issue_base(Base),
	atom_concat(Base, Local, URL).
issue_url(Properties, URL) :-
	Properties.get(source) = library(Segments), !,
	path_segments_atom(Segments, Base),
	file_name_extension(Base, html, URLFile),
	issue_base(Issues),
	atom_concat(Issues, URLFile, URL).

prolog:message(installation(checking)) -->
	{ current_prolog_flag(address_bits, Bits) },
	{ current_prolog_flag(arch, Arch) },
	{ current_prolog_flag(home, Home) },
	{ current_prolog_flag(cpu_count, Cores) },
	[ 'Checking your SWI-Prolog kit for common issues ...'-[], nl, nl ],
	[ 'Version: ~`.t~24| '-[] ], '$messages':prolog_message(version), [nl],
	[ 'Address bits: ~`.t~24| ~d'-[Bits] ], [nl],
	[ 'Architecture: ~`.t~24| ~w'-[Arch] ], [nl],
	[ 'Installed at: ~`.t~24| ~w'-[Home] ], [nl],
	[ 'Cores: ~`.t~24| ~w'-[Cores] ], [nl],
	[ nl ].
prolog:message(installation(perfect)) -->
	[ nl, 'Congratulations, your kit seems sound and complete!'-[] ].
prolog:message(installation(imperfect(N))) -->
	[ 'Found ~w issues.'-[N] ].
prolog:message(installation(checking(Feature))) -->
	[ 'Checking ~w ...'-[Feature], flush ].
prolog:message(installation(missing(Properties))) -->
	[ at_same_line, '~`.t~48| not present'-[] ],
	details(Properties).
prolog:message(installation(loading(Source))) -->
	[ 'Loading ~q ...'-[Source], flush ].
prolog:message(installation(ok)) -->
	[ at_same_line, '~`.t~48| ok'-[] ].
prolog:message(installation(optional_not_found(Properties))) -->
	[ 'Optional ~q ~`.t~48| not present'-[Properties.source] ].
prolog:message(installation(not_found(Properties))) -->
	[ '~q ~`.t~48| NOT FOUND'-[Properties.source] ],
	details(Properties).
prolog:message(installation(failed(Properties, false, []))) --> !,
	[ at_same_line, '~`.t~48| FAILED'-[] ],
	details(Properties).
prolog:message(installation(failed(Properties, exception(Ex0), []))) --> !,
	{ strip_stack(Ex0, Ex),
	  message_to_string(Ex, Msg) },
	[ '~w'-[Msg] ],
	details(Properties).
prolog:message(installation(failed(Properties, true, Messages))) --> !,
	[ at_same_line, '~`.t~48| FAILED'-[] ],
	explain(Messages),
	details(Properties).

strip_stack(error(Error, context(prolog_stack(S), Msg)),
	    error(Error, context(_, Msg))) :-
	nonvar(S).
strip_stack(Error, Error).

details(Properties) -->
	{ issue_url(Properties, URL), !
	},
	[ nl, 'See ~w'-[URL] ].
details(_) --> [].

explain(Messages) -->
	{ Messages = [message(error(shared_object(open, _Message), _), _, _)|_]
	}, !,
	[nl],
	(   { current_prolog_flag(windows, true) }
	->  [ 'Cannot load required DLL'-[] ]
	;   [ 'Cannot load required shared library'-[] ]
	).
explain(Messages) -->
	print_messages(Messages).

print_messages([]) --> [].
print_messages([message(_Term, _Kind, Lines)|T]) -->
	Lines, [nl],
	print_messages(T).

prolog:message(installation(archive(What, Names))) --> !,
	[ '  Supported ~w: '-[What] ],
	list_names(Names).

list_names([]) --> [].
list_names([H|T]) -->
	[ '~w'-[H] ],
	(   {T==[]}
	->  []
	;   [ ', '-[] ],
	    list_names(T)
	).

