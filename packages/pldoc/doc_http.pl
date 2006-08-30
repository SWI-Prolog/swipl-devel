/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_http,
	  [ doc_server/1,		% ?Port
	    doc_server/2,		% ?Port, +Options
	    doc_browser/0,
	    doc_browser/1		% +What
	  ]).
:- use_module(library(pldoc)).
:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/html_write')).
:- use_module(library('http/mimetype')).
:- use_module(library('debug')).
:- use_module(library('lists')).
:- use_module(pldoc(doc_process)).
:- use_module(pldoc(doc_html)).
:- use_module(pldoc(doc_index)).
:- use_module(pldoc(doc_search)).
:- use_module(pldoc(doc_man)).

/** <module> Documentation server

The module library(pldoc/http) provides an   embedded HTTP documentation
server that allows for browsing the   documentation  of all files loaded
_after_ library(pldoc) has been loaded.
*/

%%	doc_server(?Port) is det.
%%	doc_server(?Port, +Options) is det.
%
%	Start a documentation server in the  current Prolog process. The
%	server is started in a seperate   thread.  Options are handed to
%	http_server/2.  In  addition,   the    following   options   are
%	recognised:
%	
%		* allow(HostOrIP)
%		Allow connections from HostOrIP.  If HostOrIP is an atom
%		it is matched to the hostname.  It if starts with a .,
%		suffix match is done, matching the domain.  Finally it
%		can be a term ip(A,B,C,D). See tcp_host_to_address/2 for
%		details.
%
%		* deny(HostOrIP)
%		See allow(HostOrIP).
%	
%	The predicate doc_server/1 is defined as below, which provides a
%	good default for development.
%	
%	==
%	doc_server(Port) :-
%		doc_server(Port,
%			   [ workers(1),
%			     allow(localhost)
%			   ]).
%	==
%	
%	@see	doc_browser/1


doc_server(Port) :-
	doc_server(Port,
		   [ workers(1),
		     allow(localhost)
		   ]).

doc_server(Port, _) :-
	doc_current_server(Port), !.
doc_server(Port, Options) :-
	prepare_editor,
	auth_options(Options, ServerOptions),
	append(ServerOptions,		% Put provides options first,
	       [ port(Port),		% so they override our defaults
		 timeout(60),
		 keep_alive_timeout(1),
		 local(4000),		% keep stack sizes independent
		 global(4000),		% from main application
		 trail(4000)
	       ], HTTPOptions),
	http_server(doc_reply, HTTPOptions),
	print_message(informational, pldoc(server_started(Port))).

doc_current_server(Port) :-
	http_current_server(doc_reply, Port), !.

%%	doc_browser is det.
%%	doc_browser(+What) is semidet.
%
%	Open user's default browser on the documentation server.

doc_browser :-
	doc_browser([]).
doc_browser(Spec) :-
	doc_current_server(Port),
	browser_url(Spec, Request),
	format(string(URL), 'http://localhost:~w~w', [Port, Request]),
	www_open_url(URL).

browser_url([], '') :- !.
browser_url(Name/Arity, URL) :- !,
	format(string(S), '~q/~w', [Name, Arity]),
	www_form_encode(S, Enc),
	format(string(URL), '/man?predicate=~w', [Enc]).

%%	prepare_editor
%
%	Start XPCE as edit requests comming from the document server can
%	only be handled if XPCE is running.

prepare_editor :-
	current_prolog_flag(editor, pce_emacs), !,
	start_emacs.
prepare_editor.


doc_reply(Request) :-
	memberchk(peer(Peer), Request),
	(   allowed_peer(Peer)
	->  memberchk(path(Path), Request),
	    debug(pldoc, 'HTTP ~q', [Path]),
	    (	reply(Path, Request)
	    ->	true
	    ;	throw(http_reply(not_found(Path)))
	    )		      
	;   throw(http_reply(forbidden(/)))
	).

		 /*******************************
		 *	  ACCESS CONTROL	*
		 *******************************/

:- dynamic
	allow_from/1,
	deny_from/1.

%%	auth_options(+AllOptions, -NoAuthOptions) is det.
%
%	Filter the authorization options from   AllOptions,  leaving the
%	remaining options in NoAuthOptions.

auth_options([], []).
auth_options([H|T0], T) :-
	auth_option(H), !,
	auth_options(T0, T).
auth_options([H|T0], [H|T]) :-
	auth_options(T0, T).

auth_option(allow(From)) :-
	assert(allow_from(From)).
auth_option(deny(From)) :-
	assert(deny_from(From)).

%%	match_ip(+Spec, +Peer) is semidet.
%
%	True if Peer is covered by Spec.

match_ip(X, X).
match_ip(Name, IP) :-
	is_ip(IP),
	tcp_host_to_address(Host0, IP),
	downcase_atom(Host0, Host),
	(   Name == Host
	->  true
	;   sub_atom(Name, 0, _, _, '.'),
	    sub_atom(Host, _, _, 0, Name)
	).

is_ip(ip(A,B,C,D)) :-
	integer(A),
	integer(B),
	integer(C),
	integer(D).

%%	deny(+Peer) is semidet.
%%	allow(+Peer) is semidet.

deny(Peer) :-
	deny_from(From),
	match_ip(From, Peer), !.

allow(Peer) :-
	allow_from(From),
	match_ip(From, Peer), !.

allowed_peer(Peer) :-
	deny(Peer), !,
	allow(Peer).
allowed_peer(Peer) :-
	allow_from(_), !,
	allow(Peer).
allowed_peer(_).


%%	local(+Request) is semidet.
%
%	True if the request comes from localhost.

local(Request) :-
	memberchk(peer(Peer), Request),
	match_ip(localhost, Peer).


		 /*******************************
		 *	    USER REPLIES	*
		 *******************************/

:- discontiguous
	reply/2.

%	/
%	
%	Reply using the index-page  of   the  Prolog  working directory.
%	There are various options for the   start directory. For example
%	we could also use the file or   directory of the file that would
%	be edited using edit/0.

reply(/, _) :-
	working_directory(Dir0, Dir0),
	ensure_slash_end(Dir0, Dir1),
	doc_file_href(Dir1, Ref0),
	atom_concat(Ref0, 'index.html', Index),
	throw(http_reply(moved(Index))).

ensure_slash_end(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash_end(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).

%	/file?file=REF
%	
%	Reply using documentation of file

reply('/file', Request) :-
	http_parameters(Request,
			[ file(File, [])
			]),
	(   source_file(File)
	->  true
	;   throw(http_reply(forbidden(File)))
	),
	format('Content-type: text/html~n~n'),
	doc_for_file(File, current_output, []).

%	/edit?file=REF
%	
%	Start SWI-Prolog editor on file

reply('/edit', Request) :-
	local(Request), !,
	http_parameters(Request,
			[ file(File,     [optional(true)]),
			  module(Module, [optional(true)]),
			  name(Name,     [optional(true)]),
			  arity(Arity,   [integer, optional(true)])
			]),
	(   atom(File)
	->  Edit = file(File)
	;   atom(Name), integer(Arity)
	->  (   atom(Module)
	    ->	Edit = (Module:Name/Arity)
	    ;	Edit = (Name/Arity)
	    )
	),
	format(string(Cmd), '~q', [edit(Edit)]),
	edit(Edit),
	reply_page('Edit',
		   [ p(['Started ', Cmd])
		   ]).
reply('/edit', _Request) :-
	throw(http_reply(forbidden('/edit'))).


%	/directory?dir=Dir
%	
%	Give index of directory

reply('/directory', Request) :-
	http_parameters(Request,
			[ dir(Dir, [])
			]),
	(   source_directory(Dir)
	->  edit_options(Request, EditOptions),
	    format('Content-type: text/html~n~n'),
	    doc_for_dir(Dir, current_output, EditOptions)
	;   throw(http_reply(forbidden(Dir)))
	).


%	/doc/Path
%	
%	Reply documentation of file. Path is   the  absolute path of the
%	file for which to return the  documentation. Extension is either
%	none, the Prolog extension or the HTML extension.
%	
%	Note that we reply  with  pldoc.css   if  the  file  basename is
%	pldoc.css to allow for a relative link from any directory.

reply(ReqPath, Request) :-
	atom_concat('/doc', AbsFile0, ReqPath),
	(   sub_atom(ReqPath, _, _, 0, /)
	->  atom_concat(ReqPath, 'index.html', File),
	    throw(http_reply(moved(File)))
	;   clean_path(AbsFile0, AbsFile),
	    is_absolute_file_name(AbsFile)
	->  documentation(AbsFile, Request)
	).

documentation(Path, _Request) :-
	file_base_name(Path, 'pldoc.css'), !,
	reply_file(pldoc('pldoc.css')).
documentation(Path, Request) :-
	Index = '/index.html',
	sub_atom(Path, _, _, 0, Index), 
	atom_concat(Dir, Index, Path),
	exists_directory(Dir), !,		% Directory index
	(   source_directory(Dir)
	->  edit_options(Request, EditOptions),
	    format('Content-type: text/html~n~n'),
	    doc_for_dir(Dir, current_output, EditOptions)
	;   throw(http_reply(forbidden(Dir)))
	).
documentation(Path, Request) :-
	http_parameters(Request,
			[ public_only(Public),
			  reload(Reload)
			],
			[ attribute_declarations(param)
			]),
	pl_file(Path, File),
	(   Reload == true
	->  load_files(File, [if(changed)])
	;   true
	),
	edit_options(Request, EditOptions),
	format('Content-type: text/html~n~n'),
	doc_for_file(File, current_output,
		     [ public_only(Public)
		     | EditOptions
		     ]).


%%	edit_options(+Request, -Options) is det.
%
%	Return edit(true) in Options  if  the   connection  is  from the
%	localhost.

edit_options(Request, [edit(true)]) :-
	local(Request), !.
edit_options(_, []).


%%	pl_file(+File, -PlFile) is det.
%
%	@error existence_error(file, File)

pl_file(File, PlFile) :-
	file_name_extension(Base, html, File), !,
	absolute_file_name(Base,
			   [ file_type(prolog),
			     access(read)
			   ], PlFile).
pl_file(File, File).


%%	clean_path(+AfterDoc, -AbsPath)
%
%	Restore the path, Notably deals Windows issues

clean_path(Path0, Path) :-
	current_prolog_flag(windows, true),
	sub_atom(Path0, 2, _, _, :), !,
	sub_atom(Path0, 1, _, 0, Path).
clean_path(Path, Path).


%	/pldoc.css
%	
%	Reply the documentation style-sheet.

reply(Path, _Request) :-
	file(Path, LocalFile),
	reply_file(pldoc(LocalFile)).

file('/pldoc.css',   'pldoc.css').
file('/pldoc.js',    'pldoc.js').
file('/edit.gif',    'edit.gif').
file('/up.gif',      'up.gif').
file('/zoomin.gif',  'zoomin.gif').
file('/zoomout.gif', 'zoomout.gif').
file('/reload.gif',  'reload.gif').
file('/favicon.ico', 'favicon.ico').


%	/man?predicate=PI
%	
%	Provide documentation from the manual.
%	
%	@tbd	Make link to reference manual.

reply('/man', Request) :-
	http_parameters(Request,
			[ predicate(PI, [])
			]),
	format(string(Title), 'Manual -- ~w', [PI]),
	reply_page(Title,
		   [ \man_page(PI, [])
		   ]).

%	/doc_for?object=Term
%	
%	Provide documentation for the given term

reply('/doc_for', Request) :-
	http_parameters(Request,
			[ object(Atom, [])
			]),
	atom_to_term(Atom, Obj, _),
	(   prolog:doc_object_title(Obj, Title)
	->  true
	;   Title = Atom
	),
	edit_options(Request, EditOptions),
	reply_page(Title,
		   [ \object_page(Obj, EditOptions)
		   ]).


%	/search?for=String
%	
%	Search for String

reply('/search', Request) :-
	http_parameters(Request,
			[ for(For, [length > 1]),
			  in(In,
			     [ oneof([all,app,man]),
			       default(all)
			     ]),
			  match(Match,
				[ oneof([name,summary]),
				  default(summary)
				]),
			  resultFormat(Format, [ oneof(long,summary),
						 default(summary)
					       ])
			]),
	edit_options(Request, EditOptions),
	format(string(Title), 'Prolog search -- ~w', [For]),
	reply_page(Title,
		   [ \search_reply(For,
				   [ resultFormat(Format),
				     search_in(In),
				     search_match(Match)
				   | EditOptions
				   ])
		   ]).

%	/package/Name
%	
%	Show documentation file of a package.  Exploits the file
%	search path =package_documentation=.

reply(Path, _Request) :-
	atom_concat('/package/', Package, Path), !,
	absolute_file_name(package_documentation(Package),
			   DocFile,
			   [ access(read),
			     file_errors(fail)
			   ]),
	reply_file(DocFile).



		 /*******************************
		 *	       UTIL		*
		 *******************************/

reply_page(Title, Content) :-
	doc_page_dom(Title, Content, DOM),
	phrase(html(DOM), Tokens),
	format('Content-type: text/html~n~n'),
	print_html_head(current_output),
	print_html(Tokens).

reply_file(File) :-
	absolute_file_name(File, Path, [access(read)]),
	file_mime_type(Path, MimeType),
	throw(http_reply(file(MimeType, Path))).


		 /*******************************
		 *     HTTP PARAMETER TYPES	*
		 *******************************/

param(public_only,
      [ oneof([true,false]),
	default(true)
      ]).
param(reload,
      [ oneof([true,false]),
	default(false)
      ]).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(pldoc(server_started(Port))) -->
	[ 'Started Prolog Documentatiuon server at port ~w'-[Port], nl,
	  'You may access the server at http://localhost:~w/'-[Port]
	].


                 /*******************************
                 *        PCEEMACS SUPPORT      *
                 *******************************/

:- multifile
        emacs_prolog_colours:goal_colours/2,
        prolog:called_by/2.


emacs_prolog_colours:goal_colours(reply_page(_, HTML),
                                  built_in-[classify, Colours]) :-
        catch(html_write:html_colours(HTML, Colours), _, fail).

prolog:called_by(reply_page(_, HTML), Called) :-
        catch(phrase(html_write:called_by(HTML), Called), _, fail).
