/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
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
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/dcg_basics)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_hook)).
:- use_module(library(http/http_path)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(www_browser)).
:- use_module(pldoc(doc_process)).
:- use_module(pldoc(doc_htmlsrc)).
:- use_module(pldoc(doc_html)).
:- use_module(pldoc(doc_index)).
:- use_module(pldoc(doc_search)).
:- use_module(pldoc(doc_man)).
:- use_module(pldoc(doc_wiki)).
:- use_module(pldoc(doc_util)).
:- use_module(pldoc(doc_access)).

/** <module> Documentation server

The module library(pldoc/http) provides an   embedded HTTP documentation
server that allows for browsing the   documentation  of all files loaded
_after_ library(pldoc) has been loaded.
*/

:- dynamic
	doc_server_port/1.

http:location(pldoc, root(.), []).
http:location(pldoc_man, pldoc(refman), []).
http:location(pldoc_pkg, pldoc(package), []).
http:location(pldoc_resource, Path, []) :-
	http_location_by_id(pldoc_resource, Path).


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
%		* edit(Bool)
%		Allow editing from localhost connections? Default:
%		=true=.
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
		     allow(localhost),
		     allow(ip(127,0,0,1)) % Windows ip-->host often fails
		   ]).

doc_server(Port, _) :-
	catch(doc_current_server(Port), _, fail), !.
doc_server(Port, Options) :-
	prepare_editor,
	host_access_options(Options, ServerOptions),
	append(ServerOptions,		% Put provides options first,
	       [ port(Port),		% so they override our defaults
		 timeout(60),
		 keep_alive_timeout(1),
		 local(4000),		% keep stack sizes independent
		 global(4000),		% from main application
		 trail(4000)
	       ], HTTPOptions),
	http_server(http_dispatch, HTTPOptions),
	assert(doc_server_port(Port)),
	print_message(informational, pldoc(server_started(Port))).

%%	doc_current_server(-Port) is det.
%
%	TCP/IP port of the documentation server. Fails if no server is
%	running.
%	
%	@tbd	Trap destruction of the server.
%	@error	existence_error(http_server, pldoc)

doc_current_server(Port) :-
	(   doc_server_port(P)
	->  Port = P
	;   existence_error(http_server, pldoc)
	).

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

browser_url([], Root) :- !,
	http_location_by_id(pldoc_root, Root).
browser_url(Name, URL) :-
	atom(Name), !,
	browser_url(Name/_, URL).
browser_url(Name//Arity, URL) :-
	must_be(atom, Name),
	integer(Arity), !,
	PredArity is Arity+2,
	browser_url(Name/PredArity, URL).
browser_url(Name/Arity, URL) :- !,
	must_be(atom, Name),
	(   predicate(Name, Arity, _, _, _)
	->  http_location_by_id(pldoc_man, ManLoc),
	    format(string(S), '~q/~w', [Name, Arity]),
	    www_form_encode(S, Enc),
	    format(string(URL), '~w?predicate=~w', [ManLoc, Enc])
	;   browser_url(_:Name/Arity, URL)
	).
browser_url(Spec, URL) :- !,
	Spec = M:Name/Arity,
	doc_comment(Spec, _Pos, _Summary, _Comment), !,
	http_location_by_id(pldoc_object, ObjLoc),
	(   var(M)
	->  format(string(S), '~q/~w', [Name, Arity])
	;   format(string(S), '~q:~q/~w', [M, Name, Arity])
	),
	www_form_encode(S, Enc),
	format(string(URL), '~w?object=~w', [ObjLoc, Enc]).

%%	prepare_editor
%
%	Start XPCE as edit requests comming from the document server can
%	only be handled if XPCE is running.

prepare_editor :-
	current_prolog_flag(editor, pce_emacs), !,
	start_emacs.
prepare_editor.


		 /*******************************
		 *	    USER REPLIES	*
		 *******************************/

:- http_handler(pldoc(.),	   pldoc_root,
		[prefix, authentication(pldoc(read))]).
:- http_handler(pldoc('index.html'), pldoc_index,   []).
:- http_handler(pldoc(file),	   pldoc_file,	   []).
:- http_handler(pldoc(directory),  pldoc_dir,	   []).
:- http_handler(pldoc(edit),	   pldoc_edit,
		[authentication(pldoc(edit))]).
:- http_handler(pldoc(doc),	   pldoc_doc,	   [prefix]).
:- http_handler(pldoc(man),	   pldoc_man,	   []).
:- http_handler(pldoc(doc_for),	   pldoc_object,   []).
:- http_handler(pldoc(search),	   pldoc_search,   []).
:- http_handler(pldoc('res/'),	   pldoc_resource, [prefix]).


%%	pldoc_root(+Request)
%	
%	Reply using the index-page  of   the  Prolog  working directory.
%	There are various options for the   start directory. For example
%	we could also use the file or   directory of the file that would
%	be edited using edit/0.

pldoc_root(Request) :-
	http_parameters(Request,
			[ empty(Empty, [ oneof([true,false]),
					 default(false)
				       ])
			]),
	pldoc_root(Request, Empty).

pldoc_root(Request, false) :-
	http_location_by_id(pldoc_root, Root),
	memberchk(path(Path), Request),
	Root \== Path, !,
	existence_error(http_location, Path).
pldoc_root(_Request, false) :-
	working_directory(Dir0, Dir0),
	allowed_directory(Dir0), !,
	ensure_slash_end(Dir0, Dir1),
	doc_file_href(Dir1, Ref0),
	atom_concat(Ref0, 'index.html', Index),
	throw(http_reply(see_other(Index))).
pldoc_root(Request, _) :-
	pldoc_index(Request).


%%	pldoc_index(+Request)
%
%	HTTP handle for /index.html, providing an overall overview
%	of the available documentation.

pldoc_index(_Request) :-
	reply_html_page(title('SWI-Prolog documentation'),
			[ \doc_links('', []),
			   h1('SWI-Prolog documentation'),
			  \man_overview([])
			]).


%%	pldoc_file(+Request)
%
%	Hander for /file?file=File, providing documentation for File.

pldoc_file(Request) :-
	http_parameters(Request,
			[ file(File, [])
			]),
	(   source_file(File)
	->  true
	;   throw(http_reply(forbidden(File)))
	),
	doc_for_file(File, []).

%%	pldoc_edit(+Request)
%
%	Handler for /edit?file=REF, starting the   SWI-Prolog  editor on
%	File.

pldoc_edit(Request) :-
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
	reply_html_page(title('Edit'),
			p(['Started ', Cmd])).
pldoc_edit(_Request) :-
	throw(http_reply(forbidden('/edit'))).


%%	pldoc_dir(+Request)
%
%	Handler for /directory?dir=Dir, providing an index for
%	Dir.  Mapped to /doc/Dir/index.html.

pldoc_dir( Request) :-
	http_parameters(Request,
			[ dir(Dir0, [])
			]),
	expand_alias(Dir0, Dir),
	(   allowed_directory(Dir)
	->  format(string(IndexFile), '~w/index.html', [Dir]),
	    doc_file_href(IndexFile, HREF),
	    throw(http_reply(moved(HREF)))
	;   throw(http_reply(forbidden(Dir)))
	).


%%	allowed_directory(+Dir) is semidet.
%
%	True if we are allowed to produce and index for Dir.

allowed_directory(Dir) :-
	source_directory(Dir), !.
allowed_directory(Dir) :-
	working_directory(CWD, CWD),
	same_file(CWD, Dir).


%%	allowed_file(+File) is semidet.
%
%	True if we are allowed to serve   File.  Currently means we have
%	predicates loaded from File or the directory must be allowed.

allowed_file(File) :-
	source_file(_, File), !.
allowed_file(File) :-
	absolute_file_name(File, Canonical),
	file_directory_name(Canonical, Dir),
	allowed_directory(Dir).


%%	pldoc_resource(+Request)
%
%	Handler for /res/File, serving CSS, JS and image files.

pldoc_resource(Request) :-
	http_location_by_id(pldoc_resource, ResRoot),
	memberchk(path(Path), Request),
	atom_concat(ResRoot, File, Path),
	file(File, Local),
	http_reply_file(pldoc(Local), [], Request).

file('pldoc.css',     'pldoc.css').
file('pllisting.css', 'pllisting.css').
file('pldoc.js',      'pldoc.js').
file('edit.gif',      'edit.gif').
file('up.gif',        'up.gif').
file('source.gif',    'source.gif').
file('zoomin.gif',    'zoomin.gif').
file('zoomout.gif',   'zoomout.gif').
file('reload.gif',    'reload.gif').
file('favicon.ico',   'favicon.ico').


%%	pldoc_doc(+Request)
%	
%	Handler for /doc/Path
%	
%	Reply documentation of a file. Path is  the absolute path of the
%	file for which to return the  documentation. Extension is either
%	none, the Prolog extension or the HTML extension.
%	
%	Note that we reply  with  pldoc.css   if  the  file  basename is
%	pldoc.css to allow for a relative link from any directory.

pldoc_doc(Request) :-
	memberchk(path(ReqPath), Request),
	http_location_by_id(pldoc_doc, Me),
	atom_concat(Me, AbsFile0, ReqPath),
	(   sub_atom(ReqPath, _, _, 0, /)
	->  atom_concat(ReqPath, 'index.html', File),
	    throw(http_reply(moved(File)))
	;   clean_path(AbsFile0, AbsFile1),
	    expand_alias(AbsFile1, AbsFile),
	    is_absolute_file_name(AbsFile)
	->  documentation(AbsFile, Request)
	).

documentation(Path, Request) :-
	file_base_name(Path, Base),
	file(_, Base), !,			% serve pldoc.css, etc.
	http_reply_file(pldoc(Base), [], Request).
documentation(Path, Request) :-
	Index = '/index.html',
	sub_atom(Path, _, _, 0, Index), 
	atom_concat(Dir, Index, Path),
	exists_directory(Dir), !,		% Directory index
	(   allowed_directory(Dir)
	->  edit_options(Request, EditOptions),
	    doc_for_dir(Dir, EditOptions)
	;   throw(http_reply(forbidden(Dir)))
	).
documentation(File, _Request) :-
	(   file_name_extension(_, txt, File)
	;   file_base_name(File, Base),
	    autolink_file(Base, wiki)
	),
	(   allowed_file(File)
	->  true
	;   throw(http_reply(forbidden(File)))
	),
	doc_for_wiki_file(File, []).
documentation(Path, Request) :-
	http_parameters(Request,
			[ public_only(Public),
			  reload(Reload),
			  source(Source)
			],
			[ attribute_declarations(param)
			]),
	pl_file(Path, File),
	(   allowed_file(File)
	->  true
	;   throw(http_reply(forbidden(File)))
	),
	(   Reload == true
	->  load_files(File, [if(changed), imports([])])
	;   true
	),
	edit_options(Request, EditOptions),
	(   Source == true
	->  format('Content-type: text/html~n~n', []),
	    source_to_html(File, stream(current_output), [])
	;   doc_for_file(File,
			 [ public_only(Public)
			 | EditOptions
			 ])
	).


%%	edit_options(+Request, -Options) is det.
%
%	Return edit(true) in Options  if  the   connection  is  from the
%	localhost.

edit_options(Request, [edit(true)]) :-
	catch(http:authenticate(pldoc(edit), Request, _), _, fail), !.
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


%%	pldoc_man(+Request)
%
%	Handler for /man?predicate=PI, providing  documentation from the
%	manual on the predicate PI.

pldoc_man(Request) :-
	http_parameters(Request,
			[ predicate(PI, [])
			]),
	format(string(Title), 'Manual -- ~w', [PI]),
	reply_html_page(title(Title),
			\man_page(PI, [])).

%%	pldoc_object(+Request)
%
%	Handler for /doc_for?object=Term, Provide  documentation for the
%	given term.

pldoc_object(Request) :-
	http_parameters(Request,
			[ object(Atom, [])
			]),
	atom_to_term(Atom, Obj, _),
	(   prolog:doc_object_title(Obj, Title)
	->  true
	;   Title = Atom
	),
	edit_options(Request, EditOptions),
	reply_html_page(title(Title),
			\object_page(Obj, EditOptions)).


%%	pldoc_search(+Request)
%
%	Handler for /search?for=String, searching for String.

pldoc_search(Request) :-
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
	reply_html_page(title(Title),
			\search_reply(For,
				      [ resultFormat(Format),
					search_in(In),
					search_match(Match)
				      | EditOptions
				      ])).


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
param(source,
      [ oneof([true,false]),
	default(false)
      ]).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(pldoc(server_started(Port))) -->
	{ http_location_by_id(pldoc_root, Root) },
	[ 'Started Prolog Documentation server at port ~w'-[Port], nl,
	  'You may access the server at http://localhost:~w~w'-[Port, Root]
	].
