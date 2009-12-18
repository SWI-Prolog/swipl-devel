/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University, Amsterdam

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

:- module(http_pwp,
	  [ reply_pwp_page/3,		% :File, +Options, +Request
	    pwp_handler/2		% +Options, +Request
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pwp)).

/** <module> Serve PWP pages through the HTTP server

This  module  provides  convience  predicates  to  include  PWP  (Prolog
Well-formed Pages) in a Prolog  web-server.   It  provides the following
predicates:

    * pwp_handler/2
    This is a complete web-server aimed at serving static pages, some
    of which include PWP.  This API is intended to allow for programming
    the web-server from a hierarchy of pwp files, prolog files and static
    web-pages.

    * reply_pwp_page/3
    Return a single PWP page that is executed in the context of the calling
    module.  This API is intended for individual pages that include so much
    text that generating from Prolog is undesirable.

@tbd	Support elements in the HTML header that allow controlling the
	page, such as setting the CGI-header, authorization, etc.
@tbd	Allow external styling.  Pass through reply_html_page/2?  Allow
	filtering the DOM before/after PWP?
*/

%%	pwp_handler(+Options, +Request)
%
%	Handle PWP files. This predicate is   defined to create a simple
%	HTTP server from a hierarchy of PWP,   HTML and other files. The
%	interface      is      kept      compatible        with      the
%	library(http/http_dispatch). In the typical  usage scenario, one
%	needs to define an http location and  a file-search path that is
%	used as the root of the server.  E.g., the following declarations
%	create a self-contained web-server for files in =|/web/pwp/|=.
%
%	    ==
%	    user:file_search_path(pwp, '/web/pwp').
%
%	    :- http_handler(root(.), pwp_handler([path_alias(pwp)]), [prefix]).
%	    ==
%
%	Options include:
%
%	    * path_alias(+Alias)
%	    Search for PWP files as Alias(Path).  See absolute_file_name/3.
%	    * index(+Index)
%	    Name of the directory index (pwp) file.  This option may
%	    appear multiple times.  If no such option is provided,
%	    pwp_handler/2 looks for =|index.pwp|=.
%	    * view(+Boolean)
%	    If =true= (default is =false=), allow for ?view=source to serve
%	    PWP file as source.
%	    * index_hook(:Hook)
%	    If a directory has no index-file, pwp_handler/2 calls
%	    Hook(PhysicalDir, Options, Request).  If this semidet
%	    predicate succeeds, the request is considered handled.
%
%	@see reply_pwp_page/3
%	@error permission_error(index, http_location, Location) is
%	raised if the handler resolves to a directory that has no
%	index.

:- meta_predicate
	pwp_handler(:, +).

pwp_handler(QOptions, Request) :-
	meta_options(is_meta, QOptions, Options),
	(   memberchk(path_info(Spec), Request)
	->  true
	;   Spec = '.'
	),
	(   option(path_alias(Alias), Options)
	->  Term =.. [Alias,Spec]
	;   Term = Spec
	),
	http_safe_file(Term, Options),
	absolute_file_name(Term, Path, [access(read)]),
	(   exists_directory(Path)
	->  ensure_slash(Path, Dir),
	    (	(   member(index(Index), Options)
		*-> true
		;   Index = 'index.pwp'
		),
		atom_concat(Dir, Index, File),
		access_file(File, read)
	    ->	true
	    ;	option(index_hook(Hook), Options),
	    	call(Hook, Path, Options, Request)
	    ->	true
	    ;	memberchk(path(Location), Request),
		permission_error(index, http_location, Location)
	    )
	;   File = Path
	),
	server_file(File, Request, Options).

is_meta(index_hook).

server_file(File, _, _) :-		% index-hook did the work
	var(File), !.
server_file(File, Request, Options) :-
	file_name_extension(_, pwp, File), !,
	(   option(view(true), Options),
	    memberchk(search(Query), Request),
	    memberchk(view=source, Query)
	->  http_reply_file(File, [ mime_type(text/plain),
				    unsafe(true)
				  ], Request)
	;   merge_options(Options,
			  [ pwp_module(true)
			  ], Opts),
	    reply_pwp_page(File, [unsafe(true)|Opts], Request)
	).
server_file(File, Request, Options) :-
	http_reply_file(File, Options, Request).


ensure_slash(Path, Dir) :-
	(   sub_atom(Path, _, _, 0, /)
	->  Dir = Path
	;   atom_concat(Path, /, Dir)
	).


%%	reply_pwp_page(:File, +Options, +Request)
%
%	Reply  a  PWP  file.  This  interface   is  provided  to  server
%	individual locations from PWP files.  Using   a  PWP file rather
%	than generating the page from Prolog   may  be desirable because
%	the page contains a lot of text (which is cumbersome to generate
%	from Prolog) or because the  maintainer   is  not  familiar with
%	Prolog.
%
%	Options supported are:
%
%	    * mime_type(+Type)
%	    Serve the file using the given mime-type.  Default is
%	    text/html.
%	    * unsafe(+Boolean)
%	    Passed to http_safe_file/2 to check for unsafe paths.
%	    * pwp_module(+Boolean)
%	    If =true=, (default =false=), process the PWP file in
%	    a module constructed from its canonical absolute path.
%	    Otherwise, the PWP file is processed in the calling
%	    module.
%
%	Initial context:
%
%	    * SCRIPT_NAME
%	    Virtual path of the script.
%	    * SCRIPT_DIRECTORY
%	    Physical directory where the script lives
%	    * QUERY
%	    Var=Value list representing the query-parameters
%	    * REMOTE_USER
%	    If access has been authenticated, this is the authenticated
%	    user.
%	    * REQUEST_METHOD
%	    One of =get=, =post=, =put= or =head=
%	    * CONTENT_TYPE
%	    Content-type provided with HTTP POST and PUT requests
%	    * CONTENT_LENGTH
%	    Content-length provided with HTTP POST and PUT requests
%
%	@tbd complete the initial context, as far as possible from CGI
%	     variables.  See http://hoohoo.ncsa.illinois.edu/docs/cgi/env.html
%	@see pwp_handler/2.

:- meta_predicate
	reply_pwp_page(:, +, +).

reply_pwp_page(M:File, Options, Request) :-
	http_safe_file(File, Options),
	absolute_file_name(File, Path,
			   [ access(read)
			   ]),
	memberchk(method(Method), Request),
	file_directory_name(Path, Dir),
	load_xml_file(Path, Contents),
	findall(C, pwp_context(Request, C), Context),
	(   option(pwp_module(true), Options)
	->  PWP_M = Path
	;   PWP_M = M
	),
	pwp_xml(PWP_M:Contents, Transformed,
		[ 'REQUEST_METHOD' = Method,
		  'SCRIPT_DIRECTORY' = Dir
		| Context
		]),
	option(mime_type(Type), Options, text/html),
	format('Content-type: ~w~n~n', [Type]),
	xml_write(current_output, Transformed, []).

pwp_context(Request, 'REMOTE_USER' = User) :-
	memberchk(user(User), Request).
pwp_context(Request, 'QUERY' = Query) :-
	memberchk(search(Query), Request).
pwp_context(Request, 'SCRIPT_NAME' = Path) :-
	memberchk(path(Path), Request).
pwp_context(Request, 'CONTENT_TYPE' = ContentType) :-
	memberchk(content_type(ContentType), Request).
pwp_context(Request, 'CONTENT_LENGTH' = Length) :-
	memberchk(content_length(Length), Request).
