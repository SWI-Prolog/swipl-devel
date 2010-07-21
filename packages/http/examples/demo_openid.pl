/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007-2010, University of Amsterdam,
			      VU University Amsterdam

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


:- asserta(file_search_path(library, '..')).

:- use_module(library(uri)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).

http:location(openid, root(openid), []).

:- multifile
	http_openid:openid_hook/1.

http_openid:openid_hook(trusted(_OpenID, Server)) :-
	debug(openid(test), 'Trusting server ~q', [Server]).


%%	server
%
%	Create demo server  and  client.   After  starting  the  server,
%	contact http://localhost:8000/

server :-
	debug(openid(_)),
	Port = 8000,
	http_server(http_dispatch,
		    [ port(Port)
		    ]),
	debug(openid(test), 'Server started at http://localhost:~w/', [Port]).


assoc :-
	openid_associate('http://localhost:8000/openid/server', Handle, Assoc),
	writeln(Handle-Assoc).

%%	secret(+Request) is det.
%
%	Example of a handler that requires an  OpenID login. If the user
%	is not logged it, it will be  redirected to the login page, from
%	there to the OpenID server and back here. All this is completely
%	transparent to us.

:- http_handler(root('secret'), secret, []).

secret(Request) :-
	openid_user(Request, User, []),
	reply_html_page(title('Secret'),
			[ 'You\'ve reached the secret page as user ', %'
			  a(href(User), User)
			]).

%%	root(+Request).
%%	allow(+Request).
%
%	Shows an indirect login.

:- http_handler(root(.),	     root,				[]).
:- http_handler(root('test/verify'), openid_verify([return_to(allow)]),	[]).
:- http_handler(root('test/allow'),  allow,				[]).

root(_Request) :-
	reply_html_page(title('Demo OpenID consumer'),
			[ h1('OpenID consumer'),
			  form([ name(login),
				 action('/test/verify'),
				 method('GET')
			       ],
			       [ div([ 'OpenID: ',
				       input([ name(openid_url),
					       size(40),
					       value('http://localhost:8000/user/bob') % test
					     ]),
				       input([type(submit), value('Verify!')])
				     ])
			       ]),
			  p([ 'Or go directly to the ', a(href=secret, 'secret page') ])
			]).


allow(Request) :-
	openid_authenticate(Request, Server, Identity, _ReturnTo),
	reply_html_page(title('Success'),
			[ h1('OpenID login succeeded'),
			  p([ 'The OpenID server ',
			      a(href(Server),Server),
			      ' verified you as ',
			      a(href(Identity), Identity)
			    ])
			]).


		 /*******************************
		 *	   OpenID SERVER	*
		 *******************************/

:- http_handler(root('user/'),	user_page,	   [prefix]).
:- http_handler(openid(server),	openid_server([]), []).
:- http_handler(openid(grant),	openid_grant, []).

:- multifile
	http_openid:openid_hook/1.

http_openid:openid_hook(grant(_Request, Options)) :-
	debug(openid(test), 'Granting access to ~p', [Options]).

%%	user_page(+Request) is det.
%
%	Generate a page for user as /user/<user>.

user_page(Request) :-
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	http_location_by_id(openid_server, ServerLocation),
	uri_authority_data(host, AComp, Host),
	uri_authority_data(port, AComp, Port),
	uri_authority_components(Authority, AComp),
	uri_data(scheme, Components, http),
	uri_data(authority, Components, Authority),
	uri_data(path, Components, ServerLocation),
	uri_components(OpenIDServer, Components),
	memberchk(path_info(User), Request),
	reply_html_page([ link([ rel('openid.server'),
				 href(OpenIDServer)
			       ]),
			  title('OpenID page of ~w'-[User])
			],
			h1('OpenID page of ~w'-[User])).


		 /*******************************
		 *		DEBUG		*
		 *******************************/

:- http_handler(root(.), print_request, [prefix]).

print_request(Request) :-
	format('Content-type: text/plain~n~n'),
	pp(Request).
