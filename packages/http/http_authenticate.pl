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


:- module(authenticate,
	  [ http_authenticate/3		% +Check, +Header, -User
	  ]).
:- use_module(library(base64)).
:- use_module(library('http/dcg_basics')).
:- use_module(library(readutil)).
:- use_module(library(crypt)).
:- use_module(library(debug)).

/**	<module> Authenticate HTTP connections using 401 headers

This module provides the basics  to   validate  an  HTTP =Authorization=
error. User and  password  information  are   read  from  a  Unix/Apache
compatible password file. This information, as   well  as the validation
process is cached to achieve optimal performance.

@author	Jan Wielemaker
*/

%%	http_authenticate(+Type, +Request, -Fields)
%
%	True if Request contains the   information to continue according
%	to Type. Type identifies the required authentication technique:
%	
%		* basic(+PasswordFile)
%		Use HTTP =Basic= authetication and verify the password
%		from PasswordFile. PasswordFile is a file holding
%		usernames and passwords in a format compatible to
%		Unix and Apache. Each line is record with =|:|=
%		separated fields. The first field is the username and
%		the second the password _hash_.  Password hashes are
%		validated using crypt/2.
%		
%	Successful authorization is  cached  for   60  seconds  to avoid
%	overhead of decoding and lookup of the user and password data.
%	
%	http_authenticate/3 just validates the  header. If authorization
%	is not provided the browser must   be challenged, in response to
%	which it normally opens a   user-password dialogue. Example code
%	realising this is below. The exception   causes the HTTP wrapper
%	code to generate an HTTP 401 reply.
%	
%	==
%	(   http_authenticate(basic(passwd), Request, Fields)
%	->  true
%	;   throw(http_reply(authorise(basic, Realm)))
%	).
%	==
%	
%	@tbd	Should we also cache failures to reduce the risc of
%		DoS attacks?

http_authenticate(basic(File), Request, [user(User)]) :-
	memberchk(authorization(Text), Request),
	debug(http_authenticate, 'Authorization: ~w', [Text]),
	(   cached_authenticated(Text, File, User)
	->  true
	;   user_and_passwd(Text, Method, UserChars, Password),
	    downcase_atom(Method, basic),
	    debug(http_authenticate,
		  'User: ~s, Password: ~s', [UserChars, Password]),
	    atom_codes(User, UserChars),
	    validate(File, User, Password),
	    get_time(Now),
	    assert(authenticated(Text, File, User, Now)),
	    debug(http_authenticate, 'Authenticated ~w~n', [User])
	).

%%	user_and_passwd(+AuthorizeText, -Method, -User, -Password) is det.
%
%	Decode the HTTP =Authorization= header.

user_and_passwd(Text, Method, User, Password) :-
	atom_codes(Text, Codes),
	phrase(authorization(Method, Cookie), Codes),
	phrase(base64(UserPwd), Cookie),
	phrase(ident(User, Password), UserPwd).
	
authorization(Method, Cookie) -->
	nonblanks(MethodChars),
	{ atom_codes(Method, MethodChars)
	},
	blanks,
	nonblanks(Cookie),
	blanks.

ident(User, Password) -->
	string(User),
	":",
	string(Password).

%%	cached_authenticated(+Authorization, +File, -User)
%
%	Validate using the cache. If the entry   is not in the cache, we
%	also remove all outdated entries from the cache.

:- dynamic
	authenticated/4.		% Authorization, File, User, Time

cached_authenticated(Authorization, File, User) :-
	authenticated(Authorization, File, User, Time),
	get_time(Now),
	Now-Time =< 60, !.		% 60-second timeout
cached_authenticated(_, _, _) :-
	get_time(Now),
	(   clause(authenticated(_, _, _, Time), true, Ref),
	    Now-Time > 60,
	    erase(Ref),
	    fail
	).


%%	validate(+File, +User, +Passwd)
%
%	True if User and Passwd combination   appears in File. File uses
%	the same format as .htaccess files  from Apache or Unix password
%	files. I.e. it consists  of  one   line  per  entry  with fields
%	separated by =|:|=. The  first  field   is  the  User field, The
%	second contains the Passwd in DES   or MD5 encrypted format. See
%	crypt/2 for details.

validate(File, User, Password) :-
	update_passwd(File, Path),
	passwd(User, Path, Hash),
	crypt(Password, Hash).

%%	update_passwd(+File, -Path) is det.
%
%	Update passwd/3 to reflect the correct  passwords for File. Path
%	is the absolute path for File.

:- dynamic
	passwd/3,			% User, File, Encrypted
	last_modified/2.		% File, Stamp

update_passwd(File, Path) :-
	absolute_file_name(File, Path, [access(read)]),
	time_file(Path, Stamp),
	(   last_modified(Path, Stamp)
	->  true
	;   with_mutex(http_passwd, reload_passwd_file(Path, Stamp))
	).

reload_passwd_file(Path, Stamp) :-
	last_modified(Path, Stamp), !.	% another thread did the work
reload_passwd_file(Path, Stamp) :-
	retractall(last_modified(Path, _)),
	retractall(passwd(_, Path, _)),
	open(Path, read, Fd),
	read_line_to_codes(Fd, Line),
	read_passwd_file(Line, Fd, Path),
	close(Fd),
	assert(last_modified(Path, Stamp)).

read_passwd_file(end_of_file, _, _) :- !.
read_passwd_file(Line, Fd, Path) :-
	(   phrase(password_line(User, Hash), Line, _)
	->  assert(passwd(User, Path, Hash))
	;   true			% TBD: warning
	),
	read_line_to_codes(Fd, Line2),
	read_passwd_file(Line2, Fd, Path).
	

password_line(User, Hash) -->
	string(UserCodes),
	":",
	string(HashCodes),
	(   ":"
	;   eos
	), !,
	{ atom_codes(User, UserCodes),
	  atom_codes(Hash, HashCodes)
	}.


		 /*******************************
		 *   PLUGIN FOR HTTP_DISPATCH   *
		 *******************************/

:- multifile
	http:authenticate/3.

http:authenticate(basic(File, Realm), Request, User) :-
	(   http_authenticate(basic(File), Request, User)
	->  true
	;   throw(http_reply(authorise(basic, Realm)))
	).

