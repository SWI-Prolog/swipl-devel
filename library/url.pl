/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module(url,
	  [ parse_url/2,		% +URL, -Parts | -URL +Parts
	    parse_url/3,		% +URL|URI, +BaseURL, -Parts
	    				% -URL, +BaseURL, +Parts
	    is_absolute_url/1,		% +URL
	    global_url/3,		% +Local, +Base, -Global
	    http_location/2,		% ?Parts, ?Location
	    www_form_encode/2,		% Value <-> Encoded
	    parse_url_search/2		% Form-data <-> Form fields
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Utility library to break down URL  specifications. This library is based
on RFC-1738, specifying  the  URL  syntax   with  a  few  commonly  used
extensions:

	* Allow for ~ character in path-names.
	* Assuming http as default protocol.
	* Allow for file:<path>

Public interface:

parse_url(+URL, -Parts)
    Parse a URL into its parts.  Parts is a list of Name(Value).  Attributes
    depend on the protocol.  Currently the implementation is not defined
    for all protocols.  Defined:

        protocol	All protocols
	host		Network based protocols
	port		Network based protocols
	path		file,http,ftp
	search		http (?name=value&...)
        fragment	http (#fragment)

    For example:

    ?- parse_url('http://swi.psy.uva.nl/message.cgi?msg=Hello+World%21', P).

    P = [ protocol(http), 
	  search([msg='Hello World!']), 
	  path('/message.cgi'), 
	  host('swi.psy.uva.nl')
        ] 

parse_url(-URL, +Parts)
    Create a URL from its parts (see above). 	

parse_url(+URL, +BaseURL, -Parts)
    As above, but `URL' may be relative to some base url.  If URL is
    absolute, BaseURL is ignored.

parse_url(-URL, +BaseURL, +Parts)
    Construct absolute URL from its parts and a base url, with default
    parts taken from BaseURL. 

global_url(+URL, +BaseURL, -AbsoluteUrl)
    Transform a possible local URL into a global one.

http_location(?Parts, ?Location)
    If `Parts' is the part-list of an http URL, construct a string for
    the location.  So, to access a page with an HTTP url, do:

    	parse_url(URL, Parts),
	memberchk(host(Host), Parts),
	(   memberchk(port(Port), Parts)
	->  true
	;   Port = 80
	),
	<connect to host and port>
	http_location(Parts, Location),
	format(Stream, 'GET ~w HTTP/1.0\r\nHost: ~w\r\n\r\n',
	       [ Location, Host ]),
	...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      GLOBALISE		*
		 *******************************/

%	global_url(+URL, +Base, -Global)
%	
%	Translate a relative URL into an absolute one.  The first three
%	cases deal with commonly seen and quickly to resolve cases.

global_url(URL, BaseURL, Global) :-
	(   is_absolute_url(URL),
	    \+ sub_atom(URL, _, _, _, '%')	% may have escape, use general
	->  Global = URL
	;   sub_atom(URL, 0, _, _, '//')
	->  parse_url(BaseURL, [], Attributes),
	    memberchk(protocol(Proto), Attributes),
	    concat_atom([Proto, :, URL], Global)
	;   sub_atom(URL, 0, _, _, #)
	->  (   sub_atom(BaseURL, _, _, 0, #)
	    ->	sub_atom(URL, 1, _, 0, NoHash),
		atom_concat(BaseURL, NoHash, Global)
	    ;	atom_concat(BaseURL, URL, Global)
	    )
	;   parse_url(URL, BaseURL, Attributes),
	    phrase(curl(Attributes), Chars),
	    atom_codes(Global, Chars)
	).

%	is_absolute_url(+URL)
%	
%	Test whether a URL is absolute or relative.  We assume it is
%	absolute if it starts with a protocol.

is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'http://'), !.
is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'ftp://'), !.
is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'file:'), !.
is_absolute_url(URL) :-
	atom_codes(URL, Codes),
	phrase(absolute_url, Codes, _), !.


		 /*******************************
		 *	  CREATE URL/URI	*
		 *******************************/

%	http_location(?Parts, ?Location)
%
%	Translate the relevant parts of an URL into an HTTP location
%	on a server.

http_location(Parts, Location) :-	% Parts --> Location
	nonvar(Parts), !,
	phrase(curi(Parts), String), !,
	atom_codes(Location, String).
http_location(Parts, Location) :-	% Location --> Parts
	atom(Location), !,
	atom_codes(Location, Codes),
	phrase(http_location(Parts), Codes).
http_location(Parts, Codes) :-		% LocationCodes --> Parts
	is_list(Codes),
	phrase(http_location(Parts), Codes).


curl(A0) -->
	{ select(protocol(Protocol), A0, A1)
	},
	catomic(Protocol),
	":",
	curl(Protocol, A1).

curl(file, A) -->
	(   "//"
	->  cpart(path, "", A)
	;   cpart(path, "", A)
	).
curl(http, A) -->
	"//",
	cpart(host, "", A),
	cpart(port, ":", A),
	cpart(path, "", A),
	csearch(A),
	cpart(fragment, "#", A).

curi(A) -->
	cpart(path, "", A),
	csearch(A).

cpart(Field, Sep, A) -->
	{ Term =.. [Field, Value],
	  memberchk(Term, A)
	}, !,
	Sep,
	catomic(Value).
cpart(_,_,_) -->
	[].

catomic(A, In, Out) :-
	atom_codes(A, Codes),
	append(Codes, Out, In).

csearch(A)--> 
	(   { memberchk(search(Parameters), A) }
	->  csearch(Parameters, "?")
	;   []
	).

csearch([], _) --> 
	[].
csearch([Parameter|Parameters], Sep) --> !, 
	Sep, 
	cparam(Parameter), 
	csearch(Parameters, "&"). 

cparam(Name=Value) --> !, 
	cform(Name), 
	"=", 
	cform(Value). 
cparam(Name)--> 
	cform(Name). 
	

cform(Atom) --> 
	{ www_form_encode(Atom, Encoded) }, 
	catomic(Encoded). 

		
		 /*******************************
		 *	      PARSING		*
		 *******************************/

%	parse_url(+URL, [+BaseURL], -Attributes)
%
%	Parse a URL to a sequence of attributes.  In the version with
%	three arguments, URL may be an URL relative to BaseURL.
%
%	AttributesL
%
%		protocol(Protocol)	Protocol identifier
%		host(Host)		Name of host
%		port(Port)		Number of port to contact
%		path(Path)		The path
%		search(Search)		Search specification

parse_url(URL, Attributes) :-
	nonvar(URL), !,
	atom_codes(URL, Codes),
	phrase(url(Attributes), Codes).
parse_url(URL, Attributes) :-
	phrase(curl(Attributes), Codes), !,
	atom_codes(URL, Codes).



parse_url(URL, BaseURL, Attributes) :-
	nonvar(URL), !, 
	atom_codes(URL, Codes),
	(   phrase(absolute_url, Codes, _)
	->  phrase(url(Attributes), Codes)
	;   parse_url(BaseURL, BaseA0),
	    memberchk(protocol(Protocol), BaseA0),
	    select(path(BasePath), BaseA0, BaseA1),
	    delete(BaseA1, search(_), BaseA2),
	    delete(BaseA2, fragment(_), BaseA3),
	    phrase(uri(Protocol, URIA0), Codes),
	    select(path(LocalPath), URIA0, URIA1),
	    globalise_path(LocalPath, BasePath, Path),
	    append(BaseA3, [path(Path)|URIA1], Attributes)
	).

parse_url(URL, BaseURL, Attributes) :-
	parse_url(BaseURL, BaseAttributes), 
	memberchk(path(BasePath), BaseAttributes), 
	(memberchk(path(LocalPath), Attributes) ->
	    globalise_path(LocalPath, BasePath, Path)
	; 
	    Path= BasePath
	), 	 
	append([path(Path)|Attributes], BaseAttributes, GlobalAttributes), 
	phrase(curl(GlobalAttributes), Chars), 
	atom_codes(URL, Chars).

	
globalise_path(LocalPath, _, LocalPath) :-
	is_absolute_file_name(LocalPath), !. % make file:drive:path work on MS
globalise_path(Local, Base, Path) :-
	base_dir(Base, BaseDir),
	make_path(BaseDir, Local, Path).

base_dir(BasePath, BaseDir) :-
	(   atom_concat(BaseDir, /, BasePath)
	->  true
	;   file_directory_name(BasePath, BaseDir)
	).

make_path(Dir, Local, Path) :-
	atom_concat('../', L2, Local),
	file_directory_name(Dir, Parent),
	Parent \== Dir, !,
	make_path(Parent, L2, Path).
make_path(/, Local, Path) :- !,
	atom_concat(/, Local, Path).
make_path(Dir, Local, Path) :-
	concat_atom([Dir, /, Local], Path).


absolute_url -->
	identifier(_),
	":", !.

url(A) -->
	case_insensitive("url:"), !,
	url(A).
url([protocol(Protocol)|A]) -->
	identifier(Protocol),
	":",
	url(Protocol, A), !.
url([protocol(http)|A]) -->
	implicit_http(A).

url(file, [path(Path)]) -->
	(   "/"
	->  path(Path0),
	    { atom_concat(/, Path0, Path)
	    }
	;   { current_prolog_flag(windows, true)
	    },
	    alpha(DriveCode),
	    ":",
	    path(Path0),
	    { char_code(Drive, DriveCode),
	      concat_atom([Drive, :, Path0], Path)
	    }
	;   path(Path0),
	    { absolute_file_name(Path0, Path)
	    }
	).

url(http, Attributes) --> !,
	"//",
	implicit_http(Attributes).

implicit_http(Attributes) -->
	hostport(A0),
	http_location(A1),
	{ append(A0, A1, Attributes)
	}.

http_location(Attributes) -->
	(   "/"
	->  path(Path0),
	    { atom_concat(/, Path0, Path),
	      A1 = [path(Path)]
	    }
	;   { A1 = [path(/)]		% assume / if no path is specified
	    }
	),
	(   "?"
	->  search(Search),
	    { A2 = [search(Search)|A1]
	    }
	;   { A2 = A1
	    }
	),
	(   "#"
	->  fragment(Fragment),
	    { A3 = [fragment(Fragment)|A2]
	    }
	;   { A3 = A2
	    }
	),
	{ Attributes = A3
	}.

%	uri(+Protocol, -Attributes)
%
%	resolve attributes of local URL

uri(http, [path(Path)|A1]) -->
	path(Path),
	(   "?"
	->  search(Search),
	    { A1 = [search(Search)|A2]
	    }
	;   { A1 = A2
	    }
	),
	(   "#"
	->  fragment(Fragment),
	    { A2 = [fragment(Fragment)]
	    }
	;   { A2 = []
	    }
	).
uri(file, [path(Path)|A2]) -->
	path(Path),
	(   "#"
	->  fragment(Fragment),
	    { A2 = [fragment(Fragment)]
	    }
	;   { A2 = []
	    }
	).


		 /*******************************
		 *	    URL ELEMENTS	*
		 *******************************/

hostport([host(Host)|A]) -->
	host(Host),
	(   ":"
	->  integer(Port),
	    { A = [port(Port)]
	    }
	;   { A = []
	    }
	).

host(H) -->
	hostname(H), !.
host(H) -->
	hostnumber(H).

hostname(Host) -->
	ialpha(Part1),
	(   "."
	->  hostname(Domain),
	    { concat_atom([Part1, '.', Domain], Host)
	    }
	;   { Host = Part1
	    }
	).

hostnumber(Host) -->
	integer(A),
	".",
	integer(B),
	".",
	integer(C),
	".",
	integer(D),
	{ concat_atom([A,B,C,D], '.', Host)
	}.

path(Path) -->
	segment(S0), !,
	(   "/"
	->  path(R),
	    { concat_atom([S0, /, R], Path)
	    }
	;   { Path = S0
	    }
	).
path('') -->
	[].

segment(Segment) -->
	xalphas(Chars), !,
	{ atom_codes(Segment, Chars)
	}.

search([Parameter|Parameters])--> 
	parameter(Parameter), !,  
	(   "&"
        ->  search(Parameters)
        ;   { Parameters = [] }
        ). 
search([]) --> 
	[].

parameter(Param)--> !, 
	parameter_component(Name), 
	(   "="
        ->  parameter_component(Value), 
	    { Param = (Name = Value) }
        ;   { Param = Name }
        ).


parameter_component(Component)-->
	search_chars(String), 
	{ atom_codes(Component, String) }. 


fragment(Fragment) --> 
	fragment_chars(FragmentChars), 
	{ atom_codes(Fragment, FragmentChars) }. 


		 /*******************************
		 *	       BASICS		*
		 *******************************/

integer(I) -->
	digit(D0),
	digits(D),
	{ number_chars(I, [D0|D])
	}.

		 /*******************************
		 *	     SEQUENCES		*
		 *******************************/

digits([C|T]) -->
	digit(C), !, 
	digits(T).
digits([]) -->
	[].

xalphas([C|T]) -->
	xalpha(C), !, 
	xalphas(T).
xalphas([]) -->
	[].

search_chars([0' | T]) --> 
	"+", !, 
	search_chars(T). 
	
search_chars([C|T]) -->
	search_char(C), !, 
	search_chars(T).
search_chars([]) -->
	[].

fragment_chars([C|T]) -->
	fragment_char(C), !, 
	fragment_chars(T).
fragment_chars([]) -->
	[].



ialpha(Atom) -->
	alpha(C0),
	xalphas(S),
	{ atom_codes(Atom, [C0|S])
	}.

		 /*******************************
		 *	  URL DCG BASICS	*
		 *******************************/

digit(C, [C|T], T) :- code_type(C, digit).
alpha(C, [C|T], T) :- code_type(C, alpha).
alphanum(C, [C|T], T) :- code_type(C, alnum).
escape(C) -->
	"%",
	[C1, C2],
	{ code_type(C1, xdigit(D1)),
	  code_type(C2, xdigit(D2)),
	  C is 16*D1 + D2
	}.



xalpha(C, [C|T], T) :-
	(   code_type(C, alnum)		% alpha | digit
	;   safe(C)
	;   extra(C)
	), !.
xalpha(C) -->
	escape(C).

search_char(C, [C|T], T) :-
	(   code_type(C, alnum)		% alpha | digit
	;   search_char(C)
	;   extra(C)
	), !.
search_char(C) -->
	escape(C). 


fragment_char(C, [C|T], T):- 
	(   code_type(C, alnum)		% alpha | digit
	;   safe(C)
	;   extra(C)
        ;   reserved(C)
	), !.
fragment_char(C) --> 
	escape(C). 



safe(0'$).
safe(0'-).
safe(0'_).
safe(0'@).
safe(0'.).
safe(0'&).
safe(0'+).
safe(0'~).				% JW: not official URL
safe(0'=).				% LCF: not official URL


search_char(0'$).
search_char(0'-).
search_char(0'_).
search_char(0'@).
search_char(0'.).
search_char(0'+).
search_char(0'~).			% JW: not official URL
search_char(0'/).
search_char(0';).
search_char(0':).
search_char(0'{).			% JW: not official URL, used by ASP
search_char(0'}).			% JW: not official URL, used by ASP


extra(0'!).
extra(0'*).
extra(0'").
extra(0'').
extra(0'().
extra(0')).
extra(0',).

reserved(0'=).
reserved(0';).
reserved(0'/).
reserved(0'#).
reserved(0'?).
reserved(0':).
reserved(0' ).

national(0'{).
national(0'}).
national(0'[).
national(0']).
national(0'\).
national(0'^).
national(0'~).

		 /*******************************
		 *	     DCG BASICS		*
		 *******************************/

%	case_insensitive(+String)
%
%	match a string case insensitive.

case_insensitive([]) -->
	[].
case_insensitive([H|T]) -->
	[C],
	{ code_type(H, to_lower(C))
	},
	case_insensitive(T).


%	identifier(-Id)
%
%	Match input string holding csym characters to an identifier

identifier(Id) -->
	chars(alpha, Chars),
	{ name(Id, Chars)
	}.

%	chars(+Type, -Chars)
%
%	Get as many as possible	characters of this type

chars(Type, [C0|C]) -->
	[C0],
	{ code_type(C0, Type)
	}, !,
	chars(Type, C).
chars(_, []) -->
	[].


		 /*******************************
		 *	        FORMS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Encoding/decoding of form-fields  using   the  popular  www-form-encoded
encoding used with the HTTP GET.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	www_form_encode(+Value, -X-WWW-Form-Encoded)
%	www_form_encode(-Value, +X-WWW-Form-Encoded)
%
%	En/Decode between native value and application/x-www-form-encoded
%	Maps space to +, keeps alnum, maps anything else to %XX and newlines
%	to %od%oa.  When decoding, newlines appear as a single newline (10)
%	character.

www_form_encode(Value, Encoded) :-
	atomic(Value), !,
	atom_codes(Value, Codes),
	phrase(www_encode(EncCodes), Codes),
	atom_codes(Encoded, EncCodes).
www_form_encode(Value, Encoded) :-
	atom_codes(Encoded, EncCodes),
	phrase(www_decode(Codes), EncCodes),
	atom_codes(Value, Codes).

www_encode([0'+|T]) -->
	" ", !,
        www_encode(T).
www_encode([C|T]) -->
	alphanum(C), !,
	www_encode(T).
www_encode(Enc) -->
	(   "\r\n"
	;   "\n"
	),
	{ append("%0D%0A", T, Enc)
	},
	www_encode(T).
www_encode([]) -->
	[].

www_encode([0'%,D1,D2|T]) -->
	[C],
	{ Dv1 is (C>>4 /\ 0xf),
	  Dv2 is (C /\ 0xf),
	  code_type(D1, xdigit(Dv1)),
	  code_type(D2, xdigit(Dv2))
	},
	www_encode(T).
	
www_decode([0' |T]) -->
	"+", !,
        www_decode(T).
www_decode([C|T]) -->
	"%",
	[ D1,D2 ], !,
	{ code_type(D1, xdigit(Dv1)),
	  code_type(D2, xdigit(Dv2)),
	  C0 is ((Dv1)<<4) + Dv2
	},
	(   { C0 == 13
	    },
	    "%0",
	    [D3],
	    { code_type(D3, xdigit(10))
	    }
	->  { C = 10
	    }
	;   { C = C0
	    }
	),
	www_decode(T).
www_decode([C|T]) -->
	[C], !,
	www_decode(T).
www_decode([]) -->
	[].

		 /*******************************
		 *	     FORM DATA		*
		 *******************************/

%	parse_url_search(?Spec, ?Fields)
%
%	Parse between a list of Name=Value and the MIME-type
%	application/x-www-form-urlencoded as used to post HTTP requests

parse_url_search(Spec, Fields) :-
	atomic(Spec), !,
	atom_codes(Spec, Codes),
	phrase(search(Fields), Codes).
parse_url_search(Codes, Fields) :-
	is_list(Codes), !,
	phrase(search(Fields), Codes).
parse_url_search(Out, Fields) :-
	phrase(csearch(Fields), Codes),
	atom_codes(Out, Codes).
	
