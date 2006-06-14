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
:- use_module(library(lists)).
:- use_module(library(utf8)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Utility library to break down URL   specifications. Originally this file
was based on RFC-1738. In SWI-Prolog 5.6.13 it was upgraded to RFC 3986,
notably covering UTF-8 encoding of Unicode characters in form-encoding.

Public interface:

parse_url(+URL, -Parts)
    Parse a URL into its parts.  Parts is a list of Name(Value).  Attributes
    depend on the protocol.  Currently the implementation is not defined
    for all protocols.  Defined:

        protocol	All protocols
	user		Network based protocols
	host		Network based protocols
	port		Network based protocols
	path		file,http,ftp
	search		http (?name=value&...)
        fragment	http (#fragment)

    For example:

    ?- parse_url('http://www.swi-prolog.org/message.cgi?msg=Hello+World%21', P).

    P = [ protocol(http), 
	  search([msg='Hello World!']), 
	  path('/message.cgi'), 
	  host('www.swi-prolog.org')
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

ISSUES:

	* Lacks support for IP6 in parsing URIs
	* Constructing could avoid percent encoding for some more
	  characters
	* Using + for spaces as used in forms may be active in too
	  many places.
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
	sub_atom(URL, 0, _, _, 'https://'), !.
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


curl(A) -->
	{ memberchk(protocol(Protocol), A)
	}, !,
	catomic(Protocol),
	":",
	curl(Protocol, A).
curl(A) -->
	curl(http, A).

curl(file, A) -->
	(   "//"
	->  cpath(A)
	;   cpath(A)
	).
curl(https, A) -->
	curl(http, A).
curl(http, A) -->
	"//",
	cuser(A),
	chost(A),
	cport(A),
	cpath(A),
	csearch(A),
	cfragment(A).

curi(A) -->
	cpath(A),
	csearch(A).

cpath(A) -->
	(   { memberchk(path(Path), A) }
	->  { atom_codes(Path, Codes) },
	    www_encode(Codes, "/+:")
	;   ""
	).

cuser(A) -->
	(   { memberchk(user(User), A) }
	->  { atom_codes(User, Codes) },
	    www_encode(Codes, ":"),
	    "@"
	;   ""
	).

chost(A) -->
	(   { memberchk(host(Host), A) }
	->  { atom_codes(Host, Codes) },
	    www_encode(Codes, "")
	;   ""
	).

cport(A) -->
	(   { memberchk(port(Port), A) }
	->  { number_codes(Port, Codes) },
	    ":",
	    www_encode(Codes, "")
	;   ""
	).


catomic(A, In, Out) :-
	atom_codes(A, Codes),
	append(Codes, Out, In).

%	csearch(+Attributes, //)

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
cparam(NameValue) -->			% allow to feed Name(Value)
	{ compound(NameValue), !,
	  NameValue =.. [Name,Value]
	},
	cform(Name), 
	"=", 
	cform(Value). 
cparam(Name)--> 
	cform(Name). 
	

cform(Atom) --> 
	{ atom_codes(Atom, Codes) },
	www_encode(Codes, "").

%	cfragment(+Attributes, //)

cfragment(A) -->
	{ memberchk(fragment(Frag), A), !,
	  atom_codes(Frag, Codes)
	},
	"#",
	www_encode(Codes, "").
cfragment(_) -->
	"".
	
		
		 /*******************************
		 *	      PARSING		*
		 *******************************/

%	parse_url(+URL, [+BaseURL], -Attributes)
%
%	Parse a URL to a sequence of attributes.  In the version with
%	three arguments, URL may be an URL relative to BaseURL.
%
%	Attributes:
%
%		protocol(Protocol)	Protocol identifier
%		host(Host)		Name of host
%		port(Port)		Number of port to contact
%		path(Path)		The path
%		search(Search)		Search specification
%		
%	The parse_url/3	predicate deals with relative URLs.  BaseURL can
%	be specified as an atom or parsed URL.

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
	;   (   atomic(BaseURL)
	    ->  parse_url(BaseURL, BaseA0)
	    ;	BaseA0 = BaseURL
	    ),
	    select(path(BasePath), BaseA0, BaseA1),
	    delete(BaseA1, search(_), BaseA2),
	    delete(BaseA2, fragment(_), BaseA3),
	    phrase(relative_uri(URIA0), Codes),
	    select(path(LocalPath), URIA0, URIA1), !,
	    globalise_path(LocalPath, BasePath, Path),
	    append(BaseA3, [path(Path)|URIA1], Attributes)
	).
parse_url(URL, BaseURL, Attributes) :-
	parse_url(BaseURL, BaseAttributes), 
	memberchk(path(BasePath), BaseAttributes), 
	(   memberchk(path(LocalPath), Attributes)
	->  globalise_path(LocalPath, BasePath, Path)
	;   Path = BasePath
	), 	 
	append([path(Path)|Attributes], BaseAttributes, GlobalAttributes), 
	phrase(curl(GlobalAttributes), Chars), 
	atom_codes(URL, Chars).

	
%	globalise_path(+LocalPath, +RelativeTo, -FullPath)
%	
%	The first clause deals with the  standard URL /... global paths.
%	The second with file://drive:path on MS-Windows.   This is a bit
%	of a cludge, but unfortunately common practice is -especially on
%	Windows- not always following the standard

globalise_path(LocalPath, _, LocalPath) :-
	sub_atom(LocalPath, 0, _, _, /), !.
globalise_path(LocalPath, _, LocalPath) :-
	is_absolute_file_name(LocalPath), !.
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
	lwalpha(_),
	schema_chars(_),
	":", !.


		 /*******************************
		 *	     SEQUENCES		*
		 *******************************/

digits(L) -->
	digits(L, []).

digits([C|T0], T) -->
	digit(C), !, 
	digits(T0, T).
digits(T, T) -->
	[].


digit(C, [C|T], T) :- code_type(C, digit).

		 /*******************************
		 *	      RFC-3986		*
		 *******************************/

%	uri(-Parts, //)

url([protocol(Schema)|Parts]) -->
	schema(Schema),
	":", !,
	hier_part(Parts, P2),
	query(P2, P3),
	fragment(P3, []).
url([protocol(http)|Parts]) -->		% implicit HTTP
	authority(Parts, [path(Path)]),
	path_abempty(Path).

relative_uri(Parts) -->
	relative_part(Parts, P2),
	query(P2, P3),
	fragment(P3, []).

relative_part(Parts, Tail) -->
	"//", !,
	authority(Parts, [path(Path)|Tail]),
	path_abempty(Path).
relative_part([path(Path)|T], T) -->
	(   path_absolute(Path)
	;   path_noschema(Path)
	;   path_empty(Path)
	), !.

http_location([path(Path)|P2]) -->
	path_abempty(Path),
	query(P2, P3),
	fragment(P3, []).

%	schema(-Atom, //)
%	
%	Schema ::= ALPHA *(ALPHA|DIGIT|"+"|"-"|".")
%
%	Schema  is  case-insensitive  and  the    canonical  version  is
%	lowercase.

schema(Schema) -->
	lwalpha(C0),
	schema_chars(Codes),
	{ atom_codes(Schema, [C0|Codes]) }.

schema_chars([H|T]) -->
	schema_char(H), !,
	schema_chars(T).
schema_chars([]) -->
	[].
	
schema_char(H) -->
	[C],
	{ C < 128,
	  (   code_type(C, alpha)
	  ->  code_type(H, to_lower(C))
	  ;   code_type(C, digit)
	  ->  H = C
	  ;   schema_extra(C)
	  ->  H = C
	  )
	}.

schema_extra(0'+).
schema_extra(0'-).
schema_extra(0'.).


%	hier_part(+Schema, -Parts, ?Tail, //)
%	
%	Extract the hierarchy part.

hier_part(Parts, Tail) -->
	"//", !,
	authority(Parts, [path(Path)|Tail]),
	path_abempty(Path).
hier_part([path(Path)|T], T) -->
	(   path_absolute(Path)
	;   path_rootless(Path)
	;   path_empty(Path)
	), !.

authority(Parts, Tail) -->
	user_info_chars(UserChars),
	"@", !,
	{ atom_codes(User, UserChars),
	  Parts = [user(User),host(Host)|T0]
	},
	host(Host),
	port(T0,Tail).
authority([host(Host)|T0], Tail) -->
	host(Host),
	port(T0, Tail).
	
user_info_chars([H|T]) -->
	user_info_char(H), !,
	user_info_chars(T).
user_info_chars([]) -->
	[].

user_info_char(_) --> "@", !, {fail}.
user_info_char(C) --> pchar(C).
	
%host(Host) --> ip_literal(Host), !.		% TBD: IP6 addresses
host(Host) --> ip4_address(Host), !.
host(Host) --> reg_name(Host).

ip4_address(Atom) -->
	i256_chars(Chars, [0'.|T0]),
	i256_chars(T0, [0'.|T1]),
	i256_chars(T1, [0'.|T2]),
	i256_chars(T2, []),
	{ atom_codes(Atom, Chars) }.

i256_chars(Chars, T) -->
	digits(Chars, T),
	\+ \+ { T = [],
		Chars \== [],
		number_codes(I, Chars),
		I < 256
	      }.

reg_name(Host) -->
	reg_name_chars(Chars),
	{ atom_codes(Host, Chars) }.

reg_name_chars([H|T]) -->
	reg_name_char(H), !,
	reg_name_chars(T).
reg_name_chars([]) -->
	[].

reg_name_char(C) -->
	pchar(C),
	{ C \== 0':,
	  C \== 0'@
	}.

port([port(Port)|T], T) -->
	":", !,
	digit(D0),
	digits(Ds),
	{ number_codes(Port, [D0|Ds]) }.
port(T, T) -->
	[].

path_abempty(Path) -->
	segments_chars(Chars, []),
	{   Chars == []
	->  Path = '/'
	;   atom_codes(Path, Chars)
	}.


path_absolute(Path) -->
	"/",
	segment_nz_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, [0'/| Chars]) }.

path_noschema(Path) -->
	segment_nz_nc_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, Chars) }.

path_rootless(Path) -->
	segment_nz_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, Chars) }.

path_empty('/') -->
	"".

segments_chars([0'/|Chars], T) -->
	"/", !,
	segment_chars(Chars, T0),
	segments_chars(T0, T).
segments_chars(T, T) -->
	[].

segment_chars([H|T0], T) -->
	pchar(H), !,
	segment_chars(T0, T).
segment_chars(T, T) -->
	[].

segment_nz_chars([H|T0], T) -->
	pchar(H),
	segment_chars(T0, T).

segment_nz_nc_chars([H|T0], T) -->
	segment_nz_nc_char(H), !,
	segment_nz_nc_chars(T0, T).
segment_nz_nc_chars(T, T) -->
	[].

segment_nz_nc_char(_) --> ":", !, {fail}.
segment_nz_nc_char(C) --> pchar(C).


%	query(-Parts, ?Tail, //)
%	
%	Extract &Name=Value, ...

query([search(Params)|T], T) -->
	"?",
	search(Params).
query(T,T) -->
	[].

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

search_chars([C|T]) -->
	search_char(C), !, 
	search_chars(T).
search_chars([]) -->
	[].

search_char(_) --> "&", !, { fail }.
search_char(_) --> "=", !, { fail }.
search_char(C) --> fragment_char(C).


%	fragment(-Fragment, ?Tail, //)
%	
%	Extract the fragment (after the #)

fragment([fragment(Fragment)|T], T) -->
	"#", !,
	fragment_chars(Codes),
	{ atom_codes(Fragment, Codes) }.
fragment(T, T) -->
	[].

fragment_chars([H|T]) -->
	fragment_char(H), !,
	fragment_chars(T).
fragment_chars([]) -->
	[].


%	fragment_char(-Char)
%	
%	Find a fragment character.

fragment_char(C)   --> pchar(C), !.
fragment_char(0'/) --> "/", !.
fragment_char(0'?) --> "?", !.


		 /*******************************
		 *	CHARACTER CLASSES	*
		 *******************************/

%	pchar(-Code, //)
%	
%	unreserved|pct_encoded|sub_delim|":"|"@"
%	
%	Performs UTF-8 decoding of percent encoded strings.

pchar(0' ) --> "+", !.			% ?
pchar(C) -->
	[C],
	{   unreserved(C)
	;   sub_delim(C)
	;   C == 0':
        ;   C == 0'@
	}, !.
pchar(C) -->
	percent_coded(C).

%	lwalpha(-C, //)
%	
%	Demand alpha, return as lowercase

lwalpha(H) -->
	[C],
	{ C < 128,
	  code_type(C, alpha),
	  code_type(H, to_lower(C))
	}.

		 /*******************************
		 *	RESERVED CHARACTERS	*
		 *******************************/

%	reserved(?Code)
%	
%	URL reserved codes

%reserved(Code) :-
%	gen_delim(Code).
%reserved(Code) :-
%	sub_delim(Code).

%	gen_delim(?Code)
%	
%	General delimeters

%gen_delim(0':).
%gen_delim(0'/).
%gen_delim(0'?).
%gen_delim(0'#).
%gen_delim(0'[).
%gen_delim(0']).
%gen_delim(0'@).

%	sub_delim(?Code)
%	
%	Sub-delimiters

sub_delim(0'!).
sub_delim(0'$).
sub_delim(0'&).
sub_delim(0'').
sub_delim(0'().
sub_delim(0')).
sub_delim(0'*).
sub_delim(0'+).
sub_delim(0',).
sub_delim(0';).
sub_delim(0'=).


%	unreserved(+C)
%	
%	Characters that can be represented without procent escaping
%	RFC 3986, section 2.3

unreserved(C) :-
	code_type(C, alnum),
	C < 128.
unreserved(0'-).
unreserved(0'.).
unreserved(0'_).
unreserved(0'~).


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
%	to %OD%OA.  When decoding, newlines appear as a single newline (10)
%	character.

www_form_encode(Value, Encoded) :-
	atomic(Value), !,
	atom_codes(Value, Codes),
	phrase(www_encode(Codes, ""), EncCodes),
	atom_codes(Encoded, EncCodes).
www_form_encode(Value, Encoded) :-
	atom_codes(Encoded, EncCodes),
	phrase(www_decode(Codes), EncCodes),
	atom_codes(Value, Codes).

%	www_encode(+Codes, +ExtraUnescaped, //)

www_encode([0'\r, 0'\n|T], Extra) --> !,
	"%0D%0A",
	www_encode(T, Extra).
www_encode([0'\n|T], Extra) --> !,
	"%0D%0A",
	www_encode(T, Extra).
www_encode([H|T], Extra) -->
	percent_encode(H, Extra),
	www_encode(T, Extra).
www_encode([], _) -->
	"".

percent_encode(C, Extra) -->
	{ unreserved(C)
	; memberchk(C, Extra)
	}, !,
	[C].
percent_encode(0' , _) --> "+".
percent_encode(C, _) -->
	{ C =< 128 }, !,
	percent_byte(C).
percent_encode(C, _) -->		% Unicode characters
	{ phrase(utf8_codes([C]), Bytes) },
	percent_bytes(Bytes).

percent_bytes([]) -->
	"".
percent_bytes([H|T]) -->
	percent_byte(H),
	percent_bytes(T).

percent_byte(C) -->
	[0'%, D1, D2],
	{   nonvar(C)
	->  Dv1 is (C>>4 /\ 0xf),
	    Dv2 is (C /\ 0xf),
	    code_type(D1, xdigit(Dv1)),
	    code_type(D2, xdigit(Dv2))
	;   code_type(D1, xdigit(Dv1)),
	    code_type(D2, xdigit(Dv2)),
	    C is ((Dv1)<<4) + Dv2
	}.

percent_coded(C) -->
	percent_byte(C0), !,
	(   { C0 == 13			% %0D%0A --> \n
	    },
	    "%0",
	    ( "A" ; "a" )
	->  { C = 10
	    }
	;   { C0 >= 0xc0 },		% UTF-8 lead-in
	    utf8_cont(Cs),
	    { phrase(utf8_codes([C]), [C0|Cs]) }
	->  []
	;   { C = C0
	    }
	).

%	www_decode(-Codes, ...)

www_decode([0' |T]) -->
	"+", !,
        www_decode(T).
www_decode([C|T]) -->
	percent_coded(C), !,
	www_decode(T).
www_decode([C|T]) -->
	[C], !,
	www_decode(T).
www_decode([]) -->
	[].

utf8_cont([H|T]) -->
	percent_byte(H),
	{ between(0x80, 0xbf, H) }, !,
	utf8_cont(T).
utf8_cont([]) -->
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
parse_url_search(Codes, Fields) :-
	phrase(csearch(Fields, ""), Codes).
	
