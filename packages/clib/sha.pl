/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(crypto_hash,
	  [ sha_hash/3,			% +Data, -Hash, +Options
	    hmac_sha/4			% +Key, +Data, -Hash, +Options
	  ]).
:- use_module(library(shlib)).

:- initialization
   load_foreign_library(foreign(sha4pl)).

%%	sha_hash(+Data, -Hash, +Options) is det
%
%	Hash is the SHA hash of From,   The  conversion is controlled by
%	Options:
%	
%	  * algorithm(+Algorithm)
%	  One of =sha1= (default), =sha224=, =sha256=, =sha384= or
%	  =sha512=
%	  
%	@param	Data is either an atom, string or code-list
%	@param  Hash is a packed string

%%	hmac_sha(+Key, +Data, -Hash, +Options) is det
%
%	For Options, see sha_hash/3.
