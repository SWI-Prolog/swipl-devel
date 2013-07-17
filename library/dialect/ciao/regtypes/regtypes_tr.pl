/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%% Migrated from Ciao to SWI-Prolog

:- module(regtypes_tr, [expand_regtypes/2],
	    [assertions, nortchecks, isomodes]).

:- doc(title, "Regular type definition support").

%% ------------------------------------------------------------------------

%% If a '+' field is present by not recognizing it here as is_type_decl 
%% we simply leave it as is! (old?)
expand_regtypes((:- regtype((T # C))),    (:- prop((T + regtype # C)))).
expand_regtypes((:- regtype(S, (T # C))), (:- prop(S, (T + regtype # C)))).
expand_regtypes((:- regtype(T)),          (:- prop(T + regtype))).
expand_regtypes((:- regtype(S, T)),       (:- prop(S, T + regtype))).
