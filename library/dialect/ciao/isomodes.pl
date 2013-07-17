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

:- package(isomodes).
:- use_module(engine(hiord_rt)).

%% The ISO standard is unfortunately not very clear/formal in the
%% description of modes, but these interpretations seem the most
%% sensible. 

:- op(200, fy, [(?),(@)]).

%% Basic ISO-modes
:- modedef '+'(A) : nonvar(A).
:- modedef '-'(A) : var(A). 
%% The standard says that this should be:
% :- modedef '-'(A) : var(A) => nonvar(A).
%% but then it says that the only error possible is for not 
%% meeting the : var... what to do?
:- modedef '?'(_).
:- modedef '@'(A) + not_further_inst(A).
%% Only in older versions of standard? It is obsolete now.
%% :- modedef '*'(_).

% :- push_prolog_flag(read_hiord,on).


%% Parametric versions of above
:- modedef +(A,X) :  call(X, A).
:- modedef -(A,X) :  var(A) => call(X, A).
%% Version in standard supports this simple interpretation:
% :- modedef ?(A,X) :: X(A).
%% but all builtins conform to:
:- modedef ?(A,X) :: call(X, A) => call(X, A).
%% ..what to do??
:- modedef @(A,X) :  call(X, A) => call(X, A) + not_further_inst(A).
%% Only in older versions of standard? It is obsolete now.
%% :- modedef *(A,X) :: X(A).

% :- pop_prolog_flag(read_hiord).
