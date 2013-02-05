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

:- package(assertions).
:- new_declaration(comment/2).
:- new_declaration(doc/2).

%% To become obsolete? MH
:- op(975, xfx,(=>)).
:- op(978, xfx,(::)).

:- new_declaration((decl)/1).            :- op(1150, fx,(decl)).
:- new_declaration((decl)/2).            :- op(1150,xfx,(decl)).
:- new_declaration((pred)/1).            :- op(1150, fx,(pred)).
:- new_declaration((pred)/2).            :- op(1150,xfx,(pred)).
%% % Should be in functions library
%% :- new_declaration((func)/1).         :- op(1150, fx,(func)).
%% :- new_declaration((func)/2).         :- op(1150,xfx,(func)).
:- new_declaration((prop)/1).            :- op(1150, fx,(prop)).
:- new_declaration((prop)/2).            :- op(1150,xfx,(prop)).
:- new_declaration((modedef)/1).         :- op(1150, fx,(modedef)).
%any sense?PBC :- new_declaration((modedef)/2). :- op(1150,xfx,(modedef)).

:- new_declaration((calls)/1).           :- op(1150, fx,(calls)).
:- new_declaration((calls)/2).           :- op(1150,xfx,(calls)).
:- new_declaration((success)/1).         :- op(1150, fx,(success)).
:- new_declaration((success)/2).         :- op(1150,xfx,(success)).
:- new_declaration((test)/1).            :- op(1150, fx,(test)).
:- new_declaration((test)/2).            :- op(1150,xfx,(test)).
:- new_declaration((texec)/1).           :- op(1150, fx,(texec)).
:- new_declaration((texec)/2).           :- op(1150,xfx,(texec)).
:- new_declaration((comp)/1).            :- op(1150, fx,(comp)).
:- new_declaration((comp)/2).            :- op(1150,xfx,(comp)).

%% To become obsolete? MH 
:- new_declaration((entry)/1).           :- op(1150, fx,(entry)).
%obsolete-PBC :- new_declaration(entry/2).           :- op(1150,xfx,(entry)).

%% DTM, new declaration
:- new_declaration((exit)/1).            :- op(1150, fx,(exit)). 
:- new_declaration((exit)/2).            :- op(1150,xfx,(exit)). 

:- op(500,  yfx, (#)).
