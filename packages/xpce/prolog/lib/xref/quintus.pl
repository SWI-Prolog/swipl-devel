/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file contains a list of   Quintus  Prolog built-in predicates, used
for the XPCE/Prolog cross-referencer to compute require/1 directives
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

!.
','(_,_).
'.'(_,_).
'C'(_,_,_).
-->(_,_).
->(_,_).
:(_,_).
:-(_).
:-(_,_).
;(_,_).
<(_,_).
=(_,_).
=..(_,_).
=:=(_,_).
=<(_,_).
==(_,_).
=\=(_,_).
>(_,_).
>=(_,_).
?-(_).
@<(_,_).
@=<(_,_).
@>(_,_).
@>=(_,_).
[].
\+(_).
\==(_,_).
^(_,_).
abolish(_).
abolish(_,_).
abort.
absolute_file_name(_,_).
absolute_file_name(_,_,_).
add_advice(_,_,_).
add_spypoint(_).
append(_,_,_).
arg(_,_,_).
assert(_).
assert(_,_).
asserta(_).
asserta(_,_).
assertz(_).
assertz(_,_).
assign(_,_).
at_end_of_file(_).
at_end_of_file.
at_end_of_line(_).
at_end_of_line.
atom(_).
atom_chars(_,_).
atomic(_).
bagof(_,_,_).
break.
call(_).
callable(_).
character_count(_,_).
check_advice(_).
check_advice.
clause(_,_).
clause(_,_,_).
close(_).
compare(_,_,_).
compile(_).
compound(_).
consult(_).
copy_term(_,_).
current_advice(_,_,_).
current_atom(_).
current_input(_).
current_key(_,_).
current_module(_).
current_module(_,_).
current_op(_,_,_).
current_output(_).
current_predicate(_,_).
current_spypoint(_).
current_stream(_,_,_).
db_reference(_).
debug.
debugging.
display(_).
dynamic(_).
ensure_loaded(_).
erase(_).
expand_term(_,_).
extern(_).
fail.
false.
fetch_atom(_,_).
fileerrors.
findall(_,_,_).
float(_).
flush_output(_).
format(_,_).
format(_,_,_).
functor(_,_,_).
garbage_collect.
garbage_collect_atoms.
gc.
get(_).
get(_,_).
get0(_).
get0(_,_).
get_profile_results(_,_,_,_).
ground(_).
halt(_).
halt.
hash_term(_,_).
help(_).
help.
initialization(_).
instance(_,_).
integer(_).
is(_,_).
keysort(_,_).
leash(_).
length(_,_).
line_count(_,_).
line_position(_,_).
listing(_).
listing.
load_files(_).
load_files(_,_).
load_foreign_executable(_).
load_foreign_files(_,_).
manual(_).
manual.
meta_predicate(_).
mode(_).
module(_).
module(_,_).
multifile(_).
multifile_assertz(_).
name(_,_).
nl(_).
nl.
no_style_check(_).
nocheck_advice(_).
nocheck_advice.
nodebug.
nofileerrors.
nogc.
nonvar(_).
noprofile.
nospy(_).
nospyall.
notrace.
number(_).
number_chars(_,_).
numbervars(_,_,_).
on_exception(_,_,_).
op(_,_,_).
open(_,_,_).
open(_,_,_,_).
open_null_stream(_).
otherwise.
peek_char(_).
peek_char(_,_).
phrase(_,_).
phrase(_,_,_).
portray_clause(_).
predicate_property(_,_).
print(_).
print(_,_).
print_message(_,_).
print_message_lines(_,_,_).
profile(_).
profile(_,_).
profile(_,_,_).
profile.
prolog_flag(_,_).
prolog_flag(_,_,_).
prolog_load_context(_,_).
prompt(_,_).
prompt(_,_,_).
public(_).
put(_).
put(_,_).
raise_exception(_).
read(_).
read(_,_).
read_term(_,_).
read_term(_,_,_).
reconsult(_).
recorda(_,_,_).
recorded(_,_,_).
recordz(_,_,_).
remove_advice(_,_,_).
remove_spypoint(_).
repeat.
restore(_).
retract(_).
retractall(_).
save_modules(_,_).
save_predicates(_,_).
save_program(_).
save_program(_,_).
see(_).
seeing(_).
seek(_,_,_,_).
seen.
set_input(_).
set_output(_).
setof(_,_,_).
show_profile_results(_).
show_profile_results(_,_).
show_profile_results.
simple(_).
skip(_).
skip(_,_).
skip_line(_).
skip_line.
sort(_,_).
source_file(_).
source_file(_,_).
source_file(_,_,_).
spy(_).
statistics(_,_).
statistics.
store_atom(_,_).
stream_code(_,_).
stream_position(_,_).
stream_position(_,_,_).
style_check(_).
subsumes_chk(_,_).
tab(_).
tab(_,_).
tell(_).
telling(_).
told.
trace.
trimcore.
true.
ttyflush.
ttyget(_).
ttyget0(_).
ttynl.
ttyput(_).
ttyskip(_).
ttytab(_).
unix(_).
unknown(_,_).
use_module(_).
use_module(_,_).
use_module(_,_,_).
var(_).
version(_).
version.
vms(_).
vms(_,_).
volatile(_).
write(_).
write(_,_).
write_canonical(_).
write_canonical(_,_).
write_term(_,_).
write_term(_,_,_).
writeq(_).
writeq(_,_).
