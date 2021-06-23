/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(file_systems,
	  [ rename_file/2,		% +OldName, +NewName
	    rename_directory/2,		% +OldName, +NewName

	    delete_file/1,		% +OldName
	    delete_directory/1,		% +Directory
	    delete_directory/2,		% +Directory, +Options

	    directory_exists/1,		% +Directory
	    directory_exists/2,		% +Directory, +Mode

	    make_directory/1,		% +Directory

	    file_exists/1,		% +File
	    file_exists/2,		% +File, +Mode
	    file_must_exist/1,		% +File
	    file_must_exist/2,		% +File, +Mode
	    directory_must_exist/1,	% +File
	    directory_must_exist/2,	% +File, +Mode

	    directory_member_of_directory/2, % -BaseName, -FullName
	    directory_member_of_directory/3, % +Directory, -BaseName, -FullName
	    directory_member_of_directory/4, % +Directory, +Pattern, -BaseName, -FullName
	    file_member_of_directory/2,	% -BaseName, -FullName
	    file_member_of_directory/3,	% +Directory, -BaseName, -FullName
	    file_member_of_directory/4,	% +Directory, +Pattern, -BaseName, -FullName
	    directory_members_of_directory/1, % -Set
	    directory_members_of_directory/2, % +Directory, -Set
	    directory_members_of_directory/3, % +Directory, +Pattern, -Set
	    file_members_of_directory/1, % -Set
	    file_members_of_directory/2, % +Directory, -Set
	    file_members_of_directory/3, % +Directory, +Pattern, -Set

	    directory_property/2,	% +Directory, ?Property
	    directory_property/3,	% +Directory, ?Property, ?Value
	    file_property/2,		% +File, ?Property
	    file_property/3,		% +File, ?Property, ?Value

	    current_directory/1,	% -Directory
	    current_directory/2		% -Directory, +NewDirectory
	  ]).
:- use_module(library(lists), [member/2]).
:- use_module(system, [datime/2]).

/** <module> SICStus 4 library(file_systems).

@tbd	This library is incomplete.
	As of SICStus 4.6.0, the following predicates are missing:

	* close_all_streams/0

	Some predicates don't fully support all options available on SICStus.
	See the documentation for individual predicates for details.

	The file access mode `execute` is interpreted slightly
	differently on SICStus and SWI if the file in question
	is a directory. On SWI, checking `execute` access on a
	directory actually checks if the directory can be
	listed/searched. This matches the meaning of the POSIX
	execute permission bit.

	SICStus on the other hand adds a separate access mode
	`search` for this purpose and uses `execute` *only* for
	checking executability of regular files. Checking access
	mode `execute` on a directory or `search` on a regular
	file is equivalent to checking `exist`.

	This difference affects not just file_exists/2 and
	directory_exists/2 in this library, but also the
	built-in absolute_file_name/3 with the option
	access(Mode).

	On the other hand, file_property/2 and
	directory_property/2 with properties `executable` and
	`searchable` are *not* affected - here the emulation
	matches the native SICStus behavior.

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dfile_005fsystems.html
*/

% Note: Unlike most of SWI's built-in file system predicates,
% SICStus library(file_systems) draws a strict distinction between files and directories.
% This means that "file" predicates will operate only on regular files -
% directories must be manipulated using the corresponding "directory" predicates instead.
% Because of this,
% some of SWI's built-in file predicates
% (that can operate on both files and directories)
% need to be replaced with versions that throw an error when receiving a non-regular file.


%!	rename_file(+OldName, +NewName) is det.
%
%	Like SWI's built-in rename_file/2, but only works on regular
%	files. To rename directories, rename_directory/2 must be used.

rename_file(OldName, NewName) :-
	file_must_exist(OldName),
	system:rename_file(OldName, NewName).

%!	rename_directory(+OldName, +NewName) is det.
%
%	Like SWI's built-in rename_file/2, but only works on
%	directories. To rename regular files, rename_file/2 must be used.

rename_directory(OldName, NewName) :-
	directory_must_exist(OldName),
	system:rename_file(OldName, NewName).


%!	delete_file(+OldName) is det.
%
%	Like SWI's built-in delete_file/1, but only works on regular
%	files. To delete directories, delete_directory/1 must be used.

delete_file(OldName) :-
	file_must_exist(OldName),
	system:delete_file(OldName).

% SWI's built-in delete_directory/1 behaves like the one from SICStus library(file_systems).

%!	delete_directory(+OldName, +Options) is semidet.
%
%	Extended verison of delete_directory/1. The only implemented
%	option is `if_nonempty(error)`, which is also the default
%	behavior. On SICStus, `if_nonempty` additionally accepts the
%	values `ignore`, `fail`, and `delete`, which are not yet
%	emulated.

delete_directory(OldName, []) :- delete_directory(OldName).
delete_directory(OldName, [if_nonempty(error)]) :- delete_directory(OldName).


%!	directory_exists(+Directory) is semidet.
%!	directory_exists(+Directory, +Mode) is semidet.
%
%	True if a directory exists at path Directory and can be accessed
%	according to Mode (defaults to `exist`). Accepts the same access
%	modes as absolute_file_name/3's `access` option.

directory_exists(Directory) :- exists_directory(Directory).
directory_exists(Directory, Mode) :-
	% According to the SICStus 4.6 docs, this is "more or less equivalent".
	absolute_file_name(Directory, _, [file_type(directory), access(Mode), file_errors(fail)]).

% SWI's built-in make_directory/1 behaves like the one from SICStus library(file_systems).

%!	file_exists(+File) is semidet.
%!	file_exists(+File, +Mode) is semidet.
%
%	True if a regular file exists at path File and can be accessed
%	according to Mode (defaults to `exist`). Accepts the same access
%	modes as absolute_file_name/3's `access` option.

file_exists(File) :- exists_file(File).
file_exists(File, Mode) :-
	% According to the SICStus 4.6 docs, this is "more or less equivalent".
	absolute_file_name(File, _, [access(Mode), file_errors(fail)]).

%!	file_must_exist(+File) is det.
%!	file_must_exist(+File, +Mode) is det.
%
%	Ensure that a regular file exists at path File and can be
%	accessed according to Mode (defaults to `exist`). Otherwise an
%	exception is thrown. Accepts the same access modes as
%	absolute_file_name/3's `access` option.

file_must_exist(File) :- file_must_exist(File, exist).
file_must_exist(File, Mode) :-
	% According to the SICStus 4.6 docs, this is "more or less equivalent".
	absolute_file_name(File, _, [access(Mode), file_errors(error)]).

%!	directory_must_exist(+Directory) is det.
%!	directory_must_exist(+Directory, +Mode) is det.
%
%	Ensure that a directory exists at path Directory and can be
%	accessed according to Mode (defaults to `exist`). Otherwise an
%	exception is thrown. Accepts the same access modes as
%	absolute_file_name/3's `access` option.

directory_must_exist(Directory) :- directory_must_exist(Directory, exist).
directory_must_exist(Directory, Mode) :-
	% According to the SICStus 4.6 docs, this is "more or less equivalent".
	absolute_file_name(Directory, _, [file_type(directory), access(Mode), file_errors(error)]).


any_member_of_directory(Directory, ExtraOpts, BaseName, FullName) :-
	directory_files(Directory, Entries),
	member(BaseName, Entries),
	% SICStus docs say these special directories are never returned.
	BaseName \== (.),
	BaseName \== (..),
	% Filter entries based on ExtraOpts,
	% i. e. include only regular files or only directories.
	absolute_file_name(BaseName, FullName, [relative_to(Directory), file_errors(fail)|ExtraOpts]).
matching_member_of_directory(Directory, Pattern, ExtraOpts, BaseName, FullName) :-
	% To match patterns relative to a different directory,
	% make the pattern an absolute "path" relative to that directory.
	absolute_file_name(Pattern, FullPattern, [relative_to(Directory)]),
	atom_concat(Prefix, Pattern, FullPattern),
	expand_file_name(FullPattern, Entries),
	member(FullName, Entries),
	% Because we made the pattern absolute,
	% the returned entries will also be absolute,
	% but the interface requires us to return relative names as well.
	% This is a hacky way to convert the absolute entries back to relative.
	atom_concat(Prefix, BaseName, FullName),
	% Filter entries based on ExtraOpts,
	% i. e. include only regular files or only directories.
	% This also checks that the relative name hack above worked correctly.
	absolute_file_name(BaseName, FullName, [relative_to(Directory), file_errors(fail)|ExtraOpts]).

%!	directory_member_of_directory(-BaseName, -FullName) is nondet.
%!	directory_member_of_directory(+Directory, -BaseName, -FullName) is nondet.
%!	directory_member_of_directory(+Directory, +Pattern, -BaseName, -FullName) is nondet.
%!	file_member_of_directory(-BaseName, -FullName) is nondet.
%!	file_member_of_directory(+Directory, -BaseName, -FullName) is nondet.
%!	file_member_of_directory(+Directory, +Pattern, -BaseName, -FullName) is nondet.
%
%	True if Directory contains a directory or regular file
%	(respectively) named BaseName and the file's absolute path is
%	FullName. If Directory is not given, it defaults to the current
%	working directory. If Pattern is given, only succeeds if
%	BaseName also matches that glob pattern.
%
%	These predicates enumerate all matching files on backtracking.
%	This is also the intended usage pattern. For checking if a
%	specific file/directory exists, or to get its absolute path,
%	it's better to use file_exists/1, directory_exists/1, or
%	absolute_file_name/3.

directory_member_of_directory(BaseName, FullName) :-
	directory_member_of_directory((.), BaseName, FullName).
directory_member_of_directory(Directory, BaseName, FullName) :-
	any_member_of_directory(Directory, [file_type(directory)], BaseName, FullName).
directory_member_of_directory(Directory, Pattern, BaseName, FullName) :-
	matching_member_of_directory(Directory, Pattern, [file_type(directory)], BaseName, FullName).

file_member_of_directory(BaseName, FullName) :-
	file_member_of_directory((.), BaseName, FullName).
file_member_of_directory(Directory, BaseName, FullName) :-
	% access(exist) is needed to filter out directories.
	any_member_of_directory(Directory, [access(exist)], BaseName, FullName).
file_member_of_directory(Directory, Pattern, BaseName, FullName) :-
	% access(exist) is needed to filter out directories.
	matching_member_of_directory(Directory, Pattern, [access(exist)], BaseName, FullName).

%!	directory_members_of_directory(-Set) is det.
%!	directory_members_of_directory(+Directory, -Set) is det.
%!	directory_members_of_directory(+Directory, +Pattern, -Set) is det.
%!	file_members_of_directory(-Set) is det.
%!	file_members_of_directory(+Directory, -Set) is det.
%!	file_members_of_directory(+Directory, +Pattern, -Set) is det.
%
%	Unifies Set with a set of BaseName-FullName entries for all
%	directories or regular files (respectively) in Directory. If
%	Directory is not given, it defaults to the current working
%	directory. If Pattern is given, Set only includes entries where
%	BaseName matches that glob pattern.

directory_members_of_directory(Set) :-
	directory_members_of_directory((.), Set).
directory_members_of_directory(Directory, Set) :-
	findall(BaseName-FullName, directory_member_of_directory(Directory, BaseName, FullName), Set).
directory_members_of_directory(Directory, Pattern, Set) :-
	findall(BaseName-FullName, directory_member_of_directory(Directory, Pattern, BaseName, FullName), Set).

file_members_of_directory(Set) :-
	file_members_of_directory((.), Set).
file_members_of_directory(Directory, Set) :-
	findall(BaseName-FullName, file_member_of_directory(Directory, BaseName, FullName), Set).
file_members_of_directory(Directory, Pattern, Set) :-
	findall(BaseName-FullName, file_member_of_directory(Directory, Pattern, BaseName, FullName), Set).


%!	file_property(+Path, ?Property) is semidet.
%!	file_property(+Path, ?Property, -Value) is semidet.
%!	directory_property(+Path, ?Property) is semidet.
%!	directory_property(+Path, ?Property, -Value) is semidet.
%
%	True if a regular file or directory (respectively) exists at
%	Path and it has the given property and value. Property may be
%	unbound to backtrack over all available properties. If the Value
%	parameter is omitted, succeeds if Property has value `true`.
%
%	The following properties are currently supported:
%
%	* modify_timestamp
%	The file/directory's modification time as a Unix timestamp (as
%	returned by SWI's time_file/2)
%	* modify_localtime
%	The file/directory's modification time as a datime/6 term (as
%	returned by datime/2 from SICStus library(system)).
%	* readable
%	* writable
%	* executable
%	* searchable
%	`true` or `false` depending on whether the file/directory is
%	readable/writable/executable/searchable. `executable` is only
%	supported on regular files and `searchable` only on directories.
%	* size_in_bytes
%	The file's size in bytes. Not supported on directories.
%
%	As of SICStus 4.6.0, the following properties are not yet
%	emulated:
%
%	* create_timestamp
%	* access_timestamp
%	* create_localtime
%	* access_localtime
%	* set_user_id
%	* set_group_id
%	* save_text
%	* who_can_read
%	* who_can_write
%	* who_can_execute
%	* who_can_search
%	* owner_user_id
%	* owner_group_id
%	* owner_user_name
%	* owner_group_name

% Properties that are available and implemented identically for files and directories.
file_or_directory_property(FileOrDirectory, modify_timestamp, Value) :-
	time_file(FileOrDirectory, Value).
file_or_directory_property(FileOrDirectory, modify_localtime, Value) :-
	file_or_directory_property(FileOrDirectory, modify_timestamp, Timestamp),
	datime(Timestamp, Value).

directory_property(Directory, Property) :- directory_property(Directory, Property, true).

directory_property(Directory, readable, Value) :-
	directory_exists(Directory, read) -> Value = true ; Value = false.
directory_property(Directory, writable, Value) :-
	directory_exists(Directory, write) -> Value = true ; Value = false.
directory_property(Directory, searchable, Value) :-
	directory_exists(Directory, execute) -> Value = true ; Value = false.
directory_property(Directory, Property, Value) :-
	file_or_directory_property(Directory, Property, Value).

file_property(File, Property) :- file_property(File, Property, true).

file_property(File, readable, Value) :-
	file_exists(File, read) -> Value = true ; Value = false.
file_property(File, writable, Value) :-
	file_exists(File, write) -> Value = true ; Value = false.
file_property(File, executable, Value) :-
	file_exists(File, execute) -> Value = true ; Value = false.
file_property(File, size_in_bytes, Value) :- size_file(File, Value).
file_property(File, Property, Value) :-
	file_or_directory_property(File, Property, Value).


%!	current_directory(-Directory) is det.
%!	current_directory(-Directory, +NewDirectory) is det.
%
%	Unifies Directory with the current working directory path.
%	In the 2-argument form, also changes the working directory to
%	the path NewDirectory.

current_directory(Directory) :- working_directory(Directory, Directory).
current_directory(Directory, NewDirectory) :- working_directory(Directory, NewDirectory).
