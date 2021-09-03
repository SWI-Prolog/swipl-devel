/*  Part of SWI-Prolog

    Author:        Roy Ratcliffe
    WWW:           http://www.swi-prolog.org

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

:- module(iri_scheme_file, []).

:- register_iri_scheme(file, file_iri_hook, []).
:- public file_iri_hook/3.

%!  file_iri_hook(+Action, +IRI, -Result) is semidet.
%
%   Hooks IRI-style file system access using   the "file" scheme. Useful
%   when dragging files in macOS, for   example,  where Finder drops the
%   file reference using a file://some-file atom. Maps file-scheme hooks
%   directly to their corresponding built-in file-oriented predicates.
%
%   Has  potential  self-recursion  since    the   delegated  predicates
%   themselves re-invoke the registered IRI hooks if they receive an IRI
%   scheme. The implementation therefore   relies  on file_name_to_url/2
%   for translating file-IRI references to  non-IRI file references, and
%   thereby avoids re-invoking the hooks.
%
%   @see register_iri_scheme/3
%   @see open/4

file_iri_hook(open(Mode, Options), IRI, Stream) :-
    file_name_to_url(File, IRI),
    open(File, Mode, Stream, Options).
file_iri_hook(access(Mode), IRI, Bool) :-
    file_name_to_url(File, IRI),
    (   (   Mode == directory
        ->  exists_directory(File)
        ;   access_file(File, Mode)
        )
    ->  Bool = true
    ;   Bool = false
    ).
file_iri_hook(time, IRI, Time) :-
    file_name_to_url(File, IRI),
    time_file(File, Time).
file_iri_hook(size, IRI, Size) :-
    file_name_to_url(File, IRI),
    size_file(File, Size).
