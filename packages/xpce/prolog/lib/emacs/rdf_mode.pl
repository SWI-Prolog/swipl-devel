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

:- module(emacs_rdf_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- use_module(sgml_mode).
:- use_module(library(rdf)).

:- pce_autoload(rdf_diagram, library(rdf_diagram)).

:- emacs_begin_mode(rdf, xml,
		    "Mode for editing RDF documents",
		    [ -			     = button(sgml),
		      show_diagram	     = button(sgml)
		    ],
		    []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<?xml version="1.0" encoding="iso-8859-1"?>\n\n\
	      <!DOCTYPE rdf [\n  \
	      <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n  \
              <!ENTITY xsd  "http://www.w3.org/2000/10/XMLSchema#">\n\
              ]>\n\n\
	      <rdf:RDF\n  \
	      xmlns:rdf ="&rdf;"\n  \
	      xmlns:xsd ="&xsd;"\n\
	      >\n\n\
	      </rdf:RDF>\n').

show_diagram(M) :->
	"Show diagram of file"::
	get(M, text_buffer, TB),
	pce_open(TB, read, In),
	load_rdf(stream(In), Triples,
		 [ expand_foreach(true)
		 ]),
	close(In),
	new(D, rdf_diagram(string('RDF triple diagram'))),
	send(new(report_dialog), below, D),
	send(D, triples, Triples),
	send(D, open).

:- emacs_end_mode.


:- emacs_begin_mode(rdfs, rdf,
		    "Mode for editing RDFS documents",
		    [],
		    []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<?xml version="1.0" encoding="iso-8859-1"?>\n\n\
	      <!DOCTYPE rdfs [\n  \
	      <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n  \
     	      <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#">\n  \
              <!ENTITY xsd  "http://www.w3.org/2000/10/XMLSchema#">\n\
              ]>\n\n\
	      <rdf:RDF\n  \
	      xmlns:rdf ="&rdf;"\n  \
	      xmlns:rdfs="&rdfs;"\n  \
	      xmlns:xsd ="&xsd;"\n\
	      >\n\n\
	      </rdf:RDF>\n').

:- emacs_end_mode.



:- emacs_begin_mode(owl, rdfs,
		    "Mode for editing OWL documents",
		    [],
		    []).

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<?xml version="1.0" encoding="iso-8859-1"?>\n\n\
	      <!DOCTYPE owl [\n  \
	      <!ENTITY rdf  "http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n  \
     	      <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#">\n  \
	      <!ENTITY owl  "http://www.w3.org/2002/7/owl#">\n  \
              <!ENTITY xsd  "http://www.w3.org/2000/10/XMLSchema#">\n  \
	      <!ENTITY dc   "http://purl.org/dc/elements/1.1/">\n\
              ]>\n\n\
	      <rdf:RDF\n  \
	      xmlns:rdf ="&rdf;"\n  \
	      xmlns:rdfs="&rdfs;"\n  \
	      xmlns:owl ="&owl;"\n  \
	      xmlns:xsd ="&xsd;"\n  \
	      xmlns:dc  ="&dc;"\n\
	      >\n\n\
	      <Ontology rdf:about="">\n\
	      </Ontology>\n\n\
	      </rdf:RDF>\n').

:- emacs_end_mode.



