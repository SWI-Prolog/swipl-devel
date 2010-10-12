/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(double_metaphone,
	  [ double_metaphone/2,		% +In, -Primary
	    double_metaphone/3		% +In, -Primary, -Secondary
	  ]).

:- use_foreign_library(foreign(double_metaphone)).

/** <module> Phonetic string matching

The library library(double_metaphone) implements   the  Double Metaphone
algorithm  developed  by  Lawrence  Philips    and   described  in  "The
Double-Metaphone Search Algorithm" by L   Philips, C/C++ User's Journal,
2000. Double Metaphone creates a key  from   a  word that represents its
phonetic properties. Two words  with  the   same  Double  Metaphone  are
supposed to sound similar. The Double Metaphone algorithm is an improved
version of the Soundex algorithm.

@license The Double Metaphone algorithm is copied from the Perl library
that holds the following copyright notice. To the best of our knowledge
the Perl license is compatible to the SWI-Prolog license schema and
therefore including this module poses no additional license conditions.

    ==
    Copyright 2000, Maurice Aubrey <maurice@hevanet.com>.
    All rights reserved.

    This code is based heavily on the C++ implementation by Lawrence
    Philips and incorporates several bug fixes courtesy of Kevin
    Atkinson <kevina@users.sourceforge.net>.

    This module is free software; you may redistribute it and/or
    modify it under the same terms as Perl itself.
    ==
*/

%%	double_metaphone(+In, -MetaPhone) is det.
%
%	Same as double_metaphone/3,  but  only   returning  the  primary
%	metaphone.

%%	double_metaphone(+In, -MetaPhone, -AltMetaphone) is det.
%
%	Create metaphone and alternative metaphone  from In. The primary
%	metaphone is based on english, while   the  secondary deals with
%	common alternative pronounciation in  other   languages.  In  is
%	either and atom, string object,  code-   or  character list. The
%	metaphones are always returned as atoms.
