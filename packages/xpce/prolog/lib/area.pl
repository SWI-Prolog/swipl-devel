% File:		area.pl
% Module:	area
% Part of:	PCE library
% Author:	Jan Wielemaker & Anjo Anjewierden, UvA SWI
% Purpose:	Area related predicates
% Works with:	SWI-Prolog 1.2, PCE-3.6
% Notice:	Copyright (c) 1989 University of Amsterdam
% History:	Tue Apr 18 16:50:34 1989 (Created)

:- module(area,
	[ side_pattern/3	% Side x Side x Pattern
	]).

%   side_pattern(+SideA, +SideB, -Pattern)
%
%   Pattern is the bit if SideA on area A corresponds to SideB on area B.  

side_pattern(top,    top,    2'1).
side_pattern(top,    center, 2'10).
side_pattern(top,    bottom, 2'100).
side_pattern(center, top,    2'1000).
side_pattern(center, center, 2'10000).
side_pattern(center, bottom, 2'100000).
side_pattern(bottom, top,    2'1000000).
side_pattern(bottom, center, 2'10000000).
side_pattern(bottom, bottom, 2'100000000).
side_pattern(left,   left,   2'1000000000).
side_pattern(left,   middle, 2'10000000000).
side_pattern(left,   right,  2'100000000000).
side_pattern(middle, left,   2'1000000000000).
side_pattern(middle, middle, 2'10000000000000).
side_pattern(middle, right,  2'100000000000000).
side_pattern(right,  left,   2'1000000000000000).
side_pattern(right,  middle, 2'10000000000000000).
side_pattern(right,  right,  2'100000000000000000).
