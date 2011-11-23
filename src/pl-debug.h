/*  $Id$

    Part of SWI-Prolog

    Author:        Keri Harris
    E-mail:        keri.harris@securitease.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <stdio.h> /* get NULL */

#define DBG_LEVEL0  0
#define DBG_LEVEL1  1
#define DBG_LEVEL2  2
#define DBG_LEVEL3  3
#define DBG_LEVEL4  4
#define DBG_LEVEL5  5
#define DBG_LEVEL6  6
#define DBG_LEVEL7  7
#define DBG_LEVEL8  8
#define DBG_LEVEL9  9
#define MSG_VMI    10
#define CHK_SECURE 11

typedef struct debug_topic
{ unsigned    code;
  const char   *name;
} debug_topic;

#define DEBUG_TOPIC(Name) { Name, #Name }
#define MAX_DEBUG_TOPICS 11
