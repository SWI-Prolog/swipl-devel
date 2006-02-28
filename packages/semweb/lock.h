/*  $Id$

    Part of the SWI-Prolog Semweb package

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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
*/

#ifndef LOCK_H_INCLUDED
#define LOCK_H_INCLUDED

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

typedef struct rwlock
{
#ifdef _REENTRANT
#ifdef WIN32
  CRITICAL_SECTION	mutex;
  CRITICAL_SECTION	misc_mutex;
  win32_cond_t		rdcondvar;
  win32_cond_t		wrcondvar;
  win32_cond_t		upcondvar;
#else
  pthread_mutex_t	mutex;
  pthread_mutex_t	misc_mutex;
  pthread_cond_t	rdcondvar;
  pthread_cond_t	wrcondvar;
  pthread_cond_t	upcondvar;
#endif
  int			waiting_readers;
  int			waiting_writers;
  int			waiting_upgrade;
  int		       *read_by_thread;
  int			allow_readers;
  int			lock_level;	/* recursive locks */
#endif
  int			writer;
  int			readers;
} rwlock;

int	rdlock(rwlock *lock);
int	wrlock(rwlock *lock, int allow_readers);
int	lockout_readers(rwlock *lock);
void	reallow_readers(rwlock *lock);
int	unlock(rwlock *lock, int rd);
int	lock_misc(rwlock *lock);
int	unlock_misc(rwlock *lock);
int	init_lock(rwlock *lock);

#endif /*LOCK_H_INCLUDED*/
