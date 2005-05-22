/*
** $Id$
*/
#ifndef	imaprefs_h
#define	imaprefs_h

/*
** Copyright 2000 Double Precision, Inc.
** See COPYING for distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif

#ifdef  __cplusplus
extern "C" {
#endif

/*
** Implement REFERENCES threading.
*/

/* The data structures */

struct imap_refmsg {
	struct imap_refmsg *next, *last;	/* Link list of all msgs */
	struct imap_refmsg *parent;		/* my parent */
	struct imap_refmsg *firstchild, *lastchild; /* Children link list */
	struct imap_refmsg *prevsib, *nextsib;	/* Link list of siblings */

	char isdummy;			/* this is a dummy node (for now) */
	char flag2;			/* Additional flag */

	char *msgid;			/* msgid of this message */

	char *subj;			/* dynalloced subject of this msg */
	time_t timestamp;		/* Timestamp */
	unsigned long seqnum;		/* Sequence number */
} ;

struct imap_refmsgtable {
        struct imap_refmsg *firstmsg, *lastmsg; /* Link list of all msgs */

        /* hash table message id lookup */

        struct imap_refmsghash *hashtable[512];

        struct imap_subjlookup *subjtable[512];

        struct imap_refmsg *rootptr;            /* The root */
} ;

struct imap_refmsgtable *rfc822_threadalloc(void);
void rfc822_threadfree(struct imap_refmsgtable *);
struct imap_refmsg *rfc822_threadmsg(struct imap_refmsgtable *mt,
				     const char *msgidhdr,
				     const char *refhdr,
				     const char *subjheader,
				     const char *dateheader,
				     unsigned long seqnum);

struct imap_refmsg *rfc822_thread(struct imap_refmsgtable *mt);

	/* INTERNAL FUNCTIONS FOLLOW */


struct imap_refmsghash {
	struct imap_refmsghash *nexthash;
	struct imap_refmsg *msg;
} ;

struct imap_subjlookup {
	struct imap_subjlookup *nextsubj;
	char *subj;
	struct imap_refmsg *msg;
	int msgisrefwd;
} ;

struct imap_refmsg *rfc822_threadallocmsg(struct imap_refmsgtable *mt,
					  const char *msgid);
void rfc822_threadprune(struct imap_refmsgtable *mt);
struct imap_refmsg *rfc822_threadgetroot(struct imap_refmsgtable *mt);
struct imap_refmsg *rfc822_threadsearchmsg(struct imap_refmsgtable *mt,
					   const char *msgid);
int rfc822_threadsortsubj(struct imap_refmsgtable *mt,
			  struct imap_refmsg *root);
int rfc822_threadmergesubj(struct imap_refmsgtable *mt,
			   struct imap_refmsg *root);
int rfc822_threadsortbydate(struct imap_refmsgtable *mt);


#ifdef  __cplusplus
}
#endif

#endif
