/*
** Copyright 2000 Double Precision, Inc.
** See COPYING for distribution information.
*/

/*
** $Id$
*/

#ifndef __WINDOWS__
#include	"config.h"
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>

#include	"rfc822.h"
#include	"imaprefs.h"

static void swapmsgdata(struct imap_refmsg *a, struct imap_refmsg *b)
{
	char *cp;
	char c;
	time_t t;
	unsigned long ul;

#define swap(a,b,tmp) (tmp)=(a); (a)=(b); (b)=(tmp);

	swap(a->msgid, b->msgid, cp);
	swap(a->isdummy, b->isdummy, c);
	swap(a->flag2, b->flag2, c);

	swap(a->timestamp, b->timestamp, t);
	swap(a->seqnum, b->seqnum, ul);

#undef	swap
}

struct imap_refmsgtable *rfc822_threadalloc()
{
struct imap_refmsgtable *p;

	p=(struct imap_refmsgtable *)malloc(sizeof(struct imap_refmsgtable));
	if (p)
		memset(p, 0, sizeof(*p));
	return (p);
}

void rfc822_threadfree(struct imap_refmsgtable *p)
{
int i;
struct imap_refmsghash *h;
struct imap_subjlookup *s;
struct imap_refmsg *m;

	for (i=0; i<sizeof(p->hashtable)/sizeof(p->hashtable[0]); i++)
		while ((h=p->hashtable[i]) != 0)
		{
			p->hashtable[i]=h->nexthash;
			free(h);
		}

	for (i=0; i<sizeof(p->subjtable)/sizeof(p->subjtable[0]); i++)
		while ((s=p->subjtable[i]) != 0)
		{
			p->subjtable[i]=s->nextsubj;
			free(s->subj);
			free(s);
		}

	while ((m=p->firstmsg) != 0)
	{
		p->firstmsg=m->next;
		if (m->subj)
			free(m->subj);
		free(m);
	}
	free(p);
}

static int hashmsgid(const char *msgid)
{
unsigned long hashno=0;

	while (*msgid)
	{
	unsigned long n= (hashno << 1);

#define	HMIDS	(((struct imap_refmsgtable *)0)->hashtable)
#define	HHMIDSS	( sizeof(HMIDS) / sizeof( HMIDS[0] ))

		if (hashno & HHMIDSS )
			n ^= 1;

		hashno= n ^ (unsigned char)*msgid++;
	}

	return (hashno % HHMIDSS);
}

struct imap_refmsg *rfc822_threadallocmsg(struct imap_refmsgtable *mt,
					  const char *msgid)
{
int n=hashmsgid(msgid);
struct imap_refmsg *msgp= (struct imap_refmsg *)
	malloc(sizeof(struct imap_refmsg)+1+strlen(msgid));
struct imap_refmsghash *h, **hp;

	if (!msgp)	return (0);
	memset(msgp, 0, sizeof(*msgp));
	strcpy ((msgp->msgid=(char *)(msgp+1)), msgid);

	h=(struct imap_refmsghash *)malloc(sizeof(struct imap_refmsghash));
	if (!h)
	{
		free(msgp);
		return (0);
	}

	for (hp= &mt->hashtable[n]; *hp; hp= & (*hp)->nexthash)
	{
		if (strcmp( (*hp)->msg->msgid, msgp->msgid) > 0)
			break;
	}

	h->nexthash= *hp;
	*hp=h;
	h->msg=msgp;

	msgp->last=mt->lastmsg;

	if (mt->lastmsg)
		mt->lastmsg->next=msgp;
	else
		mt->firstmsg=msgp;

	mt->lastmsg=msgp;
	return (msgp);
}

struct imap_refmsg *rfc822_threadsearchmsg(struct imap_refmsgtable *mt,
					   const char *msgid)
{
int n=hashmsgid(msgid);
struct imap_refmsghash *h;

	for (h= mt->hashtable[n]; h; h= h->nexthash)
	{
	int	rc=strcmp(h->msg->msgid, msgid);

		if (rc == 0)	return (h->msg);
		if (rc > 0)
			break;
	}
	return (0);
}

static int findsubj(struct imap_refmsgtable *mt, const char *s, int *isrefwd,
		    int create, struct imap_subjlookup **ptr)
{
	char *ss=rfc822_coresubj(s, isrefwd);
	int n;
	struct imap_subjlookup **h;
	struct imap_subjlookup *newsubj;

	if (!ss)	return (-1);
	n=hashmsgid(ss);

	for (h= &mt->subjtable[n]; *h; h= &(*h)->nextsubj)
	{
	int	rc=strcmp((*h)->subj, ss);

		if (rc == 0)
		{
			free(ss);
			*ptr= *h;
			return (0);
		}
		if (rc > 0)
			break;
	}
	if (!create)
	{
		free(ss);
		*ptr=0;
		return (0);
	}

	newsubj=malloc(sizeof(struct imap_subjlookup));
	if (!newsubj)
	{
		free(ss);
		return (-1);
	}
	memset(newsubj, 0, sizeof(*newsubj));
	newsubj->subj=ss;
	newsubj->nextsubj= *h;
	newsubj->msgisrefwd= *isrefwd;
	*h=newsubj;
	*ptr=newsubj;
	return (0);
}

static void linkparent(struct imap_refmsg *msg, struct imap_refmsg *lastmsg)
{
	msg->parent=lastmsg;
	msg->prevsib=lastmsg->lastchild;
	if (msg->prevsib)
		msg->prevsib->nextsib=msg;
	else
		lastmsg->firstchild=msg;

	lastmsg->lastchild=msg;
	msg->nextsib=0;
}


static void breakparent(struct imap_refmsg *m)
{
	if (!m->parent)	return;

	if (m->prevsib)	m->prevsib->nextsib=m->nextsib;
	else		m->parent->firstchild=m->nextsib;

	if (m->nextsib)	m->nextsib->prevsib=m->prevsib;
	else		m->parent->lastchild=m->prevsib;
	m->parent=0;
}

static struct imap_refmsg *dorefcreate(struct imap_refmsgtable *mt,
				       const char *newmsgid,
				       struct rfc822a *a)
     /* a - references header */
{
struct imap_refmsg *lastmsg=0, *m;
struct imap_refmsg *msg;
int n;

/*
            (A) Using the Message-IDs in the message's references, link
            the corresponding messages together as parent/child.  Make
            the first reference the parent of the second (and the second
            a child of the first), the second the parent of the third
            (and the third a child of the second), etc.  The following
            rules govern the creation of these links:

               If no reference message can be found with a given
               Message-ID, create a dummy message with this ID.  Use
               this dummy message for all subsequent references to this
               ID.
*/

	for (n=0; n<a->naddrs; n++)
	{
		char *msgid=rfc822_getaddr(a, n);

		msg=*msgid ? rfc822_threadsearchmsg(mt, msgid ? msgid:""):0;
		if (!msg)
		{
			msg=rfc822_threadallocmsg(mt, msgid ? msgid:"");
			if (!msg)
			{
				if (msgid)
					free(msgid);

				return (0);
			}
			msg->isdummy=1;
		}

		if (msgid)
			free(msgid);

/*
               If a reference message already has a parent, don't change
               the existing link.
*/

		if (lastmsg == 0 || msg->parent)
		{
			lastmsg=msg;
			continue;
		}

/*
               Do not create a parent/child link if creating that link
               would introduce a loop.  For example, before making
               message A the parent of B, make sure that A is not a
               descendent of B.

*/

		for (m=lastmsg; m; m=m->parent)
			if (strcmp(m->msgid, msg->msgid) == 0)
				break;
		if (m)
		{
			lastmsg=msg;
			continue;
		}

		linkparent(msg, lastmsg);

		lastmsg=msg;
	}

/*
            (B) Create a parent/child link between the last reference
            (or NIL if there are no references) and the current message.
            If the current message has a parent already, break the
            current parent/child link before creating the new one.  Note
            that if this message has no references, that it will now
            have no parent.

               NOTE: The parent/child links MUST be kept consistent with
               one another at ALL times.

*/

	msg=*newmsgid ? rfc822_threadsearchmsg(mt, newmsgid):0;

	/*
	       If a message does not contain a Message-ID header line,
	       or the Message-ID header line does not contain a valid
	       Message ID, then assign a unique Message ID to this
	       message.

	       Implementation note: empty msgid, plus dupe check below,
	       implements that.
	*/

	if (msg && msg->isdummy)
	{
		msg->isdummy=0;
		if (msg->parent)
			breakparent(msg);
	}
	else
	{
#if 1
		/*
		** If two or more messages have the same Message ID, assign
		** a unique Message ID to each of the duplicates.
		**
		** Implementation note: just unlink the existing message from
		** it's parents/children.
		*/
		if (msg)
		{
			while (msg->firstchild)
				breakparent(msg->firstchild);
			breakparent(msg);
			newmsgid="";

			/* Create new entry with an empty msgid, if any more
			** msgids come, they'll hit the dupe check again.
			*/

		}
#endif
		msg=rfc822_threadallocmsg(mt, newmsgid);
		if (!msg)	return (0);
	}

	if (lastmsg)
	{
		for (m=lastmsg; m; m=m->parent)
			if (strcmp(m->msgid, msg->msgid) == 0)
				break;
		if (!m)
			linkparent(msg, lastmsg);
	}
	return (msg);
}

struct imap_refmsg *rfc822_threadmsg(struct imap_refmsgtable *mt,
				     const char *msgidhdr,
				     const char *refhdr,
				     const char *subjheader,
				     const char *dateheader,
				     unsigned long seqnum)
{
	struct rfc822t *t;
	struct rfc822a *a;
	struct imap_refmsg *m;

	char *msgid_s;

	t=rfc822t_alloc(msgidhdr ? msgidhdr:"", 0);
	if (!t)
		return (0);
	a=rfc822a_alloc(t);
	if (!a)
	{
		rfc822t_free(t);
		return (0);
	}

	msgid_s=a->naddrs > 0 ? rfc822_getaddr(a, 0):strdup("");

	rfc822a_free(a);
	rfc822t_free(t);

	if (!msgid_s)
		return (0);

	t=rfc822t_alloc(refhdr ? refhdr:"", 0);
	if (!t)
	{
		free(msgid_s);
		return (0);
	}

	a=rfc822a_alloc(t);
	if (!a)
	{
		rfc822t_free(t);
		free(msgid_s);
		return (0);
	}

	m=dorefcreate(mt, msgid_s, a);

	rfc822a_free(a);
	rfc822t_free(t);
	free(msgid_s);

	if (!m)
		return (0);

	if (subjheader && (m->subj=strdup(subjheader)) == 0)
		return (0);	/* Cleanup in rfc822_threadfree() */

	m->timestamp=dateheader ? rfc822_parsedt(dateheader):0;

	m->seqnum=seqnum;

	return (m);
}

/*
         (2) Gather together all of the messages that have no parents
         and make them all children (siblings of one another) of a dummy
         parent (the "root").  These messages constitute first messages
         of the threads created thus far.

*/

struct imap_refmsg *rfc822_threadgetroot(struct imap_refmsgtable *mt)
{
	struct imap_refmsg *root, *m;

	if (mt->rootptr)
		return (mt->rootptr);

	root=rfc822_threadallocmsg(mt, "(root)");

	if (!root)	return (0);

	root->parent=root;	/* Temporary */
	root->isdummy=1;

	for (m=mt->firstmsg; m; m=m->next)
		if (!m->parent)
		{
			if (m->isdummy && m->firstchild == 0)
				continue; /* Can happen in reference creation */

			linkparent(m, root);
		}
	root->parent=NULL;
	return (mt->rootptr=root);
}

/*
** 
**       (3) Prune dummy messages from the thread tree.  Traverse each
**        thread under the root, and for each message:
*/

void rfc822_threadprune(struct imap_refmsgtable *mt)
{
	struct imap_refmsg *msg;

	for (msg=mt->firstmsg; msg; msg=msg->next)
	{
		struct imap_refmsg *saveparent, *m;

		if (!msg->parent)
			continue;	// The root, need it later.

		if (!msg->isdummy)
			continue;

		/*
		**
		** If it is a dummy message with NO children, delete it.
		*/

		if (msg->firstchild == 0)
		{
			breakparent(msg);
			/*
			** Don't free the node, it'll be done on msgtable
			** purge.
			*/
			continue;
		}

		/*
		** If it is a dummy message with children, delete it, but
		** promote its children to the current level.  In other words,
		** splice them in with the dummy's siblings.
		**
		** Do not promote the children if doing so would make them
		** children of the root, unless there is only one child.
		*/

		if (msg->firstchild->nextsib &&
		    msg->parent->parent)
			continue;

		saveparent=msg->parent;
		breakparent(msg);

		while ((m=msg->firstchild) != 0)
		{
			breakparent(m);
			linkparent(m, saveparent);
		}
	}
}

/*
** (4) Gather together messages under the root that have the same
** extracted subject text.
**
** (A) Create a table for associating extracted subjects with
** messages.
**
** (B) Populate the subject table with one message per
** extracted subject.  For each message under the root:
*/

int rfc822_threadsortsubj(struct imap_refmsgtable *mt,
			  struct imap_refmsg *root)
{
	struct imap_refmsg *toproot, *p;

	for (toproot=root->firstchild; toproot; toproot=toproot->nextsib)
	{
		const char *subj;
		struct imap_subjlookup *subjtop;
		int isrefwd;

		/*
		** (i) Find the subject of this thread by extracting the
		** base subject from the current message, or its first child
		** if the current message is a dummy.
		*/

		p=toproot;
		if (p->isdummy)
			p=p->firstchild;

		subj=p->subj ? p->subj:"";


		/*
		** (ii) If the extracted subject is empty, skip this
		** message.
		*/

		if (*subj == 0)
			continue;

		/*
		** (iii) Lookup the message associated with this extracted
		** subject in the table.
		*/

		if (findsubj(mt, subj, &isrefwd, 1, &subjtop))
			return (-1);

		/*
		**
		** (iv) If there is no message in the table with this
		** subject, add the current message and the extracted
		** subject to the subject table.
		*/

		if (subjtop->msg == 0)
		{
			subjtop->msg=toproot;
			subjtop->msgisrefwd=isrefwd;
			continue;
		}

		/*
		** Otherwise, replace the message in the table with the
		** current message if the message in the table is not a
		** dummy AND either of the following criteria are true:
		*/

		if (!subjtop->msg->isdummy)
		{
			/*
			** The current message is a dummy
			**
			*/

			if (toproot->isdummy)
			{
				subjtop->msg=toproot;
				subjtop->msgisrefwd=isrefwd;
				continue;
			}

			/*
			** The message in the table is a reply or forward (its
			** original subject contains a subj-refwd part and/or a
			** "(fwd)" subj-trailer) and the current message is
			not.
			*/

			if (subjtop->msgisrefwd && !isrefwd)
			{
				subjtop->msg=toproot;
				subjtop->msgisrefwd=isrefwd;
			}
		}
	}
	return (0);
}

/*
** (C) Merge threads with the same subject.  For each message
** under the root:
*/

int rfc822_threadmergesubj(struct imap_refmsgtable *mt,
			   struct imap_refmsg *root)
{
	struct imap_refmsg *toproot, *p, *q, *nextroot;
	char *str;

	for (toproot=root->firstchild; toproot; toproot=nextroot)
	{
		const char *subj;
		struct imap_subjlookup *subjtop;
		int isrefwd;

		nextroot=toproot->nextsib;

		/*
		** (i) Find the subject of this thread as in step 4.B.i
		** above.
		*/

		p=toproot;
		if (p->isdummy)
			p=p->firstchild;

		subj=p->subj ? p->subj:"";

		/*
		** (ii) If the extracted subject is empty, skip this
		** message.
		*/

		if (*subj == 0)
			continue;

		/*
		** (iii) Lookup the message associated with this extracted
		** subject in the table.
		*/

		if (findsubj(mt, subj, &isrefwd, 0, &subjtop) || subjtop == 0)
			return (-1);

		/*
		** (iv) If the message in the table is the current message,
		** skip it.
		*/

		/* NOTE - ptr comparison IS NOT LEGAL */

		subjtop->msg->flag2=1;
		if (toproot->flag2)
		{
			toproot->flag2=0;
			continue;
		}
		subjtop->msg->flag2=0;

		/*
		** Otherwise, merge the current message with the one in the
		** table using the following rules:
		**
		** If both messages are dummies, append the current
		** message's children to the children of the message in
		** the table (the children of both messages become
		** siblings), and then delete the current message.
		*/

		if (subjtop->msg->isdummy && toproot->isdummy)
		{
			while ((p=toproot->firstchild) != 0)
			{
				breakparent(p);
				linkparent(p, subjtop->msg);
			}
			breakparent(toproot);
			continue;
		}

		/*
		** If the message in the table is a dummy and the current
		** message is not, make the current message a child of
		** the message in the table (a sibling of it's children).
		*/

		if (subjtop->msg->isdummy)
		{
			breakparent(toproot);
			linkparent(toproot, subjtop->msg);
			continue;
		}

		/*
		** If the current message is a reply or forward and the
		** message in the table is not, make the current message
		** a child of the message in the table (a sibling of it's
		** children).
		*/

		if (isrefwd)
		{
			p=subjtop->msg;
			if (p->isdummy)
				p=p->firstchild;

			subj=p->subj ? p->subj:"";

			str=rfc822_coresubj(subj, &isrefwd);

			if (!str)
				return (-1);
			free(str);	/* Don't really care */

			if (!isrefwd)
			{
				breakparent(toproot);
				linkparent(toproot, subjtop->msg);
				continue;
			}
		}

		/*
		** Otherwise, create a new dummy container and make both
		** messages children of the dummy, and replace the
		** message in the table with the dummy message.
		*/

		/* What we do is create a new message, then move the
		** contents of subjtop->msg (including its children)
		** to the new message, then make the new message a child
		** of subjtop->msg, and mark subjtop->msg as a dummy msg.
		*/

		q=rfc822_threadallocmsg(mt, "(dummy)");
		if (!q)
			return (-1);

		q->isdummy=1;

		swapmsgdata(q, subjtop->msg);

		while ((p=subjtop->msg->firstchild) != 0)
		{
			breakparent(p);
			linkparent(p, q);
		}
		linkparent(q, subjtop->msg);

		breakparent(toproot);
		linkparent(toproot, subjtop->msg);
	}
	return (0);
}

/*
** (5) Traverse the messages under the root and sort each set of
** siblings by date.  Traverse the messages in such a way that the
** "youngest" set of siblings are sorted first, and the "oldest"
** set of siblings are sorted last (grandchildren are sorted
** before children, etc).  In the case of an exact match on date,
** use the order in which the messages appear in the mailbox (that
** is, by sequence number) to determine the order.  In the case of
** a dummy message (which can only occur with top-level siblings),
** use its first child for sorting.
*/

static int cmp_msgs(const void *a, const void *b)
{
	struct imap_refmsg *ma=*(struct imap_refmsg **)a;
	struct imap_refmsg *mb=*(struct imap_refmsg **)b;
	time_t ta, tb;
	unsigned long na, nb;

	while (ma && ma->isdummy)
		ma=ma->firstchild;

	while (mb && mb->isdummy)
		mb=mb->firstchild;

	ta=tb=0;
	na=nb=0;
	if (ma)
	{
		ta=ma->timestamp;
		na=ma->seqnum;
	}
	if (mb)
	{
		tb=mb->timestamp;
		nb=mb->seqnum;
	}

	return (ta < tb ? -1: ta > tb ? 1:
		na < nb ? -1: na > nb ? 1:0);
}

struct imap_threadsortinfo {
	struct imap_refmsgtable *mt;
	struct imap_refmsg **sort_table;
	size_t sort_table_cnt;
} ;

static int dothreadsort(struct imap_threadsortinfo *,
			struct imap_refmsg *);

int rfc822_threadsortbydate(struct imap_refmsgtable *mt)
{
	struct imap_threadsortinfo itsi;
	int rc;

	itsi.mt=mt;
	itsi.sort_table=0;
	itsi.sort_table_cnt=0;

	rc=dothreadsort(&itsi, mt->rootptr);

	if (itsi.sort_table)
		free(itsi.sort_table);
	return (rc);
}

static int dothreadsort(struct imap_threadsortinfo *itsi,
			struct imap_refmsg *p)
{
	struct imap_refmsg *q;
	size_t i, n;

	for (q=p->firstchild; q; q=q->nextsib)
		dothreadsort(itsi, q);

	n=0;
	for (q=p->firstchild; q; q=q->nextsib)
		++n;

	if (n > itsi->sort_table_cnt)
	{
		struct imap_refmsg **new_array=(struct imap_refmsg **)
			(itsi->sort_table ?
			 realloc(itsi->sort_table,
				 sizeof(struct imap_refmsg *)*n)
			 :malloc(sizeof(struct imap_refmsg *)*n));

		if (!new_array)
			return (-1);

		itsi->sort_table=new_array;
		itsi->sort_table_cnt=n;
	}

	n=0;
	while ((q=p->firstchild) != 0)
	{
		breakparent(q);
		itsi->sort_table[n++]=q;
	}

	qsort(itsi->sort_table, n, sizeof(struct imap_refmsg *), cmp_msgs);

	for (i=0; i<n; i++)
		linkparent(itsi->sort_table[i], p);
	return (0);
}

struct imap_refmsg *rfc822_thread(struct imap_refmsgtable *mt)
{
	if (!mt->rootptr)
	{
		rfc822_threadprune(mt);
		if ((mt->rootptr=rfc822_threadgetroot(mt)) == 0)
			return (0);
		if (rfc822_threadsortsubj(mt, mt->rootptr) ||
		    rfc822_threadmergesubj(mt, mt->rootptr) ||
		    rfc822_threadsortbydate(mt))
		{
			mt->rootptr=0;
			return (0);
		}
	}

	return (mt->rootptr);
}
