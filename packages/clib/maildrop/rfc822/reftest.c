/*
** Copyright 2000 Double Precision, Inc.
** See COPYING for distribution information.
*/

/*
** $Id$
*/

#include	"config.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>

#include	"rfc822.h"
#include	"imaprefs.h"


static void test1()
{
struct imap_refmsgtable *mt=rfc822_threadalloc();
char	buf[20];
struct imap_refmsg *p;

        strcpy(buf, "a@b");
        p=rfc822_threadallocmsg(mt, buf);
        strcpy(buf, "c@d");
        p=rfc822_threadallocmsg(mt, buf);

	printf("%s\n", (rfc822_threadsearchmsg(mt, "a@b")
			? "found":"not found"));
	printf("%s\n", (rfc822_threadsearchmsg(mt, "c@d")
			? "found":"not found"));
	printf("%s\n", (rfc822_threadsearchmsg(mt, "e@f")
			? "found":"not found"));

	rfc822_threadfree(mt);
}

static void prtree(struct imap_refmsg *m)
{
	printf("<%s>", m->msgid ? m->msgid:"");

	if (m->isdummy)
	{
		printf(" (dummy)");
	}

	printf(".parent=");
	if (m->parent)
		printf("<%s>", m->parent->msgid ? m->parent->msgid:"");
	else
		printf("ROOT");

	printf("\n");

	for (m=m->firstchild; m; m=m->nextsib)
		prtree(m);
}

static void prpc(struct imap_refmsgtable *mt)
{
	struct imap_refmsg *root=rfc822_threadgetroot(mt), *m;

	if (!root)
		return;

	for (m=root->firstchild; m; m=m->nextsib)
		prtree(m);

	printf("\n\n");
}

static void test2()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<1>", NULL,
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<2>",
			 "<1>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<4>",
			 "<1> <2> <3>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	prpc(mt);
	rfc822_threadfree(mt);
}

static void test3()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<4>",
			 "<2> <1> <3>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<3>",
			 "<1> <2>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<2>",
			 "<1>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<1>", NULL,
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	prpc(mt);
	rfc822_threadfree(mt);
}

static void test4()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<1>", NULL,
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<2>", "<1>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<4>", "<1> <2> <3>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	prpc(mt);
	rfc822_threadprune(mt);
	prpc(mt);
	rfc822_threadfree(mt);
}

static void test5()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<4>", "<1> <2> <3>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<3>", NULL,
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	prpc(mt);
	rfc822_threadprune(mt);
	prpc(mt);
	rfc822_threadfree(mt);
}

static void prsubj(struct imap_refmsgtable *p)
{
	struct imap_subjlookup *s;
	int i;

	for (i=0; i<sizeof(p->subjtable)/sizeof(p->subjtable[0]); i++)
		for (s=p->subjtable[i]; s; s=s->nextsubj)
			printf("subject(%s)=<%s>\n", s->subj,
			       s->msg->msgid ? s->msg->msgid:"");
	printf("\n\n");
}

static void test6()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<message1>", NULL,
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message10>", NULL,
			 "subject 2",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message3>", "<message2>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message11>", NULL,
			 "Re: subject 4",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message12>", NULL,
			 "subject 4",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message13>", NULL,
			 "subject 5",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message14>", NULL,
			 "re: subject 5",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadprune(mt);
	rfc822_threadsortsubj(mt, rfc822_threadgetroot(mt));
	prpc(mt);
	prsubj(mt);
	rfc822_threadfree(mt);
}

static void test7()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<message1>", "<message1-dummy>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message2>", "<message2-dummy>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);
	rfc822_threadprune(mt);
	rfc822_threadsortsubj(mt, rfc822_threadgetroot(mt));
	prpc(mt);
	prsubj(mt);
	rfc822_threadmergesubj(mt, rfc822_threadgetroot(mt));
	prpc(mt);
	rfc822_threadfree(mt);
}

static void test8()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<message4>", NULL,
			 "subject 2",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message2>", NULL,
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message1>", "<message1-dummy>",
			 "subject 1",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message3>", NULL,
			 "Re: subject 2",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message10>", NULL,
			 "subject 10",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message11>", NULL,
			 "subject 10",
			 "Thu, 29 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadprune(mt);
	rfc822_threadsortsubj(mt, rfc822_threadgetroot(mt));
	prpc(mt);
	prsubj(mt);
	rfc822_threadmergesubj(mt, rfc822_threadgetroot(mt));
	prpc(mt);
	rfc822_threadfree(mt);
}

static void test9()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<message1>", NULL,
			 "subject 1",
			 "Thu, 20 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message2>", NULL,
			 "subject 1",
			 "Thu, 19 Jun 2000 14:41:58 -0700", 2);

	rfc822_threadmsg(mt, "<message3>", NULL,
			 "subject 1",
			 "Thu, 21 Jun 2000 14:41:58 -0700", 3);

	rfc822_threadmsg(mt, "<message4>", "<message2>",
			 "subject 2",
			 "Thu, 21 Jun 2000 14:41:58 -0700", 6);

	rfc822_threadmsg(mt, "<message5>", "<message2>",
			 "subject 2",
			 "Thu, 21 Jun 2000 14:41:58 -0700", 5);

	rfc822_threadmsg(mt, "<message6>", "<message2>",
			 "subject 2",
			 "Thu, 20 Jun 2000 14:41:58 -0700", 4);


	rfc822_threadprune(mt);
	rfc822_threadsortsubj(mt, rfc822_threadgetroot(mt));
	rfc822_threadmergesubj(mt, rfc822_threadgetroot(mt));
	rfc822_threadsortbydate(mt);
	prpc(mt);
	rfc822_threadfree(mt);
}

static void test10()
{
	struct imap_refmsgtable *mt=rfc822_threadalloc();

	rfc822_threadmsg(mt, "<message1>", NULL,
			 "subject 1",
			 "Thu, 20 Jun 2000 14:41:58 -0700", 1);

	rfc822_threadmsg(mt, "<message4>", "<message1>",
			 "subject 2",
			 "Thu, 21 Jun 2000 14:41:58 -0700", 6);

	rfc822_threadmsg(mt, "<message1>", NULL,
			 "subject 2",
			 "Thu, 21 Jun 2000 14:41:58 -0700", 5);

	rfc822_threadmsg(mt, "<message4>", "<message1>",
			 "subject 2",
			 "Thu, 21 Jun 2000 14:41:58 -0700", 6);

	rfc822_threadprune(mt);
	rfc822_threadsortsubj(mt, rfc822_threadgetroot(mt));
	rfc822_threadmergesubj(mt, rfc822_threadgetroot(mt));
	rfc822_threadsortbydate(mt);
	prpc(mt);
	rfc822_threadfree(mt);
}

int main(int argc, char **argv)
{
	test1();
	test2();
	test3();
	test4();
	test5();
	test6();
	test7();
	test8();
	test9();
	test10();
	return (0);
}
