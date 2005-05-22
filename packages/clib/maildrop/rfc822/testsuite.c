/*
** Copyright 1998 - 2000 Double Precision, Inc.
** See COPYING for distribution information.
*/

#include	"rfc822.h"
#include	<stdio.h>
#include	<stdlib.h>

static const char rcsid[]="$Id$";

static void print_func(char c, void *p)
{
	p=p;
	putchar(c);
}

static void print_separator(const char *s, void *p)
{
	p=p;
	printf("%s", s);
}

static struct rfc822t *tokenize(const char *p)
{
struct	rfc822t	*tp;
int	i;
char	buf[2];

	printf("Tokenize: %s\n", p);
	tp=rfc822t_alloc(p, NULL);
	if (!tp)	exit(0);
	buf[1]=0;
	for (i=0; i<tp->ntokens; i++)
	{
		buf[0]=tp->tokens[i].token;
		if (buf[0] == '\0' || buf[0] == '"' || buf[0] == '(')
		{
			printf("%s: ", buf[0] == '"' ? "Quote":
				buf[0] == '(' ? "Comment":"Atom");
			fwrite(tp->tokens[i].ptr, tp->tokens[i].len, 1, stdout);
			printf("\n");
		}
		else	printf("Token: %s\n", buf[0] ? buf:"atom");
	}
	return (tp);
}

static struct rfc822a *doaddr(struct rfc822t *t)
{
struct rfc822a *a=rfc822a_alloc(t);

	if (!a)	exit(0);
	printf("----\n");
	rfc822_print(a, print_func, print_separator, NULL);
	printf("\n");
	rfc822_addrlist(a, print_func, NULL);
	rfc822_namelist(a, print_func, NULL);
	return (a);
}

int main()
{
struct	rfc822t *t1, *t2, *t3, *t4;
struct	rfc822a *a1, *a2, *a3, *a4;

	t1=tokenize("nobody@example.com (Nobody (is) here\\) right)");
	t2=tokenize("Distribution  list: nobody@example.com daemon@example.com");
	t3=tokenize("Mr Nobody <nobody@example.com>, Mr. Nobody <nobody@example.com>");
	t4=tokenize("nobody@example.com, <nobody@example.com>, Mr. Nobody <nobody@example.com>");

	a1=doaddr(t1);
	a2=doaddr(t2);
	a3=doaddr(t3);
	a4=doaddr(t4);

	rfc822a_free(a4);
	rfc822a_free(a3);
	rfc822a_free(a2);
	rfc822a_free(a1);
	rfc822t_free(t4);
	rfc822t_free(t3);
	rfc822t_free(t2);
	rfc822t_free(t1);
	return (0);
}
