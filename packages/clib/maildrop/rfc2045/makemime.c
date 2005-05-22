/*
** Copyright 2000 Double Precision, Inc.  See COPYING for
** distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<time.h>
#include	<stdio.h>
#include	<errno.h>
#include	<string.h>
#include	<signal.h>
#if	HAVE_STRINGS_H
#include	<strings.h>
#endif
#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<stdlib.h>
#include	<ctype.h>
#include	<pwd.h>
#include	<fcntl.h>
#include	<signal.h>
#include	"rfc2045.h"
#include	"rfc2045charset.h"
#if HAVE_UNISTD_H
#include	<unistd.h>
#endif
#if HAVE_SYS_WAIT_H
#include	<sys/wait.h>
#endif
#include	"numlib/numlib.h"

#if     HAS_GETHOSTNAME
#else
int gethostname(const char *, size_t);
#endif

static const char rcsid[]="$Id$";

struct arg_list {
	struct arg_list *next;
	char *arg;
	} ;

/******************************************************************************

Open some file or a pipe for reading and writing.

******************************************************************************/

static FILE *openfile_or_pipe(const char *filename, const char *mode)
{
int	fd;
FILE	*fp;

	if (strcmp(filename, "-") == 0)	/* stdin or stdout */
		fd=dup( strcmp(mode, "r") ? 1:0);
	else if (*filename == '&')
		fd=dup( atoi(filename+1));	/* file descriptor */
	else fd=open(filename, (strcmp(mode, "r") ? O_WRONLY|O_CREAT|O_TRUNC:
			O_RDONLY), 0666);	/* or a file */
	if (fd < 0)
	{
		perror(filename);
		exit(1);
	}
	fp=fdopen(fd, mode);
	if (!fp)
	{
		perror("fdopen");
		exit(1);
	}
	return (fp);
}

/******************************************************************************

Open some file.  If we get a pipe, open a temporary file, and drain pipe's
contents into it.

******************************************************************************/

static FILE *openfile(const char *filename)
{
FILE	*fp=openfile_or_pipe(filename, "r");
int	fd=fileno(fp);
off_t	orig_pos;

	if ((orig_pos=lseek(fd, 0L, SEEK_CUR)) == -1 ||
		lseek(fd, 0L, SEEK_END) == -1 ||
		lseek(fd, 0L, SEEK_CUR) == -1 ||
		lseek(fd, orig_pos, SEEK_SET) == -1)	/* Must be a pipe */
	{
	FILE *t=tmpfile();
	int	c;

		if (!t)
		{
			perror("tmpfile");
			exit(1);
		}

		while ((c=getc(fp)) != EOF)
			putc(c, t);
		if (ferror(fp) || fflush(t)
			|| ferror(t) || fseek(t, 0L, SEEK_SET) == -1)
		{
			perror("write");
			exit(1);
		}
		fclose(fp);
		fp=t;
	}
	return (fp);
}

/******************************************************************************

Build argv/argc from a file.

******************************************************************************/

static void read_args(int *argcp, char ***argvp, const char *file)
{
FILE	*fp=openfile_or_pipe(file, "r");
struct arg_list *argfirst=0, *arglast=0, *argp;
char	buffer[BUFSIZ];
char	*p;
int	c;

	*argcp=0;
	while (fgets(buffer, sizeof(buffer), fp) != 0)
	{
	const	char *q;

		if ((p=strchr(buffer, '\n')) != 0)
			*p=0;
		else while ((c=getc(fp)) != '\n' && c != EOF)
			;	/* Just dump the excess */

		/* Skip the filler. */

		q=buffer;
		while (*q && isspace((int)(unsigned char)*q))
			++q;
		if (!*q)	continue;
		if (*q == '#')	continue;
		if (strcmp(buffer, "-") == 0)	break;

		argp=(struct arg_list *)malloc(sizeof(struct arg_list)+1+
			strlen(q));
		if (!argp)
		{
			perror("malloc");
			exit(1);
		}
		if (arglast)
			arglast->next=argp;
		else
			argfirst=argp;
		arglast=argp;
		++*argcp;
		argp->next=0;
		argp->arg=strcpy((char *)(argp+1), q);
	}

	if ((*argvp=malloc(sizeof (char *) * (*argcp+1))) == 0)
	{
		perror("malloc");
		exit(1);
	}
	c=0;
	for (argp=argfirst; argp; argp=argp->next)
	{
		(*argvp)[c]= argp->arg;
		++c;
	}
	(*argvp)[c]=0;
}

static void usage()
{
	fprintf(stderr,
"Usage:\n"
"  makemime -c type [-o file] [-e encoding] [-a \"Header: Contents\"] file\n"
"           -m [ type ] [-o file] [-e encoding] [-a \"Header: Contents\"] file\n"
"           -j [-o file] file1 file2\n"
"           @file\n"
"\n"
"   file:  filename    - read or write from filename\n"
"          -           - read or write from stdin or stdout\n"
"          &n          - read or write from file descriptor n\n"
"          \\( opts \\)  - read from child process, that generates [ opts ]\n"
"\n"
"Options:\n"
"\n"
"  -c type         - create a new mime section from \"file\" with this\n"
"                    Content-Type: (default is application/octet-stream).\n"
"  -m [ type ]     - create a multipart mime section from \"file\" of this\n"
"                    Content-Type: (default is multipart/mixed).\n"
"  -e encoding     - use the given encoding (7bit, 8bit, quoted-printable,\n"
"                    or base64), instead of guessing.\n"
"  -j file1 file2  - join mime section file2 to multipart section file1.\n"
"  -o file         - write ther result to file, instead of stdout (not\n"
"                    allowed in child processes).\n"
"  -a header       - prepend an additional header to the output.\n"
"\n"
"  @file - read all of the above options from file, one option or\n"
"          value on each line.\n"
	);
	exit (0);
}

/******************************************************************************

The arguments are parsed into the following structure, as a tree.

******************************************************************************/
struct mimestruct {

	/*
	** One or two input files.  We initialize either file or child,
	** depending on the source being a file, or a child process.
	** Later, we open a file pointer in either case.
	*/

	const char *inputfile1, *inputfile2;
	struct mimestruct *inputchild1, *inputchild2;
	FILE *inputfp1, *inputfp2;
	pid_t	child1, child2;

	/* Output file.  Defaults to "-", stdout */

	const char *outputfile;
	FILE	*outputfp;

		/* The handler and open functions */

	void (*handler_func)(struct mimestruct *);
	void (*open_func)(struct mimestruct *);

		/* The new mime type, and encoding (-e) */
	const char *mimetype;
	const char *mimeencoding;

		/* A list of -a headers */
	struct arg_list *a_first, *a_last;
	} ;

static void createsimplemime(struct mimestruct *);
static void createmultipartmime(struct mimestruct *);
static void joinmultipart(struct mimestruct *);

static void opencreatesimplemime(struct mimestruct *);
static void opencreatemultipartmime(struct mimestruct *);
static void openjoinmultipart(struct mimestruct *);

/******************************************************************************

Recursively build the mimestruct tree.

******************************************************************************/

struct mimestruct *parseargs(int *argcp, char ***argvp)
{
struct mimestruct *m=malloc(sizeof(struct mimestruct));
int argc= *argcp;
char **argv= *argvp;

	if (!m)
	{
		perror("malloc");
		exit(1);
	}
	memset(m, 0, sizeof(*m));

	if (argc == 0 || argv[0][0] != '-')	usage();

	if (strncmp(argv[0], "-c", 2) == 0)
	{
		m->handler_func= &createsimplemime;
		m->open_func= &opencreatesimplemime;
		if (argv[0][2])
		{
			m->mimetype=argv[0]+2;
			--argc;
			++argv;
		}
		else
		{
			--argc;
			++argv;
			if (argc && argv[0][0] != '-' && argv[0][0] != ')')
			{
				m->mimetype=argv[0];
				--argc;
				++argv;
			}
			else
				m->mimetype="application/octet-stream";
		}

		while (isspace((int)(unsigned char)*m->mimetype))
			++m->mimetype;
	}
	else if (strncmp(argv[0], "-m", 2) == 0)
	{
		m->handler_func= &createmultipartmime;
		m->open_func= &opencreatemultipartmime;
		if (argv[0][2])
		{
			m->mimetype=argv[0]+2;
			--argc;
			++argv;
		}
		else
		{
			--argc;
			++argv;
			if (argc && argv[0][0] != '-' && argv[0][0] != ')')
			{
				m->mimetype=argv[0];
				--argc;
				++argv;
			}
			else
				m->mimetype="multipart/mixed";
		}
		while (isspace((int)(unsigned char)*m->mimetype))
			++m->mimetype;
	}
	else if (strncmp(argv[0], "-j", 2) == 0)
	{
	const char *filename;

		m->handler_func= &joinmultipart;
		m->open_func= &openjoinmultipart;
		if (argv[0][2])
		{
			filename=argv[0]+2;
			--argc;
			++argv;
		}
		else
		{
			--argc;
			++argv;
			if (argc == 0)	usage();
			filename=argv[0];
			--argc;
			++argv;
		}

		while (isspace((int)(unsigned char)*filename))
			++filename;

		if (strcmp(filename, "(") == 0)
		{
			m->inputchild2=parseargs(&argc, &argv);
			if (argc == 0 || strcmp(argv[0], ")"))
				usage();
			--argc;
			++argv;
		}
		else
			m->inputfile2=filename;
	}

	/* Handle common options */

	while (argc)
	{
		if (strncmp(argv[0], "-o", 2) == 0)
		{
		const char *f=argv[0]+2;

			++argv;
			--argc;
			if (*f == 0)
			{
				if (!argc)	usage();
				f=argv[0];
				++argv;
				--argc;
			}
			while (isspace((int)(unsigned char)*f))
				++f;
			m->outputfile=f;
			continue;
		}

		if (strncmp(argv[0], "-e", 2) == 0)
		{
		char *f=argv[0]+2, *q;

			++argv;
			--argc;

			if (*f == 0)
			{
				if (!argc)	usage();
				f=argv[0];
				++argv;
				--argc;
			}

			for (q=f; *q; q++)
				*q=tolower((int)(unsigned char)*q);

			while (isspace((int)(unsigned char)*f))
				++f;

			if (strcmp(f, "7bit") && strcmp(f, "8bit") &&
				strcmp(f, "quoted-printable") &&
				strcmp(f, "base64"))
				usage();

			m->mimeencoding=f;
			continue;
		}

		if (strncmp(argv[0], "-a", 2) == 0)
		{
		char *f=argv[0]+2;
		struct arg_list *a;

			++argv;
			--argc;

			if (*f == 0)
			{
				if (!argc)	usage();
				f=argv[0];
				++argv;
				--argc;
			}

			while (isspace((int)(unsigned char)*f))
				++f;

			a=malloc(sizeof(struct arg_list));
			if (!a)
			{
				perror("malloc");
				exit(1);
			}
			if (m->a_last)
				m->a_last->next=a;
			else	m->a_first=a;
			m->a_last=a;
			a->arg=f;
			a->next=0;
			continue;
		}
		break;
	}

	/* We must now have the input file argument */

	if (!argc)	usage();

	if (strcmp(argv[0], "(") == 0)
	{
		--argc;
		++argv;
		m->inputchild1=parseargs(&argc, &argv);
		if (argc == 0 || strcmp(argv[0], ")"))
			usage();
		--argc;
		++argv;
	}
	else
	{
		m->inputfile1=argv[0];
		--argc;
		++argv;
	}

	*argcp=argc;
	*argvp=argv;
	return (m);
}

/******************************************************************************

After we're done, terminate with a zero exit code if all child processes also
terminated with a zero exit code.  Otherwise, terminate with a non-zero exit
code thus propagating any child's non-zero exit code to parent.

******************************************************************************/

static void goodexit(struct mimestruct *m, int exitcode)
{
	if (m->outputfp && (fflush(m->outputfp) || ferror(m->outputfp)))
	{
		perror("makemime");
		exit(1);
	}

	/*
	** Drain any leftover input, so that the child doesn't get
	** a SIGPIPE.
	*/

	while (m->inputfp1 && !feof(m->inputfp1) && !ferror(m->inputfp1))
		getc(m->inputfp1);

	while (m->inputfp2 && !feof(m->inputfp2) && !ferror(m->inputfp2))
		getc(m->inputfp2);

	if (m->inputfp1)
	{
		if (ferror(m->inputfp1))
		{
			perror("makemime");
			exitcode=1;
		}

		fclose(m->inputfp1);
	}
	if (m->inputfp2)
	{
		if (ferror(m->inputfp2))
		{
			perror("makemime");
			exitcode=1;
		}

		fclose(m->inputfp2);
	}

	while (m->child1 > 0 && m->child2 > 0)
	{
	int	waitstat;
	pid_t	p=wait(&waitstat);

		if (p <= 0 && errno == ECHILD)	break;

		if (p == m->child1)
			m->child1=0;
		else if (p == m->child2)
			m->child2=0;
		else	continue;
		if (waitstat)	exitcode=1;
	}
	exit(exitcode);
}

int main(int argc, char **argv)
{
struct	mimestruct *m;

	signal(SIGCHLD, SIG_DFL);
	if (argc > 1 && argv[1][0] == '@')
		read_args(&argc, &argv, argv[1]+1);
	else if (argc > 1)
	{
		--argc;
		++argv;
	}

	m=parseargs(&argc, &argv);
	if (argc)	usage();	/* Some arguments left */

	(*m->open_func)(m);
	(*m->handler_func)(m);
	goodexit(m, 0);
	return (0);
}

static struct mimestruct *base64m;

static void putbase64(const char *p, size_t l)
{
	fwrite(p, 1, l, base64m->outputfp);
}

static void createsimplemime(struct mimestruct *m)
{
int	c;
struct	arg_list *a;

	/* Determine encoding by reading the file, as follows:
	**
	** Default to 7bit.  Use 8bit if high-ascii bytes found.  Use
	** quoted printable if lines more than 200 characters found.
	** Use base64 if a null byte is found.
	*/

	if (m->mimeencoding == 0)
	{
	int	l=0;
	int	longline=0;
	long	orig_pos=ftell(m->inputfp1);

		if (orig_pos == -1)
		{
			perror("ftell");
			goodexit(m, 1);
		}

		m->mimeencoding="7bit";

		while ((c=getc(m->inputfp1)) != EOF)
		{
		unsigned char ch= (unsigned char)c;

			if (ch >= 0x80)
				m->mimeencoding="8bit";
			if (ch == 0)
			{
				m->mimeencoding="base64";
				longline=0;
				break;
			}
			if (ch == '\n')	l=0;
			else if (++l > 200)
				longline=1;

		}
		if (ferror(m->inputfp1)
			|| fseek(m->inputfp1, orig_pos, SEEK_SET)<0)
		{
			perror("fseek");
			goodexit(m, 1);
		}
		if (longline)
			m->mimeencoding="quoted-printable";
	}


	for (a=m->a_first; a; a=a->next)
		fprintf(m->outputfp, "%s\n", a->arg);

	fprintf(m->outputfp, "Content-Type: %s\n"
			"Content-Transfer-Encoding: %s\n\n",
			m->mimetype, m->mimeencoding);

	if (strcmp(m->mimeencoding, "quoted-printable") == 0)
	{
	int	l=0;
	int	c;

		while ((c=getc(m->inputfp1)) != EOF)
		{
			if (l > 72)
			{
				fprintf(m->outputfp, "=\n");
				l=0;
			}
			if (c == '\n')
				l=0;
			else if (c < ' ' || c == '=' || c > 0x7F)
			{
				fprintf(m->outputfp, "=%02X",
					(int)(unsigned char)c);
				l += 3;
				continue;
			}
			else ++l;
			putc(c, m->outputfp);
		}
		return;
	}
	if (strcmp(m->mimeencoding, "base64") == 0)
	{
	char	buf[BUFSIZ];
	int	l;

		base64m=m;
		rfc2045_base64encode_start( &putbase64 );
		while ((l=fread(buf, 1, sizeof(buf), m->inputfp1)) > 0)
			rfc2045_base64encode(buf, l);
		rfc2045_base64encode_end();
		return;
	}

	/* 7bit or 8bit */

	while ((c=getc(m->inputfp1)) != EOF)
		putc(c, m->outputfp);
}

/******************************************************************************

Satisfy paranoia by making sure that the MIME boundary we picked does not
appear in the contents of the bounded section.

******************************************************************************/

static int tryboundary(struct mimestruct *m, FILE *f, const char *bbuf)
{
char	buf[BUFSIZ];
char	*p;
int	l=strlen(bbuf);
int	c;
long	orig_pos=ftell(f);

	if (orig_pos == -1)
	{
		perror("ftell");
		goodexit(m, 1);
	}

	while ((p=fgets(buf, sizeof(buf), f)) != 0)
	{
		if (p[0] == '-' && p[1] == '-' &&
			strncmp(p+2, bbuf, l) == 0)
			break;

		if ((p=strchr(buf, '\n')) != 0)
			*p=0;
		else while ((c=getc(f)) != EOF && c != '\n')
			;
	}

	if (ferror(f) || fseek(f, orig_pos, SEEK_SET)<0)
	{
		perror("fseek");
		goodexit(m, 1);
	}

	return (p ? 1:0);
}

/******************************************************************************

Create a MIME boundary for some content.

******************************************************************************/

static const char *mkboundary(struct mimestruct *m, FILE *f)
{
pid_t	pid=getpid();
time_t	t;
static unsigned n=0;
static char bbuf[NUMBUFSIZE*4];
char	buf[NUMBUFSIZE];

	time(&t);

	do
	{
		strcpy(bbuf, "=_");
		strcat(bbuf, str_size_t(++n, buf));
		strcat(bbuf, "_");
		strcat(bbuf, str_time_t(t, buf));
		strcat(bbuf, "_");
		strcat(bbuf, str_pid_t(pid, buf));
	} while (tryboundary(m, f, bbuf));
	return (bbuf);
}

static void createmultipartmime(struct mimestruct *m)
{
const char *b=mkboundary(m, m->inputfp1);
struct arg_list *a;
int	c;

	if (m->mimeencoding == 0)
		m->mimeencoding="8bit";

	for (a=m->a_first; a; a=a->next)
		fprintf(m->outputfp, "%s\n", a->arg);
	fprintf(m->outputfp, "Content-Type: %s; boundary=\"%s\"\n"
			"Content-Transfer-Encoding: %s\n\n"
			RFC2045MIMEMSG
			"\n--%s\n",
		m->mimetype, b,
		m->mimeencoding,
		b);
	while ((c=getc(m->inputfp1)) != EOF)
		putc(c, m->outputfp);
	fprintf(m->outputfp, "\n--%s--\n", b);
}

static void joinmultipart(struct mimestruct *m)
{
const char *new_boundary;
char	*old_boundary=0;
int	old_boundary_len=0;
char	buffer[BUFSIZ];
char	*p;
int	c;

	do
	{
		new_boundary=mkboundary(m, m->inputfp1);
	} while (tryboundary(m, m->inputfp2, new_boundary));

	/* Copy the header */

	for (;;)
	{
		if (fgets(buffer, sizeof(buffer), m->inputfp2) == 0)
		{
			buffer[0]=0;
			break;
		}

		if (buffer[0] == '\n' || strncmp(buffer, "--", 2) == 0)
			break;

		if (strncasecmp(buffer, "content-type:", 13))
		{
			fprintf(m->outputfp, "%s", buffer);
			if ((p=strchr(buffer, '\n')) != 0)	continue;
			while ((c=getc(m->inputfp2)) != EOF && c != '\n')
				putc(c, m->outputfp);
			continue;
		}

		if ((p=strchr(buffer, '\n')) == 0)
			while ((c=getc(m->inputfp2)) != EOF && c != '\n')
				;

		p=strchr(buffer+13, ';');
		if (p)	*p=0;
		fprintf(m->outputfp, "Content-Type:%s; boundary=\"%s\"\n",
			buffer+13, new_boundary);

		for (;;)
		{
			c=getc(m->inputfp2);
			if (c != EOF)	ungetc(c, m->inputfp2);
			if (c == '\n' || !isspace((int)(unsigned char)c))
				break;
			while ((c=getc(m->inputfp2)) != EOF && c != '\n')
				;
		}
	}

	do
	{
		if (strncmp(buffer, "--", 2) == 0)
		{
			if (old_boundary == 0)
			{
				old_boundary=malloc(strlen(buffer)+1);
				if (!old_boundary)
				{
					perror("malloc");
					exit(1);
				}
				strcpy(old_boundary, buffer);
				if ((p=strchr(old_boundary, '\n')) != 0)
					*p=0;
				p=old_boundary+strlen(old_boundary);
				if (p >= old_boundary+4 &&
					strcmp(p-2, "--") == 0)
				p[-2]=0;
				old_boundary_len=strlen(old_boundary);
			}


			if (strncasecmp(buffer, old_boundary,
				old_boundary_len) == 0)
			{
				if ((p=strchr(buffer, '\n')) != 0)
					*p=0;
				else while ((c=getc(m->inputfp2)) != '\n'
					&& c != EOF)
					;

				c=strlen(buffer);
				if (c >= 4 && strcmp(buffer+(c-2), "--") == 0)
					break;
				fprintf(m->outputfp, "--%s\n",
					new_boundary);
				continue;
			}
		}
		fprintf(m->outputfp, "%s", buffer);
		if ((p=strchr(buffer, '\n')) == 0)
			while ((c=getc(m->inputfp2)) != '\n' && c != EOF)
				;
	} while (fgets(buffer, sizeof(buffer), m->inputfp2) != 0);

	fprintf(m->outputfp, "--%s\n", new_boundary);

	while ((c=getc(m->inputfp1)) != EOF)
		putc(c, m->outputfp);

	fprintf(m->outputfp, "\n--%s--\n", new_boundary);
	goodexit(m, 0);
}

/******************************************************************************

Open input from a child process

******************************************************************************/

static FILE *openchild(struct mimestruct *parent, struct mimestruct *child,
	pid_t	*pidptr,
	int usescratch)
{
int	pipefd[2];
char	buf[NUMBUFSIZE];
char	buf2[NUMBUFSIZE+1];
FILE	*fp;

	if (pipe(pipefd) < 0)
	{
		perror("pipe");
		exit(1);
	}

	*pidptr=fork();

	if (*pidptr < 0)
	{
		perror("fork");
		exit(1);
	}

	if (*pidptr == 0)
	{
		/* Duplicate pipe on stdout */

		close(pipefd[0]);
		close(1);
		dup(pipefd[1]);
		close(pipefd[1]);

		/* Close any input files opened by parent */

		if (parent->inputfp1)	fclose(parent->inputfp1);
		if (parent->inputfp2)	fclose(parent->inputfp2);

		/* Open, then execute the child process */

		(*child->open_func)(child);
		(*child->handler_func)(child);
		goodexit(child, 0);
	}
	close(pipefd[1]);

	/*
	** Open the pipe by calling openfile(), automatically creating
	** the scratch file, if necessary.
	*/

	buf[0]='&';
	strcpy(buf+1, str_size_t(pipefd[0], buf2));

	fp= usescratch ? openfile(buf):openfile_or_pipe(buf, "r");
	close(pipefd[0]);	/* fd was duped by openfile */
	return (fp);
}

static void openoutput(struct mimestruct *m)
{
	if (!m->outputfile)
		m->outputfile="-";

	m->outputfp= openfile_or_pipe(m->outputfile, "w");
}

static void openjoinmultipart(struct mimestruct *m)
{
	/* number two is the multipart section */
	if (m->inputchild2)
		m->inputfp2=openchild(m, m->inputchild2, &m->child2, 1);
	else
		m->inputfp2=openfile(m->inputfile2);


	if (m->inputchild1)
		m->inputfp1=openchild(m, m->inputchild1, &m->child1, 1);
	else
		m->inputfp1=openfile(m->inputfile1);
	openoutput(m);
}

static void opencreatesimplemime(struct mimestruct *m)
{
	if (m->inputchild1)
		m->inputfp1=openchild(m, m->inputchild1, &m->child1,
			m->mimeencoding ? 0:1);
	else
		m->inputfp1= m->mimeencoding
			? openfile_or_pipe(m->inputfile1, "r")
			: openfile(m->inputfile1);
	openoutput(m);
}

static void opencreatemultipartmime(struct mimestruct *m)
{
	if (m->inputchild1)
		m->inputfp1=openchild(m, m->inputchild1, &m->child1, 1);
	else
		m->inputfp1=openfile_or_pipe(m->inputfile1, "r");
	openoutput(m);
}

