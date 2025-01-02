\section{Foreign access to Prolog IO streams}
\label{sec:foreign-streams}

The SWI-Prolog foreign language interface provides access to Prolog IO
streams. This interface may be used to get hold of Prolog streams for
reading and writing. In addition, this interface allows to define new
stream types. For example, the Windows \program{swipl-win.exe}
executable that runs Prolog in a Windows GUI redefines the Prolog
standard IO streams (\const{user_input}, \const{user_output} and
\const{user_error} to read from and write to the GUI window.

The interface is built around the \ctype{IOSTREAM} type which plays a
role similar to the POSIX \ctype{FILE} type. Most of the functions are
modeled after their \ctype{FILE} counterpart, prefixed by \textbf{S},
e.g. Sfwrite(). The \ctype{IOSTREAM} type has considerably more features
though. The \ctype{IOSTREAM} type is practically disconnected from the
rest of the Prolog system. Prolog refers to streams either by
\jargon{alias} (\const{user_input}, etc. or created using the
\term{alias}{Name} option of open/4) or using a \jargon{stream handle}
which is represented as a \jargon{blob} (see \secref{blob}). Foreign
extensions that wish to access or define streams should include
\file{SWI-Stream.h} in addition to \file{SWI-Prolog.h} as below. Both
headers may be used with C as well as C++.

The interface also defines \const{Sinput}, \const{Suser}, \const{Serror}
for direct access to the operating system's input and output streams,
bypassing Prolog's control - for example, these will not be affected
by with_output_to/3. There is also a convenience function for debugging,
which goes directly to \const{stderr}: Sdprintf().\footnote{On Windows
the output is also emitted using \cfuncref{OutputDebugString}{}.}

\begin{code}
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
\end{code}

\subsection{Get IO stream handles}
\label{sec:foreign-get-iostream}

There are several ways to get access to an IO Stream handle, basically
get them from Prolog, get access to the standard streams and create a
new stream. The \jargon{standard streams} are available as
\const{Sinput}, \const{Soutput} and \const{Serror}. Note that these are
thread specific.  Creating a new stream is discussed with Snew().  Below
are the functions to obtain a stream handle from a Prolog term, obtain
and release ownership.

\begin{description}
    \cfunction{int}{PL_get_stream}{term_t t, IOSTREAM **s, int flags}
Get a stream handle from the Prolog term \arg{t}.  Returns \const{TRUE}
on success and \const{FALSE} on failure, by default generating an
exception.   The \arg{flags} argument is a bitwise disjunction of these
flags:

    \begin{description}
    \definition{\const{SIO_INPUT}}
	Get an \jargon{input stream}.  If \arg{t} is a stream pair
	(see stream_pair/3), return the input channel.  If \arg{t}
	is an output stream the function fails.
    \definition{\const{SIO_OUTPUT}}
	Get an \jargon{output stream}.  See \const{SIO_INPUT} for
        details.  If neither \const{SIO_OUTPUT} nor \const{SIO_INPUT}
	is given \arg{t} may not be a \jargon{pair}.
    \definition{\const{SIO_TRYLOCK}}
	Return \const{FALSE} if the stream cannot be locked
        immediately.  No error is generated.
    \definition{\const{SIO_NOERROR}}
	If the function fails no exception is produced.
    \end{description}

The returned stream is owned by the calling thread using
PL_acquire_stream().

    \cfunction{int}{PL_get_stream_from_blob}{atom_t b, IOSTREAM **s, int flags}
Same as PL_get_stream(), but operates directly on the blob \arg{b}. This
allows for foreign code that wishes long term access to a stream to
maintain a handle to the stream as a (registered) \ctype{atom_t} object
rather than a \ctype{IOSTREAM*}.

    \cfunction{IOSTREAM *}{PL_acquire_stream}{IOSTREAM *s}
Obtain ownership of \arg{s} and return \arg{s}. The application must
call PL_release_stream() when done. Only one thread can own a stream
and this call blocks if some other thread owns the stream. This
function may be called multiple times by the same thread
(\jargon{recursive lock}).
Note that PL_get_stream() also acquires ownership.

    \cfunction{int}{PL_release_stream}{IOSTREAM *s}
Give up ownership acquired using PL_acquire_stream() or PL_get_stream().
If the stream is an an error state, return \const{FALSE} with an
exception. Otherwise return \const{TRUE}.
\end{description}

In general, stream functions do not set any Prolog error state; that is
done by PL_release_stream(). Once a stream is in an error state, all
subsequent functions act as no-ops (returning -1) unless Sclearerr() is
called. Sferror() may be used to check whether a stream is in an error
condition. This error may be turned into a Prolog exception by calling
PL_acquire_stream() followed by PL_release_stream(). In this case,
PL_release_stream() will set the Prolog exception and return
\const{FALSE}.

Below is an example that writes ``Hello World'' to a stream provided by
Prolog. Note that PL_release_stream() raises an exception if the
Sfprintf() failed and (thus) left the stream in an error state.

\begin{code}
static foreign_t
hello_world(term_t to)
{ IOSTREAM *s;

  if ( PL_get_stream(to, &s, SIO_OUTPUT) )
  { Sfprintf(s, "Hello World!\n");
    return PL_release_stream(s);
  }

  return FALSE;
}

  ... // fragment from install function
  PL_register_foreign("hello world", 1, hello_world, 0);
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Creating an IO stream}
\label{sec:foreign-create-iostream}

A new stream is created using Snew(). Before we can create a stream we
must create a function block of type \ctype{IOFUNCTIONS} that provide
function pointers for the basic operations on the stream.  This type
is defined as follows:

\begin{code}
typedef struct io_functions
{ Sread_function	read;		/* fill the buffer */
  Swrite_function	write;		/* empty the buffer */
  Sseek_function	seek;		/* seek to position */
  Sclose_function	close;		/* close stream */
  Scontrol_function	control;	/* Info/control */
  Sseek64_function	seek64;		/* seek to position (large files) */
} IOFUNCTIONS;
\end{code}

\begin{description}
    \cfunction{ssize_t}{(*Sread_function)}{void *handle, char *buf, size_t bufsize}
Read new data into \arg{buf} that has size \arg{bufsize}, return the
number of bytes read or -1. Note that this is the same interface as the
POSIX read() API.  See \secref{iostream-errors} for raising errors.

    \cfunction{ssize_t}{(*Swrite_function)}{void *handle, char *buf, size_t bufsize}
Write the bytes from \arg{buf} with contains \arg{bufsize} bytes and
return the number of bytes written or -1. The number of bytes written
may be less than \arg{bufsize}. Bytes that were not written remain in
the stream's output buffer.  Note that this is the same interface as the
POSIX write() API.  See \secref{iostream-errors} for raising errors.

    \cfunction{long}{(*Sseek_function)}{void *handle, long pos, int whence}
\nodescription
    \cfunction{int64_t}{(*Sseek64_function)}{void *handle, int64_t pos, int whence}
Reposition the file pointer. These functions may be \const{NULL} if
repositioning is not possible on this type or they may return -1 and set
\const{errno} to \const{EPIPE} if the pointer cannot be repositioned on
this instance. The function returns the new file position. See Sseek()
for details on how repositioning is implemented. See
\secref{iostream-errors} for raising errors.

    \cfunction{int}{(*Sclose_function)}{void *handle}
Close the stream. This is used by Sclose(). Note that buffered output is
first written using the Swrite_function(). See \secref{iostream-errors}
for raising errors.

    \cfunction{int}{(*Scontrol_function)}{void *handle, int action, void *arg}
Obtain information about the stream or modify the stream. The function
should return 0 on success and -1 on failure. If some action is not
implemented the function should return -1;

    \begin{description}
    \definition{\const{SIO_GETPENDING}, \ctype{size_t*}}
    Return the number of bytes that may be written without blocking.
    Used by Spending().

    \definition{\const{SIO_LASTERROR}, \ctype{char*}}
    Called after an error is raised on a stream.  May return a C string
    that sets error details using Sseterr().

    \definition{\const{SIO_SETENCODING}, \ctype{IOENC*}}
    Called by Ssetenc() to change the encoding of the stream.  If the
    call does not return 0 the encoding is not changed.

    \definition{\const{SIO_FLUSHOUTPUT}, \const{NULL}}
    Called by Sflush() after flushing the stream's output buffer.  Note
    that this is only called on an \emph{explicit} flush using Sflush()
    or flush_output/1.  An implicit flush because the output buffer is
    full does \emph{not} call this hook.

    \definition{\const{SIO_GETSIZE}, \ctype{int64_t*}}
    Get the size of the underlying object in bytes.  Used by Ssize().

    \definition{\const{SIO_GETFILENO}, \ctype{int*}}
    If the stream is associated with an OS file handle, return this
    handle.  Used by Sfileno().

    \definition{\const{SIO_GETWINSOCK}, \ctype{SOCKET*}}
    Windows only.  If the stream is associated to a Windows socket
    return this handle.  Used by Swinsock().

    \end{description}
\end{description}

Given an \ctype{IOFUNCTIONS} block we can create a new stream from a
\arg{handle} using Snew():

\begin{description}
    \cfunction{IOSTREAM*}{Snew}{void *handle, int flags, IOFUNCTIONS *functions}
Create an \ctype{IOSTREAM*} from a handle, flags and a block of callback
functions. The \arg{flags} argument is a bitwise or of SIO_* flags.
Flags that control the creation are:

    \begin{description}
    \definition{\const{SIO_INPUT}}
    \nodescription
    \definition{\const{SIO_OUTPUT}}
    One of these flags mut be present to indicate whether this is an
    input or output stream.
    \definition{\const{SIO_NBUF}}
    \nodescription
    \definition{\const{SIO_LBUF}}
    \nodescription
    \definition{\const{SIO_FBUF}}
    One of these flags must be present to select the buffering as one of
    unbuffered (\const{SIO_NBUF}), line buffered (\const{SIO_LBUF}) or
    fully buffered (\const{SIO_FBUF})
    \definition{\const{SIO_TEXT}}
    If given, this is a text stream and the encoding is set to the
    default encoding (see the Prolog flag \prologflag{encoding}).
    Otherwise this is a binary stream and the encoding is set to
    \const{ENC_OCTET}.
    \definition{\const{SIO_RECORDPOS}}
    If given, enable position maintenance on the stream.  This is used
    by Stell(), Sseek(), stream_property/2 using the
    \const{position} property and related predicates.
    \definition{\const{SIO_NOMUTEX}}
    Used internally to create a stream that cannot be owned or locked.
    \end{description}

If the stream is associated with an OS file handle the system
initializes the \const{SIO_ISATTY} flag (on POSIX systems) and if
possible tells the OS not to inherit this stream to child processes.

The symbol \const{Sfilefunctions} is a \ctype{IOFUNCTIONS} struct that
contains the callbacks for accessing a regular file. After opening an
file using the POSIX open() API we can create a stream to this file
using Snew():

\begin{code}
  int fno = open(path, O_RDONLY);
  IOSTREAM *s;

  if ( fno >= 0 )
    s = Snew((void*)fno,
	     SIO_INPUT|SIO_FBUF|SIO_RECORDPOS|SIO_TEXT,
	     &Sfilefunctions);
  ...
\end{code}

Snew() can only fail if there is not enough memory.  In that case the
return value is \const{NULL} and \arg{errno} is set to \const{ENOMEM}.

    \cfunction{IOSTREAM*}{Sopen_pipe}{const char *command, const char *type}
Start a process from \arg{command} and connect the input or output to
the returned stream.  This wraps the POSIX popen() API.  The \arg{type}
string starts with \chr{r} or \chr{w} and may be followed by \chr{b} to
create a \jargon{binary stream}.  The default is to create a text stream
using the platform conventions and locale.

    \cfunction{IOSTREAM*}{Sopenmem}{char **buffer, size_t *sizep, const char *mode}
Open a memory area as a stream. Output streams are automatically resized
using realloc() if *\arg{size} = 0 or the stream is opened with mode
\const{"wa"}. If the buffer is allocated or enlarged, this is achieved
using malloc() or realloc(). In this case the returned buffer should be
freed by the caller when done. Example:

\begin{code}
    { char buf[1024];		  // don't allocate for small stuff
      char *s = buf;
      IOSTREAM *fd;
      size_t size = sizeof(buf);

      fd = Sopenmem(&s, &size, "w");
      ...
      Sclose(fd);
      ...
      if ( s != buf )		  // apparently moved
	Sfree(s);
    }
\end{code}

The \arg{mode} is \const{"r"} or \const{"w"}. The mode \arg{"rF"} calls
\exam{PL_free(buffer)} when closed.

\textbf{Note:} Its is \emph{not} allowed to access streams created with
this call from multiple threads. This is ok for all usage inside Prolog
itself. This call is intended to use Sfprintf() and other output functions
to create strings.

    \cfunction{void}{Sfree}{void *ptr}
This function must be used to free objects that are allocated by the
stream interface.  Currently this only applies to strings allocated
by Sopenmem().
\end{description}

A stream can be made accessible from Prolog using PL_unify_stream():

\begin{description}
    \cfunction{int}{PL_unify_stream}{term_t t, IOSTREAM *s}
Unify \arg{t} with a \jargon{blob} that points at \arg{s}.  Note that
a blob provides a unique and reliable reference to a stream.  Blobs
are subject to \jargon{atom garbage collection}.  If an open stream
is garbage collected the behaviour depends on the Prolog flag
\prologflag{agc_close_streams}.  See also Sgcclose().
\end{description}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Interacting with foreign streams}
\label{sec:iostream-functions}

\begin{description}
    \cfunction{int}{Sset_timeout}{IOSTREAM *s, int milliseconds}
Set the timeout on an input stream to \arg{milliseconds}.  If this value
is non-negative the the poll() or select() API is used to wait until
input is available.  If no input is available within the specified
time an error is raised on the stream.

    \cfunction{int}{Sunit_size}{}
Returns the size of a code unit in bytes depending on the stream's
encoding.  This returns 2 for the encodings \const{ENC_UNICODE_BE}
and \const{ENC_UNICODE_LE}, \exam{sizeof(wchar_t)} for \const{ENC_WCHAR}
and 1 for all other encodings (including multibyte encodings such as
\const{ENC_UTF8}.

    \cfunction{int}{Sputc}{int c, IOSTREAM *s}
Emit a byte to \arg{s}.  Flushes the buffer on \verb$\n$ when in
\const{SIO_LBUF} buffering mode and updates the stream position
information if enabled (\const{SIO_RECORDPOS}).  Returns 0 on
success, -1 on error.

    \cfunction{int}{Sgetc}{IOSTREAM *s}
Read a byte from \arg{s}.  Fills the input buffer if buffering
is enabled and the buffer is empty. Updates the stream position
information if enabled (\const{SIO_RECORDPOS}).  Returns -1 on
end of file or error.  Use Sferror() or Sfeof() to distinguish
end of file from an error.  This is a C macro.

    \cfunction{int}{Sfgetc}{IOSTREAM *s}
Function equivalent to Sgetc().

    \cfunction{int}{Sungetc}{int c, IOSTREAM *s}
Put a byte back into the input buffer.  Returns -1 if this
is not possible.  Deprecated.  New code should use Speekcode()
because that reliably maintains the position information on
the stream.

    \cfunction{int}{Sputcode}{int c, IOSTREAM *s}
Emit a Unicode code point to \arg{s}.  This function also
performs newline encoding (see \secref{iostream-newline}).
If the encoding of \arg{s} cannot represent \arg{c}, the
behaviour depends on the the following flags.  Only one
of these flags may be enabled.  If none of these flags
is enabled an error is raised and the function returns
-1.

    \begin{description}
    \definition{\const{SIO_REPXML}}
    Emit as XML character entity, e.g. \verb$&#4242;$
    \definition{\const{SIO_REPPL}}
    Emit as ISO escape, e.g., \verb$\x4242\$
    \definition{\const{SIO_REPPLU}}
    Emit as Unicode escape, e.g., \verb$\u4242$ or
    \verb$\U42424242$
    \end{description}

Updates the stream position information if enabled
(\const{SIO_RECORDPOS})

    \cfunction{int}{Sgetcode}{IOSTREAM *s}
Read a Unicode code point from \arg{s}. If it detects an invalid
multibyte character a warning is emitted and the code point
\const{0xfffd} is returned. Other errors and end-of-file return -1; Use
Sferror() or Sfeof() to distinguish end of file from an error.

    \cfunction{int}{Speekcode}{IOSTREAM *s}
As Sgetcode(), but leaves the character in the input buffer and
does not update the stream position.   Returns -1 if the stream
is not buffered (\const{SIO_NBUF}).

    \cfunction{int}{Sputw}{int w, IOSTREAM *s}
    \nodescription
    \cfunction{int}{Sgetw}{IOSTREAM *s}
Reads/writes an integer in native byte order.  Deprecated.

    \cfunction{size_t}{Sfread}{void *data, size_t size, size_t elems, IOSTREAM *s}
    \nodescription
    \cfunction{size_t}{Sfwrite}{const void *data, size_t size, size_t elems, IOSTREAM *s}
Emulations of the POSIX fread() and fwrite() calls for Prolog streams.
These functions read or write \arg{elems} objects of size \arg{size} and
return the number of objects successfully read or written. Data exchange
is binary (even if the stream is in text mode) and unlike read() and
write(), these functions keep reading or writing until end-of-file (for
Sfread()) or an error.

    \cfunction{int}{Sfeof}{IOSTREAM *s}
Returns non-zero if the stream is at the end. It performs the following
checks: (1) test the \const{SIO_FEOF} flag, (2) test whether the buffer
is non-empty, (3) fill the buffer and return non-zero if the
Sread_function() returned 0 (zero).

    \cfunction{int}{Sfpasteof}{IOSTREAM *s}
Returns non-zero when a read operation was performed after signalling
end-of-file.  On other words, reaching end-of-file first triggers
Sfeof() and after another read triggers Sfpasteof().

    \cfunction{int}{Ssetlocale}{IOSTREAM *s,
				struct PL_locale *new_loc,
				struct PL_locale **old_loc}
Change the locale associated with a stream.  The current system does
not provide a public C API for dealing with Prolog locale objects.
See \secref{locale}.

    \cfunction{int}{Sflush}{IOSTREAM *s}
Flush buffered output, returning 0 on success and -1 after a (write)
error occurred. Calls Scontrol_function() using the action
\const{SIO_FLUSHOUTPUT} after the buffer was successfully written.

    \cfunction{int64_t}{Ssize}{IOSTREAM *s}
Returns the size in bytes of the object associated to the stream or
-1 if this is not known.

    \cfunction{int}{Sseek}{IOSTREAM *s, long pos, int whence}
    Deprecated - use Sseek64() instead because some platforms define
    \ctype{long} as 32-bits.
    \cfunction{int}{Sseek64}{IOSTREAM *s, int64_t pos, int whence}
Reposition the file pointer in the object associated to \arg{s},
returning 0 on success and -1 otherwise.  If the stream is buffered
and position information is maintained these functions readjust the
buffer information if possible.  Otherwise they call Sseek64_function()
or Sseek_function() as a fallback iff \arg{pos} can be represented as
a C \ctype{long}.  \arg{Whence} is one of \const{SIO_SEEK_SET},
\const{SIO_SEEK_CUR} or \const{SIO_SEEK_END}, seeking relative to
the start, current position or end.

    \cfunction{long}{Stell}{IOSTREAM *s}
    Deprecated - use Stell64() instead because some platforms define
    \ctype{long} as 32-bits.
    \cfunction{int64_t}{Stell64}{IOSTREAM *s}
Return the current position in the stream.  This is obtained from
the recorded position or based on information from the seek handlers,
adjusted with the buffer information.

    \cfunction{int}{Sclose}{IOSTREAM *s}
Close the stream. This first locks the stream (see PL_acquire_stream()).
When successful it flushes pending output and calls the
Sclose_function() hook.  Finally, the stream is unlocked and all memory
associated to the stream is released.  On success, the function returns
0.  On failure a Prolog exception is raised and the return value is -1.
Regardless of the return value, \arg{s} becomes invalid after completion
of Sclose().  See also Sgcclose().

    \cfunction{int}{Sgcclose}{IOSTREAM *s, int flags}
As Sclose(), but intended to be used from the atom garbage collector if
a stream is closed because it is garbage. The SWI-Prolog atom garbage
collector normally runs in a separate thread and thus may be unable to
obtain a lock on \arg{s} if some thread lost access to the stream while
it is locked. For this situation \arg{flags} may be
\const{SIO_CLOSE_TRYLOCK} which causes Sgcclose() to return -1 with
\arg{errno} set to \const{EDEADLK} if the stream is locked.
Alternatively, using \const{SIO_CLOSE_FORCE} the stream is closed and
released without gaining a lock. This should be safe because the stream
is garbage and thus no thread can use the lock.

In addition, Sgcclose() never raises a Prolog exception because Prolog
interaction is not allowed from the blob release hook and there is no
meaningful way to raise a Prolog exception from this context.

    \cfunction{char*}{Sfgets}{char *buf, int n, IOSTREAM *s}
Read a line of input as a sequence of \emph{bytes}.  The \arg{buf}
is \arg{n} bytes long. On success, \arg{buf} is returned and contains
a 0-terminated C string that ends with a \verb$\n$ character. On
end-of-file or an error, \const{NULL} is returned. If the input line is
longer that \arg{n} bytes \arg{buf} is \textbf{not} 0-terminated.

    \cfunction{int}{Sgets}{char *buf}
Shorthand for \exam{Sfgets(buf, Slinesize, Sinput)}. Deletes the
terminating \verb$\n$ character. \arg{Slinesize} is a global variable
that defines the length of the input buffer. Deprecated.

    \cfunction{int}{Sread_pending}{IOSTREAM *s, char *buf, size_t limit, int flags}
Return the data buffered on an input stream. If \arg{flags} includes
\const{SIO_RP_BLOCK}, fill the buffer (possibly blocking) if the buffer
is empty.  Update the stream position information unless \arg{flags}
include \const{SIO_RP_NOPOS}.  This function effectively provides
functionality similar to POSIX read() on a stream.   This function
is used by read_pending_codes/3.

    \cfunction{size_t}{Spending}{IOSTREAM *s}
Return the number of bytes that can be read from \arg{s} without
blocking.  If there is buffered input, this is the number of bytes
buffered.  Otherwise it is the result of the Scontrol_function()
using the action \const{SIO_GETPENDING}.

    \cfunction{int}{Sfputs}{const char *q, IOSTREAM *s}
Emit a 0-terminated C string.  The input string \arg{q}
is handled as a sequence of unsigned characters (code
points $1\ldots255$.

    \cfunction{int}{Sputs}{const char *q}
Equivalent to \exam{Sfputs(q, Soutput)}.

    \cfunction{int}{Sfprintf}{IOSTREAM *s, const char *fm, ...}
Similar to POSIX fprintf(). This function largely accepts the same
\verb$%$ escape sequences. The \verb$%$ character is followed by numeric
arguments and modifier characters. The generic format of this is
described by the regular expression \verb$[+-0 #]*(\d*|\*)(.(\d*|\*))?$.
Here, \chr{+} implies right alignment, \chr{-} left alignment, \chr{0}
0-padding and, a space white-space padding and \chr{#} \jargon{modified}
output. The two optional numerical arguments are separated by a full
stop and may be \chr{*} to get them from the argument list. The first
numerical argument specifies the field width and the second the
precision for floating point numbers.

This sequence is followed by optional type information. For integers
this is one of \chr{l} (\ctype{long}), \chr{ll} (\ctype{long long}) or
\chr{z} (\ctype{size_t}).  For strings this is one of \chr{L} (ISO
Latin 1), \chr{U} (UTF-8) or \chr{W} (\ctype{wchar_t*}).

Finally we come to the format specifier.  This is one of

\begin{description}
    \fmtchar{%}
Emit the \chr{%} character itself.
    \fmtchar{c}
Emit a Unicode code point.
    \fmtchar{p}
Emit a pointer.
    \fmtchar{d}
    \nodescription
    \fmtchar{i}
Emit a a signed integer as decimal. The \chr{l} (\ctype{long}), \chr{ll}
(\ctype{long long}) or \chr{z} (\ctype{size_t}) denote the size.
    \fmtchar{o}
    \nodescription
    \fmtchar{u}
    \nodescription
    \fmtchar{x}
    \nodescription
    \fmtchar{X}
Emit a a unsigned integer as octal, decimal or hexadecimal.
    \fmtchar{f}
    \nodescription
    \fmtchar{e}
    \nodescription
    \fmtchar{E}
    \nodescription
    \fmtchar{g}
    \nodescription
    \fmtchar{G}
Emit a \ctype{double}.
    \fmtchar{s}
Emit a 0-terminated string.
\end{description}

Unlike the POSIX fprintf(), this function, and the related
functions (Svprintf(), etc.) returns the number of
characters written. Due to multibyte encodings the number of bytes
written can be more. On error, it returns a negative value; in some
cases there is extra information (e.g., in \const{errno}) but it
cannot be relied on.

Each call to Sfprintf() is atomic in the sense that another
thread that calls Sfprintf() on the same stream will block.
If you wish to do a series of print statements without any
other thread interleaving, you should call PL_acquire_stream()
and use its returned \ctype{IOSTREAM*} value, then call
PL_release_stream() at the end of the print statements.

    \cfunction{int}{SfprintfX}{IOSTREAM *s, const char *fm, ...}
Same as Sfprintf() but doesn't have the format-checking attribute,
which can trigger compiler warnings if the format does not match
the arguments.
This is intended for formats that include extended format specifiers
such as \exam{"\%Ws"} or \exam{"\%Us"}.

    \cfunction{int}{Sprintf}{const char *fm, ...}
Similar to Sfprintf(), printing to \arg{Soutput}

    \cfunction{int}{Svprintf}{IOSTREAM *s, const char *fm, va_list args}
Variadic argument list version of Sfprintf().

    \cfunction{int}{Ssprintf}{char *buf, const char *fm, ...}
Print to a C string.  Deprecated.  Use Ssnprintf() instead.

    \cfunction{int}{Ssnprintf}{char *buf, size_t size, const char *fm, ...}
Print to a C string, emitting a maximum of \arg{size} bytes while
ensuring \arg{buf} is 0-terminated.  The \arg{buf} is written using
UTF-8 encoding.  Unlike snprintf(), the return value is the number
of logical code points written rather than the number of bytes and if
the buffer is too small, \const{-1} is returned rather than the number
of bytes that would be written. Future versions may improve
compatibility with the POSIX functions.

    \cfunction{int}{SsnprintfX}{char *buf, size_t size, const char *fm, ...}
Same as Ssnprintf() but doesn't have the format-checking attribute.
This is intended for formats that include extended format specifiers
such as \exam{"\%Ws"} or \exam{"\%Us"}.

    \cfunction{int}{Svsprintf}{char *buf, const char *fm, va_list args}
Variadic argument list version of Ssprintf(). Deprecated. Use
Svsnprintf() instead.

    \cfunction{int}{Svsnprintf}{char *buf, size_t size, const char *fm, va_list args}
Variadic argument list version of Ssnprintf().

    \cfunction{int}{Sdprintf}{const char *fm, ...}
Print to \arg{Serror}.  This function should be used for printing debug
output from foreign code.

    \cfunction{int}{SdprintfX}{const char *fm, ...}
Same as Sdprintf() but doesn't have the format-checking attribute.
This is intended for formats that include extended format specifiers
such as \exam{"\%Ws"} and \exam{"\%Us"}.

    \cfunction{int}{Svdprintf}{const char *fm, va_list args}
Variadic argument list version of Sdprintf().

    \cfunction{int}{Slock}{IOSTREAM *s}
    \nodescription
    \cfunction{int}{StryLock}{IOSTREAM *s}
    \nodescription
    \cfunction{int}{Sunlock}{IOSTREAM *s}
Low level versions that perform only the (un)locking part of
PL_acquire_stream() and PL_release_stream().

    \cfunction{int}{Sfileno}{IOSTREAM *s}
If the stream is associated to a POSIX file handle, return this
handle.  Returns -1 otherwise.

    \cfunction{SOCKET}{Swinsock}{IOSTREAM *s}
Windows only.  If the stream is associated to a Windows socket
handle, returns this handle.  Otherwise return \const{INVALID_SOCKET}

    \cfunction{int}{Sclosehook}{void (*hook)(IOSTREAM *s)}
Register a hook function to be called by Sclose() just before the stream
is deallocated. This is used internally to update the Prolog
administration of open streams on Sclose().

    \cfunction{int}{Sset_filter}{IOSTREAM *parent, IOSTREAM *filter}
Register \arg{filter} as a stream that reads from or writes to the
stream \arg{parent}.

    \cfunction{void}{Ssetbuffer}{IOSTREAM *s, char *buf, size_t size}
Set the input or output buffer for \arg{s} to \arg{size}.  The \arg{buf}
argument is either \const{NULL}, asking the system to allocate a buffer
or points at a buffer of (at least) the indicated size long. The default
buffer size is defined by the C macro \const{SIO_BUFSIZE}
\end{description}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsubsection{Writing Prolog terms to foreign streams}
\label{sec:iostream-write-term}

\begin{description}
    \cfunction{int}{PL_write_term}{IOSTREAM *s, term_t term, int precedence,
				   int flags}
Write \arg{term} to \arg{s}.  \arg{precedence} is the initial operator
precedence, typically 1200. \arg{flags} is a bitwise or of the
constants below.  These flags map to options for write_term/2.

    \begin{description}
	\definition{\const{PL_WRT_QUOTED}}
	\definition{\const{PL_WRT_IGNOREOPS}}
	\definition{\const{PL_WRT_NUMBERVARS}}
	\definition{\const{PL_WRT_PORTRAY}}
	\definition{\const{PL_WRT_CHARESCAPES}}
        \nodescription
	\definition{\const{PL_WRT_NO_CHARESCAPES}}
The \const{PL_WRT_NO_CHARESCAPES} does not map to a write_term/2 option.
If one of \const{PL_WRT_CHARESCAPES} or \const{PL_WRT_NO_CHARESCAPES} is
specified, character escapes are (not) applied.  If neither is specified
the default depends, like for write/1, on the
\prologflag{character_escapes} flag on the module
\const{user}.\footnote{Prior to version 9.1.6 the default (no flag) was
to escape the quotes and the backslash (\chr{\}).}
	\definition{\const{PL_WRT_BACKQUOTED_STRING}}
	\definition{\const{PL_WRT_ATTVAR_IGNORE}}
	\definition{\const{PL_WRT_ATTVAR_DOTS}}
	\definition{\const{PL_WRT_ATTVAR_WRITE}}
	\definition{\const{PL_WRT_ATTVAR_PORTRAY}}
	\definition{\const{PL_WRT_BLOB_PORTRAY}}
	\definition{\const{PL_WRT_NO_CYCLES}}
	\definition{\const{PL_WRT_NEWLINE}}
	\definition{\const{PL_WRT_VARNAMES}}
	\definition{\const{PL_WRT_BACKQUOTE_IS_SYMBOL}}
	\definition{\const{PL_WRT_DOTLISTS}}
	\definition{\const{PL_WRT_BRACETERMS}}
	\definition{\const{PL_WRT_NODICT}}
	\definition{\const{PL_WRT_NODOTINATOM}}
	\definition{\const{PL_WRT_NO_LISTS}}
	\definition{\const{PL_WRT_RAT_NATURAL}}
	\definition{\const{PL_WRT_CHARESCAPES_UNICODE}}
	\definition{\const{PL_WRT_QUOTE_NON_ASCII}}
	\definition{\const{PL_WRT_PARTIAL}}
    \end{description}

For example, to print a term to \const{user_error} as the toplevel
does, use

\begin{code}
    PL_write_term(Suser_error, t, 1200,
		  PL_WRT_QUOTED|PL_WRT_PORTRAY|
		  PL_WRT_VARNAMES|PL_WRT_NEWLINE)
\end{code}
\end{description}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Foreign stream error handling}
\label{sec:iostream-errors}

\begin{description}
    \cfunction{int}{Sferror}{IOSTREAM *s}
Returns \const{TRUE} if the stream is in an error condition,
\const{FALSE} if the stream is valid and in normal condition
and -1 if the stream is invalid.

    \cfunction{void}{Sclearerr}{IOSTREAM *s}
Clear the error state of a stream.  This includes the end-of-file
state, pending warnings and errors and timeout.

    \cfunction{int}{Sseterr}{IOSTREAM *s, int which, const char *message}
Set an error or warning state on the stream. The \arg{which} argument is
one of \const{SIO_WARN} or \const{SIO_FERR}. This
causes PL_release_stream() to print a message (\const{SIO_WARN}) or
raise an exception (\const{SIO_FERR}).

    \cfunction{int}{Sset_exception}{IOSTREAM *s, term_t ex}
Associate a Prolog exception term with the stream or clear the
associated exception if \arg{ex} is 0 and set/clear the
\const{SIO_FERR} condition on the stream.  If an exception
is assocated PL_release_stream() raises this exception.
\end{description}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Foreign stream encoding}
\label{sec:iostream-encoding}

\ctype{IOSTREAM} has a field \const{encoding} that is managed at
initialization from \const{SIO_TEXT}. The available encodings are
defined as a C \jargon{enum} as below.

\begin{code}
typedef enum
{ ENC_UNKNOWN = 0,			/* invalid/unknown */
  ENC_OCTET,				/* raw 8 bit input */
  ENC_ASCII,				/* US-ASCII (0..127) */
  ENC_ISO_LATIN_1,			/* ISO Latin-1 (0..256) */
  ENC_ANSI,				/* default (multibyte) codepage */
  ENC_UTF8,
  ENC_UNICODE_BE,			/* big endian unicode file */
  ENC_UNICODE_LE,			/* little endian unicode file */
  ENC_WCHAR				/* wchar_t */
} IOENC;
\end{code}

\jargon{Binary} streams always have the encoding \const{ENC_OCTET}. The
default encoding of a text stream depends on the Prolog flag
\prologflag{encoding}. The encoding is used by all functions that
perform text I/O on a stream. The encoding can be changed at any moment
using Ssetenc() which is available from Prolog using the set_stream/2
\term{encoding}{Encoding} property.  Functions that explicitly manage
the encoding are:

\begin{description}
    \cfunction{int}{Ssetenc}{IOSTREAM *s, IOENC new_enc, IOENC *old_enc}
Set the encoding for \arg{s} to \arg{new_enc} and, if \arg{old_enc} is
not \const{NULL}, return the old encoding.  This function may fail,
returning -1 if the Scontrol_function() of the stream returns -1 on
the \const{SIO_SETENCODING} request.  On success it returns 0.  If
\arg{new_enc} is \const{ENC_OCTET} the stream is switched to binary
mode.  Otherwise text mode is enabled.

    \cfunction{int}{ScheckBOM}{IOSTREAM *s}
This function may be called on a buffered input stream immediately after
opening the stream. If the stream starts with a known \jargon{Byte Order
Mark} (BOM) the encoding is set accordingly and the flag \const{SIO_BOM}
is set on the stream. Possibly resulting encodings are \const{ENC_UTF8},
\const{ENC_UNICODE_BE} and \const{ENC_UNICODE_LE}.

    \cfunction{int}{SwriteBOM}{IOSTREAM *s}
This function writes a \jargon{Byte Order Mark} (BOM) to \arg{s} and
should be called immediately after opening a stream for writing. If the
encoding is one of \const{ENC_UTF8}, \const{ENC_UNICODE_BE} or
\const{ENC_UNICODE_LE} it writes the code point \verb$\ufeff$ (a
zero-width white space) to the stream in the current encoding and
sets the \const{SIO_BOM} flag on the stream.

    \cfunction{int}{Scanrepresent}{int c, IOSTREAM *s}
Returns 0 if the encoding of \arg{s} can represent the code point
\arg{c} and -1 otherwise.
\end{description}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Foreign stream line endings}
\label{sec:iostream-newline}

Text streams have a field \const{newline} that controls the handling of
the newline convention. Note that inside Prolog all lines end with a
single newline (\verb$\u000a$, \verb$\n$) code point. The values are
described below. The default depends on the OS and can be manipulated
using the \term{newline}{Mode} property of set_stream/2.

\begin{description}
    \definition{\const{SIO_NL_DETECT}}
This mode may be enabled on an input stream.  It causes the stream
to read up to the first newline to set the newline mode accordingly.
    \definition{\const{SIO_NL_POSIX}}
Do not do any translation on input or output.
    \definition{\const{SIO_NL_DOS}}
Emit a newline (\verb$\n$) as \verb$\r\n$.  Discard \verb$\r$ from the
input.\footnote{The current implementation does not check that the
character is followed by \chr{\}n.}
\end{description}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Foreign stream position information}
\label{sec:iostream-position}

The \ctype{IOSTREAM} has a field \const{position} that points at
a structure of type \ctype{IOPOS}.  This structure is defined as
below.

\begin{code}
typedef struct io_position
{ int64_t		byteno;		/* byte-position in file */
  int64_t		charno;		/* character position in file */
  int			lineno;		/* lineno in file */
  int			linepos;	/* position in line */
  intptr_t		reserved[2];	/* future extensions */
} IOPOS;
\end{code}

If a stream is created using the flag \const{SIO_RECORDPOS} the IO
functions maintain the position information.  Note that, supporting
the ISO stream position data (see stream_property/2), both the byte
and character position is maintained.  These may differ if the stream
uses a multibyte encoding.

The \const{linepos} is updated as follows: \verb$\n$ and \verb$\r$ reset
the position to 0 (zero). The backspace (\verb$\b$) decrements the
position if it is positive. The tab (\verb$\t$) tabs to the next
multiple of 8. Any other character increments the line position by one.
Future versions may change that, notably the tab width might no longer
be hard coded.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\subsection{Support functions for blob save/load}
\label{sec:iostream-blob-functions}

The functions in this sections are intended to support \jargon{blobs} to
define save() and load() functions so they can be part of a saved state
or \fileext{qlf} file. The matching pair of functions is guaranteed to
give the same result, regardless of byte ordering (big or little
endian). The user must not make any assumptions on the exact data
format used for storing the data.  The atom read/write functions can
only be used from the blob callback functions.

For saving an uninterpreted array of bytes, it is suggested that the
length is output as a \ctype{size_t} value using PL_qlf_put_uint32()
followed by the bytes using Sfwrite(); and for loading, the length is
read using PL_qlf_get_uint32(), a buffer is allocated, and the bytes
are read using Sfread().

\begin{description}
    \cfunction{int}{PL_qlf_put_int64}{int64_t i, IOSTREAM *s}
    \nodescription
    \cfunction{int}{PL_qlf_put_int32}{int32_t i, IOSTREAM *s}
    \nodescription
    \cfunction{int}{PL_qlf_put_uint32}{uint32 i, IOSTREAM *s}
Write integers of several sizes. Signed integers are written in
\jargon{zigzag} encoding. For unsigned integers we only write the
non-zero bytes.  The result is compact and the same for big
or little endian.

    \cfunction{int}{PL_qlf_put_double}{double f, IOSTREAM *s}
Write double as 8 byte big endian.

    \cfunction{int}{PL_qlf_put_atom}{atom_t a, IOSTREAM *s}
Write an atom. The atom may be a \jargon{blob}. Note that this function
may \emph{only} be used from a blob save() function. Calling from
another context results in a fatal error.

    \cfunction{int}{PL_qlf_get_int64}{IOSTREAM *s, int64_t *ip}
    \nodescription
    \cfunction{int}{PL_qlf_get_int32}{IOSTREAM *s, int32_t *ip}
    \nodescription
    \cfunction{int}{PL_qlf_get_uint32}{IOSTREAM *s, uint32_t *ip}
    \nodescription
    \cfunction{int}{PL_qlf_get_double}{IOSTREAM *s, double *fp}
Counterparts of corresponding PL_qlf_put_*() functions.

    \cfunction{int}{PL_qlf_get_atom}{IOSTREAM *s, atom_t *ap}
Counterpart of PL_qlf_put_atom().  Again, this may \emph{only}
be used in the context of a blob load() function.
\end{description}
