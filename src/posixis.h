/*  Header : posixis.h
    Author : Richard A. O'Keefe
    Updated: 02/20/07
    Purpose: define POSIX S_ISxxx() macros, if possible.
*/

/*  The classic Unix interface for stat() used bitmasks for file types,
    (mode & S_IFMT), with a range of S_Ixxx values.  The POSIX
    standard replaced (NOT supplemented) these with a family of
    S_ISxxx(mode) function-like macros, which might even be real functions.

    POSIX systems are allowed to provide only the S_ISxxx macros.
    Historic Unix systems provided only the S_IFMT and S_IFxxx macros.
    Windows, acccording to the MSDN web site, is like historic Unix,
    except for sticking wholly unwarranted underscores in front of
    everything.  There are other systems yet where __ is used as a prefix.

    The aim of this file is to provide
        S_ISBLK(m)              block special?
        S_ISCHR(m)              character special?
        S_ISDIR(m)              directory?
        S_ISFIFO(m)             named pipe?
        S_ISREG(m)              regular file?
        S_ISSOCK(m)             socket?
    if possible.  If S_ISREG() and S_ISDIR() cannot be provided,
    a compile-time error will result; for anything else you should
    still make your own test.

    You should include this after including <sys/stat.h>.
    This file is intended for use with stat() and fstat(), which never
    return information about symbolic links, so S_ISLNK() is omitted.
*/

#ifdef S_IFMT
#  if !defined(S_ISBLK) && defined(S_IFBLK)
#    define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
#  endif
#  if !defined(S_ISCHR) && defined(S_IFCHR)
#    define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#  endif
#  if !defined(S_ISDIR) && defined(S_IFDIR)
#    define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#  endif
#  if !defined(S_ISFIFO) && defined(S_IFIFO)
#    define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#  endif
#  if !defined(S_ISREG) && defined(S_IFREG)
#    define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#  endif
#  if !defined(S_ISSOCK) && defined(S_IFSOCK)
#    define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#  endif
#endif
#ifdef _S_IFMT
#  if !defined(S_ISBLK) && defined(_S_IFBLK)
#    define S_ISBLK(m) (((m) & _S_IFMT) == _S_IFBLK)
#  endif
#  if !defined(S_ISCHR) && defined(_S_IFCHR)
#    define S_ISCHR(m) (((m) & _S_IFMT) == _S_IFCHR)
#  endif
#  if !defined(S_ISDIR) && defined(_S_IFDIR)
#    define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#  endif
#  if !defined(S_ISFIFO) && defined(_S_IFIFO)
#    define S_ISFIFO(m) (((m) & _S_IFMT) == _S_IFIFO)
#  endif
#  if !defined(S_ISREG) && defined(_S_IFREG)
#    define S_ISREG(m) (((m) & _S_IFMT) == _S_IFREG)
#  endif
#  if !defined(S_ISSOCK) && defined(_S_IFSOCK)
#    define S_ISSOCK(m) (((m) & _S_IFMT) == _S_IFSOCK)
#  endif
#endif
#ifdef __S_IFMT
#  if !defined(S_ISBLK) && defined(__S_IFBLK)
#    define S_ISBLK(m) (((m) & __S_IFMT) == __S_IFBLK)
#  endif
#  if !defined(S_ISCHR) && defined(__S_IFCHR)
#    define S_ISCHR(m) (((m) & __S_IFMT) == __S_IFCHR)
#  endif
#  if !defined(S_ISDIR) && defined(__S_IFDIR)
#    define S_ISDIR(m) (((m) & __S_IFMT) == __S_IFDIR)
#  endif
#  if !defined(S_ISFIFO) && defined(__S_IFIFO)
#    define S_ISFIFO(m) (((m) & __S_IFMT) == __S_IFIFO)
#  endif
#  if !defined(S_ISREG) && defined(__S_IFREG)
#    define S_ISREG(m) (((m) & __S_IFMT) == __S_IFREG)
#  endif
#  if !defined(S_ISSOCK) && defined(__S_IFSOCK)
#    define S_ISSOCK(m) (((m) & __S_IFMT) == __S_IFSOCK)
#  endif
#endif
#ifndef S_ISREG
#  error S_ISREG could not be defined
#endif
#ifndef S_ISDIR
#  error S_ISDIR could not be defined
#endif
