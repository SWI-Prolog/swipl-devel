#ifndef REG_TYPE_H_INCLUDED
#define REG_TYPE_H_INCLUDED

/*
 * Do not insert extras between the "begin" and "end" lines -- this
 * chunk is automatically extracted to be fitted into regex.h.
 */
/* --- begin --- */
/* ensure certain things don't sneak in from system headers */
#ifdef __REG_WIDE_T
#undef __REG_WIDE_T
#endif
#ifdef __REG_WIDE_COMPILE
#undef __REG_WIDE_COMPILE
#endif
#ifdef __REG_WIDE_EXEC
#undef __REG_WIDE_EXEC
#endif
#ifdef __REG_REGOFF_T
#undef __REG_REGOFF_T
#endif
#ifdef __REG_VOID_T
#undef __REG_VOID_T
#endif
#ifdef __REG_CONST
#undef __REG_CONST
#endif
#ifdef __REG_NOFRONT
#undef __REG_NOFRONT
#endif
#ifdef __REG_NOCHAR
#undef __REG_NOCHAR
#endif
/* interface types */
#define	__REG_WIDE_T	charW
#define	__REG_REGOFF_T	long	/* not really right, but good enough... */
#define	__REG_VOID_T	void
#define	__REG_CONST	const
/* names and declarations */
#define	__REG_WIDE_COMPILE	re_compileW
#define	__REG_WIDE_EXEC		re_execW
#define	__REG_NOFRONT		/* don't want regcomp() and regexec() */
#define	__REG_NOCHAR		/* or the char versions */
#define	regfree			re_free
#define	regerror		re_error
/* --- end --- */

#endif /*REG_TYPE_H_INCLUDED*/
