/*	mp.c

	The MP library interface for SWI-Prolog, version 1.3
	Needs installation of GMP: the GNU Multiple Precision arithmetic library
	This version is depending on GNU version 3.0.1. Problems may arise with
	future releases of GMP.

	Version 1.2 changes:
	Improved complex arithmetic for mul, div, abs, sqrt.
	Source: ``Numerical Recipes'' by W. Press, S. Teukolsky, W. Vetterling,
	and B. Flannery, Cambridge Univ. Press.

	Version 1.3 changes:
	Adapted to GMP version 3.0.1
	Note: Line 221 contains a bug patch for GMP 3.0.1

	Copyright (c) 1999 Robert A. van Engelen, Florida State University
	engelen@cs.fsu.edu. All rights reserved.

MP foreign predicates
=====================

Convention:

N	is any MP number (integer, rational, float, complex)
Z	is integer
Q	is rational
F	is float
C	is complex

mp_setprec(+Prec)
	Set default MP floating point precision to integer Prec bits.
	All floating point and complex arithmetic operations following this call
	will use the default set precision. The precision of floating point
	and complex numbers stored in SWI-Prolog memory is not affected.
mp_getprec(+N, ?Prec)
	Get floating point precision Prec (in bits) of floating point or complex
	number N. If N is integer or rational, the default set precision will
	be returned.
mp_n(?N1, ?N2)
	Convert term N1 to MP N2, where N1 is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP integer
	float:   copy to MP float
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP integer
		 else if string contains float
		 then copy string float to MP float
		 else fail
	atom:	 see string
mp_z(?N, ?Z)
	Convert N to MP integer Z, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP integer
	float:   truncate to MP integer
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP integer
		 else if string contains float
		 then truncate string float to MP integer
		 else fail
	atom:	 see string
	If N is variable, N is unified with a Prolog integer for MP integer Z
	if within range. Fails otherwise.
mp_q(+N, ?Q)
	Convert N to MP rational Q, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP rational
	float:   error
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP rational
		 else if string contains float
		 then error
		 else fail
	atom:	 see string
	If the resulting denominator is 1, an integer is returned instead
mp_f(?N, ?F)
	Convert term N to MP float F, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: convert to MP float
	float:   copy to MP float
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP float
		 else if string contains float
		 then copy string float to MP float
		 else fail
	atom:	 see string
	If N is variable, N is unified with a Prolog float for MP float/int F
mp_c(+N, ?MPC)
	Convert term N to MP complex C, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP complex
	float:   copy to MP complex
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP complex
		 else if string contains float
		 then copy string float to MP complex
		 else fail
	atom:	 see string
mp_add(+N1, +N2, ?N3)
	N3 = N1+N2
mp_sub(+N1, +N2, ?N3)
	N3 = N1-N2
mp_mul(+N1, +N2, ?N3)
	N3 = N1*N2
mp_div(+N1, +N2, ?N3)
	N3 = N1/N2
mp_tdivrem(+Z1, +Z2, ?Z3, ?Z4)
	Z3 = truncate(Z1/Z2)
	Z4 = remainder(Z1/Z2) (remainder of truncate)
mp_fdivrem(+Z1, +Z2, ?Z3, ?Z4)
	Z3 = floor(Z1/Z2)
	Z4 = remainder(Z1/Z2) (remainder of floor)
mp_cdivrem(+Z1, +Z2, ?Z3, ?Z4)
	Z3 = ceil(Z1/Z2)
	Z4 = remainder(Z1/Z2) (remainder of ceil)
mp_mod(+Z1, +Z2, ?Z3)
	Z3 = mod(Z1, Z2)
mp_lsh(+N1, +Z, ?N2)
	N2 = N1 << Z
	If N1 is rational, float N2 is returned
	If N1 is complex, real part is shifted
mp_rsh(+N1, +Z, ?N2)
	N2 = N1 >> Z
	If N1 is rational, float N2 is returned
	If N1 is complex, real part is shifted
mp_cmp(+N1, +N2, ?Rel)
	Compare N1 with N2.
	Rel = (<) if N1 < N2
	Rel = (=) if N1 = N2
	Rel = (>) if N1 > N2
	For complex numbers, the real part is compared
mp_neg(+N1, ?N2)
	N2 = -N1
mp_inv(+N1, ?N2)
	N2 = 1/N1
mp_abs(+N1, ?N2)
	N2 = |N1|
	If N1 is complex, N2 is the magnitude of N1
mp_sgn(+N1, ?Sign)
	Sign = (<) if N1 < 0
	Sign = (=) if N1 = 0
	Sign = (>) if N1 > 0
	If N1 is complex, the sign of the real part is returned
mp_sqrt(+N1, ?N2)
	N2 = sqrt(N1)
	If N1 is rational, N2 is float
mp_pow(+Z1, +Z2, ?Z3)
	Integer power operation, where the exponent Z2 > 0
	Z3 = Z1^Z2
mp_gcd(+Z1, +Z2, ?Z3)
	Z3 = gcd(Z1, Z2)
mp_bin(+Z1, +Z2, ?Z3)
	Z3 = binomial(Z1, Z2)
mp_fac(+Z1, ?Z2)
	Z2 = Z1!
mp_fib(+Z1, ?Z2)
	Z2 = fibonacci(Z1)
mp_floor(+N, ?Z)
	Z = floor(N)
mp_ceil(+N, ?Z)
	Z = ceil(N)
mp_trunc(+N, ?Z)
	Z = trunc(N)
mp_jacobi(+Z1, +Z2, ?Z3)
	Z3 = J(Z1, Z2)
mp_legendre(+Z1, +Z2, ?Z3)
	Z3 = Legendre function of Z1 and Z2
mp_reldiff(+F1, +F2, ?F3)
	F3 = relative difference between F1 and F2
mp_and(+Z1, +Z2, ?Z3)
	Z3 = bitwise-and of Z1 and Z2
mp_or(+Z1, +Z2, ?Z3)
	Z3 = bitwise-or of Z1 and Z2
mp_not(+Z1, ?Z2)
	Z2 = bitwise complement of Z1
mp_setbit(+Z1, +Z2, ?Z3)
	Set bit Z2 of Z1 and return Z3
mp_clrbit(+Z1, +Z2, ?Z3)
	Clear bit Z2 of Z1 and return Z3
mp_term(+N, +Base, +NumDigits, ?Term)
	Convert N to a readable Prolog term Term using base Base (2<=Base<=36)
	and max number of digits NumDigits. If NumDigits = 0, floating point
	and complex numbers are converted with the maximum number of digits
	allowed by the precision of the number.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include "SWI-Prolog.h"
#include <gmp.h>
#include "pl-mp.h"

#if(__GNU_MP_VERSION-3||__GNU_MP_VERSION_MINOR||__GNU_MP_VERSION_PATCHLEVEL-1)
#define mp_get_prec(f) mpf_get_prec(f)
#else
#define mp_get_prec(f) (mpf_get_prec(f)-mp_bits_per_limb+1)	/* version 3.0.1 */
#endif

		/*******************************
		*      MP DATA STRUCTURES      *
		*******************************/

static functor_t/* functors used by mp: */
	mpz,	/* $mpz/1: holds `invisible' GMP integer */
	mpq,	/* $mpq/2: holds `invisible' GMP rational */
	mpf,	/* $mpf/1: holds `invisible' GMP float */
	mpc,	/* $mpc/2: holds `invisible' complex (added to GMP) */
	mpr,	/* (/)/2: visible rational (division) */
	mpx,	/* complex/2: visible complex */
	mpn;	/* (-)/1: visible unary minus */

static atom_t
	mp0,		/* $mp0/0: indeterminate rational 0/0 */
	relatom[3];	/* atoms `<', '=', and '>' */

#define relidx(X) (X > 0 ? 2 : X < 0 ? 0 : 1)	/* index func. for rel atoms */

static long mp_min_int, mp_max_int;	/* min and max SWI-Prolog ints */

#define BUFSIZE (32768)	/* holds digits for conversion of MP to readable form.
			   Unfortunately, this limits precision of the output */

static char buf[BUFSIZE];

static mp_tp mp_convert(mp_t *mp, mp_tp t);

		/*******************************
		*         MP PRIMITIVES        *
		*******************************/

/*	mpf_normalize(mpf_t f)
	Utility function used by mp_to_term to normalize floats by removing
	zeros at the low end
*/

void mpf_normalize(mpf_t f)
{	mp_ptr up = f->_mp_d;
	int i, size = f->_mp_size, usize = abs(size);
	if (size == 0)
	{	f->_mp_prec = (53+2*mp_bits_per_limb-1)/mp_bits_per_limb;
		f->_mp_exp = 0;
	}
	while (up[0] == 0 && usize > 0)
	{	up++;
		usize--;
	}
	if (usize != abs(size))
	{	for (i = 0; i < usize; i++)
			f->_mp_d[i] = up[i];
		f->_mp_size = size > 0 ? usize : -usize;
	}
}

/*	mpq_normalize(mp_t *mp)
	Utility function to convert rational mp into integer when denominator
	is one. When denominator is zero, rational is converted to MP0
	(0/0 rational)
*/

void mpq_normalize(mp_t *mp)
{	if (mpz_cmp_ui(mpq_denref(mp->n.q), (unsigned long)1) == 0)
	{	mp->t = MPZ;
		mpz_clear(mpq_denref(mp->n.q));
		mp->n.z[0] = *mpq_numref(mp->n.q);
	}
	else if (mpz_cmp_ui(mpq_denref(mp->n.q), (unsigned long)0) == 0)
	{	mp->t = MP0;
		mpq_clear(mp->n.q);
	}
}

/*	foreign_t mp_to_term(mp_t *mp, term_t term)
	Convert mp number to SWI-Prolog term where term is one of
	$mpz/1: integer
	$mpq/2: rational
	$mpf/1: float
	$mpc/2: complex
	$mp0/0: 0/0 indeterminate rational
	Afterwards, the mp number is cleared from memory.
	Special care has been taken to ensure that numbers are stored in
	normalized form so terms representing equal numbers can be unified.
	This function heavily depends on the GMP 2.0.2 data structures as
	defined in gmp.h. Incompatibility may arise with future releases of GMP
*/

static foreign_t mp_to_term(mp_t *mp, term_t term)
{	term_t new_term = PL_new_term_ref();
	switch (mp->t) {
	case MPZ:
	{	term_t arg = PL_new_term_ref();
		int size = sizeof(mp_limb_t)*abs(mp->n.z->_mp_size);
		char *buf;
		if ((buf = malloc(size+sizeof(mpz_t))) == NULL)
			return PL_warning("MP: cannot allocate space for integer");
		*((mpz_ptr)buf) = mp->n.z[0];
		((mpz_ptr)buf)->_mp_alloc = 0;
		/* set to 0, enabling Prolog unification */
		((mpz_ptr)buf)->_mp_d = NULL;
		/* set to NULL, enabling Prolog unification */
		memcpy(buf+sizeof(mpz_t), mp->n.z->_mp_d, size);
		mpz_clear(mp->n.z);
		PL_put_string_nchars(arg, size+sizeof(mpz_t), buf);
		free(buf);
		PL_cons_functor(new_term, mpz, arg);
		break;
	}
	case MPQ:
	{	term_t num = PL_new_term_ref();
		term_t den = PL_new_term_ref();
		int numsize = sizeof(mp_limb_t)*abs(mpq_numref(mp->n.q)->_mp_size);
		int densize = sizeof(mp_limb_t)*abs(mpq_denref(mp->n.q)->_mp_size);
		char *buf;
		if ((buf = malloc(max(numsize, densize)+sizeof(mpz_t))) == NULL)
			return PL_warning("MP: cannot allocate space for rational");
		*((mpz_ptr)buf) = *(mpq_numref(mp->n.q));
		((mpz_ptr)buf)->_mp_alloc = 0;
		/* set to 0, enabling Prolog unification */
		((mpz_ptr)buf)->_mp_d = NULL;
		/* set to NULL, enabling Prolog unification */
		memcpy(buf+sizeof(mpz_t), mpq_numref(mp->n.q)->_mp_d, numsize);
		PL_put_string_nchars(num, numsize+sizeof(mpz_t), buf);
		*((mpz_ptr)buf) = *(mpq_denref(mp->n.q));
		((mpz_ptr)buf)->_mp_alloc = 0;
		/* set to 0, enabling Prolog unification */
		((mpz_ptr)buf)->_mp_d = NULL;
		/* set to NULL, enabling Prolog unification */
		memcpy(buf+sizeof(mpz_t), mpq_denref(mp->n.q)->_mp_d, densize);
		mpq_clear(mp->n.q);
		PL_put_string_nchars(den, densize+sizeof(mpz_t), buf);
		free(buf);
		PL_cons_functor(new_term, mpq, num, den);
		break;
	}
	case MPF:
	{	term_t arg = PL_new_term_ref();
		int size;
		char *buf;
		mpf_normalize(mp->n.f);
		size = sizeof(mp_limb_t)*abs(mp->n.f->_mp_size);
		if ((buf = malloc(size+sizeof(mpf_t))) == NULL)
			return PL_warning("MP: cannot allocate space for float");
		*((mpf_ptr)buf) = mp->n.f[0];
		((mpf_ptr)buf)->_mp_d = NULL;
		/* set to NULL, enabling Prolog unification */
		memcpy(buf+sizeof(mpf_t), mp->n.f->_mp_d, size);
		mpf_clear(mp->n.f);
		PL_put_string_nchars(arg, size+sizeof(mpf_t), buf);
		free(buf);
		PL_cons_functor(new_term, mpf, arg);
		break;
	}
	case MPC:
	{	term_t re = PL_new_term_ref();
		term_t im = PL_new_term_ref();
		int resize, imsize;
		char *buf;
		mpf_normalize(mp->n.c.r);
		mpf_normalize(mp->n.c.i);
		resize = sizeof(mp_limb_t)*abs(mp->n.c.r->_mp_size);
		imsize = sizeof(mp_limb_t)*abs(mp->n.c.i->_mp_size);
		if ((buf = malloc(max(resize, imsize)+sizeof(mpf_t))) == NULL)
			return PL_warning("MP: cannot allocate space for complex");
		*((mpf_ptr)buf) = mp->n.c.r[0];
		((mpf_ptr)buf)->_mp_d = NULL;
		/* set to NULL, enabling Prolog unification */
		memcpy(buf+sizeof(mpf_t), mp->n.c.r->_mp_d, resize);
		mpf_clear(mp->n.c.r);
		PL_put_string_nchars(re, resize+sizeof(mpf_t), buf);
		*((mpf_ptr)buf) = mp->n.c.i[0];
		((mpf_ptr)buf)->_mp_d = NULL;
		/* set to NULL, enabling Prolog unification */
		memcpy(buf+sizeof(mpf_t), mp->n.c.i->_mp_d, imsize);
		mpf_clear(mp->n.c.i);
		PL_put_string_nchars(im, imsize+sizeof(mpf_t), buf);
		free(buf);
		PL_cons_functor(new_term, mpc, re, im);
		break;
	}
	case MP0:
	{	PL_unify_atom(new_term, mp0);
		break;
	}
	default: PL_fail;
	}
	return PL_unify(new_term, term);
}

/*	void mp_clear(mp_t *mp)
	Clean up mp number.
*/

void mp_clear(mp_t *mp)
{	switch (mp->t) {
	case MPZ:
		mpz_clear(mp->n.z);
		break;
	case MPQ:
		mpq_clear(mp->n.q);
		break;
	case MPF:
		mpf_clear(mp->n.f);
		break;
	case MPC:
		mpf_clear(mp->n.c.r);
		mpf_clear(mp->n.c.i);
	}
	mp->t = MPE;
}

/*	void mp_str2mp(char *s, mp_t *mp)
	Used by term_to_mp() to convert string to MP
*/

void mp_str2mp(char *s, mp_t *mp)
{	if (!mpz_init_set_str(mp->n.z, s, 0))
		mp->t = MPZ;
	else
	{	mpf_init2(mp->n.f, (10*strcspn(s, "eE"))/3);
		if (!mpf_set_str(mp->n.f, s, 10))
			mp->t = MPF;
		else	mpf_clear(mp->n.f);
	}
}

/*	mp_tp term_to_mp(term_t term, mp_t *mp)
	Convert SWI-Prolog term to mp number where term is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP integer
	float:   copy to MP float
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP integer
		 else if string contains float
		 then copy string float to MP float
		 else return MPE (not an MP number)
	atom:	 see string
	other:   return MPE (not an MP number)
	The mp number is allocated in memory, so eventually it must be cleared.
	This function heavily depends on the GMP 3.0.1 data structures as
	defined in gmp.h. Incompatibility may arise with future versions of GMP
*/

mp_tp term_to_mp(term_t term, mp_t *mp)
{	mp->t = MPE;
	switch (PL_term_type(term)) {
	case PL_TERM:
	{	functor_t f;
		PL_get_functor(term, &f);
		if (f == mpz)
		{	term_t arg = PL_new_term_ref();
			char *s;
			int n;
			_PL_get_arg(1, term, arg);
			if (PL_get_string(arg, &s, &n))
			{	mpz_t z;
				mp->t = MPZ;
				z[0] = *((mpz_ptr)s);
				/* z->_mp_alloc=0, but this works */
				z[0]._mp_d = (mp_ptr)(s+sizeof(mpz_t));
				mpz_init_set(mp->n.z, z);
			}
		}
		else if (f == mpq)
		{	term_t num = PL_new_term_ref();
			term_t den = PL_new_term_ref();
			char *snum, *sden;
			int nnum, nden;
			_PL_get_arg(1, term, num);
			_PL_get_arg(2, term, den);
			if (PL_get_string(num, &snum, &nnum) && PL_get_string(den, &sden, &nden))
			{	mpz_t z;
				mp->t = MPQ;
				/* mpq_init(mp->n.q); */
				z[0] = *((mpz_ptr)snum);
				/* z->_mp_alloc=0, but this works */
				z[0]._mp_d = (mp_ptr)(snum+sizeof(mpz_t));
				mpz_init_set(mpq_numref(mp->n.q), z);
				/* mpq_set_num(mp->n.q, z); */
				z[0] = *((mpz_ptr)sden);
				/* z->_mp_alloc=0, but this works */
				z[0]._mp_d = (mp_ptr)(sden+sizeof(mpz_t));
				/* mpq_set_den(mp->n.q, z); */
				mpz_init_set(mpq_denref(mp->n.q), z);
			}
		}
		else if (f == mpf)
		{	term_t arg = PL_new_term_ref();
			char *s;
			int n;
			_PL_get_arg(1, term, arg);
			if (PL_get_string(arg, &s, &n))
			{	mpf_t f;
				mp->t = MPF;
				f[0] = *((mpf_ptr)s);
				f[0]._mp_d = (mp_ptr)(s+sizeof(mpf_t));
				/* f->_mp_d points to less limbs than _mp_prec suggests, but it works */
				mpf_init2(mp->n.f, mp_get_prec(f));
				mpf_set(mp->n.f, f);
			}
		}
		else if (f == mpc)
		{	term_t re = PL_new_term_ref();
			term_t im = PL_new_term_ref();
			char *sre, *sim;
			int nre, nim;
			_PL_get_arg(1, term, re);
			_PL_get_arg(2, term, im);
			if (PL_get_string(re, &sre, &nre) && PL_get_string(im, &sim, &nim))
			{	mpf_t f;
				mp->t = MPC;
				f[0] = *((mpf_ptr)sre);
				f[0]._mp_d = (mp_ptr)(sre+sizeof(mpf_t));
				mpf_init2(mp->n.c.r, mp_get_prec(f));
				mpf_set(mp->n.c.r, f);
				f[0] = *((mpf_ptr)sim);
				f[0]._mp_d = (mp_ptr)(sim+sizeof(mpf_t));
				mpf_init2(mp->n.c.i, mp_get_prec(f));
				mpf_set(mp->n.c.i, f);
			}
		}
		else if (f == mpr)
		{	term_t num = PL_new_term_ref();
			term_t den = PL_new_term_ref();
			mp_t mpnum, mpden;
			_PL_get_arg(1, term, num);
			_PL_get_arg(2, term, den);
			if (term_to_mp(num, &mpnum) == MPZ)
			{	if (term_to_mp(den, &mpden) == MPZ)
				{	mp->t = MPQ;
					*(mpq_numref(mp->n.q)) = mpnum.n.z[0];
					*(mpq_denref(mp->n.q)) = mpden.n.z[0];
					mpq_canonicalize(mp->n.q);
				}
				else
				{	mp_clear(&mpnum);
					mp_clear(&mpden);
				}
			}
			else	mp_clear(&mpnum);
		}
		else if (f == mpx)
		{	term_t re = PL_new_term_ref();
			term_t im = PL_new_term_ref();
			mp_t mpre, mpim;
			_PL_get_arg(1, term, re);
			_PL_get_arg(2, term, im);
			if (term_to_mp(re, &mpre) != MPE
			 && term_to_mp(im, &mpim) != MPE)
			{	mp_convert(&mpre, MPF);
				mp_convert(&mpim, MPF);
				mp->t = MPC;
				mp->n.c.r[0] = mpre.n.f[0];
				mp->n.c.i[0] = mpim.n.f[0];
			}
		}
		else if (f == mpn)
		{	term_t arg = PL_new_term_ref();
			_PL_get_arg(1, term, arg);
			switch (term_to_mp(arg, mp)) {
			case MPZ:
				mpz_neg(mp->n.z, mp->n.z);
				break;
			case MPQ:
				mpq_neg(mp->n.q, mp->n.q);
				break;
			case MPF:
				mpf_neg(mp->n.f, mp->n.f);
				break;
			case MPC:
				mpf_neg(mp->n.c.r, mp->n.c.r);
			}
		}
		break;
	}
	case PL_INTEGER:
	{	long z;
		PL_get_long(term, &z);
		mp->t = MPZ;
		mpz_init_set_si(mp->n.z, z);
		break;
	}
	case PL_FLOAT:
	{	double f;
		PL_get_float(term, &f);
		mp->t = MPF;
		mpf_init_set_d(mp->n.f, f);
		break;
	}
	case PL_STRING:
	{	char *s;
		int n;
		PL_get_string(term, &s, &n);
		mp_str2mp(s, mp);
		break;
	}
	case PL_ATOM:
	{	char *s;
		PL_get_atom_chars(term, &s);
		mp_str2mp(s, mp);
	}
	}
	return mp->t;
}

/*	Primitive MP number conversion routines of the form
		mp_tp mp_x2y(mp_t *mp)
	That convert mp of type x to y and return type y
*/

mp_tp mp_z2q(mp_t *mp)
{	mpq_t q;
	mpq_init(q);
	mpq_set_z(q, mp->n.z);
	mpz_clear(mp->n.z);
	(mp->n.q)[0] = q[0];
	mp->t = MPQ;
	return MPQ;
}

mp_tp mp_z2f(mp_t *mp)
{	mpf_t f;
	mpf_init(f);
	mpf_set_z(f, mp->n.z);
	mpz_clear(mp->n.z);
	(mp->n.f)[0] = f[0];
	mp->t = MPF;
	return MPF;
}

mp_tp mp_z2c(mp_t *mp)
{	mpf_t r;
	mpf_init(r);
	mpf_set_z(r, mp->n.z);
	mpz_clear(mp->n.z);
	(mp->n.c.r)[0] = r[0];
	mpf_init(mp->n.c.i);
	mp->t = MPC;
	return MPC;
}

mp_tp mp_q2z(mp_t *mp)
{	mpz_t z;
	mpz_init(z);
	mpz_set_q(z, mp->n.q);
	mpq_clear(mp->n.q);
	(mp->n.z)[0] = z[0];
	mp->t = MPZ;
	return MPZ;
}

mp_tp mp_q2f(mp_t *mp)
{	mpf_t f;
	mpf_init(f);
	mpf_set_q(f, mp->n.q);
	mpq_clear(mp->n.q);
	(mp->n.f)[0] = f[0];
	mp->t = MPF;
	return MPF;
}

mp_tp mp_q2c(mp_t *mp)
{	mpf_t r;
	mpf_init(r);
	mpf_set_q(r, mp->n.q);
	mpq_clear(mp->n.q);
	(mp->n.c.r)[0] = r[0];
	mpf_init(mp->n.c.i);
	mp->t = MPC;
	return MPC;
}

mp_tp mp_f2z(mp_t *mp)
{	mpz_t z;
	mpz_init(z);
	mpz_set_f(z, mp->n.f);
	mpf_clear(mp->n.f);
	(mp->n.z)[0] = z[0];
	mp->t = MPZ;
	return MPZ;
}

mp_tp mp_f2q(mp_t *mp)
{	PL_warning("MP: cannot convert float to rational");
	return MPE;
}

mp_tp mp_f2c(mp_t *mp)
{	(mp->n.c.r)[0] = (mp->n.f)[0];
	mpf_init2(mp->n.c.i, mp_get_prec(mp->n.c.r));
	mp->t = MPC;
	return MPC;
}

mp_tp mp_c2z(mp_t *mp)
{	mpz_t z;
	mpz_init(z);
	mpz_set_f(z, mp->n.c.r);
	mpf_clear(mp->n.c.r);
	mpf_clear(mp->n.c.i);
	(mp->n.z)[0] = z[0];
	mp->t = MPZ;
	return MPZ;
}

mp_tp mp_c2q(mp_t *mp)
{	PL_warning("MP: cannot convert complex to rational");
	return MPE;
}

mp_tp mp_c2f(mp_t *mp)
{	(mp->n.f)[0] = (mp->n.c.r)[0];
	mpf_clear(mp->n.c.i);
	mp->t = MPF;
	return MPF;
}

/*	mp_tp mp_convert(mp_t *mp, mp_tp t)
	Convert MP number mp to type t and return t.
*/

mp_tp mp_convert(mp_t *mp, mp_tp t)
{	switch (mp->t) {
	case MPZ:
		switch (t) {
		case MPZ: return MPZ;		/* type is ok: do nothing */
		case MPQ: return mp_z2q(mp);
		case MPF: return mp_z2f(mp);
		case MPC: return mp_z2c(mp);
		default: return MPE;
		}
	case MPQ:
		switch (t) {
		case MPZ: return mp_q2z(mp);
		case MPQ: return MPQ;		/* type is ok: do nothing */
		case MPF: return mp_q2f(mp);
		case MPC: return mp_q2c(mp);
		default: return MPE;
		}
	case MPF:
		switch (t) {
		case MPZ: return mp_f2z(mp);
		case MPQ: return mp_f2q(mp);
		case MPF: return MPF;		/* type is ok: do nothing */
		case MPC: return mp_f2c(mp);
		default: return MPE;
		}
	case MPC:
		switch (t) {
		case MPZ: return mp_c2z(mp);
		case MPQ: return mp_c2q(mp);
		case MPF: return mp_c2f(mp);
		case MPC: return MPC;		/* type is ok: do nothing */
		default: return MPE;
		}
	}
	return MPE;
}

/*	mp_tp mp_conformargs(term_t arg1, term_t arg2, mp_t *mp1, mp_t *mp2)
	Make SWI-Prolog arguments arg1 and arg2 of a binary operation conforming
	by converting one of the two arguments to the type of the other if
	necessary. MP number mp1 is the converted arg1 and mp2 is the converted
	arg2. Numbers are converted `upwards' by respecting the following total
	order on MP numbers:
	integer < rational < float < complex
	When both arguments are float, the precisions are adjusted to the
	smallest precision of the two.
*/

mp_tp mp_conformargs(term_t arg1, term_t arg2, mp_t *mp1, mp_t *mp2)
{	switch (term_to_mp(arg1, mp1)) {
	case MPZ:
		return mp_convert(mp1, term_to_mp(arg2, mp2));
	case MPQ:
		switch (term_to_mp(arg2, mp2)) {
		case MPZ: return mp_convert(mp2, MPQ);
		default: return mp_convert(mp1, mp2->t);
		}
	case MPF:
		switch (term_to_mp(arg2, mp2)) {
		case MPZ:
		case MPQ: return mp_convert(mp2, MPF);
		case MPF:
		{	unsigned long p1 = mp_get_prec(mp1->n.f),
				      p2 = mp_get_prec(mp2->n.f);
			if (p1 > p2)
				mpf_set_prec(mp1->n.f, p2);
			else	mpf_set_prec(mp2->n.f, p1);
		}
		default: return mp_convert(mp1, mp2->t);
		}
	case MPC:
		switch (term_to_mp(arg2, mp2)) {
		case MPZ:
		case MPQ:
		case MPF: return mp_convert(mp2, MPC);
		default: return mp_convert(mp1, mp2->t);
		}
	default: return MPE;
	}
}

/*	int mpf_to_buf(mpf_t f, int b, int n)
	Convert GMP mpf to string and store in static buffer. Used by mp_term()
*/

int mpf_to_buf(mpf_t f, int b, int n)
{	mp_exp_t e;
	if (n<=0)	/* can't we figure out buffer size from number f ??? */
		n = BUFSIZE-15;
	else if (n+15>BUFSIZE)
		return -1;
	mpf_get_str(buf+1, &e, b, n, f);
	if (buf[1] == '-')
	{	buf[0] = '-';
		buf[1] = buf[2];
		buf[2] = '.';
		if (buf[3] == '\0') {
			buf[3] = '0';
			buf[4] = '\0';
		}
	} else if (buf[1] == '\0')	/* 0.0 shows as empty string in GMP */
	{	buf[0] = '0';
		buf[1] = '.';
		buf[2] = '0';
		buf[3] = '\0';
		e++;
	} else
	{	buf[0] = buf[1];
		buf[1] = '.';
		if (buf[2] == '\0') {
			buf[2] = '0';
			buf[3] = '\0';
		}
	}
	if (--e != 0)
		sprintf(buf+strlen(buf), "E%ld", e);
	return 0;
}

		/*******************************
		*    MP FOREIGN PREDICATES     *
		*******************************/

/*	The following conventions for predicate arguments are used:
	Z = an integer (MP or SWI-Prolog integer or string with integer)
	Q = a rational (MP or SWI-Prolog N/D)
	F = a float (MP or SWI-Prolog float or string with float)
	C = a complex (MP or SWI-Prolog complex(R, I))
	N = any number
*/

/*	mp_n(?N1, ?N2)
	Convert term N1 to MP N2, where N1 is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP integer
	float:   copy to MP float
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP integer
		 else if string contains float
		 then copy string float to MP float
		 else fail
	atom:	 see string
*/

static foreign_t mp_n(term_t term, term_t term_n)
{	mp_t mp;
	term_to_mp(term, &mp);
	return mp_to_term(&mp, term_n);
}

/*	mp_z(?N, ?Z)
	Convert N to MP integer Z, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP integer
	float:   truncate to MP integer
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP integer
		 else if string contains float
		 then truncate string float to MP integer
		 else fail
	atom:	 see string
	If N is variable, N is unified with a Prolog integer for MP integer Z
	if within range. Fails otherwise.
*/

static foreign_t mp_z(term_t term, term_t term_z)
{	mp_t mp;
	if (PL_is_variable(term))
	{	if (term_to_mp(term_z, &mp) == MPZ
		 && mpz_cmp_si(mp.n.z, mp_min_int) >= 0
		 && mpz_cmp_si(mp.n.z, mp_max_int) <= 0)
		{	long z = mpz_get_si(mp.n.z);
			mpz_clear(mp.n.z);
			return PL_unify_integer(term, z);
		}
	}
	else
	{	term_to_mp(term, &mp);
		if (mp_convert(&mp, MPZ) == MPZ)
			return mp_to_term(&mp, term_z);
	}
	mp_clear(&mp);
	PL_fail;
}

/*	mp_q(+N, ?Q)
	Convert N to MP rational Q, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP rational
	float:   error
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP rational
		 else if string contains float
		 then error
		 else fail
	atom:	 see string
	If the resulting denominator is 1, an integer is returned instead
*/

static foreign_t mp_q(term_t term, term_t new_term)
{	mp_t mp;
	term_to_mp(term, &mp);
	if (mp_convert(&mp, MPQ) == MPQ)
	{	mpq_normalize(&mp);
		return mp_to_term(&mp, new_term);
	}
	mp_clear(&mp);
	PL_fail;
}

/*	mp_f(?N, ?F)
	Convert term N to MP float F, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: convert to MP float
	float:   copy to MP float
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP float
		 else if string contains float
		 then copy string float to MP float
		 else fail
	atom:	 see string
	If N is variable, N is unified with a Prolog float for MP float/int F
*/

static foreign_t mp_f(term_t term, term_t term_f)
{	mp_t mp;
	if (PL_is_variable(term))
	{	switch (term_to_mp(term_f, &mp)) {
		case MPZ:
		{	double f = mpz_get_d(mp.n.z);
			mpz_clear(mp.n.z);
			if (f >= DBL_MAX || f <= -DBL_MAX)
				PL_fail;
			return PL_unify_float(term, f);
		}
		case MPF:
		{	double f = mpf_get_d(mp.n.f);
			mpf_clear(mp.n.f);
			if (f >= DBL_MAX || f <= -DBL_MAX)
				PL_fail;
			return PL_unify_float(term, f);
		}
		}
	}
	else
	{	term_to_mp(term, &mp);
		if (mp_convert(&mp, MPF) == MPF)
			return mp_to_term(&mp, term_f);
	}
	mp_clear(&mp);
	PL_fail;
}

/*	mp_c(+N, ?MPC)
	Convert term N to MP complex C, where N is one of
	$mpz/1:  integer
	$mpq/2:  rational
	$mpf/1:  float
	$mpc/2:  complex
	(/)/2:   rational (operands must be integer)
	(-)/1:   negative number, operand = integer, rational, float, or complex
	integer: copy to MP complex
	float:   copy to MP complex
	string:	 if string contains integer (digits with optional minus sign)
	         then copy string integer to MP complex
		 else if string contains float
		 then copy string float to MP complex
		 else fail
	atom:	 see string
*/

static foreign_t mp_c(term_t term, term_t new_term)
{	mp_t mp;
	term_to_mp(term, &mp);
	if (mp_convert(&mp, MPC) == MPC)
		return mp_to_term(&mp, new_term);
	mp_clear(&mp);
	PL_fail;
}

/*	mp_setprec(+Prec)
	Set default MP floating point precision to integer Prec bits.
	All floating point and complex arithmetic operations following this call
	will use the default set precision. The precision of floating point
	and complex numbers stored in SWI-Prolog memory is not affected.
*/

static foreign_t mp_setprec(term_t term)
{	mp_t mp;
	if (term_to_mp(term, &mp) == MPZ)
	{	mpf_set_default_prec(mpz_get_ui(mp.n.z));
		mpz_clear(mp.n.z);
		PL_succeed;
	}
	mp_clear(&mp);
	PL_fail;
}

/*	mp_getprec(+N, ?Prec)
	Get floating point precision Prec (in bits) of floating point or complex
	number N. If N is integer or rational, the default set precision will
	be returned.
*/

static foreign_t mp_getprec(term_t op, term_t prec)
{	mp_t mp;
	long n;
	term_to_mp(op, &mp);
	if (mp_convert(&mp, MPF) == MPF)
		/* THIS IS TO CORRECT AN ERROR IN GMP 3.0.1: */
		n = (long)mp_get_prec(mp.n.f);
	else	PL_fail;
	mp_clear(&mp);
	return PL_unify_integer(prec, n);
}

/*	mp_add(+N1, +N2, ?N3)
	N3 = N1+N2
*/

static foreign_t mp_add(term_t addend1, term_t addend2, term_t sum)
{	mp_t mp1, mp2;
	switch (mp_conformargs(addend1, addend2, &mp1, &mp2)) {
	case MPZ:	
		mpz_add(mp1.n.z, mp1.n.z, mp2.n.z);
		mpz_clear(mp2.n.z);
		break;
	case MPQ:
		mpq_add(mp1.n.q, mp1.n.q, mp2.n.q);
		mpq_clear(mp2.n.q);
		mpq_normalize(&mp1);
		break;
	case MPF:
		mpf_add(mp1.n.f, mp1.n.f, mp2.n.f);
		mpf_clear(mp2.n.f);
		break;
	case MPC:
		mpf_add(mp1.n.c.r, mp1.n.c.r, mp2.n.c.r);
		mpf_clear(mp2.n.c.r);
		mpf_add(mp1.n.c.i, mp1.n.c.i, mp2.n.c.i);
		mpf_clear(mp2.n.c.i);
		break;
	default: PL_fail;
	}
	return mp_to_term(&mp1, sum);
}

/*	mp_sub(+N1, +N2, ?N3)
	N3 = N1-N2
*/

static foreign_t mp_sub(term_t minuend, term_t subtrahend, term_t difference)
{	mp_t mp1, mp2;
	switch (mp_conformargs(minuend, subtrahend, &mp1, &mp2)) {
	case MPZ:	
		mpz_sub(mp1.n.z, mp1.n.z, mp2.n.z);
		mpz_clear(mp2.n.z);
		break;
	case MPQ:
		mpq_sub(mp1.n.q, mp1.n.q, mp2.n.q);
		mpq_clear(mp2.n.q);
		mpq_normalize(&mp1);
		break;
	case MPF:
		mpf_sub(mp1.n.f, mp1.n.f, mp2.n.f);
		mpf_clear(mp2.n.f);
		break;
	case MPC:
		mpf_sub(mp1.n.c.r, mp1.n.c.r, mp2.n.c.r);
		mpf_clear(mp2.n.c.r);
		mpf_sub(mp1.n.c.i, mp1.n.c.i, mp2.n.c.i);
		mpf_clear(mp2.n.c.i);
		break;
	default: PL_fail;
	}
	return mp_to_term(&mp1, difference);
}

/*	mp_mul(+N1, +N2, ?N3)
	N3 = N1*N2
*/

static foreign_t mp_mul(term_t multiplier, term_t multiplicant, term_t product)
{	mp_t mp1, mp2;
	switch (mp_conformargs(multiplier, multiplicant, &mp1, &mp2)) {
	case MPZ:	
		mpz_mul(mp1.n.z, mp1.n.z, mp2.n.z);
		mpz_clear(mp2.n.z);
		break;
	case MPQ:
		mpq_mul(mp1.n.q, mp1.n.q, mp2.n.q);
		mpq_clear(mp2.n.q);
		mpq_normalize(&mp1);
		break;
	case MPF:
		mpf_mul(mp1.n.f, mp1.n.f, mp2.n.f);
		mpf_clear(mp2.n.f);
		break;
	case MPC:
	{	mpf_t f1, f2;
		mpf_init(f1);
		mpf_init(f2);
#ifdef FAST_COMPLEX_MUL
		mpf_mul(f1, mp1.n.c.i, mp2.n.c.i);
		mpf_add(mp1.n.c.i, mp1.n.c.r, mp1.n.c.i);
		mpf_mul(f2, mp1.n.c.r, mp2.n.c.r);
		mpf_sub(mp1.n.c.r, f2, f1);
		mpf_add(f2, f1, f2);
		mpf_clear(f1);
		mpf_add(mp2.n.c.i, mp2.n.c.r, mp2.n.c.i);
		mpf_clear(mp2.n.c.r);
		mpf_mul(mp1.n.c.i, mp1.n.c.i, mp2.n.c.i);
		mpf_clear(mp2.n.c.i);
		mpf_sub(mp1.n.c.i, mp1.n.c.i, f2);
		mpf_clear(f2);
#else
		mpf_mul(f1, mp1.n.c.r, mp2.n.c.r);
		mpf_mul(f2, mp1.n.c.i, mp2.n.c.i);
		mpf_sub(f1, f1, f2);
		mpf_mul(f2, mp1.n.c.r, mp2.n.c.i);
		mpf_clear(mp2.n.c.i);
		mpf_set(mp1.n.c.r, f1);
		mpf_clear(f1);
		mpf_mul(mp1.n.c.i, mp1.n.c.i, mp2.n.c.r);
		mpf_clear(mp2.n.c.r);
		mpf_add(mp1.n.c.i, mp1.n.c.i, f2);
		mpf_clear(f2);
#endif
		break;
	}
	default: PL_fail;
	}
	return mp_to_term(&mp1, product);
}

/*	mp_div(+N1, +N2, ?N3)
	N3 = N1/N2
	Note: N3 is rational when N1 and N2 are integer.
*/

static foreign_t mp_div(term_t dividend, term_t divisor, term_t quotient)
{	mp_t mp1, mp2;
	switch (mp_conformargs(dividend, divisor, &mp1, &mp2)) {
	case MPZ:	
	{	mpq_t q;
		mpq_init(q);
		mpq_set_num(q, mp1.n.z);
		mpq_set_den(q, mp2.n.z);
		mpq_canonicalize(q);
		if (mpz_cmp_ui(mpq_denref(q), (unsigned long)1) == 0)
		{	mp1.t = MPZ;
			mpq_get_num(mp1.n.z, q);
			mpq_clear(q);
		}
		else if (mpz_cmp_ui(mpq_denref(q), (unsigned long)0) == 0)
		{	mp1.t = MP0;
			mpz_clear(mp1.n.z);
			mpq_clear(q);
		}
		else
		{	mp1.t = MPQ;
			mpz_clear(mp1.n.z);
			mp1.n.q[0] = q[0];
		}
		mpz_clear(mp2.n.z);
		break;
	}
	case MPQ:
		mpq_div(mp1.n.q, mp1.n.q, mp2.n.q);
		mpq_clear(mp2.n.q);
		break;
	case MPF:
		mpf_div(mp1.n.f, mp1.n.f, mp2.n.f);
		mpf_clear(mp2.n.f);
		break;
	case MPC:
	{	mpf_t f1, f2;
		mpf_init(f1);
		mpf_init(f2);
		mpf_abs(f1, mp2.n.c.r);
		mpf_abs(f2, mp2.n.c.i);
		if (mpf_cmp(f1, f2) >= 0)
		{	mpf_div(f1, mp2.n.c.i, mp2.n.c.r);
			mpf_mul(mp2.n.c.i, mp2.n.c.i, f1);
			mpf_add(mp2.n.c.r, mp2.n.c.r, mp2.n.c.i);
			mpf_clear(mp2.n.c.i);
			mpf_mul(f2, mp1.n.c.i, f1);
			mpf_mul(f1, mp1.n.c.r, f1);
			mpf_add(mp1.n.c.r, mp1.n.c.r, f2);
			mpf_clear(f2);
			mpf_sub(mp1.n.c.i, mp1.n.c.i, f1);
			mpf_clear(f1);
			mpf_div(mp1.n.c.r, mp1.n.c.r, mp2.n.c.r);
			mpf_div(mp1.n.c.i, mp1.n.c.i, mp2.n.c.r);
			mpf_clear(mp2.n.c.r);
		}
		else
		{	mpf_div(f1, mp2.n.c.r, mp2.n.c.i);
			mpf_mul(mp2.n.c.r, mp2.n.c.r, f1);
			mpf_add(mp2.n.c.r, mp2.n.c.r, mp2.n.c.i);
			mpf_clear(mp2.n.c.i);
			mpf_mul(f2, mp1.n.c.r, f1);
			mpf_mul(f1, mp1.n.c.i, f1);
			mpf_add(mp1.n.c.i, f2, mp1.n.c.i);
			mpf_clear(f2);
			mpf_sub(mp1.n.c.r, f1, mp1.n.c.r);
			mpf_div(f1, mp1.n.c.r, mp2.n.c.r);
			mpf_div(mp1.n.c.r, mp1.n.c.i, mp2.n.c.r);
			mpf_clear(mp2.n.c.r);
			mpf_set(mp1.n.c.i, f1);
			mpf_clear(f1);
		}
		break;
	}
	default: PL_fail;
	}
	return mp_to_term(&mp1, quotient);
}

/*	mp_tdivrem(+Z1, +Z2, ?Z3, ?Z4)
	Z3 = truncate(Z1/Z2)
	Z4 = remainder(Z1/Z2)
*/

static foreign_t mp_tdivrem(term_t op1, term_t op2, term_t div, term_t rem)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_tdiv_qr(mp1.n.z, mp2.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	if (mp_to_term(&mp1, div) == TRUE)
		return mp_to_term(&mp2, rem);
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
}

/*	mp_fdivrem(+Z1, +Z2, ?Z3, ?Z4)
	Z3 = floor(Z1/Z2)
	Z4 = remainder(Z1/Z2) (rem of floor)
*/

static foreign_t mp_fdivrem(term_t op1, term_t op2, term_t div, term_t rem)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_fdiv_qr(mp1.n.z, mp2.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	if (mp_to_term(&mp1, div) == TRUE)
		return mp_to_term(&mp2, rem);
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
}

/*	mp_cdivrem(+Z1, +Z2, ?Z3, ?Z4)
	Z3 = ceil(Z1/Z2)
	Z4 = remainder(Z1/Z2) (rem of ceil)
*/

static foreign_t mp_cdivrem(term_t op1, term_t op2, term_t div, term_t rem)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_cdiv_qr(mp1.n.z, mp2.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	if (mp_to_term(&mp1, div) == TRUE)
		return mp_to_term(&mp2, rem);
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
}

/*	mp_mod(+Z1, +Z2, ?Z3)
	Z3 = Z1 mod Z2
*/

static foreign_t mp_mod(term_t op1, term_t op2, term_t mod)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_mod(mp1.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, mod);
}

/*	mp_lsh(+N1, +Z, ?N2)
	N2 = N1 << Z
	If N1 is rational, float N2 is returned
	If N1 is complex, real part is shifted
*/

static foreign_t mp_lsh(term_t op1, term_t op2, term_t lsh)
{	mp_t mp1, mp2;
	if (term_to_mp(op2, &mp2) != MPZ
	 || mpz_sgn(mp2.n.z) < 0
	 || mpz_cmp_si(mp2.n.z, mp_max_int) > 0)
	{	mp_clear(&mp2);
		PL_fail;
	}
	switch (term_to_mp(op1, &mp1)) {
	case MPZ:	
		mpz_mul_2exp(mp1.n.z, mp1.n.z, mpz_get_ui(mp2.n.z));
		break;
	case MPQ:
		mp_convert(&mp1, MPF);
	case MPF:
		mpf_mul_2exp(mp1.n.f, mp1.n.f, mpz_get_ui(mp2.n.z));
		break;
	case MPC:
		mpf_mul_2exp(mp1.n.c.r, mp1.n.c.r, mpz_get_ui(mp2.n.z));
		break;
	default: PL_fail;
	}
	mpz_clear(mp2.n.z);
	return mp_to_term(&mp1, lsh);
}

/*	mp_rsh(+N1, +Z, ?N2)
	N2 = N1 >> Z
	If N1 is rational, float N2 is returned
	If N1 is complex, real part is shifted
*/

static foreign_t mp_rsh(term_t op1, term_t op2, term_t rsh)
{	mp_t mp1, mp2;
	if (term_to_mp(op2, &mp2) != MPZ
	 || mpz_sgn(mp2.n.z) < 0
	 || mpz_cmp_si(mp2.n.z, mp_max_int) > 0)
	{	mp_clear(&mp2);
		PL_fail;
	}
	switch (term_to_mp(op1, &mp1)) {
	case MPZ:	
		mpz_div_2exp(mp1.n.z, mp1.n.z, mpz_get_ui(mp2.n.z));
		break;
	case MPQ:
		mp_convert(&mp1, MPF);
	case MPF:
		mpf_div_2exp(mp1.n.f, mp1.n.f, mpz_get_ui(mp2.n.z));
		break;
	case MPC:
		mpf_div_2exp(mp1.n.c.r, mp1.n.c.r, mpz_get_ui(mp2.n.z));
		break;
	default: PL_fail;
	}
	mpz_clear(mp2.n.z);
	return mp_to_term(&mp1, rsh);
}

/*	mp_cmp(+N1, +N2, ?Rel)
	Compare N1 with N2.
	Rel = (<) if N1 < N2
	Rel = (=) if N1 = N2
	Rel = (>) if N1 > N2
	For complex numbers, the real part is compared
*/

static foreign_t mp_cmp(term_t op1, term_t op2, term_t rel)
{	mp_t mp1, mp2;
	int n;
	switch (mp_conformargs(op1, op2, &mp1, &mp2)) {
	case MPZ:	
		n = mpz_cmp(mp1.n.z, mp2.n.z);
		mpz_clear(mp1.n.z);
		mpz_clear(mp2.n.z);
		break;
	case MPQ:
		n = mpq_cmp(mp1.n.q, mp2.n.q);
		mpq_clear(mp1.n.q);
		mpq_clear(mp2.n.q);
		break;
	case MPF:
		n = mpf_cmp(mp1.n.f, mp2.n.f);
		mpf_clear(mp1.n.f);
		mpf_clear(mp2.n.f);
		break;
	case MPC:
		n = mpf_cmp(mp1.n.c.r, mp2.n.c.r);
		mpf_clear(mp1.n.c.r);
		mpf_clear(mp1.n.c.i);
		mpf_clear(mp2.n.c.r);
		mpf_clear(mp2.n.c.i);
		break;
	default: PL_fail;
	}
	return PL_unify_atom(rel, relatom[relidx(n)]);
}

/*	mp_neg(+N1, ?N2)
	N2 = -N1
*/

static foreign_t mp_neg(term_t op, term_t neg)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		mpz_neg(mp.n.z, mp.n.z);
		break;
	case MPQ:
		mpz_neg(mpq_numref(mp.n.q), mpq_numref(mp.n.q));
		break;
	case MPF:
		mpf_neg(mp.n.f, mp.n.f);
		break;
	case MPC:
		mpf_neg(mp.n.c.r, mp.n.c.r);
		break;
	default: PL_fail;
	}
	return mp_to_term(&mp, neg);
}

/*	mp_inv(+N1, ?N2)
	N2 = 1/N1
*/

static foreign_t mp_inv(term_t op, term_t inv)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		mp_convert(&mp, MPQ);
	case MPQ:
		if (mpz_cmp_ui(mpq_numref(mp.n.q), (unsigned long)0) == 0)
		{	mp.t = MP0;
			mpq_clear(mp.n.q);
		}
		else
		{	mpq_inv(mp.n.q, mp.n.q);
			mpq_normalize(&mp);
		}
		break;
	case MPF:
		mpf_ui_div(mp.n.f, (unsigned long)1, mp.n.f);
		break;
	case MPC:
	{	mpf_t f1, f2;
		mpf_init(f1);
		mpf_init(f2);
		mpf_mul(f1, mp.n.c.r, mp.n.c.r);
		mpf_mul(f2, mp.n.c.i, mp.n.c.i);
		mpf_add(f1, f1, f2);
		mpf_clear(f2);
		mpf_div(mp.n.c.r, mp.n.c.r, f1);
		mpf_neg(mp.n.c.i, mp.n.c.i);
		mpf_div(mp.n.c.i, mp.n.c.i, f1);
		mpf_clear(f1);
		break;
	}
	default: PL_fail;
	}
	return mp_to_term(&mp, inv);
}

/*	mp_abs(+N1, ?N2)
	N2 = |N1|
	If N1 is complex, N2 is the magnitude of N1
*/

static foreign_t mp_abs(term_t op, term_t abs)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		mpz_abs(mp.n.z, mp.n.z);
		break;
	case MPQ:
		mpz_abs(mpq_numref(mp.n.q), mpq_numref(mp.n.q));
		break;
	case MPF:
		mpf_abs(mp.n.f, mp.n.f);
		break;
	case MPC:
	{	mpf_t f1, f2;
		mpf_init(f1);
		mpf_init(f2);
		mpf_abs(f1, mp.n.c.r);
		mpf_abs(f2, mp.n.c.i);
		if (mpf_cmp(f1, f2) >= 0)
		{	mpf_clear(f2);
			mpf_div(mp.n.c.r, mp.n.c.i, mp.n.c.r);
			mpf_mul(mp.n.c.r, mp.n.c.r, mp.n.c.r);
			mpf_add_ui(mp.n.c.r, mp.n.c.r, (unsigned long)1);
			mpf_sqrt(mp.n.c.r, mp.n.c.r);
			mpf_mul(mp.n.c.r, mp.n.c.r, f1);
			mpf_clear(f1);
		}
		else
		{	mpf_clear(f1);
			mpf_div(mp.n.c.r, mp.n.c.r, mp.n.c.i);
			mpf_mul(mp.n.c.r, mp.n.c.r, mp.n.c.r);
			mpf_add_ui(mp.n.c.r, mp.n.c.r, (unsigned long)1);
			mpf_sqrt(mp.n.c.r, mp.n.c.r);
			mpf_mul(mp.n.c.r, mp.n.c.r, f2);
			mpf_clear(f2);
		}
		mp_convert(&mp, MPF);
		break;
	}
	default: PL_fail;
	}
	return mp_to_term(&mp, abs);
}

/*	mp_sgn(+N1, ?Sign)
	Sign = (<) if N1 < 0
	Sign = (=) if N1 = 0
	Sign = (>) if N1 > 0
	If N1 is complex, the sign of the real part is returned
*/

static foreign_t mp_sgn(term_t op, term_t sgn)
{	mp_t mp;
	int n;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		n = mpz_sgn(mp.n.z);
		mpz_clear(mp.n.z);
		break;
	case MPQ:
		n = mpq_sgn(mp.n.q);
		mpq_clear(mp.n.q);
		break;
	case MPF:
		n = mpf_sgn(mp.n.f);
		mpf_clear(mp.n.f);
		break;
	case MPC:
		n = mpf_sgn(mp.n.c.r);
		mpf_clear(mp.n.c.r);
		mpf_clear(mp.n.c.i);
		break;
	default: PL_fail;
	}
	return PL_unify_atom(sgn, relatom[relidx(n)]);
}

/*	mp_sqrt(+N1, ?N2)
	N2 = sqrt(N1)
	If N1 is rational, N2 is float
*/

static foreign_t mp_sqrt(term_t op, term_t sqrt)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
	case MPQ:
		mp_convert(&mp, MPF);
	case MPF:
		if (mpf_sgn(mp.n.f) >= 0)
			mpf_sqrt(mp.n.f, mp.n.f);
		else
		{	mpf_neg(mp.n.f, mp.n.f);
			mpf_sqrt(mp.n.f, mp.n.f);
			mp_convert(&mp, MPC);
			mpf_set_ui(mp.n.c.i, (unsigned long)1);
		}
		break;
	case MPC:
	{	mpf_t f1, f2, f3;
		if (mpf_sgn(mp.n.c.r) == 0 && mpf_sgn(mp.n.c.i) == 0)
		{	mp_convert(&mp, MPF);
			break;
		}
		mpf_init(f1);
		mpf_init(f2);
		mpf_init(f3);
		mpf_abs(f1, mp.n.c.r);
		mpf_abs(f2, mp.n.c.i);
		if (mpf_cmp(f1, f2) >= 0)
		{	mpf_sqrt(f3, f1);
			mpf_div(f1, mp.n.c.i, mp.n.c.r);
			mpf_mul(f1, f1, f1);
			mpf_add_ui(f1, f1, (unsigned long)1);
			mpf_sqrt(f1, f1);
			mpf_add_ui(f1, f1, (unsigned long)1);
		}
		else
		{	mpf_div(f1, mp.n.c.r, mp.n.c.i);
			mpf_abs(f3, f1);
			mpf_mul(f1, f1, f1);
			mpf_add_ui(f1, f1, (unsigned long)1);
			mpf_sqrt(f1, f1);
			mpf_add(f1, f1, f3);
			mpf_sqrt(f3, f2);
		}
		mpf_div_2exp(f1, f1, (unsigned long)1);
		mpf_sqrt(f1, f1);
		mpf_mul(f1, f1, f3);
		mpf_clear(f3);
		if (mpf_sgn(f1) == 0)
		{	mpf_set(mp.n.c.r, f1);
			mpf_clear(f1);
			mpf_clear(f2);
			mp_convert(&mp, MPF);
		}
		else if (mpf_sgn(mp.n.c.r) >= 0)
		{	mpf_set(mp.n.c.r, f1);
			mpf_clear(f1);
			mpf_clear(f2);
			mpf_div(mp.n.c.i, mp.n.c.i, mp.n.c.r);
			mpf_div_2exp(mp.n.c.i, mp.n.c.i, (unsigned long)1);
		}
		else if (mpf_sgn(mp.n.c.i) >= 0)
		{	mpf_set(mp.n.c.i, f1);
			mpf_set(mp.n.c.r, f2);
			mpf_clear(f1);
			mpf_clear(f2);
			mpf_div(mp.n.c.r, mp.n.c.r, mp.n.c.i);
			mpf_div_2exp(mp.n.c.r, mp.n.c.r, (unsigned long)1);
		}
		else
		{	mpf_set(mp.n.c.i, f1);
			mpf_set(mp.n.c.r, f2);
			mpf_clear(f1);
			mpf_clear(f2);
			mpf_div(mp.n.c.r, mp.n.c.r, mp.n.c.i);
			mpf_div_2exp(mp.n.c.r, mp.n.c.r, (unsigned long)1);
			mpf_neg(mp.n.c.i, mp.n.c.i);
		}
		break;
	}
	default: PL_fail;
	}
	return mp_to_term(&mp, sqrt);
}

/*	mp_pow(+Z1, +Z2, ?Z3)
	Integer power operation, where the exponent Z2 > 0
	Z3 = Z1^Z2
*/

static foreign_t mp_pow(term_t op1, term_t op2, term_t pow)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ
		 && mpz_sgn(mp2.n.z) >= 0
		 && mpz_cmp_si(mp2.n.z, mp_max_int) <= 0)
			mpz_pow_ui(mp1.n.z, mp1.n.z, mpz_get_ui(mp2.n.z));
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, pow);
}

/*	mp_gcd(+Z1, +Z2, ?Z3)
	Z3 = gcd(Z1, Z2)
*/

static foreign_t mp_gcd(term_t op1, term_t op2, term_t gcd)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_gcd(mp1.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, gcd);
}

/*	mp_bin(+Z1, +Z2, ?Z3)
	Z3 = binomial(Z1, Z2)
*/

static foreign_t mp_bin(term_t op1, term_t op2, term_t bin)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ
		 && mpz_sgn(mp2.n.z) >= 0
		 && mpz_cmp_si(mp2.n.z, mp_max_int) <= 0)
			mpz_bin_ui(mp1.n.z, mp1.n.z, mpz_get_ui(mp2.n.z));
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, bin);
}

/*	mp_fac(+Z1, ?Z2)
	Z2 = Z1!
*/

static foreign_t mp_fac(term_t op, term_t fac)
{	mp_t mp;
	if (term_to_mp(op, &mp) == MPZ
	 && mpz_sgn(mp.n.z) >= 0
	 && mpz_cmp_si(mp.n.z, mp_max_int) <= 0)
		mpz_fac_ui(mp.n.z, mpz_get_ui(mp.n.z));
	else
	{	mp_clear(&mp);
		PL_fail;
	}
	return mp_to_term(&mp, fac);
}

/*	mp_fib(+Z1, ?Z2)
	Z2 = fibonacci(Z1)
*/

static foreign_t mp_fib(term_t op, term_t fib)
{	mp_t mp;
	if (term_to_mp(op, &mp) == MPZ
	 && mpz_sgn(mp.n.z) >= 0
	 && mpz_cmp_si(mp.n.z, mp_max_int) <= 0)
		mpz_fib_ui(mp.n.z, mpz_get_ui(mp.n.z));
	else
	{	mp_clear(&mp);
		PL_fail;
	}
	return mp_to_term(&mp, fib);
}

/*	mp_floor(+N, ?Z)
	Z = floor(N)
*/

static foreign_t mp_floor(term_t op, term_t floor)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		return PL_unify(op, floor);
	case MPQ:
		mp_q2f(&mp);
	case MPF:
		break;
	default:
	{	mp_clear(&mp);
		PL_fail;
	}
	}
	mpf_floor(mp.n.f, mp.n.f);
	mp_f2z(&mp);
	return mp_to_term(&mp, floor);
}

/*	mp_ceil(+N, ?Z)
	Z = ceil(N)
*/

static foreign_t mp_ceil(term_t op, term_t ceil)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		return PL_unify(op, ceil);
	case MPQ:
		mp_q2f(&mp);
	case MPF:
		break;
	default:
	{	mp_clear(&mp);
		PL_fail;
	}
	}
	mpf_ceil(mp.n.f, mp.n.f);
	mp_f2z(&mp);
	return mp_to_term(&mp, ceil);
}

/*	mp_trunc(+N, ?Z)
	Z = trunc(N)
*/

static foreign_t mp_trunc(term_t op, term_t trunc)
{	mp_t mp;
	switch (term_to_mp(op, &mp)) {
	case MPZ:
		return PL_unify(op, trunc);
	case MPQ:
		mp_q2f(&mp);
	case MPF:
		break;
	default:
	{	mp_clear(&mp);
		PL_fail;
	}
	}
	mp_f2z(&mp);
	return mp_to_term(&mp, trunc);
}

/*	mp_jacobi(+Z1, +Z2, ?Z3)
	Z3 = J(Z1, Z2)
*/

static foreign_t mp_jacobi(term_t op1, term_t op2, term_t jacobi)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ
	 	 && mpz_sgn(mp1.n.z) > 0
		 && mpz_sgn(mp2.n.z) > 0)
			mpz_set_si(mp1.n.z, (long)mpz_jacobi(mp1.n.z, mp2.n.z));
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	mpz_clear(mp2.n.z);
	return mp_to_term(&mp1, jacobi);
}

/*	mp_legendre(+Z1, +Z2, ?Z3)
	Z3 = Legendre function of Z1 and Z2
*/

static foreign_t mp_legendre(term_t op1, term_t op2, term_t legendre)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ
	 	 && mpz_sgn(mp1.n.z) > 0
		 && mpz_sgn(mp2.n.z) > 0)
			mpz_set_si(mp1.n.z, (long)mpz_legendre(mp1.n.z, mp2.n.z));
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	mpz_clear(mp2.n.z);
	return mp_to_term(&mp1, legendre);
}

/*	mp_reldiff(+F1, +F2, ?F3)
	F3 = relative difference between F1 and F2
*/

static foreign_t mp_reldiff(term_t op1, term_t op2, term_t reldiff)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPF)
	{	if (term_to_mp(op2, &mp2) == MPF)
			mpf_reldiff(mp1.n.f, mp1.n.f, mp2.n.f);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, reldiff);
}

/*	mp_and(+Z1, +Z2, ?Z3)
	Z3 = bitwise-and of Z1 and Z2
*/

static foreign_t mp_and(term_t op1, term_t op2, term_t and)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_and(mp1.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, and);
}

/*	mp_or(+Z1, +Z2, ?Z3)
	Z3 = bitwise-or of Z1 and Z2
*/

static foreign_t mp_or(term_t op1, term_t op2, term_t or)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_ior(mp1.n.z, mp1.n.z, mp2.n.z);
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, or);
}

/*	mp_not(+Z1, ?Z2)
	Z2 = bitwise complement of Z1
*/

static foreign_t mp_not(term_t op, term_t not)
{	mp_t mp;
	if (term_to_mp(op, &mp) == MPZ)
		mpz_com(mp.n.z, mp.n.z);
	else
	{	mp_clear(&mp);
		PL_fail;
	}
	return mp_to_term(&mp, not);
}

/*	mp_setbit(+Z1, +Z2, ?Z3)
	Set bit Z2 of Z1 and return Z3
*/

static foreign_t mp_setbit(term_t op1, term_t op2, term_t op)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_setbit(mp1.n.z, mpz_get_ui(mp2.n.z));
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, op);
}

/*	mp_clrbit(+Z1, +Z2, ?Z3)
	Clear bit Z2 of Z1 and return Z3
*/

static foreign_t mp_clrbit(term_t op1, term_t op2, term_t op)
{	mp_t mp1, mp2;
	if (term_to_mp(op1, &mp1) == MPZ)
	{	if (term_to_mp(op2, &mp2) == MPZ)
			mpz_clrbit(mp1.n.z, mpz_get_ui(mp2.n.z));
		else
		{	mp_clear(&mp1);
			mp_clear(&mp2);
			PL_fail;
		}
	}
	else
	{	mp_clear(&mp1);
		PL_fail;
	}
	return mp_to_term(&mp1, op);
}

/*	mp_term(+N, +Base, +NumDigits, ?Term)
	Convert N to a readable Prolog term Term using base Base (2<=Base<=36)
	and max number of digits NumDigits. If NumDigits = 0, floating point
	and complex numbers are converted with the maximum number of digits
	allowed by the precision of the number.
*/

static foreign_t mp_term(term_t mp_term, term_t base, term_t numdigits, term_t term)
{	term_t new_term = PL_new_term_ref();
	mp_t mp;
	int b, n;
	if (!PL_get_integer(base, &b) || !PL_get_integer(numdigits, &n))
		return PL_warning("MP: integer base and numdigits expected");
	switch (term_to_mp(mp_term, &mp)) {
	case MPZ:
		if (b == 10
		 && n == 0
		 && mpz_cmp_si(mp.n.z, mp_min_int) >= 0
		 && mpz_cmp_si(mp.n.z, mp_max_int) <= 0)
			PL_put_integer(new_term, mpz_get_si(mp.n.z));
		else
		{	if (mpz_sizeinbase(mp.n.z, b)+2>BUFSIZE)
				return PL_warning("MP: integer representation too large");
			mpz_get_str(buf, b, mp.n.z);
			PL_put_string_chars(new_term, buf);
		}
		mpz_clear(mp.n.z);
		break;
	case MPQ:
	{	term_t num = PL_new_term_ref();
		term_t den = PL_new_term_ref();
		mpz_ptr mpnum = mpq_numref(mp.n.q);
		mpz_ptr mpden = mpq_denref(mp.n.q);
		if (b == 10
		 && n == 0
		 && mpz_cmp_si(mpnum, mp_min_int) >= 0
		 && mpz_cmp_si(mpnum, mp_max_int) <= 0
		 && mpz_cmp_si(mpden, mp_min_int) >= 0
		 && mpz_cmp_si(mpden, mp_max_int) <= 0)
		{	PL_put_integer(num, mpz_get_si(mpnum));
			PL_put_integer(den, mpz_get_si(mpden));
		}
		else
		{	if (mpz_sizeinbase(mpnum, b)+2>BUFSIZE
			 || mpz_sizeinbase(mpden, b)+2>BUFSIZE)
				return PL_warning("MP: rational representation too large");
			mpz_get_str(buf, b, mpnum);
			PL_put_string_chars(num, buf);
			mpz_get_str(buf, b, mpden);
			PL_put_string_chars(den, buf);
		}
		PL_cons_functor(new_term, mpr, num, den);
		mpq_clear(mp.n.q);
		break;
	}
	case MPF:
		mpf_to_buf(mp.n.f, b, n);
		PL_put_string_chars(new_term, buf);
		mpf_clear(mp.n.f);
		break;
	case MPC:
	{	term_t re = PL_new_term_ref();
		term_t im = PL_new_term_ref();
		if (mpf_to_buf(mp.n.c.r, b, n))
			return PL_warning("MP: real part of complex representation too large");
		PL_put_string_chars(re, buf);
		if (mpf_to_buf(mp.n.c.i, b, n))
			return PL_warning("MP: imaginary part of complex representation too large");
		PL_put_string_chars(im, buf);
		PL_cons_functor(new_term, mpx, re, im);
		mpf_clear(mp.n.c.r);
		mpf_clear(mp.n.c.i);
		break;
	}
	default: PL_fail;
	}
	return PL_unify(new_term, term);
}

		/*******************************
		*       MP INSTALLATION        *
		*******************************/

PL_extension mp_predicates[] =
{
/*{ "name",      arity, function,       PL_FA_<flags> },*/
  { "mp_setprec", 1,	mp_setprec,	0 },
  { "mp_getprec", 2,	mp_getprec,	0 },
  { "mp_n",	  2,	mp_n,		0 },
  { "mp_z",	  2,	mp_z,		0 },
  { "mp_q",	  2,	mp_q,		0 },
  { "mp_f",	  2,	mp_f,		0 },
  { "mp_c",	  2,	mp_c,		0 },
  { "mp_add",	  3,	mp_add,		0 },
  { "mp_sub",	  3,	mp_sub,		0 },
  { "mp_mul",	  3,	mp_mul,		0 },
  { "mp_div",	  3,	mp_div,		0 },
  { "mp_tdivrem", 4,	mp_tdivrem,	0 },
  { "mp_fdivrem", 4,	mp_fdivrem,	0 },
  { "mp_cdivrem", 4,	mp_cdivrem,	0 },
  { "mp_mod",	  3,	mp_mod,		0 },
  { "mp_lsh",	  3,	mp_lsh,		0 },
  { "mp_rsh",	  3,	mp_rsh,		0 },
  { "mp_cmp",	  3,	mp_cmp,		0 },
  { "mp_neg",	  2,	mp_neg,		0 },
  { "mp_inv",	  2,	mp_inv,		0 },
  { "mp_abs",	  2,	mp_abs,		0 },
  { "mp_sgn",	  2,	mp_sgn,		0 },
  { "mp_sqrt",	  2,	mp_sqrt,	0 },
  { "mp_pow",	  3,	mp_pow,		0 },
  { "mp_gcd",	  3,	mp_gcd,		0 },
  { "mp_bin",	  3,	mp_bin,		0 },
  { "mp_fac",	  2,	mp_fac,		0 },
  { "mp_fib",	  2,	mp_fib,		0 },
  { "mp_floor",	  2,	mp_floor,	0 },
  { "mp_ceil",	  2,	mp_ceil,	0 },
  { "mp_trunc",	  2,	mp_trunc,	0 },
  { "mp_jacobi",  3,	mp_jacobi,	0 },
  { "mp_legendre",3,	mp_legendre,	0 },
  { "mp_reldiff", 3,	mp_reldiff,	0 },
  { "mp_and",	  3,	mp_and,		0 },
  { "mp_or",	  3,	mp_or,		0 },
  { "mp_not",	  2,	mp_not,		0 },
  { "mp_setbit",  3,	mp_setbit,	0 },
  { "mp_clrbit",  3,	mp_clrbit,	0 },
  { "mp_term",	  4,	mp_term,	0 },
  { NULL,         0,	NULL,		0 }     /* terminating line */
};


void mp_install()
{	PL_register_extensions(mp_predicates);

	mpz = PL_new_functor(PL_new_atom("$mpz"), 1);		/* invisible GMP integer */
	mpq = PL_new_functor(PL_new_atom("$mpq"), 2);		/* invisible GMP rational */
	mpf = PL_new_functor(PL_new_atom("$mpf"), 1);		/* invisible GMP float */
	mpc = PL_new_functor(PL_new_atom("$mpc"), 2);		/* invisible complex (added to GMP) */
	mpr = PL_new_functor(PL_new_atom("/"), 2);		/* visible rational */
	mpx = PL_new_functor(PL_new_atom("complex"), 2);	/* visible complex */
	mpn = PL_new_functor(PL_new_atom("-"), 1);		/* visible minus sign */
	mp0 = PL_new_atom("$mp0");	/* 0/0 indeterminate rational */
	relatom[0] = PL_new_atom("<");
	relatom[1] = PL_new_atom("=");
	relatom[2] = PL_new_atom(">");
	mp_min_int = PL_query(PL_QUERY_MIN_INTEGER);
	mp_max_int = PL_query(PL_QUERY_MAX_INTEGER);
}
