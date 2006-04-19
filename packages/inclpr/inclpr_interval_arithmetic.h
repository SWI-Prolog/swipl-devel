/*  
    
    Part of INCLP(R)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, K.U. Leuven

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

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

struct i{double l; double r;};
//
double fmin(double d1, double d2);
double fmax(double d1, double d2);
double trunc(double d1);

static foreign_t pl_ia_sum(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_sum_2(term_t t1l, term_t t1r, term_t t2l, term_t t2r,
    term_t t3l, term_t t3r);
static foreign_t pl_ia_difference(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_difference_2(term_t t1l, term_t t1r, term_t t2l, term_t t2r,
    term_t t3l, term_t t3r);
static foreign_t pl_ia_product(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_product_2(term_t t1l, term_t t1r, term_t t2l, term_t t2r,
    term_t t3l, term_t t3r);
static foreign_t pl_ia_quotient(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_quotient_2(term_t t1l, term_t t1r, term_t t2l, term_t t2r,
    term_t t3l, term_t t3r);
static foreign_t pl_ia_additive_inverse(term_t t1, term_t t2);
static foreign_t pl_ia_additive_inverse_2(term_t t1l, term_t t1r, term_t t2l,
    term_t t2r);
static foreign_t pl_ia_power(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_power_2(term_t t1l, term_t t1r, term_t t2, term_t t3l,
    term_t t3r);
static foreign_t pl_ia_root(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_root_2(term_t t1l, term_t t1r, term_t t2, term_t t3l,
    term_t t3r);
static foreign_t pl_ia_slope_power_2(term_t tuxl, term_t tuxr, term_t tucl,
    term_t tucr, term_t tk, term_t tsl, term_t tsr);

//
static foreign_t pl_ia_intersection(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_intersection_2(term_t t1l, term_t t1r, term_t t2l, term_t t2r,
    term_t t3l, term_t t3r);
static foreign_t pl_ia_union(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_union_2(term_t t1l, term_t t1r, term_t t2l, term_t t2r,
    term_t t3l, term_t t3r);
//
static foreign_t pl_ia_split(term_t t1, term_t t2, term_t t3, term_t t4);
static foreign_t pl_ia_split_excluding_zero(term_t t1, term_t t2, term_t t3);
static foreign_t pl_ia_exclude_zero_bound(term_t t1, term_t t2);
static foreign_t pl_ia_center(term_t t1, term_t t2);
static foreign_t pl_ia_class(term_t t1, term_t t2);
static foreign_t pl_ia_contains(term_t t1, term_t t2);
static foreign_t pl_ia_negative_part(term_t t1, term_t t2);
static foreign_t pl_ia_positive_part(term_t t1, term_t t2);
static foreign_t pl_ia_quadratic_inverse(term_t a, term_t b, term_t c, term_t r);
//

//
static void p_to_c(term_t t, struct i* i);
static void c_to_p(struct i i, term_t t);
static void two_c_to_p(struct i i1, struct i i2, term_t t);
//
static int pos_int_canonical_interval(struct i i1);
static void print_interval(struct i i);
static struct i ia_sum(struct i i1, struct i i2);
static struct i ia_difference(struct i i1, struct i i2);
static struct i ia_product(struct i i1, struct i i2);
static struct i ia_quotient(struct i i1, struct i i2);
static struct i ia_additive_inverse(struct i i1);
static struct i ia_power(struct i i1, int n);
static int ia_root(struct i i1, int n, struct i* i2, struct i* i3);
static struct i ia_intersection(struct i i1, struct i i2);
static struct i ia_union(struct i i1, struct i i2);
static struct i ia_center(struct i i1);
static int ia_quadratic_inverse(struct i a, struct i b, struct i c, struct i* r1,
    struct i* r2);
static struct i num_to_int(double num);
static struct i ia_discriminant(struct i a, struct i b, struct i c);
static void ia_split(struct i i1, double factor, struct i* i3, struct i* i4);
static void ia_split_excluding_zero(struct i i1, struct i* i2, struct i* i3);

static char ia_class(struct i i);
static int contains(struct i i, double d);
static int valid_interval(struct i i);
static int finite_interval(struct i i);

//
install_t install_inclpr_interval_arithmetic();

