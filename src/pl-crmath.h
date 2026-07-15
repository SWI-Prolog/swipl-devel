/*
 *	Required subset from crmath repo. (License??)
 *  Files not in subset are commented out.
 */
 
#pragma once

#ifdef __cplusplus
extern "C" {
#endif

double cr_acos(double x);
double cr_acosh(double x);
//double cr_acospi(double x);
double cr_asin(double x);
double cr_asinh(double x);
//double cr_asinpi(double x);
double cr_atan(double x);
double cr_atan2(double y0, double x0);
//double cr_atan2pi(double y, double x);
double cr_atanh(double x);
//double cr_atanpi(double x);
//double cr_cbrt(double x);
double cr_cos(double x);
double cr_cosh(double x);
//double cr_cospi(double x);
double cr_erf(double x);
double cr_erfc(double x);
double cr_exp(double x);
//double cr_exp10(double x);
//double cr_exp10m1(double x);
//double cr_exp2(double x);
//double cr_exp2m1(double x);
//double cr_expm1(double x);
//double cr_hypot(double x, double y);
double cr_log(double x);
double cr_log10(double x);
//double cr_log10p1(double x);
//double cr_log1p(double x);
//double cr_log2(double x);
//double cr_log2p1(double x);
double cr_pow(double x, double y);
//double cr_rsqrt(double x);
double cr_sin(double x);
//double cr_sincos(double x, double* s, double* c);
double cr_sinh(double x);
//double cr_sinpi(double x);
double cr_tan(double x);
double cr_tanh(double x);
//double cr_tanpi(double x);
//double cr_tgamma(double x);

#ifdef __cplusplus
}
#endif
