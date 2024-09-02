/*
 * C utilities
 *
 * Copyright (c) 2017 Fabrice Bellard
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#ifndef _CUTILS_H
#define _CUTILS_H

#include <inttypes.h>
#include <stdbool.h>

#ifdef _MSC_VER
#include <intrin.h>
typedef intptr_t ssize_t;
#define likely(x) (!!(x))
#define unlikely(x) (!!(x))
#define force_inline __forceinline
#define no_inline
#define __maybe_unused
#define __builtin_clz(v)   _lzcnt_u32(v)
#define __builtin_clzll(v) _lzcnt_u64(v)
#define __builtin_ctz(v)   _tzcnt_u32(v)
#define __builtin_ctzll(v) _tzcnt_u64(v)
#define PACK( __Declaration__ ) \
	__pragma( pack(push, 1) ) struct __Declaration__ __pragma( pack(pop))

/* two-complement.  MSVC warns that -u is still unsgined */
#define UNEG(i) _Generic((i), \
			 limb_t: (limb_t)(-(slimb_t)(i)), \
			 dlimb_t: (dlimb_t)(-(sdlimb_t)(i)))

#pragma intrinsic(_BitScanForward)
static __forceinline int ffsl(long x)
{ unsigned long i;

  if (_BitScanForward(&i, x))
    return (i + 1);
  return (0);
}

static __forceinline int ffs(int x)
{ return (ffsl(x));
}

#pragma intrinsic(_BitScanForward64)

static __forceinline int ffsll(unsigned __int64 x)
{ unsigned long i;

  if (_BitScanForward64(&i, x))
    return (i + 1);
  return (0);
}

#define __builtin_ffs(i)   ffs(i)
#define __builtin_ffsll(i) ffsll(i)

#pragma intrinsic (__mulh)

static int __forceinline
__builtin_mul_overflow(uint64_t l, uint64_t r, uint64_t *rc)
{ *rc = l*r;
  return __umulh(l,r) == 0;
}

#else /*GCC,Clang*/

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#define force_inline inline __attribute__((always_inline))
#define no_inline __attribute__((noinline))
#define __maybe_unused __attribute__((unused))
#define PACK( __Declaration__ ) \
	struct __attribute__((__packed__)) __Declaration__
#define UNEG(i) (-(i))

#endif /*_MSC_VER*/

#define xglue(x, y) x ## y
#define glue(x, y) xglue(x, y)
#define stringify(s)    tostring(s)
#define tostring(s)     #s

#ifndef offsetof
#define offsetof(type, field) ((size_t) &((type *)0)->field)
#endif
#define countof(x) (sizeof(x) / sizeof(x[0]))

void pstrcpy(char *buf, ssize_t buf_size, const char *str);
char *pstrcat(char *buf, ssize_t buf_size, const char *s);
int strstart(const char *str, const char *val, const char **ptr);

static inline int max_int(int a, int b)
{
    if (a > b)
        return a;
    else
        return b;
}

static inline int min_int(int a, int b)
{
    if (a < b)
        return a;
    else
        return b;
}

/* WARNING: undefined if a = 0 */
static inline int clz32(unsigned int a)
{
    return __builtin_clz(a);
}

/* WARNING: undefined if a = 0 */
static inline int clz64(uint64_t a)
{
    return (int)__builtin_clzll(a);
}

/* WARNING: undefined if a = 0 */
static inline int ctz32(unsigned int a)
{
    return __builtin_ctz(a);
}

/* WARNING: undefined if a = 0 */
static inline int ctz64(uint64_t a)
{
    return (int)__builtin_ctzll(a);
}

PACK(packed_u32) {
    uint32_t v;
};

PACK(packed_u16) {
    uint16_t v;
};

static inline uint32_t get_u32(const uint8_t *tab)
{
    return ((struct packed_u32 *)tab)->v;
}

static inline void put_u32(uint8_t *tab, uint32_t val)
{
    ((struct packed_u32 *)tab)->v = val;
}

static inline uint32_t get_u16(const uint8_t *tab)
{
    return ((struct packed_u16 *)tab)->v;
}

static inline void put_u16(uint8_t *tab, uint16_t val)
{
    ((struct packed_u16 *)tab)->v = val;
}

typedef void *DynBufReallocFunc(void *opaque, void *ptr, size_t size);

typedef struct {
    uint8_t *buf;
    size_t size;
    size_t allocated_size;
    bool error; /* true if a memory allocation error occurred */
    DynBufReallocFunc *realloc_func;
    void *opaque; /* for realloc_func */
} DynBuf;

void dbuf_init(DynBuf *s);
void dbuf_init2(DynBuf *s, void *opaque, DynBufReallocFunc *realloc_func);
int dbuf_realloc(DynBuf *s, size_t new_size);
int dbuf_write(DynBuf *s, size_t offset, const uint8_t *data, size_t len);
int dbuf_put(DynBuf *s, const uint8_t *data, size_t len);
int dbuf_putc(DynBuf *s, uint8_t c);
int dbuf_putstr(DynBuf *s, const char *str);
static inline int dbuf_put_u32(DynBuf *s, uint32_t val)
{
    return dbuf_put(s, (uint8_t *)&val, 4);
}
static inline int dbuf_put_u16(DynBuf *s, uint16_t val)
{
    return dbuf_put(s, (uint8_t *)&val, 2);
}

int
#ifndef _MSC_VER
__attribute__((format(printf, 2, 3)))
#endif
dbuf_printf(DynBuf *s, const char *fmt, ...);

void dbuf_free(DynBuf *s);
static inline bool dbuf_error(DynBuf *s) {
    return s->error;
}

#endif
