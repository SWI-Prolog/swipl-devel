#include <wchar.h>

#ifdef WIN32
#define wcrtomb(s, wc, ps)	ms_wcrtomb(s, wc, ps)
#define mbrtowc(pwc, s, n, ps)	ms_mbrtowc(pwc, s, n, ps)

extern size_t ms_wcrtomb(char *s, wchar_t wc, mbstate_t *ps);
extern size_t ms_mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps);
#endif
