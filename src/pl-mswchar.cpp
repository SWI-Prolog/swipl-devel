#include <wchar.h>

extern "C" size_t
ms_wcrtomb(char *s, wchar_t wc, mbstate_t *ps)
{ return wcrtomb(s, wc, ps);
}

extern "C" size_t
ms_mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps)
{ return mbrtowc(pwc, s, n, ps);
}
