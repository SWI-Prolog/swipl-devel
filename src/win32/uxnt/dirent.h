/* dirent.h: reading directories a la Unix
*/

#ifndef _DIRENT_H_INCLUDED
#define _DIRENT_H_INCLUDED

#include <io.h>

#undef _export
#if defined(_UXNT_KERNEL) && !defined(__LCC__)
#define _export _declspec(dllexport)
#else
#define _export extern
#endif

#ifndef NAME_MAX
#define NAME_MAX 255
#endif

typedef struct dirent
{ void *      		data;		/* actually WIN32_FIND_DATA * */
  int			first;
  void *		handle;		/* actually HANDLE */
  void *		map;		/* win32s long filename mapping */
					/* dirent */
  char			d_name[NAME_MAX+1];
} DIR;

_export DIR *		opendir(const char *path);
_export int		closedir(DIR *dp);
_export struct dirent *	readdir(DIR *dp);

#endif /*_DIRENT_H_INCLUDED*/
