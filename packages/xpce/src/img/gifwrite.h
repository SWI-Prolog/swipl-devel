/*=======================================================================
  FILE:         gifwrite.h
  Author:       Dr. Sung, Kah-Kay
                Dept. of Information Systems & Computer Science
                National University of Singapore
  Description:  Stand alone routines for writing GIF87 image files.

  Modified:	Use XPCE/SWI-Prolog IOSTREAM instead of FILE (Jan Wielemaker)
 =======================================================================*/

#define O_IOSTREAM 1			/* use IOSTREAM rather then FILE */
#include <h/stream.h>

int gifwrite_grey(IOSTREAM *fp, unsigned char *image, long w, long h);

int gifwrite_rgb(IOSTREAM *fp,
		 unsigned char *rgbimage, unsigned char *mask,
		 long w, long h);

int gifwrite_colmap(IOSTREAM *fp, unsigned char *image, long w, long h,
		     unsigned char *red_map, unsigned char *green_map,
		     unsigned char *blue_map);

