/*
 * Copyright (c) 1994, 1995, SimSoft
 */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#ifndef FAR
#define FAR
#endif
#endif

HBITMAP CALLBACK ReadFileIntoDDB (LPSTR, HPALETTE FAR *, BOOL);
LPVOID CALLBACK ReadFileIntoDIB (LPSTR);
long CALLBACK GetLastImgLibError (void);
LPVOID CALLBACK WinGHalftoneDIB (LPVOID, HPALETTE FAR *);
void CALLBACK DIBFree (LPVOID);
HPALETTE CALLBACK CreateDIBPalette (LPVOID);
LPVOID CALLBACK DDBToDIB (HBITMAP, HPALETTE);
HBITMAP CALLBACK DIBToDDB (LPVOID, HPALETTE FAR *);
LPVOID CALLBACK BrightenDIB (LPVOID, short);
LPVOID CALLBACK ReduceDIB (LPVOID, long, BOOL);
LPVOID CALLBACK SmoothDIB (LPVOID, short);
LPVOID CALLBACK HalftoneDIB (LPVOID);
LPVOID CALLBACK GrayDIB (LPVOID);
LPVOID CALLBACK MergeDIB (LPVOID, LPVOID, long, long);
LPVOID CALLBACK ExpandToTrueDIB (LPVOID);
LPVOID CALLBACK RotateDIB (LPVOID, short);
LPVOID CALLBACK MirrorDIB (LPVOID, BOOL);
LPVOID CALLBACK ClipDIB (LPVOID, long, long, long, long);
LPVOID CALLBACK ZoomDIB (LPVOID, long, long);
DWORD CALLBACK GetDIBSize (LPVOID);
LPVOID CALLBACK CopyDIB (LPVOID);
BOOL CALLBACK WriteDIBToFile (LPVOID, LPCSTR, short);
BOOL CALLBACK GetImgLibVersion (LPSTR, short);

#define ERROR_READ_ACCESS_DENIED        1
#define ERROR_WRITE_ACCESS_DENIED       2
#define ERROR_NO_MEMORY                 3
#define ERROR_NO_DLL                    4
#define ERROR_INVALID_POINTER           5
#define ERROR_INVALID_ARGUMENT          6
#define ERROR_UNSUPPORTED_IMAGE         7
#define ERROR_INCOMPATIBLE_IMAGE        8
#define ERROR_WRITE_ERROR               9

#define FILETYPE_BMP                    0x0     /* Regular Windows-style BMP */
#define FILETYPE_TIFF_DEFAULT           0x10    /* Let ImgLib pick the best default */
#define FILETYPE_TIFF_NO_COMPRESSION    0x11    /* Don't compress at all */
#define FILETYPE_TIFF_HUFFMAN           0x12    /* Use Huffman encoding on monochrome bitmaps */
#define FILETYPE_TIFF_PACKBITS          0x13    /* Run-length encode */
#define FILETYPE_TIFF_G3                0x14    /* CCITT Group 3 compression */
#define FILETYPE_TIFF_G4                0x15    /* CCITT Group 4 compression */

#ifdef __cplusplus
}
#endif
