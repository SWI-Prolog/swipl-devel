
////////////////////////////////////////////////////////////////////////
//
//	This is the writing portion of the GIFFile class.
//	It is based on code from Programming for Graphics Files	by John Levine
//
//	This is free to use and modify provided proper credit is given
//
//	This writes 256 color GIFs version GIF87a.
//
//	see GIFFile.h for example
////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include <io.h>

#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>
#include "giffile.h"

////////////
//
//	GIF writing section
//
////////////

/* a code_int must be able to hold 2**BITS values of type int, and also -1
 */


static int 				Width, Height;
static int				curx, cury;
static long 			CountDown;
static unsigned long	cur_accum = 0;
static int				cur_bits = 0;
static unsigned char	*buffer;


/*
 * Bump the 'curx' and 'cury' to point to the next pixel
 */
void GIFFile::BumpPixel()
{
    /*
     * Bump the current X position
     */
    ++curx;

    if( curx == Width ) {
        curx = 0;
		++cury;
    }
}

/*******************************************************************************
* Return the next pixel from the image
*******************************************************************************/

int GIFFile::GIFNextPixel( )
{
    unsigned long index;
    int r;
    
    if( CountDown == 0 )
        return EOF;

    --CountDown;
    
    index= (unsigned long)curx + (unsigned long)cury * (unsigned long)Width;
    
	r = *(buffer+index);

    BumpPixel();

    return r;
}

/*******************************************************************************
*	here's the entry point. 
*	file ptr, screen width, height, background color, bits per pixel and
*	arrays of color values (0-255)
*******************************************************************************/
BOOL GIFFile::GIFWriteFileFrom256Color(unsigned char  * buf,
							CString name,
							int GWidth, 
							int GHeight,
							int BackGround,
							int Red[], int Green[], int Blue[])
{                       
	FILE *fp;
	int B;
	int RWidth, RHeight;
	int LeftOfs, TopOfs;
	int Resolution;
	int ColorMapSize;
	int InitCodeSize;
	int i;
	int BitsPerPixel = 8;

	fp=fopen(name,"wb");
	if (fp==NULL) {
		m_GIFErrorText="Can't open GIF for writing";
		return FALSE;
	}
	
	ColorMapSize = 1 << BitsPerPixel;

	buffer=buf;

	RWidth = Width = GWidth;
	RHeight = Height = GHeight;
	LeftOfs = TopOfs = 0;

	cur_accum = 0;
	cur_bits = 0;

	Resolution = BitsPerPixel;

	CountDown = (long)Width * (long) Height;

	if (BitsPerPixel <=1)
	InitCodeSize=2;
	else
	InitCodeSize = BitsPerPixel;

	curx = cury =0;

	fwrite("GIF87a",1,6,fp);

	Putword(RWidth,fp);
	Putword(RHeight,fp);

	B=0x80;
	
	B |=(Resolution -1) << 5;

	B |=(BitsPerPixel - 1);

	fputc(B,fp);

	fputc(BackGround,fp);
	
	fputc(0,fp);

	for(i=0; i<ColorMapSize; ++i) {
		fputc(Red[i],fp);
		fputc(Green[i],fp);
		fputc(Blue[i],fp);
	}

	fputc(',',fp);

	Putword(LeftOfs,fp);
	Putword(TopOfs,fp);
	Putword(Width,fp);
	Putword(Height,fp);

	fputc(0x00,fp);

    /*
     * Write out the initial code size
     */
    fputc( InitCodeSize, fp );
    /*
     * Go and actually compress the data
     */

    compress(  InitCodeSize+1, fp);

    /*
     * Write out a Zero-length packet (to end the series)
     */
    fputc( 0, fp );

    /*
     * Write the GIF file terminator
     */
    fputc( ';', fp );

    /*
     * And close the file
     */
    fclose( fp );

	return TRUE;
}

/*******************************************************************************
 * Write out a word to the GIF file
*******************************************************************************/

void GIFFile::Putword(int w, FILE *fp )
{
    fputc( w & 0xff, fp );
    fputc( (w / 256) & 0xff, fp );
}


/***************************************************************************
 *
 *  GIFCOMPR.C       - GIF Image compression routines
 *
 *  Lempel-Ziv compression based on 'compress'.  GIF modifications by
 *  David Rowley (mgardi@watdcsu.waterloo.edu)
 *
 ***************************************************************************/

/*
 * General DEFINEs
 */

#define BITS    12

#define HSIZE  5003            /* 80% occupancy */

typedef        unsigned char   char_type;

/*
 *
 * GIF Image compression - modified 'compress'
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 * By Authors:  Spencer W. Thomas       (decvax!harpo!utah-cs!utah-gr!thomas)
 *              Jim McKie               (decvax!mcvax!jim)
 *              Steve Davies            (decvax!vax135!petsd!peora!srd)
 *              Ken Turkowski           (decvax!decwrl!turtlevax!ken)
 *              James A. Woods          (decvax!ihnp4!ames!jaw)
 *              Joe Orost               (decvax!vax135!petsd!joe)
 *
 */

static int n_bits;                        /* number of bits/code */
static int maxbits = BITS;                /* user settable max # bits/code */
static code_int maxcode;                  /* maximum code, given n_bits */
static code_int maxmaxcode = (code_int)1 << BITS; /* should NEVER generate this
code */

#define MAXCODE(n_bits)        (((code_int) 1 << (n_bits)) - 1)

static count_int htab [HSIZE];
static unsigned short codetab [HSIZE];
#define HashTabOf(i)       htab[i]
#define CodeTabOf(i)    codetab[i]

static code_int free_ent = 0;                  /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int clear_flg = 0;

/*
 * compress pixels to GIF packets
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

static int g_init_bits;
static FILE* g_outfile;

static int ClearCode;
static int EOFCode;

/*******************************************************************************
*
*******************************************************************************/

void GIFFile::compress( int init_bits, FILE* outfile)
{
    register long fcode;
    register code_int i /* = 0 */;
    register int c;
    register code_int ent;
    register code_int disp;
    register int hshift;

    /*
     * Set up the globals:  g_init_bits - initial number of bits
     *                      g_outfile   - pointer to output file
     */
    g_init_bits = init_bits;
    g_outfile = outfile;

    /*
     * Set up the necessary values
     */
    clear_flg = 0;
    maxcode = MAXCODE(n_bits = g_init_bits);

    ClearCode = (1 << (init_bits - 1));
    EOFCode = ClearCode + 1;
    free_ent = ClearCode + 2;

    char_init();

    ent = GIFNextPixel( );

    hshift = 0;
    for ( fcode = (long) HSIZE;  fcode < 65536L; fcode *= 2L )
        ++hshift;
    hshift = 8 - hshift;                /* set hash code range bound */

    cl_hash( (count_int) HSIZE);            /* clear hash table */

    output( (code_int)ClearCode );

    while ( (c = GIFNextPixel( )) != EOF ) {	/* } */

        fcode = (long) (((long) c << maxbits) + ent);
        i = (((code_int)c << hshift) ^ ent);    /* xor hashing */

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        } else if ( (long)HashTabOf (i) < 0 )      /* empty slot */
            goto nomatch;
        disp = HSIZE - i;           /* secondary hash (after G. Knott) */
        if ( i == 0 )
            disp = 1;
probe:
        if ( (i -= disp) < 0 )
            i += HSIZE;

        if ( HashTabOf (i) == fcode ) {
            ent = CodeTabOf (i);
            continue;
        }
        if ( (long)HashTabOf (i) > 0 )
            goto probe;
nomatch:
        output ( (code_int) ent );
        ent = c;
        if ( free_ent < maxmaxcode ) {	/* } */
            CodeTabOf (i) = free_ent++; /* code -> hashtable */
            HashTabOf (i) = fcode;
        } else
                cl_block();
    }
    /*
     * Put out the final code.
     */
    output( (code_int)ent );
    output( (code_int) EOFCode );
}

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *              that n_bits =< (long)wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a BITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static unsigned long masks[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                  0x001F, 0x003F, 0x007F, 0x00FF,
                                  0x01FF, 0x03FF, 0x07FF, 0x0FFF,
                                  0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF };

void GIFFile::output( code_int  code)
{
    cur_accum &= masks[ cur_bits ];

    if( cur_bits > 0 )
        cur_accum |= ((long)code << cur_bits);
    else
        cur_accum = code;

    cur_bits += n_bits;

    while( cur_bits >= 8 ) {
        char_out( (unsigned int)(cur_accum & 0xff) );
        cur_accum >>= 8;
        cur_bits -= 8;
    }

    /*
     * If the next entry is going to be too big for the code size,
     * then increase it, if possible.
     */
   if ( free_ent > maxcode || clear_flg ) {
        if( clear_flg ) {
            maxcode = MAXCODE (n_bits = g_init_bits);
            clear_flg = 0;
        } else {
            ++n_bits;
            if ( n_bits == maxbits )
                maxcode = maxmaxcode;
            else
                maxcode = MAXCODE(n_bits);
        }
    }
	
	if( code == EOFCode ) {
        /*
         * At EOF, write the rest of the buffer.
         */
        while( cur_bits > 0 ) {
            char_out( (unsigned int)(cur_accum & 0xff) );
            cur_accum >>= 8;
            cur_bits -= 8;
        }
	
        flush_char();
	
        fflush( g_outfile );
	
        if( ferror( g_outfile ) ) {
			AfxMessageBox("Write Error in GIF file",MB_OK);
		}
    }
}

void GIFFile::cl_block()
{
	cl_hash((count_int)HSIZE);
	free_ent=ClearCode+2;
	clear_flg=1;

	output((code_int)ClearCode);
}

void GIFFile::cl_hash(register count_int hsize)

{
	register count_int *htab_p = htab+hsize;

	register long i;
	register long m1 = -1L;

	i = hsize - 16;

	do {
		*(htab_p-16)=m1;
		*(htab_p-15)=m1;
		*(htab_p-14)=m1;
		*(htab_p-13)=m1;
		*(htab_p-12)=m1;
		*(htab_p-11)=m1;
		*(htab_p-10)=m1;
		*(htab_p-9)=m1;
		*(htab_p-8)=m1;
		*(htab_p-7)=m1;
		*(htab_p-6)=m1;
		*(htab_p-5)=m1;
		*(htab_p-4)=m1;
		*(htab_p-3)=m1;
		*(htab_p-2)=m1;
		*(htab_p-1)=m1;
		
		htab_p-=16;
	} while ((i-=16) >=0);

	for (i+=16;i>0;--i)
		*--htab_p=m1;
}

/*******************************************************************************
*	GIF specific
*******************************************************************************/

static int a_count;

void GIFFile::char_init()
{
	a_count=0;
}

static char accum[256];

void GIFFile::char_out(int c)
{
	accum[a_count++]=c;
	if (a_count >=254)
		flush_char();
}

void GIFFile::flush_char()
{
	if (a_count > 0) {
		fputc(a_count,g_outfile);
		fwrite(accum,1,a_count,g_outfile);
		a_count=0;
	}
}
