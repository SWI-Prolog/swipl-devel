/* DVI COMMANDS */
#define  SETC_000         0     /* typeset character 0 and move right */
#define  SETC_127       127     /* typeset character 127 and move right */
#define  SET1           128     /* typeset a character and move right */
#define  SET2           129     /* ??? */
#define  SET3           130     /* ??? */
#define  SET4           131     /* ??? */
#define  SET_RULE       132     /* typeset a rule and move right */
#define  PUT1           133     /* typeset a character */
#define  PUT2           134     /* ??? */
#define  PUT3           135     /* ??? */
#define  PUT4           136     /* ??? */
#define  PUT_RULE       137     /* typeset a rule */
#define  NOP            138     /* no operation */
#define  BOP            139     /* beginning of page */
#define  EOP            140     /* ending of page */
#define  PUSH           141     /* save the current positions */
#define  POP            142     /* restore previous positions */
#define  RIGHT1         143     /* move right */
#define  RIGHT2         144     /* ??? */
#define  RIGHT3         145     /* ??? */
#define  RIGHT4         146     /* ??? */
#define  W0             147     /* move right by |w| */
#define  W1             148     /* move right and set |w| */
#define  W2             149     /* ??? */
#define  W3             150     /* ??? */
#define  W4             151     /* ??? */
#define  X0             152     /* move right by |x| */
#define  X1             153     /* move right and set |x| */
#define  X2             154     /* ??? */
#define  X3             155     /* ??? */
#define  X4             156     /* ??? */
#define  DOWN1          157     /* move down */
#define  DOWN2          158     /* ??? */
#define  DOWN3          159     /* ??? */
#define  DOWN4          160     /* ??? */
#define  Y0             161     /* move down by |y| */
#define  Y1             162     /* move down and set |y| */
#define  Y2             163     /* ??? */
#define  Y3             164     /* ??? */
#define  Y4             165     /* ??? */
#define  Z0             166     /* move down by |z| */
#define  Z1             167     /* move down and set |z| */
#define  Z2             168     /* ??? */
#define  Z3             169     /* ??? */
#define  Z4             170     /* ??? */
#define  FONT_00        171     /* set current font to 0 */
#define  FONT_63        234     /* set current font to 0 */
#define  FNT1           235     /* set current font */
#define  FNT2           236     /* Same as FNT1, except that arg is 2 bytes */
#define  FNT3           237     /* Same as FNT1, except that arg is 3 bytes */
#define  FNT4           238     /* Same as FNT1, except that arg is 4 bytes */
#define  XXX1           239     /* extension to \.DVI primitives */
#define  XXX2           240     /* Like XXX1, but 0<=k<65536 */
#define  XXX3           241     /* Like XXX1, but 0<=k<@t$2^{24}$@> */
#define  XXX4           242     /* potentially long extension to \.DVI
                                   primitives */
#define  FNT_DEF1       243     /* define the meaning of a font number */
#define  FNT_DEF2       244     /* ??? */
#define  FNT_DEF3       245     /* ??? */
#define  FNT_DEF4       246     /* ??? */
#define  PRE            247     /* preamble */
#define  POST           248     /* postamble beginning */
#define  POST_POST      249     /* postamble ending */
#define  TRAILER        223     /* trailer bytes in dvi file */

        /*  undefined_commands           250,251,252,253,254,255 */
