#ifndef DOUBLE_METAPHONE__H
#define DOUBLE_METAPHONE__H


typedef struct
{
    char *str;
    int length;
    int bufsize;
    int free_string_on_destroy;
}
metastring;      


#ifndef __SWI_PROLOG__
void
DoubleMetaphone(char *str,
                char **codes);
#endif


#endif /* DOUBLE_METAPHONE__H */
