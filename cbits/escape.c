#include <HsFFI.h>
#include <stdint.h>

/* reference: <http://dev.mysql.com/doc/refman/5.7/en/string-literals.html>

    * Escape Sequence	Character Represented by Sequence
    * \0              	An ASCII NUL (X'00') character
    * \'              	A single quote (“'”) character
    * \"              	A double quote (“"”) character
    * \b              	A backspace character
    * \n              	A newline (linefeed) character
    * \r              	A carriage return character
    * \t              	A tab character
    * \Z              	ASCII 26 (Control+Z); see note following the table
    * \\              	A backslash (“\”) character
    * \%              	A “%” character; see note following the table
    * \_              	A “_” character; see note following the table

The @\%@ and @\_@ sequences are used to search for literal instances of @%@ and @_@ in pattern-matching contexts where they would otherwise be interpreted as wildcard characters, so we won't auto escape @%@ or @_@ here.
*/

static const int mysql_escape_char_table[256] =
  { 2,1,1,1,1,1,1,1,2,2,2,1,1,2,1,1,
    1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,
    1,1,2,1,1,1,1,2,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

HsInt escape_mysql_string(const unsigned char *src, HsInt srcoff, HsInt srclen, unsigned char *dest, HsInt desoff){
    const unsigned char *i = src + srcoff;
    const unsigned char *srcend = i + srclen;
    unsigned char *j = dest + desoff;
    for (; i < srcend; i++){
        if (mysql_escape_char_table[*i] == 1) {
            *j++ = *i;
        } else {
            switch (*i) {
                case 0: *j++ = '\\'; *j++ = '0'; break;
                case '\'': *j++ = '\\'; *j++ = '\''; break;
                case '\"': *j++ = '\\'; *j++ = '\"'; break;
                case '\b': *j++ = '\\'; *j++ = 'b'; break;
                case '\n': *j++ = '\\'; *j++ = 'n'; break;
                case '\r': *j++ = '\\'; *j++ = 'r'; break;
                case '\t': *j++ = '\\'; *j++ = 't'; break;
                case 26: *j++ = '\\'; *j++ = 'Z'; break;
                default: 
                    *j++ = '\\'; *j++ = '\\'; break;
            }
        }
    }
    return (HsInt)(j-dest);
}
