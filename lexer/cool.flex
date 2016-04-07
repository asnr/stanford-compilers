/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int addToStrLitBuffer(char *yytext, int yyleng, int curr_str_len);

%}


/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-
WSPACE_NO_NEWLINE  [ \f\r\t\v]
IDCHAR  [A-Za-z0-9_]


%x comment singcmmt
%x strlit strerr
%x eof

%%

    int comment_depth = 0;
    int curr_str_len = 0;


<eof><<EOF>>        { return 0; }


 /*
  *  Nested comments
  */
 
"(*"    { BEGIN(comment); comment_depth++; }
"*)"    { 
          cool_yylval.error_msg = "Unmatched *)";
          return (ERROR);
        }

<comment>{

[^*(\n]*   /* Eat up comments */
"*"+")"    { if (--comment_depth < 1) BEGIN(0); }
"*"+       {/* Eat up asterisks not followed by a ")" */}
"("+"*"    comment_depth++;
"("+       {/* Eat up "("s not followed by an asterisk */}
\n         curr_lineno++;
<<EOF>>    { 
             BEGIN(eof);
             cool_yylval.error_msg = "EOF in a comment";
             return (ERROR);
           }
}

 /*
  *  Not nested comments
  */
"--"    BEGIN(singcmmt);
<singcmmt>{
  [^\n]*
  \n        { curr_lineno++; BEGIN(0);}
  <<EOF>>   BEGIN(eof);
}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
\<\=          { return (LE); }

 /*
  *  The single-character symbols. Return ASCII value of character
  */ 
\.    { return (46); }
\,    { return (44); }
\@    { return (64); }
\~    { return (126); }
\*    { return (42); }
\/    { return (47); }
\+    { return (43); }
\-    { return (45); }
\<    { return (60); }
\=    { return (61); }
\{    { return (123); }
\}    { return (125); }
\:    { return (58); }
\;    { return (59); }
\(    { return (40);}
\)    { return (41);}


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
[Cc][Ll][Aa][Ss][Ss]    { return (CLASS); }
[Ee][Ll][Ss][Ee]        { return (ELSE);  }
[Ii][Ff]                { return (IF);    }
[Ff][Ii]                { return (FI);    }
[Ii][Nn]                { return (IN); }
[Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]    { return (INHERITS); }
[Ll][Ee][Tt]            { return (LET); }
[Ll][Oo][Oo][Pp]        { return (LOOP); }
[Pp][Oo][Oo][Ll]        { return (POOL); }
[Tt][Hh][Ee][Nn]        { return (THEN); }
[Ww][Hh][Ii][Ll][Ee]    { return (WHILE); }
[Cc][Aa][Ss][Ee]        { return (CASE); }
[Ee][Ss][Aa][Cc]        { return (ESAC); }
[Oo][Ff]                { return (OF); }
[Nn][Ee][Ww]            { return (NEW); }
[Ii][Ss][Vv][Oo][Ii][Dd]    { return (ISVOID); }
[Nn][Oo][Tt]            { return (NOT); }

 /*
  *  Boolean constants
  */
t[Rr][Uu][Ee]           { cool_yylval.boolean = 1; return (BOOL_CONST); }
f[Aa][Ll][Ss][Ee]       { cool_yylval.boolean = 0; return (BOOL_CONST); }

 /*
  *  Integer constants
  */
[0-9]+   { 
            cool_yylval.symbol = inttable.add_string(yytext);
            return (INT_CONST);
         }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  */

\"      { 
          BEGIN(strlit);
          curr_str_len = 0;
          string_buf[0] = '\0';
        }

<strlit,strerr>{

  [^\\"\n\0]*   curr_str_len = addToStrLitBuffer(yytext, yyleng, curr_str_len);
  \\\0|\0   {
              BEGIN(strerr);
              cool_yylval.error_msg = "String contains null character";
            }
  \\\n   { 
           curr_lineno++;
           curr_str_len = addToStrLitBuffer("\n", 1, curr_str_len);
         }
  \\b    curr_str_len = addToStrLitBuffer("\b", 1, curr_str_len);
  \\t    curr_str_len = addToStrLitBuffer("\t", 1, curr_str_len);
  \\n    curr_str_len = addToStrLitBuffer("\n", 1, curr_str_len);
  \\f    curr_str_len = addToStrLitBuffer("\f", 1, curr_str_len);
  \\[^\0\n]   curr_str_len = addToStrLitBuffer(yytext+1, 1, curr_str_len); 
  \n     {
           curr_lineno++;

           if (YY_START != strerr)
             cool_yylval.error_msg = "Unterminated string constant";

           // else cool_yylval.error_msg already has an error msg

           BEGIN(0);

           return (ERROR);
         }
  \"     {
           if (YY_START == strerr) {
             BEGIN(0);
             return (ERROR);
           }

           BEGIN(0);
           cool_yylval.symbol = stringtable.add_string(string_buf);
           return (STR_CONST);
         }
  <<EOF>>   { 
              BEGIN(eof);
              if (YY_START != strerr)
                cool_yylval.error_msg = "EOF in string constant";
              return (ERROR);
            }
}


 /*
  *  Whitespace
  */
{WSPACE_NO_NEWLINE}+  /* Eat up whitespace */
\n                    curr_lineno++;

 /*
  *  Identifiers
  */
[a-z]{IDCHAR}*  {
                  cool_yylval.symbol = idtable.add_string(yytext);
                  return (OBJECTID);
                }
[A-Z]{IDCHAR}*  {
                  cool_yylval.symbol = idtable.add_string(yytext);
                  return (TYPEID);
                }

 /*
  *  Invalid characters
  */
.   { cool_yylval.error_msg = yytext; return (ERROR); }

%%


// Side-effects: reads MAX_STR_CONST, modifies string_buf
int addToStrLitBuffer(char *yytext, int yyleng, int curr_str_len)
{  
  // Note that *(yytext + yyleng) == '\0'
  int new_str_len = curr_str_len + yyleng;

  // Need to check YY_START to avoid overwriting previous errors
  if (YY_START != strerr && new_str_len >= MAX_STR_CONST) {
    BEGIN(strerr);
    cool_yylval.error_msg = "String constant too long";
  }

  if (YY_START != strerr)
    strcpy(string_buf + curr_str_len, yytext);

  return (new_str_len);
}
