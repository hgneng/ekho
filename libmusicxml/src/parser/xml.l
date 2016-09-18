%{
 
/* 
	Basic relaxed xml lexical definition.
	This is a basic definition of the lexical elements necessary to cover 
	the MusicXML format. It is a simplified form based on the XML document
	grammar as defined in 
	"XML in a nutshell - 2nd edition" E.R.Harold and W.S.Means,
	O'Reilly, June 2002, pp:366--371
*/

#include "xmlparse.hpp"
#include <stdio.h>

#define YY_NO_UNISTD_H

extern int libmxmllval;

static int utf16 = 0;
static int bigendian = 1;
static int start = 1;

static int wgetc(FILE * fd) {
	int c = getc(fd);
	if (start) {
		if (c == 0xff) {
			utf16 = 1; bigendian = 0;
			getc(fd); c = getc(fd);
		}
		else if (c == 0xfe) {
			utf16 = 1; bigendian = 1;
			getc(fd); c = getc(fd);
		}
		start = 0;
	}
	if (utf16) {
		if (bigendian) c = getc(fd);
		else getc(fd);
	}

	return c;
}

static size_t wfread (void * buf, size_t size, size_t nmemb, FILE * fd) {
	char * ptr = (char *)buf;
	size_t n=0;
	while (nmemb--) {
		*ptr++ = wgetc(fd);
		if (feof(fd) || ferror(fd) ) break;
		n++;
	}
	return n;
}
		 
void lexinit(FILE* fd) {
	utf16 = 0;
	bigendian = 1;
	start = 1;
	yyrestart(fd);
}
		 
void lexend() {
	if (YY_CURRENT_BUFFER) {
		libmxml_delete_buffer (YY_CURRENT_BUFFER);
	}
}

#define getc	wgetc
#define fread	wfread

%}

letter		[a-zA-Z]
digit		[0-9]
space		[ \t]
S			[ \t\x0a\x0d]
firstchar	[a-zA-Z_]
namechar	[-a-zA-Z0-9_.:]
anychar		[^<>]
valchar		[^<>&"']
quote		['"]
anything	[^-]


%option yylineno
%option never-interactive

%S COMMENTSECT DATASECT XMLSECT PISECT DOCTYPESECT

%%


{S}*"<!--"					{ BEGIN COMMENTSECT; }
<COMMENTSECT>"-->"			{ BEGIN 0; }
<COMMENTSECT>[^-]*|"-"		{ return COMMENT; }


{S}*"<?xml"						{ BEGIN XMLSECT; return XMLDECL; }
<XMLSECT>{space}*"?>"			{ BEGIN 0; return ENDXMLDECL; }
<XMLSECT>"version"				{ return VERSION; }
<XMLSECT>"encoding"				{ return ENCODING; }
<XMLSECT>"standalone"			{ return STANDALONE; }
<XMLSECT>{quote}"yes"{quote}	{ libmxmllval=1; return YES; }
<XMLSECT>{quote}"no"{quote}		{ libmxmllval=0; return NO; }

{S}*"<?"						{ BEGIN PISECT; }
<PISECT>.*"?>"					{ BEGIN 0; return PI; }
<PISECT>[^?]*{S}				{ return PI; }

{S}*"<!DOCTYPE"					{ BEGIN DOCTYPESECT; return DOCTYPE; }
<DOCTYPESECT>"PUBLIC"			{ return PUBLIC; }
<DOCTYPESECT>"SYSTEM"			{ return SYSTEM; }

{S}*"<"{space}*					{ BEGIN 0; return LT; }
{space}*">"						{ BEGIN DATASECT; return GT; }
{S}*"</"						{ BEGIN 0; return ENDXMLS; }
{space}*"/>"					{ return ENDXMLE; }

{S}+							{ return SPACE; }
<DATASECT>{anychar}+			{ return DATA; }
{firstchar}{namechar}*			{ return NAME; }
{quote}{valchar}*{quote}		{ return QUOTEDSTR; }
{space}*"="{space}*				{ return EQ; }

S*								{ /* extra space ignored*/ }

<<EOF>>		yyterminate();
