%{
/* 
	Basic xml grammar definition
	This is a basic definition of the xml grammar necessary to cover 
	the MusicXML format. It is a simplified form based on the XML document
	grammar as defined in 
	"XML in a nutshell - 2nd edition" E.R.Harold and W.S.Means,
	O'Reilly, June 2002, pp:366--371
*/


#include <stdio.h>
#include <string.h>
#include <iostream>
#include "reader.h"

#include "xmllex.c++"

using namespace std;

int yyline = 1;

#ifdef __cplusplus
extern "C" {
#endif
int		yyparse (void);
void	yyerror(char *s);
int		libmxmlwrap();
bool	readfile   (const char * file, reader * r);
bool	readstream (FILE * file, reader * r);
bool	readbuffer (const char * buffer, reader * r);
#ifdef __cplusplus
}
#endif

int		yylex (void);
extern char * libmxmltext;
extern int libmxmllineno;
extern FILE * libmxmlin;

#define YYERROR_VERBOSE
#define ERROR(str)	{ yyerror((char*)str); YYABORT; }
#define MAXLEN	1024
#define VLEN	256
char attributeName[MAXLEN];
char attributeVal[MAXLEN];

char xmlversion[VLEN];
char xmlencoding[MAXLEN];
int xmlStandalone = -1;

char eltName[MAXLEN];
char doctypeStart[MAXLEN];
char doctypePub[MAXLEN];
char doctypeSys[MAXLEN];

reader * gReader;

static void init (reader * r) {
	gReader = r;
	xmlStandalone = -1;
	eltName[0]		= 0;
	attributeName[0] = 0;
	attributeVal[0] = 0;
	xmlversion[0]   = 0;
	xmlencoding[0]  = 0;
	doctypeStart[0]	= 0;
	doctypePub[0]	= 0;
	doctypeSys[0]	= 0;
}

static char * unquote (char * text) {
	int n = strlen(text);
	if (n > 0) {
		text[n-1]=0;
		return &text[1];
	}
	return text;
}

static void store (char * dst, const char * text) {
	strcpy (dst, text);
}

int		libmxmlwrap()		{ return(1); }

%}


%start document

%token XMLDECL VERSION ENCODING STANDALONE YES NO ENDXMLDECL
%token DOCTYPE PUBLIC SYSTEM COMMENT PI
%token NAME DATA QUOTEDSTR SPACE
%token LT GT ENDXMLS ENDXMLE EQ


%%      				/* beginning of rules section */


document	: prolog element misc ;

prolog		: xmldecl doctype;

element		: eltstart data eltstop
			| emptyelt 
			| procinstr ;
			| comment ;

eltstart	: LT eltname GT
			| LT eltname SPACE attributes GT ;

eltstop		: ENDXMLS endname GT;

emptyelt	: LT eltname ENDXMLE					{ if (!gReader->endElement(eltName)) ERROR("end element error") }
			| LT eltname SPACE attributes ENDXMLE   { if (!gReader->endElement(eltName)) ERROR("end element error") }

eltname		: NAME							{ store(eltName, libmxmltext); if (!gReader->newElement(libmxmltext)) ERROR("element error") }
endname		: NAME							{ if (!gReader->endElement(libmxmltext)) ERROR("end element error") }

attribute	: attrname EQ value				{ if (!gReader->newAttribute (attributeName, attributeVal)) ERROR("attribute error") }
attrname	: NAME							{ store(attributeName, libmxmltext); }
value		: QUOTEDSTR						{ store(attributeVal, unquote(libmxmltext)); }

attributes  : attribute	
			| attributes SPACE attribute;

data		: /* empty */
			| cdata
			| elements ;

cdata		: DATA							{ gReader->setValue (libmxmltext); }

procinstr	: PI							{ gReader->newProcessingInstruction (libmxmltext); }
comment		: COMMENT						{ gReader->newComment (libmxmltext); }

elements	: element
			| elements element;


xmldecl		: /* empty */
			| XMLDECL versiondec decl ENDXMLDECL { if (!gReader->xmlDecl (xmlversion, xmlencoding, xmlStandalone)) ERROR("xmlDecl error") }

decl		: /* empty */
			| encodingdec 
			| stdalonedec
			| encodingdec stdalonedec ;

versiondec	: SPACE VERSION EQ QUOTEDSTR 		{ store(xmlversion, unquote(libmxmltext)); }
encodingdec	: SPACE ENCODING EQ QUOTEDSTR 		{ store(xmlencoding, unquote(libmxmltext)); }
stdalonedec	: SPACE STANDALONE EQ bool  		{ xmlStandalone = yylval; }
bool		: YES | NO ;

doctype		: DOCTYPE SPACE startname SPACE id GT
startname	: NAME 							{ store(doctypeStart, libmxmltext); }
id			: PUBLIC SPACE publitteral SPACE syslitteral	{ gReader->docType (doctypeStart, true, doctypePub, doctypeSys); }
			| SYSTEM SPACE syslitteral						{ gReader->docType (doctypeStart, false, doctypePub, doctypeSys); }
publitteral : QUOTEDSTR 						{ store(doctypePub, unquote(libmxmltext)); }
syslitteral : QUOTEDSTR 						{ store(doctypeSys, unquote(libmxmltext)); }

misc		: /* empty */
			| SPACE ;


%%

#define yy_delete_buffer	libmxml_delete_buffer
#define yy_scan_string		libmxml_scan_string

bool readbuffer (const char * buffer, reader * r) 
{
	if (!buffer) return false;		// error for empty buffers

	init(r);
	YY_BUFFER_STATE b;
    // Copy string into new buffer and Switch buffers
    b = yy_scan_string (buffer);
    // Parse the string
	int ret = yyparse();
    // Delete the new buffer
	yy_delete_buffer(b);
	BEGIN(INITIAL);
 	return ret==0;
}

bool readfile (const char * file, reader * r) 
{
	FILE * fd = fopen (file, "r");
	if (!fd) {
		cerr << "can't open file " << file << endl;
		return false;
	}
	init(r);
	libmxmlrestart(fd);
	libmxmlin = fd;
 	int ret = yyparse();
 	fclose (fd);
	BEGIN(INITIAL);
 	return ret==0;
}

bool readstream (FILE * fd, reader * r) 
{
	if (!fd) return false;
	init(r);
	libmxmlrestart(fd);
	libmxmlin = fd;
 	int ret = yyparse();
	BEGIN(INITIAL);
 	return ret==0;
}

void	yyerror(char *s)	{ gReader->error (s, libmxmllineno); }

#ifdef MAIN

class testreader : public reader
{ 
	public:
		bool	xmlDecl (const char* version, const char *encoding, bool standalone) {
			cout << "xmlDecl: " << version << " " << encoding << " " << standalone << endl;
			return true;
		}
		bool	docType (const char* start, bool status, const char *pub, const char *sys) {
			cout << "docType: " << start << " " << (status ? "PUBLIC" : "SYSTEM") << " " << pub << " " << sys << endl;
			return true;
		}

		bool	newElement (const char* eltName) {
			cout << "newElement: " << eltName << endl;
			return true;
		}
		bool	newAttribute (const char* eltName, const char *val) {
			cout << "    newAttribute: " << eltName << "=" << val << endl;
			return true;
		}
		void	setValue (const char* value) {
			cout << "  -> value: " << value << endl;
		}
		bool	endElement (const char* eltName) {
			cout << "endElement: " << eltName << endl;
			return true;
		}
		void	error (const char* s, int lineno) {
			cerr << s  << " on line " << lineno << endl;
		}

};


int main (int argc, char * argv[])
{
	if (argc > 1) {
		testreader r;
		return readfile (argv[1], &r) ? 0 : 1;
	}
 	return 0;
}
#endif
