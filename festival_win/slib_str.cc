/*  
 *                   COPYRIGHT (c) 1988-1994 BY                             *
 *        PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.       *
 *        See the source file SLIB.C for more information.                  *

 * Reorganization of files (Mar 1999) by Alan W Black <awb@cstr.ed.ac.uk>

 * String functions

*/
#include <cstdio>
#include <cstring>
#include <setjmp.h>
#include <cstdlib>
#include <cctype>

#include "EST_Pathname.h"
#include "EST_cutils.h"
#include "siod.h"
#include "siodp.h"

LISP strintern(const char *data)
{
    return strcons(strlen(data),data);
}

LISP strcons(long length,const char *data)
{long flag;
 LISP s;
 flag = no_interrupt(1);
 s = cons(NIL,NIL);
 s->type = tc_string;
 s->storage_as.string.data = must_malloc(length+1);
 s->storage_as.string.dim = length;
 if (data)
   memmove(s->storage_as.string.data,data,length+1);
 no_interrupt(flag);
 return(s);}

LISP cstrcons(const char *data)
{long flag;
 LISP s;
 flag = no_interrupt(1);
 s = cons(NIL,NIL);
 s->type = tc_string;
 s->storage_as.string.data = (char *)(void *)data;
 s->storage_as.string.dim = strlen(data);
 no_interrupt(flag);
 return(s);}

static int rfs_getc(unsigned char **p)
{int i;
 i = **p;
 if (!i) return(EOF);
 *p = *p + 1;
 return(i);}

static void rfs_ungetc(unsigned char c,unsigned char **p)
{(void)c;
 *p = *p - 1;}

LISP read_from_lstring(LISP x)
{return read_from_string(get_c_string(x));}

LISP read_from_string(const char *string)
{char *p,*q;
 LISP r; 
 struct gen_readio s;
 q = wstrdup(string);
 p = q;
 s.getc_fcn = (int (*)(char *))rfs_getc;
 s.ungetc_fcn = (void (*)(int, char *))rfs_ungetc;
 s.cb_argument = (char *) &p;
 r = readtl(&s);
 wfree(q);
 return r;
}

LISP string_append(LISP args)
{long size;
 LISP l,s;
 char *data;
 size = 0;
 for(l=args;NNULLP(l);l=cdr(l))
   size += strlen(get_c_string(car(l)));
 s = strcons(size,NULL);
 data = s->storage_as.string.data;
 data[0] = 0;
 for(l=args;NNULLP(l);l=cdr(l))
   strcat(data,get_c_string(car(l)));
 return(s);}

LISP string_length(LISP string)
{if NTYPEP(string,tc_string) err("not a string",string);
 return(flocons((double)string->storage_as.string.dim));}

LISP parse_number(LISP x)
{const char *c;
 c = get_c_string(x);
 return(flocons(atof(c)));}

LISP string_downcase(LISP symbol)
{
    const char *symname = get_c_string(symbol);
    char *downsym = wstrdup(symname);
    LISP newsym;
    int i;

	/*!!! begin change by hgneng !!!*/
	/* original code:
    for (i=0; symname[i] != '\0'; i++)
	if (isupper(symname[i]))
	    downsym[i] = tolower(symname[i]);
	else
	    downsym[i] = symname[i];
	*/

	for (i=0; symname[i] != '\0'; i++) {
		unsigned char c = (unsigned char)symname[i];
		if (isupper(c))
			downsym[i] = tolower(c);
		else
			downsym[i] = c;
	}
	/*!!! end change by hgneng !!!*/

    downsym[i] = '\0';
    newsym = strintern(downsym);
    wfree(downsym);
    
    return newsym;
}

LISP string_upcase(LISP symbol)
{
    const char *symname = get_c_string(symbol);
    char *upsym = wstrdup(symname);
    LISP newsym;
    int i;

	/*!!! begin change by hgneng !!!*/
	/* original code:
    for (i=0; symname[i] != '\0'; i++)
	if (islower(symname[i]))
	    upsym[i] = toupper(symname[i]);
	else
	    upsym[i] = symname[i];
	*/

	for (i=0; symname[i] != '\0'; i++) {
		unsigned char c = (unsigned char)symname[i];
		if (islower(c))
			upsym[i] = toupper(c);
		else
			upsym[i] = c;
	}
	/*!!! end change by hgneng !!!*/
    upsym[i] = '\0';
    newsym = strintern(upsym);
    wfree(upsym);
    
    return newsym;
}

LISP path_is_dirname(LISP lpath)
{
  EST_Pathname path(get_c_string(lpath));

  return path.is_dirname()?lpath:NIL;
}

LISP path_is_filename(LISP lpath)
{
  EST_Pathname path(get_c_string(lpath));

  return path.is_filename()?lpath:NIL;
}

LISP path_as_directory(LISP lpath)
{
  EST_Pathname path(get_c_string(lpath));
  EST_Pathname res(path.as_directory());
  return strintern(res);
}

LISP path_as_file(LISP lpath)
{
  EST_Pathname path(get_c_string(lpath));
  EST_Pathname res(path.as_file());

  return strintern(res);
}

LISP path_append(LISP lpaths)
{
  if (CONSP(lpaths))
    {
      EST_Pathname res(get_c_string(car(lpaths)));
      lpaths = cdr(lpaths);
      while(lpaths != NIL)
	{
	  res = res +get_c_string(car(lpaths));
	  lpaths = cdr(lpaths);
	}
      return strintern(res);
    }
  return NIL;
}

LISP path_basename(LISP lpath)
{
  EST_Pathname path(get_c_string(lpath));
  EST_Pathname res(path.basename(1));

  return strintern(res);
}

LISP symbol_basename(LISP path, LISP suffix)
{
    //  Like UNIX basename
    const char *pathstr = get_c_string(path);
    const char *suff;
    char *bname;
    int i, j, k, start, end;
    LISP newsym;

    if (suffix == NIL)
	suff = "";
    else
	suff = get_c_string(suffix);

    for (i=strlen(pathstr); i >= 0; i--)
	if (pathstr[i] == '/')
	    break;
    start = i+1;
    for (j=strlen(pathstr),k=strlen(suff); k >= 0; k--,j--)
	if (pathstr[j] != suff[k])
	    break;
    if (k != -1)
	end = strlen(pathstr);
    else
	end = j+1;

    bname = walloc(char,end-start+1);
    memcpy(bname,&pathstr[start],end-start);
    bname[end-start] = '\0';
    newsym = strcons(strlen(bname),bname);
    wfree(bname);
    
    return newsym;
}


static LISP lisp_to_string(LISP l)
{
    EST_String s;

    s = siod_sprint(l);
    printf("%s\n",(const char *)s);
    return strintern(s);
}

static LISP symbolconc(LISP args)
{long size;
 LISP l,s;
 size = 0;
 tkbuffer[0] = 0;
 for(l=args;NNULLP(l);l=cdr(l))
   {s = car(l);
    if NSYMBOLP(s) err("wrong type of argument(non-symbol) to symbolconc",s);
    size = size + strlen(PNAME(s));
    if (size >  TKBUFFERN) err("symbolconc buffer overflow",NIL);
    strcat(tkbuffer,PNAME(s));}
 return(rintern(tkbuffer));}

LISP symbolexplode(LISP name)
{
    LISP e=NIL;
    const char *pname = get_c_string(name);
    char tt[2];
    int i;

    tt[1]='\0';

    for (i=0; pname[i] != '\0'; i++)
    {
	tt[0] = pname[i];
	e = cons(rintern(tt),e);
    }
    return reverse(e);
}

LISP l_matches(LISP atom, LISP regex)
{
    // t if printname of atom matches regex, nil otherwise
    const EST_String pname = get_c_string(atom);

    if (pname.matches(make_regex(get_c_string(regex))) == TRUE)
	return truth;
    else
	return NIL;
}

LISP l_strequal(LISP atom1, LISP atom2)
{
    
    if (streq(get_c_string(atom1),get_c_string(atom2)))
	return truth;
    else
	return NIL;
}

LISP l_substring(LISP string, LISP l_start, LISP l_length)
{
    // As string might actually be a buffer containing nulls we
    // do this a little carefully.
    if (NTYPEP(string,tc_string))
	err("not a string",string);
	
    const char *data = string->storage_as.string.data;
    int dim = string->storage_as.string.dim;

    int start = ( get_c_int(l_start) < dim ? get_c_int(l_start) : dim );
    int length = ( (get_c_int(l_length) + start) < dim ? 
		   get_c_int(l_length) 
		   : dim-start
		   );

    char *nbuffer = walloc(char, length+1);
    memmove(nbuffer,data+start,length);
    nbuffer[length] = '\0';

    LISP ncell = strcons(length, nbuffer);

    wfree(nbuffer);

    return ncell;
}

static LISP l_sbefore(LISP atom, LISP before)
{
    // Wraparound for EST_String.before function 
    EST_String pname = get_c_string(atom);
    EST_String b = get_c_string(before);
    EST_String n = pname.before(b);
    
    return strintern(n);
}

static LISP l_safter(LISP atom, LISP after)
{
    // Wraparound for EST_String.after function 
    EST_String pname = get_c_string(atom);
    EST_String a = get_c_string(after);
    EST_String n = pname.after(a);

    return strintern(n);
}

void init_subrs_str(void)
{
 init_lsubr("string-append",string_append,
 "(string-append STR1 STR2 ...)\n\
 Return a string made from the concatenation of the print names of STR1\n\
 STR2 ...");
 init_subr_1("string-length",string_length,
  "(string-length SYMBOL)\n\
 Return the number of characters in the print name of SYMBOL.");
 init_subr_1("print_string",lisp_to_string,
 "(print_string DATA)\n\
  Returns a string representing the printing of DATA." );
 init_subr_1("read-from-string",read_from_lstring,
  "(read-from-string SYMBOL)\n\
 Return first s-expression in print name of SYMBOL.");
 init_subr_1("downcase",string_downcase,
 "(downcase SYMBOL)\n\
  Returns a string with the downcased version of SYMBOL's printname.");
 init_subr_1("upcase",string_upcase,
 "(upcase SYMBOL)\n\
  Returns a string with the upcased version of SYMBOL's printname.");
 init_subr_2("string-matches",l_matches,
 "(string-matches ATOM REGEX)\n\
  Returns t if ATOM's printname matches the regular expression REGEX,\n\
  otherwise it returns nil.");
 init_subr_2("string-equal",l_strequal,
 "(string-equal ATOM1 ATOM2)\n\
  Returns t if ATOM's printname is equal to ATOM's print name, otherwise\n\
  it returns nil.");
 init_subr_3("substring", l_substring,
 "(substring STRING START LENGTH)\n\
  Return a substring of STRING starting at START of length LENGTH.");
 init_subr_2("string-before",l_sbefore,
 "(string-before ATOM BEFORE)\n\
  Returns an atom whose printname is the substring of ATOM's printname \n\
  which appears before BEFORE.  This is a wraparound for the EST_String.before \n\
  function in C++, and hence has the same conditions for boundary cases.");
 init_subr_2("string-after",l_safter,
 "(string-after ATOM AFTER)\n\
  Returns an atom whose printname is the substring of ATOM's printname \n\
  which appears after AFTER.  This is a wraparound for the EST_String.after \n\
  function in C++, and hence has the same conditions for boundary cases.");

 init_lsubr("symbolconc",symbolconc,
 "(symbolconc SYMBOL1 SYMBOL2 ...)\n\
  Form new symbol by concatenation of the print forms of each of SYMBOL1\n\
  SYMBOL2 etc.");
 init_subr_1("symbolexplode",symbolexplode,
 "(symbolexplode SYMBOL)\n\
  Returns list of atoms one for each character in the print name of SYMBOL.");

 init_subr_1("parse-number",parse_number,
 "(parse-number SYMBOL)\n\
  Returns a number form a symbol or string whose print name is a number.");

 init_subr_2("basename",symbol_basename,
 "(basename PATH SUFFIX)\n\
  Return a string with directory removed from basename.  If SUFFIX is\n\
  specified remove that from end of PATH.  Basically the same function\n\
  as the UNIX command of the same name.");


 init_subr_1("path-is-filename", path_is_filename,
 "(path-is-filename PATHNAME)\n\
  Is PATH a non-directory name.");

 init_subr_1("path-as-directory", path_as_directory,
 "(path-as-directory PATHNAME)\n\
  Return PATH as a directory name.");

 init_subr_1("path-as-file", path_as_file,
 "(path-as-file PATHNAME)\n\
  Return PATH as a non-directory name.");

 init_lsubr("path-append", path_append,
 "(path-append DIRECTORY-PATH ADDITION1 ADDITION2 ...)\n\
  Return a the path for ADDITION in DIRECTORY.");

 init_subr_1("path-basename", path_basename,
 "(path-basename PATHNAME)\n\
  Return name part of PATH.");


 init_subr_1("path-is-dirname", path_is_dirname,
 "(path-is-dirname PATHNAME)\n\
  Is PATH a directory name.");

}
