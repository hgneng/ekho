 /*************************************************************************/
 /*                                                                       */
 /*                Centre for Speech Technology Research                  */
 /*                     University of Edinburgh, UK                       */
 /*                        Copyright (c) 1997                             */
 /*                        All Rights Reserved.                           */
 /*                                                                       */
 /*  Permission is hereby granted, free of charge, to use and distribute */
 /*  this software and its documentation without restriction, including  */
 /*  without limitation the rights to use, copy, modify, merge, publish, */
 /*  distribute, sublicense, and/or sell copies of this work, and to     */
 /*  permit persons to whom this work is furnished to do so, subject to  */
 /*  the following conditions:                                           */
 /*   1. The code must retain the above copyright notice, this list of   */
 /*      conditions and the following disclaimer.                        */
 /*   2. Any modifications must be clearly marked as such.               */
 /*   3. Original authors' names are not deleted.                        */
 /*   4. The authors' names are not used to endorse or promote products  */
 /*      derived from this software without specific prior written       */
 /*      permission.                                                     */
 /*                                                                       */
 /*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
 /*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
 /*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
 /*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
 /*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
 /*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
 /*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
 /*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
 /*  THIS SOFTWARE.                                                       */
 /*                                                                       */
 /*************************************************************************/
 /*               Authors :  Alan W Black (awb@cstr.ed.ac.uk)             */
 /*                Date   :  January, February 1997                       */
 /*  -------------------------------------------------------------------- */
 /*                                                                       */
 /*  A non-GNU implementation of a EST_String class to use with non-G++   */
 /*  compilers.                                                           */
 /*                                                                       */
 /*  Note this is not a full implementation of libg++'s EST_String class  */
 /*  just the bits we need                                                */
 /*                                                                       */
 /*************************************************************************/


#include <iostream>
#include <cstring>
#include <cstdio>
#include <cctype>
#include "EST_String.h"
// #include "EST_error.h"
#include "string_version.h"
#include "EST_math.h"

extern "C" {
#include "regexp.h"
}

const char *EST_String::version = "CSTR String Class " STRING_VERSION " " STRING_DATE;

const EST_String EST_String::Empty("");

EST_String EST_String_nullString = "";

struct subst {
  EST_String::EST_string_size start, end;
  char *s;
  int slen;
} ;

#if !__GSUB_REENTRANT__
static struct subst *substitutions=NULL;
int num_substitutions=0;
#endif
  

 /********************************************************************\
 *                                                                    *
 * Locate is the basic utility method behind many of the              *
 * manipulations, it finds something in a EST_String, returns a       *
 * success or failure flag and sets start and end to where it was.    *
 *                                                                    *
 \********************************************************************/

int EST_String::locate(const char *s, int len, int from, int &start, int &end) const
{
  CHECK_STRING_ARG(s);
      
  const char *sub=NULL;

  if (!s)
    return 0;

  if (from < 0 && -from < size)
    {
      int endpos=size+from+1;
      int p=0;
      const char *nextsub;

      while ((nextsub=strstr(str()+p, s)))
	{
	  p=nextsub-str()+1;
	  if (p > endpos)
	    break;
	  sub=nextsub;
	}
    }
  else if (from>=0 && from <= size)
    sub= strstr(str()+from, s);
  
  if (sub != NULL)
    {
      start = sub-str();
      end = start + len;
      return 1;
    }
  else
    {
      return 0;
    }

}

int EST_String::locate(EST_Regex &ex, int from, int &start, int &end, int *starts, int *ends) const
{
  int match_start, match_end;

  if (from < 0 && -from < size)
    {
      int endpos=size+from+1;
      int p=0;
      int found=0;

      while (ex.run(str(), p, match_start, match_end, starts, ends))
	{
	  found++;
	  start=match_start;
	  end=match_end;
	  p = match_start+1;
	  if (p > endpos)
	    break;
	}
      return found >0;
    }
  else if (from >=0 && from <= size)
    {
      if (ex.run(str(), from, match_start, match_end, starts, ends))
	{
	  start = match_start;
	  end=match_end;
	  return 1;
	}
      else
	return 0;
    }
  else
    return 0;
}

int EST_String::extract(const char *s, int len, int pos, int &start, int &end) const
{
  CHECK_STRING_ARG(s);
      
  if (!s)
    return 0;

  if (pos < 0)
    return locate(s, len, 0, start, end);

  if (pos <= size-len && memcmp(str()+pos, s, len)==0)
    {
      start = pos;
      end = pos + len;
      return 1;
    }
  else
      return 0;
}

int EST_String::extract(EST_Regex &ex, int pos, int &start, int &end) const
{
  int match_start, match_end;

  if (pos < 0)
    return locate(ex, 0, start, end);

  if (pos < size && ex.run(str(), pos, match_start, match_end) && match_start == pos)
    {
      start = match_start;
      end = match_end;
      return 1;
    }
  else
      return 0;
}

EST_String EST_String::chop_internal(int from, int len, EST_chop_direction mode) const
{
  int start, end;

  if (from < 0)
    {
      start = size+from;
    }
  else
    {
      start = from;
    }

  end=start+len;

  if (start >=0 && end <=size && size > 0)
    switch (mode)
      {
      case Chop_Before:
	return EST_String(str(), size, 0, start); break;
      case Chop_At:
	return EST_String(str(), size, start, end-start); break;
      case Chop_After:
	return EST_String(str(), size, end, -1);
      }
  return EST_String();

}

EST_String EST_String::chop_internal(const char *it, int len, int from, EST_chop_direction mode) const
{
  CHECK_STRING_ARG(it);
      
  int start, end;
  
  if (it && locate(it, len, from, start, end))
    switch (mode)
      {
      case Chop_Before:
	return EST_String(str(), size, 0, start); break;
      case Chop_At:
	return EST_String(str(), size, start, end-start); break;
      case Chop_After:
	return EST_String(str(), size, end, -1);
      }
  return EST_String();

}

EST_String EST_String::chop_internal (EST_Regex &it, int from, EST_chop_direction mode) const
{
  int start=0, end=0;
  
  if (locate(it, from, start, end))
    switch (mode)
      {
      case Chop_Before:
	return EST_String(str(), size, 0, start); break;
      case Chop_At:
	return EST_String(str(), size, start, end-start); break;
      case Chop_After:
	return EST_String(str(), size, end, -1);
      }
  return EST_String();

}


int EST_String::gsub_internal (const char *os, int olength, const char *s, int length)
{
  CHECK_STRING_ARG(os);
  CHECK_STRING_ARG(s);
      
  int pos=0, n=0, change=0;
  EST_ChunkPtr new_memory;

  const char *from;
  char *to;
  
#if __GSUB_REENTRANT__
  struct subst {
    EST_String::EST_string_size start, end;
  } *substitutions=NULL;

  int num_substitutions=0;
#endif

  if (s && os && size > 0 && *os != '\0')
    {
      {
	int start, end;
	while (locate(os, olength, pos, start, end))
	  {
	    if (num_substitutions <= n)
	      substitutions = wrealloc(substitutions, struct subst, (num_substitutions +=10));
	    
	    substitutions[n].start = start;
	    substitutions[n].end = end;
	    
	    change += length - (end-start);
	    
	    n++;
	    pos=end;
	  }
      }

      // dubious dealings with the inside of the string

      from = (const char *)memory;

      if (change > 0)
	{
	  // Spurious braces make temporary ref to chunk go away
	  {new_memory = chunk_allocate(size+change+1);}
	  to = new_memory;
	}
      else
	{
	  cp_make_updatable(memory, size);
	  to = memory;
	}

      int i, at=0;
      char *p=to;
      
      for(i=0; i<n; i++)
	{
	  int start = substitutions[i].start;
	  int end = substitutions[i].end;
	  memcpy(p, from+at, start-at);
	  p += start-at;
	  memcpy(p, s, length);
	  p += length;
	  at=end;
	}
      memcpy(p, from+at, size-at);

      p += size-at;
      *p = '\0';
  
      if (change > 0)
	memory = new_memory; 
	  

      size += change;
    }

  //  cout << "gsub n=" << memory.count() << "\n";

#if __GSUB_REENTRANT__
  if (substitutions)
    wfree(substitutions);
#endif

  return n;

}

int EST_String::gsub_internal (EST_Regex &ex, const char *s, int length)
{

  int bracket_num=-1;

  if (s==NULL)
    bracket_num = length;
      
  int pos=0, n=0, change=0;
  EST_ChunkPtr new_memory;

  const char *from;
  char *to;
  
#if __GSUB_REENTRANT__
  struct subst *substitutions=NULL;

  int num_substitutions=0;
#endif

  // printf("match '%s'\n", (const char *)(*this));

  if (size > 0)
    {
      {
	int start, starts[EST_Regex_max_subexpressions], ends[EST_Regex_max_subexpressions], mlen;
	while ((start = search(ex, mlen, pos, starts, ends))>=0)
	  {
	    // printf("match %d-%d, %d-%d, %d-%d\n", start, start+mlen, starts[0], ends[0], starts[1], ends[1]);
	    if (num_substitutions <= n)
	      substitutions = wrealloc(substitutions, struct subst, (num_substitutions +=10));
	    
	    substitutions[n].start = start;
	    substitutions[n].end = start+mlen;
	    
	    if (s)
	      change += length - mlen;
	    else
	      {
		int slen = ends[bracket_num]-starts[bracket_num];
		change += slen - mlen;
		substitutions[n].slen = slen;
		substitutions[n].s = walloc(char, slen);
		memcpy(substitutions[n].s, (const char *)memory+starts[bracket_num], slen);
	      
	      }
	    
	    n++;
	    pos=start+mlen;
	  }
      }

      // dubious dealings with the inside of the string

      from = (const char *)memory;

      if (change > 0)
	{
	  // Spurious braces make temporary ref to chunk go away
	  {new_memory = chunk_allocate(size+change+1);}
	  to = new_memory;
	}
      else
	{
	  cp_make_updatable(memory, size);
	  to = memory;
	}

      int i, at=0;
      char *p=to;
      
      for(i=0; i<n; i++)
	{
	  int start = substitutions[i].start;
	  int end = substitutions[i].end;
	  memcpy(p, from+at, start-at);
	  p += start-at;
	  if (s)
	    {
	      memcpy(p, s, length);
	      p += length;
	    }
	  else
	    {
	      memcpy(p, substitutions[i].s, substitutions[i].slen);
	      wfree(substitutions[i].s);
	      substitutions[i].s=NULL;
	      p += substitutions[i].slen;
	    }
	  at=end;
	}
      memcpy(p, from+at, size-at);

      p += size-at;
      *p = '\0';
  
      if (change > 0)
	memory = new_memory; 
      
      size += change;
    }

#if __GSUB_REENTRANT__
  if (substitutions)
    wfree(substitutions);
#endif

  return n;

}

int EST_String::subst(EST_String source, 
		      int (&starts)[EST_Regex_max_subexpressions], 
		      int (&ends)[EST_Regex_max_subexpressions])
{
  int n=0, change=0;
  EST_ChunkPtr new_memory;

  const char *from;
  char *to;
  
#if __GSUB_REENTRANT__
  struct subst *substitutions=NULL;

  int num_substitutions=0;
#endif

  // printf("match '%s'\n", (const char *)(*this));

  int i;
  if (size > 0)
    {
    int escaped=0;

      for(i=0; i<size; i++)
	{
	  if (escaped)
	    {
	      if (memory[i] >= '0' &&memory[i] <= '9')
		{
		  int snum = memory[i] - '0';
		  if (ends[snum] >= 0 && starts[snum] >=0)
		    {
		      if (num_substitutions <= n)
			substitutions = wrealloc(substitutions, struct subst, (num_substitutions +=10));
		
		      substitutions[n].start = i-1;
		      substitutions[n].end = i+1;
		      substitutions[n].s = ((char *)(void *)(const char *)source.memory) + starts[snum];
		      substitutions[n].slen = ends[snum] - starts[snum];
		      change += substitutions[n].slen - 2;

		      n++;
		    }
		}
	      escaped=0;
	    }
	  else if (memory[i] == '\\')
	    escaped=1;
	}


      // dubious dealings with the inside of the string

      from = (const char *)memory;

      if (change > 0)
	{
	  // Spurious braces make temporary ref to chunk go away
	  {new_memory = chunk_allocate(size+change+1);}
	  to = new_memory;
	}
      else
	{
	  cp_make_updatable(memory, size);
	  to = memory;
	}

      int at=0;
      char *p=to;
      
      for(i=0; i<n; i++)
	{
	  int start = substitutions[i].start;
	  int end = substitutions[i].end;
	  memcpy(p, from+at, start-at);
	  p += start-at;

	  memcpy(p, substitutions[i].s, substitutions[i].slen);
	  substitutions[i].s=NULL;
	  p += substitutions[i].slen;
	  at=end;
	}
      memcpy(p, from+at, size-at);

      p += size-at;
      *p = '\0';
  
      if (change > 0)
	memory = new_memory; 
      
      size += change;
    }

#if __GSUB_REENTRANT__
  if (substitutions)
    wfree(substitutions);
#endif

  return n;
}

// Pass in the two possible separators as pointers so we don't have to
// duplicate all the code. Inline definitions of the friend functions
// takes care of the pretty interface.

int EST_String::split_internal(EST_String result[], int max, 
			       const char *s_seperator, int slen,
			       EST_Regex *re_seperator, 
			       char quote) const
{
  int n=0;
  int pos=0;
  int start, end;
  int lastspace=0;

  if (size>0)
    {
      while (pos < length())
	{
	  start= -1;
	  end= -1;
	  if ((*this)(pos) == quote)
	    {
	      start=pos;
	      pos++;
	      while (pos < length())
		{
		  if ((*this)(pos) == quote)
		    {
		      pos++;
		      if ((*this)(pos) != quote)
			break;
		      else
			pos++;
		    }
		  else
		    pos++;
		}
	      end=pos;
	    }
	  else
	    {
	      int mstart, mend, matched;
	      if (s_seperator)
		matched = locate(s_seperator, slen, pos, mstart, mend);
	      else
		matched = locate(*re_seperator, pos, mstart, mend);
	      
	      if (matched)
		if (mstart != pos)
		  {
		    start=pos;
		    end=mstart;
		    pos=mend;
		    lastspace=mend;
		  }
		else if (pos ==lastspace)
		  {
		    start=pos;
		    end=pos;
		    pos=mend;
		    lastspace=mend;
		  }
		else
		  {
		    pos=mend;
		    lastspace=mend;
		  }
	      else
		{
		  start=pos;
		  end=length();
		  pos=end;
		}
	    }
	  if (start>=0)
	    result[n++] = EST_String(*this, start, end-start);
	  if (n==max)
	    break;
	}
    }

  return n;
}

int EST_String::matches(const char *s, int pos) const
{
  CHECK_STRING_ARG(s);
      
  int start, end;

  if (!s)
    return 0;

  int len=safe_strlen(s);

  if (extract(s, len, pos, start, end))
      return start==pos && end==len;
  else
      return 0;
}

int EST_String::matches(const EST_String &s, int pos) const
{
  int start, end;

  if (extract(s.str(), s.size, pos, start, end))
      return start==pos && end==s.size;
  else
      return 0;
}

int EST_String::matches(EST_Regex &e, int pos, int *starts, int *ends) const
{
  if (size==0)
    return e.run_match("", pos, starts, ends) >0;
  else
    return e.run_match(str(), pos, starts, ends) >0; 
}


EST_String operator + (const EST_String &a, const char *b)
{
  CHECK_STRING_ARG(b);

    int al = a.size;
    int bl = safe_strlen(b);

    if (al == 0)
      return EST_String(b, 0, bl);
    if (bl == 0)
      return EST_String(a);

    EST_ChunkPtr c = chunk_allocate(al+bl+1, a.str(), al);

    if (bl>0)
      memmove((char *)c + al, b, bl);
    c(al+bl)='\0';

    return EST_String(al+bl, c);
}

EST_String operator + (const EST_String &a, const EST_String &b)
{
    int al = a.size;
    int bl = b.size;

    if (al == 0)
      return EST_String(b);
    if (bl == 0)
      return EST_String(a);

    EST_ChunkPtr c = chunk_allocate(al+bl+1, a.str(), al);

    memmove((char *)c+al,b.str(),bl);
    c(al+bl)='\0';

    return EST_String(al+bl, c);
}

EST_String operator + (const char *a, const EST_String &b)
{
  CHECK_STRING_ARG(a);

    int al = safe_strlen(a);
    int bl = b.size;

    if (bl == 0)
      return EST_String(a, 0, al);
    if (al == 0)
      return EST_String(b);

    EST_ChunkPtr c = chunk_allocate(al+bl+1, a, al);

    memmove((char *)c + al, b.str(), bl);

    c(al+bl)='\0';

    return EST_String(al+bl, c);
}

EST_String operator * (const EST_String &s, int n)
{

  if (n<1)
    return "";

  int l = s.length();
  int sz = n * l;

  EST_String it(NULL, 0, sz);
  
  for(int j=0; j<n; j++)
    strncpy(((char *)it)+j*l, (const char *)s, l);

  return it;
}

EST_String &EST_String::operator +=(const char *b) 

{
  CHECK_STRING_ARG(b);
      
    int bl = safe_strlen(b);

    if (size == 0)
      {
	memory = chunk_allocate(bl+1, b, bl);
	size = bl;
	return *this;
      }

    grow_chunk(memory, size, size+bl+1);
    
    memmove((char *)memory + size,b,bl);
    memory(size+bl)='\0';
    size += bl;

    return *this;
}

EST_String &EST_String::operator += (const EST_String b) 

{
    int bl = b.size;

    if (size == 0)
      {
	memory = NON_CONST_CHUNKPTR(b.memory);
	size = b.size;
	return *this;
      }

    grow_chunk(memory, size, size+bl+1);

    if (bl >0)
      memmove((char *)memory + size,b.str(),bl);

    memory(size+bl)='\0';
    size += bl;

    return *this;
}

EST_String::EST_String(const char *s) 
{
      CHECK_STRING_ARG(s);
      
      size=safe_strlen(s);

       if (size != 0)
	 memory = chunk_allocate(size+1, s, size); 
       else 
	 memory=NULL;
    }


EST_String::EST_String(const char *s, int start_or_fill, int len) 
{

  if (s)
    {
      int start= start_or_fill;
      if (len <0)
	len=safe_strlen(s)-start;
      
      size=len;
      if (size != 0)
	memory = chunk_allocate(len+1, s+start, len);
      else
	memory=NULL;
    }
  else
    {
      char fill = start_or_fill;
      if (len<0) len=0;
      size=len;
      if (size != 0)
	{
	  memory = chunk_allocate(len+1);
	  char *p = memory;
	  for(int j=0; j<len;j++)
	    p[j] = fill;
	  p[len]='\0';
	}
      else
	memory=NULL;
    }
}

EST_String::EST_String(const char *s, int s_size, int start, int len) 
{
  CHECK_STRING_ARG(s);
      
  if (len <0)
    len=s_size-start;

  size=len;
  if (size != 0)
    memory = chunk_allocate(len+1, s+start, len);
  else
    memory=NULL;
}

EST_String::EST_String(const EST_String &s, int start, int len) 
{
  if (len <0)
    len=s.size-start;
      
  size=len;

  if (start == 0 && len == s.size)
    memory = NON_CONST_CHUNKPTR(s.memory);
  else if (size != 0)
    memory = chunk_allocate(len+1, s.memory, start, len);
  else
    memory = NULL;
}

/*
EST_String::EST_String(const EST_String &s) 
{
#if 1
  static EST_ChunkPtr hack = NON_CONST_CHUNKPTR(s.memory);
  memory = NON_CONST_CHUNKPTR(s.memory);
  size = s.size;
#else
    *(struct EST_dumb_string *)this = *(struct EST_dumb_string *)(&s);
#endif
}
*/

#if __FSF_COMPATIBILITY__
EST_String::EST_String(const char c) 
{
      size=1;
      memory= chunk_allocate(2, &c, 1);
}
#endif

EST_String &EST_String::operator = (const char *str) 
{
      CHECK_STRING_ARG(str);
      int len = safe_strlen(str);
      if (!len)
	memory = NULL;
      else if (!shareing() && len < size)
	memcpy((char *)memory, str, len+1);
      else if (len)
	memory = chunk_allocate(len+1, str, len);
      size=len;
      return *this;
}

EST_String &EST_String::operator = (const char c)
{
      memory = chunk_allocate(2, &c, 1);
      size=1;
      return *this;
}

EST_String &EST_String::operator = (const EST_String &s) 
{
#if 1
/*  static EST_ChunkPtr hack = s.memory;  */
  memory = NON_CONST_CHUNKPTR(s.memory);
  size = s.size;
#else
      *(struct EST_dumb_string *)this = *(struct EST_dumb_string *)(&s);
#endif
      return *this;
}
    
EST_String downcase(const EST_String &s)
{
    EST_String t = EST_String(s.size, chunk_allocate(s.size+1, s.str(), s.size));
    int i;
	/*!!! begin change by hgneng !!!*/
	/* original code
    for (i=0; i < s.length(); i++)
	if (isupper(s(i)))
	    t[i] = tolower(s(i));
	else
	    t[i] = s(i);
	*/

	for (i=0; i < s.length(); i++) {
		unsigned char c = (unsigned char)s(i);
		if (isupper(c))
			t[i] = tolower(c);
		else
			t[i] = s(i);
	}
	/*!!! end change by hgneng !!!*/

    return t;
}	

EST_String upcase(const EST_String &s)
{
    EST_String t = EST_String(s.size, chunk_allocate(s.size+1, s.str(), s.size));
    int i;

    for (i=0; i < s.length(); i++)
	if (islower(s(i)))
	    t[i] = toupper(s(i));
	else
	    t[i] = s(i);
    return t;
}	


int
EST_String::freq(const EST_String &s) const
{
  int pos=0;
  int n=0;
  int start, end;

  while (locate(s, pos, start, end))
    {
      n++;
      pos=end;
    }
  return n;
}

int
EST_String::freq(const char *s) const
{
  CHECK_STRING_ARG(s);
      
  int pos=0;
  int n=0;
  int start, end;
  int len=safe_strlen(s);

  while (locate(s, len, pos, start, end))
    {
      n++;
      pos=end;
    }
  return n;
}

int
EST_String::freq(EST_Regex &ex) const
{
  int pos=0;
  int n=0;
  int start, end=0;

  while (locate(ex, pos, start, end))
    {
      n++;
      pos=end;
    }
  return n;
}

EST_String EST_String::quote(const char quotec) const
{

  const char quotequote[3] = {quotec, quotec, '\0'};

  EST_String result(*this);

  result.gsub(quotequote+1, quotequote+0);

  return EST_String::cat(quotequote+1, result, quotequote+1);   
}

EST_String EST_String::unquote(const char quotec) const
{

  const char quotequote[3] = {quotec, quotec, '\0'};

  EST_String result(*this);

  // cout << "before unqote '" << result << "'\n";

  result.gsub(quotequote+0, quotequote+1);

  //  cout << "after unqote '" << result << "'\n";

  if (result[0] == quotec && result[result.length()-1] == quotec )
    {
#if 1
      /* Spurious local variable to get arounf SunCC 4.0 being broken */
      EST_String res= result.at(1, result.length()-2);
      return res;
#else
      return result.at(1, result.length()-2);
#endif
    }
  else
    return result;
}

EST_String EST_String::quote_if_needed(const char quotec) const
{

  if (contains(RXwhite) || contains(quotec))
    return quote(quotec);

  return *this;
}


EST_String EST_String::unquote_if_needed(const char quotec) const
{

  if ((*this)(0) == quotec && (*this)(length()-1) == quotec )
    return unquote(quotec);

  return *this;
}

ostream &operator << (ostream &s, const EST_String &str)

{
  if (str.size > 0)
    return (s << str.str());
  else
    return (s << "");
}

EST_String EST_String::cat(const EST_String s1, 
			   const EST_String s2, 
			   const EST_String s3, 
			   const EST_String s4, 
			   const EST_String s5,
			   const EST_String s6,
			   const EST_String s7,
			   const EST_String s8,
			   const EST_String s9
			   )
{
  int   len=(s1.length()+s2.length()+s3.length()+s4.length()+s5.length() +
	     s6.length()+s7.length()+s8.length()+s9.length());

  EST_String result;

  result.size=len;
  result.memory= chunk_allocate(len+1, (const char *)s1, s1.length());

  int p = s1.length();
  if (s2.length())
    { strncpy((char *)result.memory + p, (const char *)s2, s2.length()); p += s2.length(); }
  if (s3.length())
    { strncpy((char *)result.memory + p, (const char *)s3, s3.length()); p += s3.length(); }
  if (s4.length())
    { strncpy((char *)result.memory + p, (const char *)s4, s4.length()); p += s4.length(); }
  if (s5.length())
    { strncpy((char *)result.memory + p, (const char *)s5, s5.length()); p += s5.length(); }
  if (s6.length())
    { strncpy((char *)result.memory + p, (const char *)s6, s6.length()); p += s6.length(); }
  if (s7.length())
    { strncpy((char *)result.memory + p, (const char *)s7, s7.length()); p += s7.length(); }
  if (s8.length())
    { strncpy((char *)result.memory + p, (const char *)s8, s8.length()); p += s8.length(); }
  if (s9.length())
    { strncpy((char *)result.memory + p, (const char *)s9, s9.length()); p += s9.length(); }

    result.memory(p) = '\0';

  return result;
}

int compare(const EST_String &a, const EST_String &b)
{
    if (a.size == 0 && b.size == 0)
	return 0;
    else if (a.size == 0)
	return -1;
    else if (b.size == 0)
	return 1;
    else 
	return strcmp(a.str(), b.str());
}

int compare(const EST_String &a, const char *b)
{
    if (a.size == 0 && (b==NULL || *b == '\0'))
	return 0;
    else if (a.size == 0)
	return -1;
    else if (b == NULL || *b == '\0')
	return 1;
    else 
	return strcmp(a.str(), b);
}

int fcompare(const EST_String &a, const EST_String &b, 
				const unsigned char *table) 
{
    if (a.size == 0 && b.size == 0)
	return 0;
    else if (a.size == 0)
	return -1;
    else if (b.size == 0)
	return 1;
    else 
	return EST_strcasecmp(a.str(), b.str(), table);
}

int fcompare(const EST_String &a, const char *b, 
				const unsigned char *table) 
{
    int bsize = (b ? strlen((const char *)b) : 0);
    if (a.size == 0 && bsize == 0)
	return 0;
    else if (a.size == 0)
	return -1;
    else if (bsize == 0)
	return 1;
    else 
	return EST_strcasecmp(a.str(), (const char *)b, table);
}

int operator == (const char *a, const EST_String &b)
{
    CHECK_STRING_ARG(a);
      
    if (!a)
	return 0;
    else if (b.size==0)
	return *a == '\0';
    else 
	return (*a == b(0)) && strcmp(a, b.str())==0;
}

int operator == (const EST_String &a, const EST_String &b)
{
    if (a.size==0)
	return b.size == 0;
    else if (b.size == 0)
	return 0;
    else 
	return a.size == b.size && a(0) == b(0) && memcmp(a.str(),b.str(),a.size)==0;
};

EST_String EST_String::Number(int i, int b)
{
  char buf[64];
  const char *format;

  switch (b)
    {
    case 8:
      format="0%o";
      break;
    case 10:
      format="%d";
      break;
    case 16:
      format="0x%x";
      break;
    default:
      format="??%d??";
      break;
    }
  sprintf(buf, format, i);

  return EST_String(buf);
}

EST_String EST_String::Number(long i, int b)
{
  char buf[64];
  const char *format;

  switch (b)
    {
    case 8:
      format="0%lo";
      break;
    case 10:
      format="%ld";
      break;
    case 16:
      format="0x%lx";
      break;
    default:
      format="??%ld??";
      break;
    }
  sprintf(buf, format, i);

  return EST_String(buf);
}

EST_String EST_String::Number(float f)
{
  char buf[64];

  sprintf(buf, "%f", f);

  return EST_String(buf);
}

EST_String EST_String::Number(double d)
{
  char buf[64];

  sprintf(buf, "%f", d);

  return EST_String(buf);
}

long EST_String::Long(bool *valid) const
{
  char *end;

  long val = strtol(str(), &end, 10);

  if (end==NULL|| *end != '\0')
  {
    if (valid != NULL)
      {
	*valid=0;
	return 0L;
      }
    else
      {
	printf("bad integer number format '%s'\n",
		(const char *)str());
	exit(0);
      }
  }

  if (valid)
    *valid=1;
  
  return val;
}

int EST_String::Int(bool *valid) const
{
  long val = Long(valid);

  if (valid && !*valid)
    return 0L;

  if (val > INT_MAX || val < INT_MIN)
  {
    if (valid != NULL)
      {
	*valid=0;
	return 0L;
      }
    else
      {
	printf("number out of range for integer %ld",
	       val);
	exit(0);
      }
  }

  return val;
}

double EST_String::Double(bool *valid) const
{
  char *end;

  double val = strtod(str(), &end);

  if (end==NULL|| *end != '\0')
  {
    if (valid != NULL)
      {
	*valid=0;
	return 0.0;
      }
    else
      {
	printf("bad decimal number format '%s'",
		(const char *)str());
	exit(0);
      }
  }

  if (valid)
    *valid=1;
  
  return val;
}

float EST_String::Float(bool *valid) const
{
  double val = Double(valid);

  if (valid && !*valid)
    return 0.0;

  if (val > FLT_MAX || val < -FLT_MAX) 
  {
    if (valid != NULL)
      {
	*valid=0;
	return 0.0;
      }
    else
      {
	printf("number out of range for float %f",
	       val);
	exit(0);
      }
  }

  return val;
}



