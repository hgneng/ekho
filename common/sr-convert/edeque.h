/*
EDEQUE.H -- this file dates back to before the Standard Template
Library was widely available.

------------

This file is part of the sr-convert sample rate conversion utility
Copyright (c) 2002, 2003 by Edward Kiser

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*/

#ifndef __EDEQUE_H
#define __EDEQUE_H
#else
#error Multiple includes of EDEQUE.H not allowed
#endif

#ifndef __dj_include_assert_h_
#include <assert.h>
#endif

/*
Interface summary:
*/
#if 0

template <class T /*with default constructor and assignment operator*/ >
class edeque
{ public:
  edeque();

  edeque(edeque<T> const & s);
  edeque<T> const & operator=(edeque<T> const & s);
  ~edeque();

  void push_back(T const & d);
  void push_front(T const & d);
  void pop_back();
  void pop_front();

  bool is_empty() const;
  int size() const;
  T const & operator[](int i) const;
  T & operator[](int i);
  private:
  ...
};
#endif

template <class T>
class edeque
{ public:
  edeque(): mitem(0), msize(0), mspace(0), mfirst(0) {};

  edeque(edeque<T> const & s) { copy(s); };

  edeque<T> const & operator=(edeque<T> const & s)
   { drop(); copy(s); return s; };

  ~edeque() { drop(); };

  void push_back(T const & d)
  { if (msize==mspace) realloc((mspace==0)?1:(mspace<<1));
    mitem[adjidx(msize)]=d;
    msize++;
  };

  void push_front(T const & d)
  { if (msize==mspace) realloc((mspace==0)?1:(mspace<<1));
    mfirst=adjidx(-1);
    mitem[mfirst]=d;
    msize++;
  };

  void pop_back()
  { assert(!is_empty());
    msize--;
    if ((msize>>1) == mspace) realloc(mspace>>1);
  };

  void pop_front()
  { assert(!is_empty());
    mfirst=adjidx(1);
    msize--;
    if ((msize>>1) == mspace) realloc(mspace>>1);
  };

  bool is_empty() const { return msize == 0; };

  int size() const { return msize; };

  T const & operator[](int i) const
  { assert(i>=0);
    assert(i<msize);
    int k=adjidx(i);
    assert(k>=0);
    assert(k<mspace);
    return mitem[k];
  };

  T & operator[](int i)
  { assert(i>=0);
    assert(i<msize);
    int k=adjidx(i);
    assert(k>=0);
    assert(k<mspace);
    return mitem[k];
  };

  private:

  void drop()
  { if (mitem) delete[] mitem;
    mspace=0; msize=0; mfirst=0;
  };

  void copy(edeque<T> const & s)
  { mspace=s.mspace;
    msize=s.msize;
    mfirst=0;
    if (s.mitem)
    { mitem=new T[mspace];
      for (int i=0; i<msize; i++)
      { mitem[i]=s.mitem[s.adjidx(i)];
       }
    } else
    { mitem=0;
    }
  };

  void realloc(int newspace)
  { assert(newspace>msize);
    T * big=new T[newspace];
    if (mitem)
    { for (int i=0; i<msize; i++)
      { big[i]=mitem[adjidx(i)];
      };
      delete[] mitem;
    };
    mitem=big;
    mfirst=0;
    mspace=newspace;
  };

  int adjidx(int idx) const
  { idx+=mfirst;
    while (idx<0) idx+=mspace;
    while (idx>=mspace) idx-=mspace;
    return idx;
  };

  T * mitem;
  int msize, mspace, mfirst;
};
