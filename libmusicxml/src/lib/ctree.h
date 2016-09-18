/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __ctree__
#define __ctree__

#include <iostream>
#include <stack>
#include <vector>
#include <iterator>
#include "smartpointer.h"
#include "visitable.h"

namespace MusicXML2 
{

//______________________________________________________________________________
template <typename T> class EXP treeIterator : public std::iterator<std::input_iterator_tag, T>
{
	protected:
		typedef typename std::vector<T>::iterator nodes_iterator;
		typedef std::pair<nodes_iterator, T> state;

		std::stack<state>	fStack;
		T					fRootElement;
		nodes_iterator		fCurrentIterator;

	public:
				 treeIterator() {}
				 treeIterator(const T& t, bool end=false) {
					 fRootElement = t;
					 if (end) fCurrentIterator = t->elements().end();
					 else forward_down (t);
				 }
				 treeIterator(const treeIterator& a)  { *this = a; }
		virtual ~treeIterator() {}
		
		T operator  *() const	{ return *fCurrentIterator; }
		T operator ->() const	{ return *fCurrentIterator; } 
		
		//________________________________________________________________________
		T getParent() const		{ return fStack.size() ? fStack.top().second : fRootElement; }
		
		//________________________________________________________________________
		// current element has sub-elements: go down to sub-elements first			
		virtual void forward_down(const T& t) {
			fCurrentIterator = t->elements().begin();
			if (fCurrentIterator != t->elements().end())
				fStack.push( make_pair(fCurrentIterator+1, t));
		}

		//________________________________________________________________________
		// current element is empty: go up to parent element and possibly down to neighbor element
		void forward_up() {
			while (fStack.size()) {
				state s = fStack.top();
				fStack.pop();

				fCurrentIterator = s.first;
				if (fCurrentIterator != s.second->elements().end()) {
					fStack.push( make_pair(fCurrentIterator+1, s.second));
					return;
				}
			}
		}
		
		//________________________________________________________________________
		// move the iterator forward
		void forward() {
			if ((*fCurrentIterator)->size()) forward_down(*fCurrentIterator);
			else forward_up();
		}
		treeIterator& operator ++()		{ forward(); return *this; }
		treeIterator& operator ++(int)	{ forward(); return *this; }

		//________________________________________________________________________
		treeIterator& erase() {
			T parent = getParent();
			fCurrentIterator = parent->elements().erase(fCurrentIterator);
			if (fStack.size()) fStack.pop();
			if (fCurrentIterator != parent->elements().end()) {
				fStack.push( make_pair(fCurrentIterator+1, parent));
			}
			else forward_up();
			return *this; 
		}

		//________________________________________________________________________
		treeIterator& insert(const T& value) {
			T parent = getParent();
			fCurrentIterator = parent->elements().insert(fCurrentIterator, value);
			if (fStack.size()) fStack.pop();
			fStack.push( make_pair(fCurrentIterator+1, parent));
			return *this;
		}

		//________________________________________________________________________
		bool operator ==(const treeIterator& i) const		{ 
			// we check that the iterators have the same parent (due to iterator compatibility issue with visual c++)
			return getParent() == i.getParent() ?  ( fCurrentIterator==i.fCurrentIterator ) : false;
		}
		bool operator !=(const treeIterator& i) const		{ return !(*this == i); }
};

/*!
\brief a simple tree representation
*/
//______________________________________________________________________________
template <typename T> class EXP ctree : virtual public smartable
{
	public:
		typedef SMARTP<T>					treePtr;	///< the node sub elements type
		typedef std::vector<treePtr>		branchs;	///< the node sub elements container type
		typedef typename branchs::iterator	literator;	///< the current level iterator type
		typedef treeIterator<treePtr>		iterator;	///< the top -> bottom iterator type

		static treePtr new_tree() { ctree<T>* o = new ctree<T>; assert(o!=0); return o; }
		
		branchs& elements()						{ return fElements; }		
		const branchs& elements() const			{ return fElements; }		
		virtual void push (const treePtr& t)	{ fElements.push_back(t); }
		virtual int  size  () const				{ return fElements.size(); }
		virtual bool empty () const				{ return fElements.size()==0; }

		iterator begin()			{ treePtr start=dynamic_cast<T*>(this); return iterator(start); }
		iterator end()				{ treePtr start=dynamic_cast<T*>(this); return iterator(start, true); }
		iterator erase(iterator i)	{ return i.erase(); }
		iterator insert(iterator before, const treePtr& value)	{ return before.insert(value); }
		
		literator lbegin() { return fElements.begin(); }
		literator lend()   { return fElements.end(); }

	protected:
				 ctree() {}
		virtual ~ctree() {}

	private:
		branchs	 fElements;
};


}

#endif
