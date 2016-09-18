/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __unrolled_xml_tree_browser__
#define __unrolled_xml_tree_browser__

#include <map>
#include "visitor.h"
#include "xml.h"
#include "browser.h"
#include "typedefs.h"

namespace MusicXML2 
{

/*!
\addtogroup MusicXML
@{
*/

//______________________________________________________________________________
/*!
\brief An xml tree browser that "unroll" the score.

  To "unroll" the score, structural information denoted by \b repeat, 
  \b ending, \b coda and \b segno signs is interpreted and the score
  is visited similarly to a musician that would play the score ie:
  for example a section repeated twice is visited twice.
  
  The unrolled_xml_tree_browser makes use of iterators collected along the visit 
  and makes arbitrary jumps to these iterators to modify the tree visit. These iterators are expected
  to contains S_measure iterators only. This is ensured by the visit(S_part) method.

  When visiting measures, forwarding the visit to the embedded visitor depends on the current state. 
  In particular, it is verified that the current time a given measure is played corresponds to the expected 
  time (if any), and that we're not currently jumping to a coda, which location could be unknown at the
  time of the jump.

\todo Management of multiple jump and sound \b ForwardRepeat attribute.
*/
class EXP unrolled_xml_tree_browser : public browser<xmlelement>,
	public visitor<Sxmlelement>,
	public visitor<S_measure>,
	public visitor<S_ending>,
	public visitor<S_repeat>,
	public visitor<S_part>,
	public visitor<S_sound>
{
	private:
		typedef struct { int current; int next; } state;
		std::map<S_repeat,int>	fRepeatMap;			// used to avoid loops in miswritten scores

		enum { kUndefined=-1, kNoInstruction=0 };
		enum { kNoJump,  kDaCapo, kDalSegno, kToCoda };

		bool	fForward;		///< a boolean to control forwarding of the current measure visit to the visitor
		state	fJump;			///< indicates the current and next jump state
		state	fRound;			///< the current time we're visiting a section
		state	fExpectedRound;	///< the expected time to visit a section (0 when no instruction)
		int		fSectionIndex;	///< the current measure number within the current section
		
		void reset();

	protected:
		basevisitor*	fVisitor;

		ctree<xmlelement>::literator	fFirstMeasure;		///< the first measure iterator
		ctree<xmlelement>::literator	fForwardRepeat;		///< the forward repeat location
		ctree<xmlelement>::literator	fEndIterator;		///< indicates the end of to visit
		ctree<xmlelement>::literator	fNextIterator;		///< indicates the next elt to visit
		ctree<xmlelement>::literator	fSegnoIterator;		///< the segno location
		ctree<xmlelement>::literator	fCodaIterator;		///< the segno location
		ctree<xmlelement>::literator*	fStoreIterator;		///< a pointer to store the current iterator
		int	fStoreDelay;		///< an iterations counter for delayed store

	public:
				 unrolled_xml_tree_browser(basevisitor* v) : fForward(true), fVisitor(v) {}
		virtual ~unrolled_xml_tree_browser() {}

		virtual void visitStart( Sxmlelement& elt);
		virtual void visitStart( S_measure& elt);
		virtual void visitStart( S_ending& elt);
		virtual void visitStart( S_repeat& elt);
		virtual void visitStart( S_part& elt);
		virtual void visitStart( S_sound& elt);

		virtual void browse (xmlelement& t);
		virtual void forwardBrowse (xmlelement& t);
		virtual void enter (xmlelement& t)		{ t.acceptIn(*fVisitor); }
		virtual void leave (xmlelement& t)		{ t.acceptOut(*fVisitor); }
};

/*! @} */

} // namespace MusicXML2


#endif
