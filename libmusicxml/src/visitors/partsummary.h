/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __partsummary__
#define __partsummary__

#include <map>
#include <vector>

#include "notevisitor.h"
#include "smartlist.h"

namespace MusicXML2
{

/*!
\addtogroup visitors
@{
*/

/*!
\brief Produces a summary of a MusicXML part.

	A part summary consists in 
	- a count of staves
	- a map that associate each stave with the corresponding number of notes
	- a map that associate each stave with the corresponding voices and notes
*/
class EXP partsummary : 
	public notevisitor,
	public visitor<S_part>,
	public visitor<S_staves>
{
	public:
				 partsummary() : fStavesCount(1) {};
		virtual	~partsummary() {};
		
		//! returns the number of staves for the part
		int countStaves () const				{ return fStavesCount; }
		//! returns the number of voices 
		int countVoices () const				{ return fVoices.size(); }
		//! returns the number of voices on a staff 
		int countVoices (int staff) const;

		//! returns the staff ids list
		smartlist<int>::ptr getStaves() const;
		//! returns the staff ids list for one voice
		smartlist<int>::ptr getStaves (int voice) const;
		//! returns the count of notes on a staff
		int getStaffNotes (int id) const;

		//! returns the voices ids list
		smartlist<int>::ptr getVoices () const;
		//! returns the voices ids list for one staff
		smartlist<int>::ptr getVoices (int staff) const;
		//! returns the id of the staff that contains the more of the voice notes
		int getMainStaff (int voiceid) const;
		//! returns the count of notes on a voice
		int getVoiceNotes (int voiceid) const;
		//! returns the count of notes on a voice and a staff
		int getVoiceNotes (int staffid, int voiceid) const;
		
	protected:
		virtual void visitStart ( S_part& elt);
		virtual void visitStart ( S_staves& elt);
		virtual void visitEnd   ( S_note& elt);
		
	private:
		// count of staves (from the staves element)
		int		fStavesCount;
		// staves and corresponding count of notes
		std::map<int, int>	fStaves;
		// voices and corresponding count of notes
		std::map<int, int>	fVoices;
		// staves and corresponding voices + count of notes
		std::map<int, std::map<int, int> >	fStaffVoices;
};

/*! @} */

}

#endif
