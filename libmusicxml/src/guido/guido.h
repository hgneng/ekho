/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __guido__
#define __guido__

#include <vector>
#include <string>

#include "exports.h"
#include "smartpointer.h"

namespace MusicXML2 
{

class guidovisitor;
class guidoelement;
class guidoparam;
typedef SMARTP<guidoelement> 	Sguidoelement;
typedef SMARTP<guidoparam> 		Sguidoparam;

EXP std::ostream& operator<< (std::ostream& os, const Sguidoelement& elt);

/*!
\addtogroup guido
@{
*/

/*!
\brief A guidotag parameter representation.

	A parameter is represented by its value.
*/
class EXP guidoparam : public smartable {
	public:
        static SMARTP<guidoparam> create(std::string value, bool quote=true);
        static SMARTP<guidoparam> create(long value, bool quote=true);

		//! the parameter value
		void set (std::string value, bool quote=true);
		void set (long value, bool quote=true);
		std::string get () const 						{ return fValue; }
		bool   quote () const 						{ return fQuote; }
        
    protected:
		guidoparam(std::string value, bool quote);
		guidoparam(long value, bool quote);
		virtual ~guidoparam ();
        
    private:
		std::string 	fValue;
		bool	fQuote;
};

/*!
\brief A generic guido element representation.

	An element is represented by its name and the
	list of its enclosed elements plus optional parameters.
*/
class EXP guidoelement : public smartable {
	public:
        static SMARTP<guidoelement> create(std::string name, std::string sep=" ");
		
		long add (Sguidoelement& elt);
		long add (Sguidoparam& param);
		long add (Sguidoparam param);
		void print (std::ostream& os);

		//! the element name
		void 	setName (std::string name)			{ fName = name; }
		std::string 	getName () const				{ return fName; }
		std::string 	getStart () const				{ return fStartList; }
		std::string 	getEnd () const					{ return fEndList; }
		std::string 	getSep () const					{ return fSep; }
        std::vector<Sguidoelement>& elements()		{ return fElements; }
		const std::vector<Sguidoelement>& elements() const 	{ return fElements; }
        const std::vector<Sguidoparam>& parameters() const 	{ return fParams; }
		
		bool empty () const { return fElements.empty(); }

    protected:
		guidoelement(std::string name, std::string sep=" ");
		virtual ~guidoelement();

		std::string	fName;
		//! the contained element start marker (default to empty)
		std::string	fStartList;
		//! the contained element end marker (default to empty)
		std::string	fEndList;
		//! the element separator (default to space)
		std::string	fSep;
		//! list of the enclosed elements
		std::vector<Sguidoelement>	fElements;
		//! list of optional parameters
		std::vector<Sguidoparam>	fParams;		
};

/*!
\brief A guido note duration representation.

	A note duration is represented by a numerator 
    (denotes the number of beats), a denominator (denotes the beat value)
     and optional dots.
     Triplets are repesented as 1/3, 1/6, ... quintuplets, septuplets and so on
     are handled analogously.
*/
class EXP guidonoteduration {
	public:
		guidonoteduration(long num, long denom, long dots=0) 
            { set (num, denom, dots); }
		virtual ~guidonoteduration() {}
        
        void set (long num, long denom, long dots=0) 
            { fNum=num; fDenom=denom; fDots=dots; }
        guidonoteduration& operator= (const guidonoteduration& dur)	
            { fNum=dur.fNum; fDenom=dur.fDenom; fDots=dur.fDots; return *this; }
        bool operator!= (const guidonoteduration& dur) const	
            { return (fNum!=dur.fNum) || (fDenom!=dur.fDenom) || (fDots!=dur.fDots); }

        long	fNum;
		long	fDenom;
		long	fDots;
};

/*!
\brief A guido note representation.

	A note is represented by its name, optional accidentals,
    duration (in the form of numerator/denominator) and optional dots.
*/
class EXP guidonote : public guidoelement {
	public:
        static SMARTP<guidonote> create(unsigned short voice);
        static SMARTP<guidonote> create(unsigned short voice, std::string name, char octave,
                                                guidonoteduration& dur, std::string acc="");
		
		void set (unsigned short voice, std::string name, char octave, guidonoteduration& dur, std::string acc);
		void setName (const std::string name)			{ fNote = name; } 
		void setOctave (char octave)					{ fOctave = octave; }
		void setDuration (const guidonoteduration& dur)	{ fDuration = dur; }
		void setAccidental (const std::string acc)		{ fAccidental = acc; }

		const char * 	name() const		{ return fNote.c_str(); }
		const char * 	accidental() const	{ return fAccidental.c_str(); }
		char 			octave() const		{ return fOctave; }
		const guidonoteduration& duration() const { return fDuration; }

	protected:
		guidonote(unsigned short voice);
		guidonote(unsigned short voice, std::string name, char octave, 
                    guidonoteduration& dur, std::string acc="");
		virtual ~guidonote();
	
	std::string 	fNote;
	std::string 	fAccidental;
	char 	fOctave;
	guidonoteduration fDuration;

};
typedef SMARTP<guidonote> Sguidonote;

/*!
\brief Represents the current status of notes duration and octave.

    Octave and duration may be ommitted for guido notes. If so,
    they are infered from preceeding notes (or rest), within the same 
    sequence or chord, or assumed to have standard values.
\n
	The object is defined as a multi-voices singleton: a single
    object is allocated for a specific voice and thus it will
	not operate correctly on a same voice parrallel formatting 
    operations.

\todo handling the current beat value for \e *num duration form.
*/
class EXP guidonotestatus {
	public:
        enum { kMaxInstances=128 };
        
		static guidonotestatus* get(unsigned short voice);
		static void resetall();
		static void freeall();

        enum { defoctave=1, defnum=1, defdenom=4 };
        
        void reset()	{ fOctave=defoctave; fDur.set(defnum, defdenom, 0); }
        guidonotestatus& operator= (const guidonoteduration& dur)	{ fDur = dur; return *this; }
        bool operator!= (const guidonoteduration& dur) const		{ return fDur!= dur; }
            
		char				fOctave;
		guidonoteduration 	fDur;
//		char				fBeat;

	protected:
		guidonotestatus() :	fOctave(defoctave), fDur(defnum, defdenom, 0) {}
	private:
		static guidonotestatus * fInstances[kMaxInstances];
};

/*!
\brief The guido sequence element
*/
class EXP guidoseq : public guidoelement {
	public:
        static SMARTP<guidoseq> create();
	protected:
		guidoseq();
		virtual ~guidoseq();
};
typedef SMARTP<guidoseq> Sguidoseq;

/*!
\brief The guido chord element
*/
class EXP guidochord : public guidoelement {
	public:
        static SMARTP<guidochord> create();
	protected:
		guidochord ();
		virtual ~guidochord();
};
typedef SMARTP<guidochord> Sguidochord;

/*!
\brief A guido tag representation.

	A tag is represented by its name and optional parameters.
	A range tag contains enclosed elements.
*/
class EXP guidotag : public guidoelement {
	public:
        static SMARTP<guidotag> create(std::string name);
	protected:
		guidotag(std::string name);
		virtual ~guidotag();
};
typedef SMARTP<guidotag> Sguidotag;
/*! @} */

}

#endif
