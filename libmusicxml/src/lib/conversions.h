/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __conversions__
#define __conversions__

#include "exports.h"
#include "rational.h"
#include "bimap.h"
#include <string>

using namespace std;

namespace MusicXML2 
{

/*!
\brief provides conversions between numeric trill start notes and strings
*/
class EXP TrillStart {
    public:
    enum type { undefined, upper, main, below, last=below };

    //! convert a numeric start note value to a MusicXML string
    static const string	xml (type d);
    //! convert a MusicXML string to a numeric start note value
    static       type	xml (const string str);

    private:
	static bimap<string, type> fSN2String;
	static type 		fSNTbl[];
	static string 		fSNStrings[];        
};

/*!
\brief provides conversions between numeric trill step, turn and strings
*/
class EXP TrillStep {
    public:
    enum type { undefined, whole, half, unison, none, last=none };

    //! convert a numeric trill step or turn value to a MusicXML string
    static const string	xml (type d);
    //! convert a MusicXML string to a numeric trill step or turn value
    static       type	xml (const string str);

    private:
	static bimap<string, type> fTS2String;
	static type 		fTSTbl[];
	static string 		fTSStrings[];        
};

/*!
\brief provides conversions between numeric note size types and strings
*/
class EXP FullCue {
    public:
    enum type { undefined, full, cue, last=cue };

    //! convert a numeric size value to a MusicXML string
    static const string	xml (type d);
    //! convert a MusicXML string to a numeric size value
    static       type	xml (const string str);

    private:
	static bimap<string, type> fFC2String;
	static type 		fFCTbl[];
	static string 		fFCStrings[];        
};

/*!
\brief provides conversions between numeric yes-no types and strings
*/
class EXP YesNo {
    public:
    enum type { undefined, yes, no, last=no };

    //! convert a numeric yes-no value to a MusicXML string
    static const string	xml (type d);
    //! convert a MusicXML string to a numeric yes-no value
    static       type	xml (const string str);

    private:
	static bimap<string, type> fYN2String;
	static type 	fYNTbl[];
	static string 	fYNStrings[];        
};

/*!
\brief provides conversions between numeric start-stop types and strings
*/
class EXP StartStop {
    public:
    enum type { undefined, start, stop, cont, last=cont };

    //! convert a numeric start-stop value to a MusicXML string
    static const string	xml (type d);
    //! convert a MusicXML string to a numeric start-stop value
    static       type	xml (const string str);

    private:
	static bimap<string, type> fStartStop2String;
	static type 	fStartStopTbl[];
	static string 	fStartStopStrings[];        
};

/*!
\brief provides conversions between numeric line types and strings
*/
class EXP LineType {
    public:
    enum type { undefined, solid, dashed, dotted, wavy, last=wavy };

    //! convert a numeric start-stop value to a MusicXML string
    static const string	xml (type d);
    //! convert a MusicXML string to a numeric start-stop value
    static       type	xml (const string str);

    private:
	static bimap<string, type> fLineType2String;
	static type 	fLineTypeTbl[];
	static string 	fLineTypeStrings[];        
};

/*!
\brief provides conversions between numeric note types and strings

	Type indicates the graphic note type. Valid values (from
	shortest to longest) are 256th, 128th, 64th, 32nd, 16th,
	eighth, quarter, half, whole, breve, and long.
*/
class EXP NoteType {
    public:
        enum type { undefined,
                    t256th=1, t128th=1<<1, t64th=1<<2, t32nd=1<<3, 
                    t16th=1<<4, eighth=1<<5, quarter=1<<6, 
                    half=1<<7, whole=1<<8, breve=1<<9, tlong=1<<10, 
                    count=11 };

	//! convert an integer note to a rational representation
    static rational type2rational(type d); 
	//! convert an integer note type to a MusicXML string
    static const string	xml (type d);
    //! convert an MusicXML string to an integer note type
    static       type	xml (const string str);

    private:
	static bimap<string, type> fType2String;
	static type 	fTypeTbl[];
	static string 	fTypeStrings[];        
};

} // namespace MusicXML2

#endif
