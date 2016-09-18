/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifndef __transposition__
#define __transposition__

#include <map>
#include <vector>

#include "exports.h"
#include "typedefs.h"
#include "transposevisitor.h"
#include "notevisitor.h"


namespace MusicXML2 
{

/*!
\addtogroup visitors
@{
*/

typedef int	Chromatic;
typedef int	Diatonic;

/*!
\brief A visitor that computes a diatonic transposition of a score.

	The transposition visitor computes a diatonic transposition of
	a score. The transposition interval is specified as a number of chromatic
	steps, the simplest enharmonic diatonic transposition is automatically selected.
	The operation makes use of a fifth cycle table to compute transposed values.
	
\todo transposing ornaments elements
*/
class EXP transposition : 
	public notevisitor,
	public visitor<S_cancel>,
	public visitor<S_fifths>,
	public visitor<S_part>
{
    protected:
		Chromatic	fChromaticSteps;			// the target transposing interval

		std::vector<pair<std::string,int> >	fFifthCycle;	// the fifth cycle table

		int		fTableShift;			// the current shift into the table of fifths
		int		fOctaveChange;			// the target octave change computed from fChromaticSteps
		int		fKeySign;				// the target key signature 
		int		fCurrentKeySign;		// the current key signature

		void	initialize	();

		/*! Transpose a pitch expressed as a diatonic value + alteration + octave
			\param pitch on input a diatonic pitch value (where C=1), on output the new pitch value
			\param alter on input the accidental value, on output the new accidental value
			\param octave on input the octave number, on output the new octave number
			\param tableshift shift in the table of fifth cycle
			\return the corresponding diatonic steps count
		*/
		void	transpose ( string& pitch, float& alter, int& octave, int tableshift ) const;
		// set the note pitch according to the current shift into the table of fifths
		void	setPitch  ( notevisitor& v );

		ctree<xmlelement>::iterator insertAlter		( S_note& elt, ctree<xmlelement>::iterator pos, float value );
		ctree<xmlelement>::iterator insertAccident	( S_note& elt, ctree<xmlelement>::iterator pos, const string& value );

		virtual void visitStart ( S_part& elt );
		virtual void visitEnd( S_note& elt );
		// non standard keys are not yet supported
		virtual void visitStart ( S_cancel& elt );
		virtual void visitStart ( S_fifths& elt );
		
    public:
				 transposition(int steps);
		virtual ~transposition();


		/*! computes an octaves count from a chromatic interval
			\param steps a chromatic steps count
			\return the count of octaves
		*/
		static int  getOctave ( Chromatic steps )			{ return steps/12; }

		/*! computes a step number wihin an octave
			\param steps a chromatic steps count
			\return the step number within an octave
		*/
		static Chromatic  getOctaveStep ( Chromatic steps ) { return steps%12; }

		/*! computes a key sign corresponding to a chromatic degree 
			\param steps a chromatic step
			\return a numeric key signature representing a number of sharps when positive
			and a number of flats when negative
		*/
		static int  getKey ( Chromatic steps );

		/*! gives a note accidental according to a key signature
			\param pitch a diatonic pitch (where C=1)
			\param key the key signature expressed as a number of sharps when > 0 or flats when < 0
			\return an accidental as a positive value for sharps and negative for flats
		*/
		static int getAccidental ( Diatonic pitch, int key );

		/*! gives the accident string corresponding to an alter value
			\param alter a float pitch alter value
			\return an accidental string (sharp, flat, natural...)
		*/
		static string getAccident ( float alter );

		/*! Transpose a key expressed as a count of sharps (positive keys) or flats (negative key).
			The method may do enharmonic adjustments to choose the simplest key. 		
			\param key the key to be transposed
			\param steps chromatic transposition steps
			\param enharmonicChange on output, possible adjustment to enharmonic keys
			\return the transposed key
		*/
		static int transposeKey ( int key, Chromatic steps, int& enharmonicChange );
};

/*! @} */

} // namespace MusicXML2


#endif
