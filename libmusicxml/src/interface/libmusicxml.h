/*
  MusicXML Library
  Copyright (C) Grame 2006-2013

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr
*/

#ifdef VC6
# pragma warning (disable : 4786)
#endif

#include <iostream>
#include <string>
#include <stdio.h>
#include "exports.h"

namespace MusicXML2 
{
enum xmlErr { kNoErr, kInvalidFile };

#ifdef __cplusplus
extern "C" {
#endif

class musicxmlfactory;
class xmlelement;
class xmlattribute;

typedef xmlelement*		TElement;
typedef xmlattribute*	TAttribute;
typedef musicxmlfactory* TFactory;


/*!
	\brief Gives the library version number.
	\return the library version number as an integer (e.g. version 1.0.0 is returned as 100)
*/
EXP int				musicxmllibVersion();
/*!
	\brief Gives the library version as a string.
	\return a string
*/
EXP const char*		musicxmllibVersionStr();


/*!
\addtogroup Converting MusicXML to Guido Music Notation format

The library includes a high level API to convert from the MusicXML format to the
Guido Music Notation (GMN) format. For more information about this format, 
see  http://guidolib.sourceforge.net
@{
*/

/*!
	\brief Gives the guido converter version number.
	\return a version number as an integer (e.g. version 1.0.0 is returned as 100)
*/
EXP int				musicxml2guidoVersion();
/*!
	\brief Gives the guido converter version as a string.
	\return a string
*/
EXP	const char*		musicxml2guidoVersionStr();

/*!
	\brief Converts a MusicXML representation to the Guido format.
	\param file a file name 
	\param generateBars a boolean to force barlines generation
	\param out the output stream
	\return an error code (\c kNoErr when success)
*/
EXP xmlErr			musicxmlfile2guido	(const char *file, bool generateBars, std::ostream& out);

/*!
	\brief Converts a MusicXML representation to the Guido format.
	\param fd a file descriptor 
	\param generateBars a boolean to force barlines generation
	\param out the output stream
	\return an error code (\c kNoErr when success)
*/
EXP xmlErr			musicxmlfd2guido	(FILE* fd, bool generateBars, std::ostream& out);

/*!
	\brief Converts a MusicXML representation to the Guido format.
	\param buff a string containing MusicXML code
	\param generateBars a boolean to force barlines generation
	\param out the output stream
	\return an error code (\c kNoErr when success)
*/
EXP xmlErr			musicxmlstring2guido(const char *buff, bool generateBars, std::ostream& out);
/*! @} */


/*!
\addtogroup Factory Building a MusicXML representations


	The MusicXMl factory API.

	The factory provides a high level API to build a MusicXML tree but gives also a low 
	level access to the music representation. The idea is to make simple scores easy to build	
	while complex scores accessible with a godd knowledge of the MusicXML format.
	The main feature of the factory is the automatic sort of the representation according to the dtd.
	Actually, only a small subset of the containers is not handled due to forms like (A, B)*
	where ordering should be specified at element construction. These containers are:
		- credit
		- direction-type
		- key
		- lyric
		- metronome
		- ornaments
		- time

	<b>Note:</b> for many elements (e.g. the measure element) the dtd do not impose an order. 
	They have a form like ( A | B | C )* and thus any order is legal and the corresponding elements 
	are not sorted. However, the elements semantic may require a given order, 
	for example, the division attribute may be expected before a first duration element is encountered.
	It's the encoding application responsibility to build the elements with the appropriate order.
	These elements are:
		- direction
		- harp-pedals
		- scordatura
		- grouping
		- identification
		- encoding
		- miscellaneous
		- staff-layout
		- measure-layout
		- note
		- articulations
		- part-list
		- group-name-display
		- group-abbreviation-display
		- score-partwise
		- score-timewise
		- measure
		- part

	<b>Note concerning the memory management: </b>
	all the elements or attributes allocated using the factory API (functions that return a TElement 
	or a TAttribute) should be released using factoryFreeElement or factoryFreeAttribute unless they are
	added to the other elements (factoryAddElement or factoryAddAttribute) or to the music description 
	(factoryAddPartlist)

	<b>Overview of the format and API</b>

	A MusicXML partwise score structure is basically made of a header containing various information 
	and a require parts list, followed by a list of parts, including measures, made of various 'music data'.
	<pre>
		'partwise score'  := 'opt. header' 'part-list' 'part'  ...  'part'
		'part'            := 'measure' ... 'measure'
		'measure'         := 'music-data' ... 'music-data'
	</pre>
	The factory API reflects this structure: 
	- it provides functions to create the header information: factoryHeader(), factoryCreator(), factoryRights(), factoryEncoding()
	- it provides functions to describe the parts list: factoryScorepart(), factoryAddPart() (with a score-part element as argument), factoryAddGroup()
	- it provides high level functions to create measures, notes, rests and chords: factoryMeasureA(), factoryMeasureB(), factoryNote(), factoryRest(), factoryChord() 
	- it provides high level functions to add "compound" attributes to notes like tuplet specification (factoryTuplet()), ties (factoryTie()) 
	  or to create "compound" elements (factoryDynamic(), factoryBarline())
	- the remaining functions are low level function to create elements, attributes and hierarchy of elements
@{
*/

/*!
	\brief Creates a new MusicXML factory.
	\return an opaque reference to the factory.
*/
EXP TFactory	factoryOpen  ();

/*!
	\brief Close a MusicXML factory.
	\param f the MusicXML factory
*/
EXP void		factoryClose (TFactory f);

/*!
	\brief Print the MusicXML representation to stream.
	\param f the MusicXML factory
	\param out the output stream
*/
EXP void		factoryPrint (TFactory f, std::ostream& out);


/*!
	\brief Creates header information.
	\param f the MusicXML factory
	\param worknumber corresponds to the 'work-number' element
	\param worktitle corresponds to the 'work-titel' element
	\param movementnumber corresponds to the 'movement-number' element
	\param movementtitle corresponds to the 'movement-title' element

	A null pointer prevents the corresponding element creation.
*/
EXP void		factoryHeader	(TFactory f, const char* worknumber, const char* worktitle, const char* movementnumber, const char* movementtitle);


/*!
	\brief Creates creator information.
	\param f the MusicXML factory
	\param c the creator name (should not be null or empty)
	\param type the creator type, corresponds to the 'type' attribute. A null pointer prevents the attribute creation.
*/
EXP void		factoryCreator	(TFactory f, const char* c, const char* type);


/*!
	\brief Creates rights information.
	\param f the MusicXML factory
	\param r the rights string (should not be null or empty)
	\param type the rights type, corresponds to the 'type' attribute. A null pointer prevents the attribute creation.
*/
EXP void		factoryRights	(TFactory f, const char* r, const char* type);

/*!
	\brief Creates encoding information.
	\param f the MusicXML factory
	\param software corresponds to the 'software' encoding element. Uses the default when null.
	\note The encoding element is automatically created with the current 'encoding-date'.
*/
EXP void		factoryEncoding	(TFactory f, const char* software);

/*!
	\brief Adds a part. 
	\param f the MusicXML factory
	\param part actually a 'score-part' or a 'part' element. Pushed to the adequate location, depending on the element type.
*/
EXP void		factoryAddPart (TFactory f, TElement part);

/*!
	\brief Adds parts grouped in a 'part-group'. 
	\param f the MusicXML factory
	\param number the group number
	\param name the group name
	\param abbrev the group abbreviated name
	\param groupbarline	a boolean for group barlines
	\param parts a null terminated array of parts to be grouped
	\note you should refer to the MusicXML dtd for more information about parts groups.
*/
EXP void		factoryAddGroup (TFactory f, int number, const char* name, const char* abbrev, bool groupbarline, TElement* parts);

/*!
	\brief Adds an element to another element. 
	\param f the MusicXML factory
	\param elt a container element.
	\param subelt the element added to the container element.
	\note once added to an element, an element is automatically released when its container element is released.
*/
EXP void		factoryAddElement	(TFactory f, TElement elt, TElement subelt);

/*!
	\brief Adds a set of elements to another element. 
	\param f the MusicXML factory
	\param elt the destination container.
	\param subelts a null terminated array of elements.
	\note once added to an element, an element is automatically released when its container element is released.
*/
EXP void		factoryAddElements	(TFactory f, TElement elt, TElement* subelts);

/*!
	\brief Adds an attribute to an element. 
	\param f the MusicXML factory
	\param elt an element.
	\param attr the attribute to be added.
	\note once added to an element, an attribute is automatically released when the element is released.
*/
EXP void		factoryAddAttribute	(TFactory f, TElement elt, TAttribute attr);

/*!
	\brief Creates a 'score-part' element.
	\param f the MusicXML factory
	\param id the required 'id' attribute
	\param name the part name
	\param abbrev the part abbreviated named
*/
EXP TElement	factoryScorepart(TFactory f, const char* id, const char* name, const char* abbrev);

/*!
	\brief Creates a 'part' element.
	\param f the MusicXML factory
	\param id the required 'id' attribute
*/
EXP TElement	factoryPart		(TFactory f, const char* id);

/*!
	\brief Creates a 'measure' element.
	\param f the MusicXML factory
	\param number the required 'number' attribute
*/
EXP TElement	factoryMeasure	(TFactory f, int number);

/*!
	\brief Creates a 'measure' element with a set of attributes.
	\param f the MusicXML factory
	\param number the required 'number' attribute
	\param time the time signature encoded as a string with the form "n/n". A null pointer prevents the 'time' element creation.
	\param clef the clef in "G", "F", "C", "percussion", "TAB" or "none"
	\param line an optional clef line (0 prevents the line element creation)
	\param key	an optional key signature (0 prevents the key element creation)
	\param division an optional divisions element (0 prevents the divisions element creation)
*/
EXP TElement	factoryMeasureWithAttributes	(TFactory f, int number, const char* time, const char* clef, int line, int key, int division);

/*!
	\brief Creates a note.
	\param f the MusicXML factory
	\param step the pitch step using letters A through G
	\param alter chromatic alteration in number of semitones (0 prevents the alter element creation)
	\param octave a number in 0 to 9, where 4 indicates the octave started by middle C.
	\param duration the sounding duration (in divisions count) (0 prevents the duration element creation)
	\param type the graphic note type; in 256th, 128th, 64th, 32nd, 16th, eighth, quarter, half, whole, breve or long
*/
EXP TElement	factoryNote		(TFactory f, const char* step, float alter, int octave, int duration, const char* type);

/*!
	\brief Creates a dynamics element containing a dynamic \c type.
	\param f the MusicXML factory
	\param type the dynamic type
	\param placement optional placement dynamics attribute. Should be in "above" or "below". 
	 A null value prevents the attribute creation. 
*/
EXP TElement	factoryDynamic	(TFactory f, int type, const char* placement);

/*!
	\brief Creates a barline element with the corresponding sub-elements.
	\param f the MusicXML factory
	\param location the barline location attribute. A null value prevents the attribute creation.
	\param barstyle optional optional bar-style element. Should be in regular, dotted, dashed, heavy, light-light, 
	light-heavy, heavy-light, heavy-heavy, tick (a short stroke through the top line), short (a partial barline 
	between the 2nd and 4th lines), and none. A null value prevents the barstyle creation. 
	\param repeat optional repeat element with the corresponding required direction attribute. Should be
	in backward or forward. A null value prevents the repeat creation.
*/
EXP TElement	factoryBarline	(TFactory f, const char* location, const char* barstyle, const char *repeat);

/*!
	\brief Makes a tuplet with notes.
	\param f the MusicXML factory
	\param actual the actual notes count
	\param normal the normal notes count
	\param notes a null terminated list of notes.
	\note: factoryTuplet creates the corresponding time-modification element for each note and 
	the tuplet elements for the first and last notes of the list.
*/
EXP void		factoryTuplet	(TFactory f, int actual, int normal, TElement * notes);

/*!
	\brief Tie two notes.
	\param f the MusicXML factory
	\param from the tie start note
	\param to the tie end note
	\note: factoryTie creates the corresponding tie and tied elements.
*/
EXP void		factoryTie	(TFactory f, TElement from, TElement to);

/*!
	\brief Add an element to a note notations element.
	\param f the MusicXML factory
	\param elt a note element
	\param notation an element to add to the note notations
*/
EXP void		factoryNotation	(TFactory f, TElement elt, TElement notation);

/*!
	\brief Add an articulation to a note.
	\param f the MusicXML factory
	\param elt a note element
	\param articulation an articulation element
*/
EXP void		factoryArticulation	(TFactory f, TElement elt, TElement articulation);

/*!
	\brief Creates a rest.
	\param f the MusicXML factory
	\param duration the sounding duration (in divisions count) (0 prevents the duration element creation)
	\param type the graphic note type; in 256th, 128th, 64th, 32nd, 16th, eighth, quarter, half, whole, breve or long
*/
EXP TElement	factoryRest		(TFactory f, int duration, const char* type);

/*!
	\brief Makes a chord from the gievn notes.
	\param f the MusicXML factory
	\param notes a null terminated list of notes.
	Actually, chords are denoted by a 'chord' element included to the note element, indicating that the note is 
	an additional chord tone with the preceding note. For convenience, all the chord notes should be given to 
	the function but the first one is skipped when adding the 'chord' element.
*/
EXP void		factoryChord	(TFactory f, TElement * notes);


/*!
	\brief Creates an arbitrary MusicXML element.
	\param f the MusicXML factory
	\param type the element type.
*/
EXP TElement	factoryElement		(TFactory f, int type);

/*!
	\brief Creates an arbitrary MusicXML element with a string value.
	\param f the MusicXML factory
	\param type the element type.
	\param value the element value.
*/
EXP TElement	factoryStrElement	(TFactory f, int type, const char * value);
/*!
	\brief Creates an arbitrary MusicXML element with an integer value.
	\param f the MusicXML factory
	\param type the element type.
	\param value the element value.
*/
EXP TElement	factoryIntElement	(TFactory f, int type, int value);
/*!
	\brief Creates an arbitrary MusicXML element with a float value.
	\param f the MusicXML factory
	\param type the element type.
	\param value the element value.
*/
EXP TElement	factoryFloatElement	(TFactory f, int type, float value);


/*!
	\brief Creates an arbitrary attribute with a string value.
	\param f the MusicXML factory
	\param name the attribute name.
	\param value the attribute value.
*/
EXP TAttribute	factoryStrAttribute		(TFactory f, const char * name, const char* value);
/*!
	\brief Creates an arbitrary attribute with an integer value.
	\param f the MusicXML factory
	\param name the attribute name.
	\param value the attribute value.
*/
EXP TAttribute	factoryIntAttribute		(TFactory f, const char * name, int value);
/*!
	\brief Creates an arbitrary attribute with a float value.
	\param f the MusicXML factory
	\param name the attribute name.
	\param value the attribute value.
*/
EXP TAttribute	factoryFloatAttribute	(TFactory f, const char * name, float value);

/*!
	\brief Frees a previously allocated element
	\param f the MusicXML factory
	\param elt an element.
	\note you must not free an element that has been added to an element.
	\see factoryAddElement
*/
EXP void		factoryFreeElement		(TFactory f, TElement elt);

/*!
	\brief Frees a previously allocated attribute
	\param f the MusicXML factory
	\param attr an attribute.
	\note you must not free an attribute that has been added to an element.
	\see factoryAddAttribute
*/
EXP void		factoryFreeAttribute	(TFactory f, TAttribute attr);

/*! @} */


#ifdef __cplusplus
}
#endif

}
