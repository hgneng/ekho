<!--
	MusicXML™ attributes.mod module

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	The attributes DTD module contains the attributes element
	and its children, such as key and time signatures.
-->

<!-- Elements -->

<!--
	The attributes element contains musical information that
	typically changes on measure boundaries. This includes
	key and time signatures, clefs, transpositions, and staving.
-->
<!ELEMENT attributes (%editorial;, divisions?, key*, time*,
	staves?, part-symbol?, instruments?, clef*, staff-details*,
	transpose?, directive*, measure-style*)>

<!--	
	Traditional key signatures are represented by the number
	of flats and sharps, plus an optional mode for major/
	minor/mode distinctions. Negative numbers are used for
	flats and positive numbers for sharps, reflecting the
	key's placement within the circle of fifths (hence the
	element name). A cancel element indicates that the old
	key signature should be cancelled before the new one
	appears. This will always happen when changing to C major
	or A minor and need not be specified then. The cancel
	value matches the fifths value of the cancelled key
	signature (e.g., a cancel of -2 will provide an explicit
	cancellation for changing from B flat major to F major).
	The optional location attribute indicates whether the
	cancellation appears to the left or the right of the new
	key signature. It is left by default.
	
	Non-traditional key signatures can be represented using
	the Humdrum/Scot concept of a list of altered tones.
	The key-step and key-alter elements are represented the
	same way as the step and alter elements are in the pitch
	element in the note.mod file. The different element names
	indicate the different meaning of altering notes in a scale
	versus altering a sounding pitch.
	
	Valid mode values include major, minor, dorian, phrygian,
	lydian, mixolydian, aeolian, ionian, and locrian.

	The optional number attribute refers to staff numbers, 
	from top to bottom on the system. If absent, the key
	signature applies to all staves in the part.

	The optional list of key-octave elements is used to specify
	in which octave each element of the key signature appears.
	The content specifies the octave value using the same
	values as the display-octave element. The number attribute
	is a positive integer that refers to the key signature
	element in left-to-right order. If the cancel attribute is
	set to yes, then this number refers to an element specified
	by the cancel element. It is no by default.
-->
<!ELEMENT key (((cancel?, fifths, mode?) |
	((key-step, key-alter)*)), key-octave*)>
<!ATTLIST key
    number CDATA #IMPLIED
    %print-style;
    %print-object;
>
<!ELEMENT cancel (#PCDATA)>
<!ATTLIST cancel
    location %left-right; #IMPLIED
>
<!ELEMENT fifths (#PCDATA)>
<!ELEMENT mode (#PCDATA)>
<!ELEMENT key-step (#PCDATA)>
<!ELEMENT key-alter (#PCDATA)>
<!ELEMENT key-octave (#PCDATA)>
<!ATTLIST key-octave
    number NMTOKEN #REQUIRED
    cancel %yes-no; #IMPLIED
>

<!--
	Musical notation duration is commonly represented as
	fractions. The divisions element indicates how many 
	divisions per quarter note are used to indicate a note's
	duration. For example, if duration = 1 and divisions = 2,
	this is an eighth note duration. Duration and divisions
	are used directly for generating sound output, so they
	must be chosen to take tuplets into account. Using a
	divisions element lets us use just one number to 
	represent a duration for each note in the score, while
	retaining the full power of a fractional representation.
	For maximum compatibility with Standard MIDI Files, the
	divisions value should not exceed 16383.
-->
<!ELEMENT divisions (#PCDATA)>

<!--
	Time signatures are represented by two elements. The
	beats element indicates the number of beats, as found in
	the numerator of a time signature. The beat-type element
	indicates the beat unit, as found in the denominator of
	a time signature. The symbol attribute is used to
	indicate another notation beyond a fraction: the common 
	and cut time symbols, as well as a single number with an
	implied denominator. Normal (a fraction) is the implied
	symbol type if none is specified. Multiple pairs of
	beat and beat-type elements are used for composite
	time signatures with multiple denominators, such as
	2/4 + 3/8. A composite such as 3+2/8 requires only one
	beat/beat-type pair. A senza-misura element explicitly
	indicates that no time signature is present. 

	The print-object attribute allows a time signature to be
	specified but not printed, as is the case for excerpts
	from the middle of a score. The value is "yes" if
	not present. The optional number attribute refers to staff
	numbers within the part, from top to bottom on the system. 
	If absent, the time signature applies to all staves in the 
	part.
-->
<!ELEMENT time ((beats, beat-type)+ | senza-misura)>
<!ATTLIST time
    number CDATA #IMPLIED
    symbol (common | cut | single-number | normal) #IMPLIED
    %print-style;
    %print-object;
>
<!ELEMENT beats (#PCDATA)>
<!ELEMENT beat-type (#PCDATA)>
<!ELEMENT senza-misura EMPTY>

<!--
	Staves are used if there is more than one staff
	represented in the given part (e.g., 2 staves for
	typical piano parts). If absent, a value of 1 is assumed.
	Staves are ordered from top to bottom in a part in
	numerical order, with staff 1 above staff 2.
-->
<!ELEMENT staves (#PCDATA)>

<!--
	The part-symbol element indicates how a symbol for a
	multi-staff part is indicated in the score. Values include
	none, brace, line, and bracket; brace is the default. The
	top-staff and bottom-staff elements are used when the brace
	does not extend across the entire part. For example, in a
	3-staff organ part, the top-staff will typically be 1 for
	the right hand, while the bottom-staff will typically be 2
	for the left hand. Staff 3 for the pedals is usually outside
	the brace.
 -->
<!ELEMENT part-symbol (#PCDATA)>
<!ATTLIST part-symbol
	top-staff CDATA #IMPLIED
	bottom-staff CDATA #IMPLIED
    %position;
    %color;
>

<!--
	Instruments are only used if more than one instrument is
	represented in the part (e.g., oboe I and II where they
	play together most of the time). If absent, a value of 1
	is assumed.
-->
<!ELEMENT instruments (#PCDATA)>

<!--
	Clefs are represented by the sign, line, and
	clef-octave-change elements. Sign values include G, F, C,
	percussion, TAB, and none. Line numbers are counted from
	the bottom of the staff. Standard values are 2 for the
	G sign (treble clef), 4 for the F sign (bass clef), 3 
	for the C sign (alto clef) and 5 for TAB (on a 6-line
	staff). The clef-octave-change element is used for
	transposing clefs (e.g., a treble clef for tenors would
	have a clef-octave-change value of -1). The optional 
	number attribute refers to staff numbers within the part,
	from top to bottom on the system. A value of 1 is 
	assumed if not present.

	Sometimes clefs are added to the staff in non-standard
	line positions, either to indicate cue passages, or when
	there are multiple clefs present simultaneously on one
	staff. In this situation, the additional attribute is set to
	"yes" and the line value is ignored. The size attribute
	is used for clefs where the additional attribute is "yes".
	It is typically used to indicate cue clefs.
-->
<!ELEMENT clef (sign, line?, clef-octave-change?)>
<!ATTLIST clef
    number CDATA #IMPLIED
    additional %yes-no; #IMPLIED
    size        %symbol-size;  #IMPLIED
    %print-style;
    %print-object;
>
<!ELEMENT sign (#PCDATA)>
<!ELEMENT line (#PCDATA)>
<!ELEMENT clef-octave-change (#PCDATA)>

<!--
	The staff-details element is used to indicate different
	types of staves. The staff-type element can be ossia,
	cue, editorial, regular, or alternate. An alternate staff
	indicates one that shares the same musical data as the
	prior staff, but displayed differently (e.g., treble and
	bass clef, standard notation and tab). The staff-lines
	element specifies the number of lines for non 5-line
	staffs. The staff-tuning and capo elements are used to
	specify tuning when using tablature notation. The optional
	number attribute specifies the staff number from top to
	bottom on the system, as with clef. The optional show-frets
	attribute indicates whether to show tablature frets as
	numbers (0, 1, 2) or letters (a, b, c). The default choice
	is numbers. The print-object attribute is used to indicate
	when a staff is not printed in a part, usually in large
	scores where empty parts are omitted. It is yes by default.
	If print-spacing is yes while print-object is no, the score
	is printed in cutaway format where vertical space is left
	for the empty part.
-->
<!ELEMENT staff-details (staff-type?, staff-lines?, 
	staff-tuning*, capo?, staff-size?)>
<!ATTLIST staff-details
    number         CDATA                #IMPLIED
    show-frets     (numbers | letters)  #IMPLIED
    %print-object;
    %print-spacing;
>
<!ELEMENT staff-type (#PCDATA)>
<!ELEMENT staff-lines (#PCDATA)>

<!--
	The tuning-step, tuning-alter, and tuning-octave
	elements are defined in the common.mod file. Staff
	lines are numbered from bottom to top.
-->
<!ELEMENT staff-tuning
	(tuning-step, tuning-alter?, tuning-octave)>
<!ATTLIST staff-tuning
    line CDATA #REQUIRED
>

<!--
	The capo element indicates at which fret a capo should
	be placed on a fretted instrument. This changes the
	open tuning of the strings specified by staff-tuning
	by the specified number of half-steps.
-->
<!ELEMENT capo (#PCDATA)>

<!--
	The staff-size element indicates how large a staff
	space is on this staff, expressed as a percentage of 
	the work's default scaling. Values less than 100 make
	the staff space smaller while values over 100 make the
	staff space larger. A staff-type of cue, ossia, or 
	editorial implies a staff-size of less than 100, but
	the exact value is implementation-dependent unless
	specified here. Staff size affects staff height only,
	not the relationship of the staff to the left and
	right margins.
-->
<!ELEMENT staff-size (#PCDATA)>

<!--
	If the part is being encoded for a transposing instrument
	in written vs. concert pitch, the transposition must be
	encoded in the transpose element. The transpose element
	represents what must be added to the written pitch to get
	the correct sounding pitch.

	The transposition is represented by chromatic steps
	(required) and three optional elements: diatonic pitch
	steps, octave changes, and doubling an octave down. The
	chromatic and octave-change elements are numeric values
	added to the encoded pitch data to create the sounding
	pitch. The diatonic element is also numeric and allows
	for correct spelling of enharmonic transpositions.
-->
<!ELEMENT transpose (diatonic?, chromatic, octave-change?,
	double?)>
<!ELEMENT diatonic (#PCDATA)>
<!ELEMENT chromatic (#PCDATA)>
<!ELEMENT octave-change (#PCDATA)>
<!ELEMENT double EMPTY>

<!--
	Directives are like directions, but can be grouped together 
	with attributes for convenience. This is typically used for
	tempo markings at the beginning of a piece of music. This
	element has been deprecated in Version 2.0 in favor of
	the directive attribute for direction elements. Language 
	names come from ISO 639, with optional country subcodes
	from ISO 3166.
-->
<!ELEMENT directive (#PCDATA)>
<!ATTLIST directive
    %print-style;
    xml:lang NMTOKEN #IMPLIED
>

<!--
	A measure-style indicates a special way to print partial
	to multiple measures within a part. This includes multiple
	rests over several measures, repeats of beats, single, or
	multiple measures, and use of slash notation.
	
	The multiple-rest and measure-repeat symbols indicate the
	number of measures covered in the element content. The
	beat-repeat and slash elements can cover partial measures.
	All but the multiple-rest element use a type attribute to 
	indicate starting and stopping the use of the style. The
	optional number attribute specifies the staff number from
	top to bottom on the system, as with clef.
-->
<!ELEMENT measure-style (multiple-rest | 
	measure-repeat | beat-repeat | slash)>
<!ATTLIST measure-style
    number CDATA #IMPLIED
    %font;
    %color;
>

<!--
	The slash-type and slash-dot elements are optional children
	of the beat-repeat and slash elements. They have the same
	values as the type and dot elements, and define what the
	beat is for the display of repetition marks. If not present,
	the beat is based on the current time signature.
-->
<!ELEMENT slash-type (#PCDATA)>
<!ELEMENT slash-dot EMPTY>

<!--
	The text of the multiple-rest element indicates the number
	of measures in the multiple rest. Multiple rests may use
	the 1-bar / 2-bar / 4-bar rest symbols, or a single shape.
	The use-symbols attribute indicates which to use; it is no
	if not specified.
-->
<!ELEMENT multiple-rest (#PCDATA)>
<!ATTLIST multiple-rest
    use-symbols %yes-no; #IMPLIED
>

<!--
	The measure-repeat and beat-repeat element specify a
	notation style for repetitions. The actual music being
	repeated needs to be repeated within the MusicXML file.
	These elements specify the notation that indicates the
	repeat.
-->

<!--
	The measure-repeat element is used for both single and
	multiple measure repeats. The text of the element indicates
	the number of measures to be repeated in a single pattern.
	The slashes attribute specifies the number of slashes to
	use in the repeat sign. It is 1 if not specified. Both the
	start and the stop of the measure-repeat must be specified.
-->
<!ELEMENT measure-repeat (#PCDATA)>
<!ATTLIST measure-repeat
    type %start-stop; #REQUIRED
    slashes NMTOKEN #IMPLIED
>

<!--
	The beat-repeat element is used to indicate that a single
	beat (but possibly many notes) is repeated. Both the start
	and stop of the beat being repeated should be specified.
	The slashes attribute specifies the number of slashes to
	use in the symbol. The use-dots attribute indicates whether
	or not to use dots as well (for instance, with mixed rhythm
	patterns). By default, the value for slashes is 1 and the
	value for use-dots is no.
-->
<!ELEMENT beat-repeat ((slash-type, slash-dot*)?)>
<!ATTLIST beat-repeat
    type %start-stop; #REQUIRED
    slashes NMTOKEN #IMPLIED
    use-dots %yes-no; #IMPLIED
>

<!--
	The slash element is used to indicate that slash notation
	is to be used. If the slash is on every beat, use-stems is
	no (the default). To indicate rhythms but not pitches,
	use-stems is set to yes. The type attribute indicates
	whether this is the start or stop of a slash notation
	style. The use-dots attribute works as for the beat-repeat
	element, and only has effect if use-stems is no.
-->
<!ELEMENT slash ((slash-type, slash-dot*)?)>
<!ATTLIST slash
    type %start-stop; #REQUIRED
    use-dots %yes-no; #IMPLIED
    use-stems %yes-no; #IMPLIED
>
