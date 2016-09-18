<!--
	MusicXML™ direction.mod module

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	This direction DTD module contains the direction element
	and its children. Directions are not note-specific, but
	instead are associated with a part or the overall score.
	
	Harmony indications and general print and sound
	suggestions are likewise not necessarily attached to
	particular note elements, and are included here as well.
-->

<!-- Elements -->

<!--
	A direction is a musical indication that is not attached
	to a specific note. Two or more may be combined to
	indicate starts and stops of wedges, dashes, etc.

	By default, a series of direction-type elements and a 
	series of child elements of a direction-type within a 
	single direction element follow one another in sequence
	visually. For a series of direction-type children, non-
	positional formatting attributes are carried over from
	the previous element by default.
-->
<!ELEMENT direction (direction-type+, offset?,
	%editorial-voice;, staff?, sound?)>
<!ATTLIST direction
    %placement; 
    %directive;
>

<!--
	Textual direction types may have more than 1 component
	due to multiple fonts. The dynamics element may also be
	used in the notations element, and is defined in the
	common.mod file.
-->
<!ELEMENT direction-type (rehearsal+ | segno+ | words+ |
	coda+ | wedge | dynamics+ | dashes | bracket | pedal | 
	metronome | octave-shift | harp-pedals | damp | 
	damp-all | eyeglasses | scordatura | image |
	accordion-registration | other-direction)>

<!--
	Entities related to print suggestions apply to the
	individual direction-type, not to the overall direction.
-->

<!--
	Language is Italian ("it") by default. Enclosure is
	square by default.
-->
<!ELEMENT rehearsal (#PCDATA)>
<!ATTLIST rehearsal
    %print-style;
    %text-decoration;
    xml:lang NMTOKEN #IMPLIED
    %text-direction;
    %text-rotation;
    enclosure (square | circle | none) #IMPLIED
>

<!--
	Left justification is assumed if not specified. 
	Language is Italian ("it") by default. Enclosure
	is none by default.
-->
<!ELEMENT words (#PCDATA)>
<!ATTLIST words
    %text-formatting;
>

<!--
	Wedge spread is measured in tenths of staff line space.
	The type is crescendo for the start of a wedge that is
	closed at the left side, and diminuendo for the start
	of a wedge that is closed on the right side. Spread 
	values at the start of a crescendo wedge or end of a
	diminuendo wedge are ignored.
-->
<!ELEMENT wedge EMPTY>
<!ATTLIST wedge
    type (crescendo | diminuendo | stop) #REQUIRED
    number %number-level; #IMPLIED
    spread CDATA #IMPLIED
    %position;
    %color;
>

<!--
	Dashes, used for instance with cresc. and dim. marks.
-->
<!ELEMENT dashes EMPTY>
<!ATTLIST dashes
    type %start-stop; #REQUIRED
    number %number-level; #IMPLIED
    %position;
    %color;
>

<!--
	Brackets are combined with words in a variety of
	modern directions. The line-end attribute specifies
	if there is a jog up or down (or both), an arrow,
	or nothing at the start or end of the bracket. If
	the line-end is up or down, the length of the jog
	can be specified using the end-length attribute.
	The line-type is solid by default.
-->
<!ELEMENT bracket EMPTY>
<!ATTLIST bracket
    type %start-stop; #REQUIRED
    number %number-level; #IMPLIED
    line-end (up | down | both | arrow | none) #REQUIRED
    end-length %tenths; #IMPLIED
    %line-type;
    %position;
    %color;
>

<!-- 
	Piano pedal marks. The line attribute is yes if pedal
	lines are used, no if Ped and * signs are used. The
	change type is used with line set to yes.
-->
<!ELEMENT pedal EMPTY>
<!ATTLIST pedal
    type (start | stop | change) #REQUIRED
    line %yes-no;	#IMPLIED
    %print-style; 
>

<!--
	Metronome marks and other metric relationships.
	
	The beat-unit values are the same as for a type element,
	and the beat-unit-dot works like the dot element. The
	per-minute element can be a number, or a text description
	including numbers. The parentheses attribute indicates
	whether or not to put the metronome mark in parentheses;
	its value is no if not specified. If a font is specified for
	the per-minute element, it overrides the font specified for
	the overall metronome element. This allows separate
	specification of a music font for beat-unit and a text
	font for the numeric value in cases where a single
	metronome font is not used.

	The metronome-note and metronome-relation elements
	allow for the specification of more complicated metric
	relationships, such as swing tempo marks where 
	two eighths are equated to a quarter note / eighth note
	triplet. The metronome-type, metronome-beam, and
	metronome-dot elements work like the type, beam, and
	dot elements. The metronome-tuplet element uses the
	same element structure as the time-modification element
	along with some attributes from the tuplet element. The
	metronome-relation element describes the relationship
	symbol that goes between the two sets of metronome-note
	elements. The currently allowed value is equals, but this
	may expand in future versions. If the element is empty,
	the equals value is used. The metronome-relation and
	the following set of metronome-note elements are optional
	to allow display of an isolated Grundschlagnote.
-->
<!ELEMENT metronome 
	((beat-unit, beat-unit-dot*, 
	 (per-minute | (beat-unit, beat-unit-dot*))) |
	(metronome-note+, (metronome-relation, metronome-note+)?))>
<!ATTLIST metronome
    %print-style;
    parentheses %yes-no; #IMPLIED
>
<!ELEMENT beat-unit (#PCDATA)>
<!ELEMENT beat-unit-dot EMPTY>
<!ELEMENT per-minute (#PCDATA)> 
<!ATTLIST per-minute
    %font;
>

<!ELEMENT metronome-note
	(metronome-type, metronome-dot*,
	 metronome-beam*, metronome-tuplet?)>
<!ELEMENT metronome-relation (#PCDATA)>
<!ELEMENT metronome-type (#PCDATA)>
<!ELEMENT metronome-dot EMPTY>
<!ELEMENT metronome-beam (#PCDATA)>
<!ATTLIST metronome-beam
    number %beam-level; "1"
>
<!ELEMENT metronome-tuplet
	(actual-notes, normal-notes, 
	 (normal-type, normal-dot*)?)>
<!ATTLIST metronome-tuplet
    type %start-stop; #REQUIRED
    bracket %yes-no; #IMPLIED
    show-number (actual | both | none) #IMPLIED
>

<!--
	Octave shifts indicate where notes are shifted up or down
	from their true pitched values because of printing
	difficulty. Thus a treble clef line noted with 8va will
	be indicated with an octave-shift down from the pitch
	data indicated in the notes. A size of 8 indicates one
	octave; a size of 15 indicates two octaves.
-->
<!ELEMENT octave-shift EMPTY>
<!ATTLIST octave-shift
    type (up | down | stop) #REQUIRED
    number %number-level; #IMPLIED
    size CDATA "8"
    %print-style; 
>

<!-- 
	The harp-pedals element is used to create harp pedal
	diagrams. The pedal-step and pedal-alter elements use
	the same values as the step and alter elements. For
	easiest reading, the pedal-tuning elements should follow
	standard harp pedal order, with pedal-step values of
	D, C, B, E, F, G, and A.
-->
<!ELEMENT harp-pedals (pedal-tuning)+>
<!ATTLIST harp-pedals
    %print-style; 
>
<!ELEMENT pedal-tuning (pedal-step, pedal-alter)>
<!ELEMENT pedal-step (#PCDATA)>
<!ELEMENT pedal-alter (#PCDATA)>

<!-- Harp damping marks -->
<!ELEMENT damp EMPTY>
<!ATTLIST damp
    %print-style; 
>
<!ELEMENT damp-all EMPTY>
<!ATTLIST damp-all
    %print-style; 
>

<!-- Eyeglasses, common in commercial music. -->
<!ELEMENT eyeglasses EMPTY>
<!ATTLIST eyeglasses
    %print-style; 
>

<!-- 
	Scordatura string tunings are represented by a series
	of accord elements. The tuning-step, tuning-alter, 
	and tuning-octave elements are also used with the 
	staff-tuning element, and are defined in the common.mod
	file. Strings are numbered from high to low.
-->
<!ELEMENT scordatura (accord+)>
<!ELEMENT accord
	(tuning-step, tuning-alter?, tuning-octave)>
<!ATTLIST accord
    string CDATA #REQUIRED
>

<!--
	The image element is used to include graphical images
	in a score. The required source attribute is the URL
	for the image file. The required type attribute is the
	MIME type for the image file format. Typical choices
	include application/postscript, image/gif, image/jpeg,
	image/png, and image/tiff.
-->
<!ELEMENT image EMPTY>
<!ATTLIST image
    source CDATA #REQUIRED
    type CDATA #REQUIRED
    %position;
    %halign;
    %valign-image; 
>

<!--
	The accordion-registration element is use for accordion
	registration symbols. These are circular symbols divided
	horizontally into high, middle, and low sections that
	correspond to 4', 8', and 16' pipes. Each accordion-high,
	accordion-middle, and accordion-low element represents
	the presence of one or more dots in the registration
	diagram. The accordion-middle element may have text
	values of 1, 2, or 3, corresponding to have 1 to 3 dots
	in the middle section. An accordion-registration element
	needs to have at least one of the child elements present.
-->
<!ELEMENT accordion-registration
	(accordion-high?, accordion-middle?, accordion-low?)>
<!ATTLIST accordion-registration
    %print-style; 
>
<!ELEMENT accordion-high EMPTY>
<!ELEMENT accordion-middle (#PCDATA)>
<!ELEMENT accordion-low EMPTY>

<!--
	The other-direction element is used to define any direction
	symbols not yet in the current version of the MusicXML
	format. This allows extended representation, though without
	application interoperability.
-->
<!ELEMENT other-direction (#PCDATA)>
<!ATTLIST other-direction
	%print-object;
    %print-style; 
>

<!--
	An offset is represented in terms of divisions, and
	indicates where the direction will appear relative to
	the current musical location. This affects the visual
	appearance of the direction. If the sound attribute is
	"yes", then the offset affects playback too. If the sound
	attribute is "no", then any sound associated with the
	direction takes effect at the current location. The sound
	attribute is "no" by default for compatibility with earlier
	versions of the MusicXML format. If an element within a
	direction includes a default-x attribute, the offset value
	will be ignored when determining the appearance of that
	element.
-->
<!ELEMENT offset (#PCDATA)>
<!ATTLIST offset
    sound %yes-no; #IMPLIED
>

<!-- Harmony -->

<!--
	The harmony elements are based on Humdrum's **harm
	encoding, extended to support chord symbols in popular
	music as well as functional harmony analysis in classical
	music.
	
	If there are alternate harmonies possible, this can be
	specified using multiple harmony elements differentiated
	by type. Explicit harmonies have all note present in the
	music; implied have some notes missing but implied;
	alternate represents alternate analyses. 
	
	The harmony object may be used for analysis or for
	chord symbols. The print-object attribute controls
	whether or not anything is printed due to the harmony
	element. The print-frame attribute controls printing
	of a frame or fretboard diagram. The print-style entity
	sets the default for the harmony, but individual elements
	can override this with their own print-style values.
	
	A harmony element can contain many stacked chords (e.g.
	V of II). A sequence of harmony-chord entities is used
	for this type of secondary function, where V of II would
	be represented by a harmony-chord with a V function
	followed by a harmony-chord with a II function.
-->
<!ENTITY % harmony-chord "((root | function), kind,
	inversion?, bass?, degree*)">

<!ELEMENT harmony ((%harmony-chord;)+, frame?, 
	offset?, %editorial;, staff?)>
<!ATTLIST harmony
    type (explicit | implied | alternate) #IMPLIED
    %print-object;
    print-frame  %yes-no; #IMPLIED
    %print-style;
    %placement;
>

<!--
	A root is a pitch name like C, D, E, where a function
	is an indication like I, II, III. Root is generally
	used with pop chord symbols, function with classical
	functional harmony. It is an either/or choice to avoid
	data inconsistency. Function requires that the key be
	specified in the encoding. 

	The root element has a root-step and optional root-alter 
	similar to the step and alter elements in a pitch, but
	renamed to distinguish the different musical meanings.
	The root-step text element indicates how the root should
	appear on the page if not using the element contents.
	In some chord styles, this will include the root-alter
	information as well. In that case, the print-object
	attribute of the root-alter element can be set to no.
	The root-alter location attribute indicates whether
	the alteration should appear to the left or the right
	of the root-step; it is right by default.
-->
<!ELEMENT root (root-step, root-alter?)>
<!ELEMENT root-step (#PCDATA)>
<!ATTLIST root-step
    text CDATA #IMPLIED
    %print-style;
>
<!ELEMENT root-alter (#PCDATA)>
<!ATTLIST root-alter
    %print-object;
    %print-style;
    location %left-right; #IMPLIED
>
<!ELEMENT function (#PCDATA)>
<!ATTLIST function
    %print-style;
>

<!--
	Kind indicates the type of chord. Degree elements
	can then add, subtract, or alter from these
	starting points. Values include:
	
	Triads:
	    major (major third, perfect fifth)
	    minor (minor third, perfect fifth)
	    augmented (major third, augmented fifth)
	    diminished (minor third, diminished fifth)
	Sevenths:
	    dominant (major triad, minor seventh)
	    major-seventh (major triad, major seventh)
	    minor-seventh (minor triad, minor seventh)
	    diminished-seventh
	        (diminished triad, diminished seventh)
	    augmented-seventh
	        (augmented triad, minor seventh)
	    half-diminished
	        (diminished triad, minor seventh)
	    major-minor
	        (minor triad, major seventh)
	Sixths:
	    major-sixth (major triad, added sixth)
	    minor-sixth (minor triad, added sixth)
	Ninths:
	    dominant-ninth (dominant-seventh, major ninth)
	    major-ninth (major-seventh, major ninth)
	    minor-ninth (minor-seventh, major ninth)
	11ths (usually as the basis for alteration):
	    dominant-11th (dominant-ninth, perfect 11th)
	    major-11th (major-ninth, perfect 11th)
	    minor-11th (minor-ninth, perfect 11th)
	13ths (usually as the basis for alteration):
	    dominant-13th (dominant-11th, major 13th)
	    major-13th (major-11th, major 13th)
	    minor-13th (minor-11th, major 13th)
	Suspended:
	    suspended-second (major second, perfect fifth)
	    suspended-fourth (perfect fourth, perfect fifth)
	Functional sixths:
	    Neapolitan
	    Italian
	    French
	    German
	Other:
	    pedal (pedal-point bass)
	    power (perfect fifth)
	    Tristan
	
	The "other" kind is used when the harmony is entirely
	composed of add elements. The "none" kind is used to
	explicitly encode absence of chords or functional
	harmony.

	The attributes are used to indicate the formatting
	of the symbol. Since the kind element is the constant
	in all the harmony-chord entities that can make up
	a polychord, many formatting attributes are here.

	The use-symbols attribute is yes if the kind should be
	represented when possible with harmony symbols rather
	than letters and numbers. These symbols include:

	    major: a triangle, like Unicode 25B3
	    minor: -, like Unicode 002D
	    augmented: +, like Unicode 002B
	    diminished: °, like Unicode 00B0
	    half-diminished: ø, like Unicode 00F8

	The text attribute describes how the kind should be 
	spelled if not using symbols; it is ignored if use-symbols
	is yes. The stack-degrees attribute is yes if the degree 
	elements should be stacked above each other. The 
	parentheses-degrees attribute is yes if all the degrees 
	should be in parentheses. The bracket-degrees attribute
	is yes if all the degrees should be in a bracket. If not 
	specified, these values are implementation-specific.
	The alignment attributes are for the entire harmony-chord
	entity of which this kind element is a part.
-->
<!ELEMENT kind (#PCDATA)>
<!ATTLIST kind
    use-symbols          %yes-no;   #IMPLIED
    text                 CDATA      #IMPLIED
    stack-degrees        %yes-no;   #IMPLIED
    parentheses-degrees  %yes-no;   #IMPLIED
    bracket-degrees      %yes-no;   #IMPLIED
    %print-style;
    %halign;
    %valign;
>

<!--
	Inversion is a number indicating which inversion is used:
	0 for root position, 1 for first inversion, etc.
-->
<!ELEMENT inversion (#PCDATA)>
<!ATTLIST inversion
    %print-style;
>

<!--
	Bass is used to indicate a bass note in popular music
	chord symbols, e.g. G/C. It is generally not used in
	functional harmony, as inversion is generally not used
	in pop chord symbols. As with root, it is divided into
	step and alter elements, similar to pitches. The attributes
	for bass-step and bass-alter work the same way as
	the corresponding root-step and root-alter attributes.
-->
<!ELEMENT bass (bass-step, bass-alter?)>
<!ELEMENT bass-step (#PCDATA)>
<!ATTLIST bass-step
    text CDATA #IMPLIED
    %print-style;
>
<!ELEMENT bass-alter (#PCDATA)>
<!ATTLIST bass-alter
    %print-object;
    %print-style;
    location (left | right) #IMPLIED
>

<!--
	The degree element is used to add, alter, or subtract
	individual notes in the chord. The degree-value element
	is a number indicating the degree of the chord (1 for
	the root, 3 for third, etc). The degree-alter element
	is like the alter element in notes: 1 for sharp, -1 for
	flat, etc. The degree-type element can be add, alter, or
	subtract. If the degree-type is alter or subtract, the
	degree-alter is relative to the degree already in the
	chord based on its kind element. If the degree-type is
	add, the degree-alter is relative to a dominant chord
	(major and perfect intervals except for a minor 
	seventh). The print-object attribute can be used to
	keep the degree from printing separately when it has
	already taken into account in the text attribute of
	the kind element. The plus-minus attribute is used to
	indicate if plus and minus symbols should be used
	instead of sharp and flat symbols to display the degree
	alteration; it is no by default. The degree-value and
	degree-type text attributes specify how the value and
	type of the degree should be displayed.
	
	A harmony of kind "other" can be spelled explicitly by
	using a series of degree elements together with a root.
-->
<!ELEMENT degree (degree-value, degree-alter, degree-type)>
<!ATTLIST degree
    %print-object;
>
<!ELEMENT degree-value (#PCDATA)>
<!ATTLIST degree-value
    text CDATA #IMPLIED
    %print-style;
>
<!ELEMENT degree-alter (#PCDATA)>
<!ATTLIST degree-alter
    %print-style;
    plus-minus %yes-no; #IMPLIED
>
<!ELEMENT degree-type (#PCDATA)>
<!ATTLIST degree-type
    text CDATA #IMPLIED
    %print-style;
>

<!--
	The frame element represents a frame or fretboard diagram
	used together with a chord symbol. The representation is
	based on the NIFF guitar grid with additional information.
	The frame-strings and frame-frets elements give the overall
	size of the frame in vertical lines (strings) and horizontal
	spaces (frets). The first-fret indicates which fret is shown
	in the top space of the frame; it is fret 1 if the element
	is not present. The optional text attribute indicates how
	this is represented in the fret diagram, while the location
	attribute indicates whether the text appears to the left or
	right of the frame. The frame-note element represents each
	note included in the frame. The definitions for string,
	fret, and fingering are found in the common.mod file. An
	open string will have a fret value of 0, while a muted
	string will not be associated with a frame-note element.
-->
<!ELEMENT frame 
	(frame-strings, frame-frets, first-fret?, frame-note+)>
<!ATTLIST frame 
    %position;
    %color;
    %halign;
    %valign;
    height  %tenths;  #IMPLIED
    width   %tenths;  #IMPLIED
>
<!ELEMENT frame-strings (#PCDATA)>
<!ELEMENT frame-frets (#PCDATA)>
<!ELEMENT first-fret (#PCDATA)>
<!ATTLIST first-fret
    text CDATA #IMPLIED
    location %left-right; #IMPLIED
>
<!ELEMENT frame-note (string, fret, fingering?, barre?)>

<!-- 
	The barre element indicates placing a finger over 
	multiple strings on a single fret. The type is "start" 
	for the lowest pitched string (e.g., the string with 
	the highest MusicXML number) and is "stop" for the 
	highest pitched string.
-->
<!ELEMENT barre EMPTY>
<!ATTLIST barre
    type %start-stop; #REQUIRED
    %color;
>

<!--
	The grouping element is used for musical analysis. When
	the element type is "start" or "single", it usually contains
	one or more feature elements. The number attribute is used
	for distinguishing between overlapping and hierarchical
	groupings. The member-of attribute allows for easy
	distinguishing of what grouping elements are in what
	hierarchy. Feature elements contained within a "stop"
	type of grouping may be ignored.
	
	This element is flexible to allow for non-standard analyses.
	Future versions of the MusicXML format may add elements
	that can represent more standardized categories of analysis
	data, allowing for easier data sharing.
-->
<!ELEMENT grouping ((feature)*)>
<!ATTLIST grouping
    type %start-stop-single; #REQUIRED
    number CDATA "1"
    member-of CDATA #IMPLIED
>
<!ELEMENT feature (#PCDATA)>
<!ATTLIST feature
    type CDATA #IMPLIED
>

<!--
	The print element contains general printing parameters,
	including the layout elements defined in the layout.mod
	file. The part-name-display and part-abbreviation-display
	elements used in the score.mod file may also be used here
	to change how a part name or abbreviation is displayed over
	the course of a piece. They take effect when the current
	measure or a succeeding measure starts a new system.
	
	The new-system and new-page attributes indicate whether
	to force a system or page break, or to force the current
	music onto the same system or page as the preceding music.
	Normally this is the first music data within a measure.
	If used in multi-part music, they should be placed in the
	same positions within each part, or the results are
	undefined. The page-number attribute sets the number of a
	new page; it is ignored if new-page is not "yes". Version
	2.0 adds a blank-page attribute. This is a positive integer
	value that specifies the number of blank pages to insert
	before the current measure. It is ignored if new-page is
	not "yes". These blank pages have no music, but may have
	text or images specified by the credit element. This is
	used to allow a combination of pages that are all text,
	or all text and images, together with pages of music.

	Staff spacing between multiple staves is measured in
	tenths of staff lines (e.g. 100 = 10 staff lines). This is
	deprecated as of Version 1.1; the staff-layout element
	should be used instead. If both are present, the
	staff-layout values take priority.

	Layout elements in a print statement only apply to the
	current page, system, staff, or measure. Music that
	follows continues to take the default values from the
	layout included in the defaults element.
-->
<!ELEMENT print (page-layout?, system-layout?, staff-layout*,
    measure-layout?, measure-numbering?, part-name-display?, 
    part-abbreviation-display?)>
<!ATTLIST print
    staff-spacing %tenths; #IMPLIED
    new-system %yes-no; #IMPLIED
    new-page %yes-no; #IMPLIED
    blank-page NMTOKEN #IMPLIED
    page-number CDATA #IMPLIED	
>

<!--
	The measure-numbering element describes how measure
	numbers are displayed on this part. Values may be none,
	measure, or system. The number attribute from the measure
	element is used for printing. Measures with an implicit
	attribute set to "yes" never display a measure number,
	regardless of the measure-numbering setting.
-->
<!ELEMENT measure-numbering (#PCDATA)>
<!ATTLIST measure-numbering
    %print-style;
>

<!-- 
	The sound element contains general playback parameters.
	They can stand alone within a part/measure, or be a
	component element within a direction.
	
	Tempo is expressed in quarter notes per minute. If 0,
	the sound-generating program should prompt the user at the
	time of compiling a sound (MIDI) file.
	
	Dynamics (or MIDI velocity) are expressed as a percentage
	of the default forte value (90 for MIDI 1.0).
	
	Dacapo indicates to go back to the beginning of the
	movement. When used it always has the value "yes".
	
	Segno and dalsegno are used for backwards jumps to a
	segno sign; coda and tocoda are used for forward jumps
	to a coda sign. If there are multiple jumps, the value
	of these parameters can be used to name and distinguish
	them. If segno or coda is used, the divisions attribute
	can also be used to indicate the number of divisions
	per quarter note. Otherwise sound and MIDI generating
	programs may have to recompute this.
	
	By default, a dalsegno or dacapo attribute indicates that
	the jump should occur the first time through, while a 
	tocoda attribute indicates the jump should occur the second
	time through. The time that jumps occur can be changed by
	using the time-only attribute.
	
	Forward-repeat is used when a forward repeat sign is
	implied, and usually follows a bar line. When used it
	always has the value of "yes".
	
	The fine attribute follows the final note or rest in a
	movement with a da capo or dal segno direction. If numeric,
	the value represents the actual duration of the final note or
	rest, which can be ambiguous in written notation and 
	different among parts and voices. The value may also be 
	"yes" to indicate no change to the final duration.
	
	If the sound element applies only one time through a
	repeat, the time-only attribute indicates which time
	to apply the sound element.
	
	Pizzicato in a sound element effects all following notes.
	Yes indicates pizzicato, no indicates arco.

	The pan and elevation attributes are deprecated in 
	Version 2.0. The pan and elevation elements in
	the midi-instrument element should be used instead.
	The meaning of the pan and elevation attributes is
	the same as for the pan and elevation elements. If
	both are present, the mid-instrument elements take
	priority.
	
	The damper-pedal, soft-pedal, and sostenuto-pedal 
	attributes effect playback of the three common piano
	pedals and their MIDI controller equivalents. The yes
	value indicates the pedal is depressed; no indicates 
	the pedal is released. A numeric value from 0 to 100
	may also be used for half pedaling. This value is the
	percentage that the pedal is depressed. A value of 0 is
	equivalent to no, and a value of 100 is equivalent to yes.
	
	MIDI instruments are changed using the midi-instrument
	element defined in the common.mod file.

	The offset element is used to indicate that the sound takes
	place offset from the current score position. If the sound
	element is a child of a direction element, the sound offset
	element overrides the direction offset element if both 
	elements are present. Note that the offset reflects the
	intended musical position for the change in sound. It
	should not be used to compensate for latency issues in 
	particular hardware configurations.
-->
<!ELEMENT sound ((midi-instrument*), offset?)>
<!ATTLIST sound
    tempo CDATA #IMPLIED
    dynamics CDATA #IMPLIED
    dacapo %yes-no; #IMPLIED
    segno CDATA #IMPLIED
    dalsegno CDATA #IMPLIED
    coda CDATA #IMPLIED
    tocoda CDATA #IMPLIED
    divisions CDATA #IMPLIED
    forward-repeat %yes-no; #IMPLIED
    fine CDATA #IMPLIED
    time-only CDATA #IMPLIED
    pizzicato %yes-no; #IMPLIED
    pan CDATA #IMPLIED
    elevation CDATA #IMPLIED
    damper-pedal %yes-no-number; #IMPLIED
    soft-pedal %yes-no-number; #IMPLIED
    sostenuto-pedal %yes-no-number; #IMPLIED
>
