<!--
	MusicXML™ note.mod module

	Version 3.0
	
	Copyright © 2004-2011 MakeMusic, Inc.
	http://www.makemusic.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Public License Version 3.0,
	available from:
	
		http://www.musicxml.org/dtds/license.html
-->

<!--
	The note DTD module contains the note representations for
	the MusicXML format. It contains the note element, all its
	children elements, and related entities.
-->

<!-- Entities -->

<!-- Structures -->

<!--
	The common note elements between cue/grace notes and
	regular (full) notes: pitch, chord, and rest information,
	but not duration (cue and grace notes do not have
	duration encoded here). Unpitched elements are used for
	unpitched percussion, speaking voice, and other musical
	elements lacking determinate pitch.
-->
<!ENTITY % full-note "(chord?, (pitch | unpitched | rest))">

<!-- Elements -->

<!--
	Notes are the most common type of MusicXML data. The
	MusicXML format keeps the MuseData distinction between
	elements used for sound information and elements used for
	notation information (e.g., tie is used for sound, tied for
	notation). Thus grace notes do not have a duration element.
	Cue notes have a duration element, as do forward elements,
	but no tie elements. Having these two types of information
	available can make interchange considerably easier, as
	some programs handle one type of information much more
	readily than the other. 
-->
<!ELEMENT note 
	(((grace, %full-note;, (tie, tie?)?) |
	  (cue, %full-note;, duration) |
	  (%full-note;, duration, (tie, tie?)?)),
	 instrument?, %editorial-voice;, type?, dot*,
	 accidental?, time-modification?, stem?, notehead?,
	 notehead-text?, staff?, beam*, notations*, lyric*, play?)>

<!--
	The position and printout entities for printing suggestions
	are defined in the common.mod file.
	
	The dynamics and end-dynamics attributes correspond to
	MIDI 1.0's Note On and Note Off velocities, respectively.
	They are expressed in terms of percentages of the default
	forte value (90 for MIDI 1.0). The attack and release
	attributes are used to alter the starting and stopping time
	of the note from when it would otherwise occur based on
	the flow of durations - information that is specific to a
	performance. They are expressed in terms of divisions,
	either positive or negative. A note that starts a tie should
	not have a release attribute, and a note that stops a tie
	should not have an attack attribute. If a note is played
	only particular times through a repeat, the time-only entity
	shows which times to play the note. The pizzicato attribute
	is used when just this note is sounded pizzicato, vs. the
	pizzicato element which changes overall playback between
	pizzicato and arco.
-->
<!ATTLIST note
    %print-style; 
    %printout;
    dynamics CDATA #IMPLIED
    end-dynamics CDATA #IMPLIED
    attack CDATA #IMPLIED
    release CDATA #IMPLIED
    %time-only;
    pizzicato %yes-no; #IMPLIED
>

<!--
	Pitch is represented as a combination of the step of the
	diatonic scale, the chromatic alteration, and the octave.
	The step element uses the English letters A through G. 
	The alter element represents chromatic alteration in
	number of semitones (e.g., -1 for flat, 1 for sharp).
	Decimal values like 0.5 (quarter tone sharp) are 
	used for microtones. The octave element is represented
	by the numbers 0 to 9, where 4 indicates the octave
	started by middle C.
-->
<!ELEMENT pitch (step, alter?, octave)>
<!ELEMENT step (#PCDATA)>
<!ELEMENT alter (#PCDATA)>
<!ELEMENT octave (#PCDATA)>

<!--
	The cue and grace elements indicate the presence of cue and
	grace notes. The slash attribute for a grace note is yes for
	slashed eighth notes. The other grace note attributes come
	from MuseData sound suggestions. The steal-time-previous
	attribute indicates the percentage of time to steal from the
	previous note for the grace note. The steal-time-following
	attribute indicates the percentage of time to steal from the
	following note for the grace note, as for appoggiaturas. The
	make-time attribute indicates to make time, not steal time;
	the units are in real-time divisions for the grace note. 
-->
<!ELEMENT cue EMPTY>
<!ELEMENT grace EMPTY>
<!ATTLIST grace
    steal-time-previous CDATA #IMPLIED
    steal-time-following CDATA #IMPLIED
    make-time CDATA #IMPLIED
    slash %yes-no; #IMPLIED
>

<!--
	The chord element indicates that this note is an additional
	chord tone with the preceding note. The duration of this
	note can be no longer than the preceding note. In MuseData,
	a missing duration indicates the same length as the previous
	note, but the MusicXML format requires a duration for chord
	notes too.
-->
<!ELEMENT chord EMPTY>

<!--
	The unpitched element indicates musical elements that are
	notated on the staff but lack definite pitch, such as
	unpitched percussion and speaking voice. Like notes, it
	uses step and octave elements to indicate placement on the
	staff, following the current clef. If percussion clef is
	used, the display-step and display-octave elements are
	interpreted as if in treble clef, with a G in octave 4 on
	line 2. If not present, the note is placed on the middle
	line of the staff, generally used for a one-line staff.
-->
<!ELEMENT unpitched ((display-step, display-octave)?)>
<!ELEMENT display-step (#PCDATA)>
<!ELEMENT display-octave (#PCDATA)>

<!--
	The rest element indicates notated rests or silences. Rest
	elements are usually empty, but placement on the staff can
	be specified using display-step and display-octave
	elements. If the measure attribute is set to yes, it
	indicates this is a complete measure rest.
-->
<!ELEMENT rest ((display-step, display-octave)?)>
<!ATTLIST rest
    measure %yes-no; #IMPLIED
>

<!--
	Duration is a positive number specified in division units.
	This is the intended duration vs. notated duration (for
	instance, swing eighths vs. even eighths, or differences
	in dotted notes in Baroque-era music). Differences in
	duration specific to an interpretation or performance
	should use the note element's attack and release
	attributes. 

	The tie element indicates that a tie begins or ends with
	this note. If the tie element applies only particular times
	through a repeat, the time-only attribute indicates which
	times to apply it. The tie element indicates sound; the tied
	element indicates notation. 
-->
<!ELEMENT duration (#PCDATA)>
<!ELEMENT tie EMPTY>
<!ATTLIST tie
    type %start-stop; #REQUIRED
    %time-only;
>

<!--
	If multiple score-instruments are specified on a
	score-part, there should be an instrument element for
	each note in the part. The id attribute is an IDREF back
	to the score-instrument ID.
-->
<!ELEMENT instrument EMPTY>
<!ATTLIST instrument
    id IDREF #REQUIRED
>

<!--
	Type indicates the graphic note type, Valid values (from
	shortest to longest) are 1024th, 512th, 256th, 128th,
	64th, 32nd, 16th, eighth, quarter, half, whole, breve,
	long, and maxima. The size attribute indicates full, cue,
	or large size, with full the default for regular notes and
	cue the default for cue and grace notes.
-->
<!ELEMENT type (#PCDATA)>
<!ATTLIST type
    size %symbol-size; #IMPLIED
>

<!--
	One dot element is used for each dot of prolongation.
	The placement element is used to specify whether the
	dot should appear above or below the staff line. It is
	ignored for notes that appear on a staff space.
-->
<!ELEMENT dot EMPTY>
<!ATTLIST dot
    %print-style;
    %placement; 
>

<!--
	Actual notated accidentals. Valid values include: sharp,
	natural, flat, double-sharp, sharp-sharp, flat-flat,
	natural-sharp, natural-flat, quarter-flat, quarter-sharp,
	three-quarters-flat, three-quarters-sharp, sharp-down,
	sharp-up, natural-down, natural-up, flat-down, flat-up,
	triple-sharp, triple-flat, slash-quarter-sharp,
	slash-sharp, slash-flat, double-slash-flat, sharp-1,
	sharp-2, sharp-3, sharp-5, flat-1, flat-2, flat-3,
	flat-4, sori, and koron.

	The quarter- and three-quarters- accidentals are
	Tartini-style quarter-tone accidentals. The -down and -up
	accidentals are quarter-tone accidentals that include
	arrows pointing down or up. The slash- accidentals
	are used in Turkish classical music. The numbered
	sharp and flat accidentals are superscripted versions
	of the accidental signs, used in Turkish folk music.
	The sori and koron accidentals are microtonal sharp and
	flat accidentals used in Iranian and Persian music.

	Editorial and cautionary indications are indicated
	by attributes. Values for these attributes are "no" if not 
	present. Specific graphic display such as parentheses, 
	brackets, and size are controlled by the level-display
	entity defined in the common.mod file.
-->
<!ELEMENT accidental (#PCDATA)>
<!ATTLIST accidental
    cautionary %yes-no; #IMPLIED
    editorial %yes-no; #IMPLIED
    %level-display;
    %print-style;
>

<!--
	Time modification indicates tuplets, double-note tremolos,
	and other durational changes. A time-modification element
	shows how the cumulative, sounding effect of tuplets and
	double-note tremolos compare to the written note type 
	represented by the type and dot elements. The child elements
	are defined in the common.mod file. Nested tuplets and other
	notations that use more detailed information need both the 
	time-modification and tuplet elements to be represented
	accurately.
-->
<!ELEMENT time-modification
	(actual-notes, normal-notes, (normal-type, normal-dot*)?)>

<!--
	Stems can be down, up, none, or double. For down and up
	stems, the position attributes can be used to specify
	stem length. The relative values specify the end of the
	stem relative to the program default. Default values
	specify an absolute end stem position. Negative values of
	relative-y that would flip a stem instead of shortening
	it are ignored. A stem element associated with a rest 
	refers to a stemlet.
-->
<!ELEMENT stem (#PCDATA)>
<!ATTLIST stem
    %position;
    %color;
>

<!--
	The notehead element indicates shapes other than the open
	and closed ovals associated with note durations. The element
	value can be slash, triangle, diamond, square, cross, x,
	circle-x, inverted triangle, arrow down, arrow up, slashed,
	back slashed, normal, cluster, circle dot, left triangle,
	rectangle, or none. For shape note music, the element values
	do, re, mi, fa, fa up, so, la, and ti are also used,
	corresponding to Aikin's 7-shape system. The fa up shape is
	typically used with upstems; the fa shape is typically used
	with downstems or no stems.

	The arrow shapes differ from triangle and inverted triangle
	by being centered on the stem. Slashed and back slashed 
	notes include both the normal notehead and a slash. The 
	triangle shape has the tip of the triangle pointing up;
	the inverted triangle shape has the tip of the triangle 
	pointing down. The left triangle shape is a right triangle
	with the hypotenuse facing up and to the left.
	
	For the enclosed shapes, the default is to be hollow for
	half notes and longer, and filled otherwise. The filled
	attribute can be set to change this if needed.
	
	If the parentheses attribute is set to yes, the notehead
	is parenthesized. It is no by default.

	The notehead-text element indicates text that is displayed
	inside a notehead, as is done in some educational music. 
	It is not needed for the numbers used in tablature or jianpu 
	notation. The presence of a TAB or jianpu clefs is sufficient
	to indicate that numbers are used. The display-text and
	accidental-text elements allow display of fully formatted
	text and accidentals.
-->
<!ELEMENT notehead (#PCDATA)>
<!ATTLIST notehead
    filled %yes-no; #IMPLIED
    parentheses %yes-no; #IMPLIED
    %font;
    %color;
>
<!ELEMENT notehead-text
	((display-text | accidental-text)+)>

<!--
	Beam types include begin, continue, end, forward hook, and
	backward hook. Up to eight concurrent beams are available to
	cover up to 1024th notes, using an enumerated type defined
	in the common.mod file. Each beam in a note is represented
	with a separate beam element, starting with the eighth note
	beam using a number attribute of 1.

	Note that the beam number does not distinguish sets of
	beams that overlap, as it does for slur and other elements.
	Beaming groups are distinguished by being in different
	voices and/or the presence or absence of grace and cue
	elements.

	Beams that have a begin value can also have a fan attribute to
	indicate accelerandos and ritardandos using fanned beams. The
	fan attribute may also be used with a continue value if the
	fanning direction changes on that note. The value is "none"
	if not specified.
	
	The repeater attribute has been deprecated in MusicXML 3.0.
	Formerly used for tremolos, it needs to be specified with a
	"yes" value for each beam using it.
-->
<!ELEMENT beam (#PCDATA)>
<!ATTLIST beam
    number %beam-level; "1"
    repeater %yes-no; #IMPLIED
    fan (accel | rit | none) #IMPLIED
    %color;
>

<!--
	Notations are musical notations, not XML notations. Multiple
	notations are allowed in order to represent multiple editorial
	levels. The print-object attribute, added in Version 3.0,
	allows notations to represent details of performance technique,
	such as fingerings, without having them appear in the score.
-->
<!ELEMENT notations
	(%editorial;, 
	 (tied | slur | tuplet | glissando | slide | 
	  ornaments | technical | articulations | dynamics |
	  fermata | arpeggiate | non-arpeggiate | 
	  accidental-mark | other-notation)*)>
<!ATTLIST notations
    %print-object;
>

<!--
	The tied element represents the notated tie. The tie element 
	represents the tie sound.

	The number attribute is rarely needed to disambiguate ties,
	since note pitches will usually suffice. The attribute is
	implied rather than defaulting to 1 as with most elements. 
	It is available for use in more complex tied notation 
	situations.
-->
<!ELEMENT tied EMPTY>
<!ATTLIST tied
    type %start-stop-continue; #REQUIRED
    number %number-level; #IMPLIED
    %line-type;
    %dashed-formatting;
    %position;
    %placement;
    %orientation;
    %bezier;
    %color;
>

<!--
	Slur elements are empty. Most slurs are represented with
	two elements: one with a start type, and one with a stop
	type. Slurs can add more elements using a continue type.
	This is typically used to specify the formatting of cross-
	system slurs, or to specify the shape of very complex slurs.
-->
<!ELEMENT slur EMPTY>
<!ATTLIST slur
    type %start-stop-continue; #REQUIRED
    number %number-level; "1"
    %line-type;
    %dashed-formatting;
    %position;
    %placement;
    %orientation;
    %bezier;
    %color;
>

<!--
	A tuplet element is present when a tuplet is to be displayed
	graphically, in addition to the sound data provided by the
	time-modification elements. The number attribute is used to
	distinguish nested tuplets. The bracket attribute is used
	to indicate the presence of a bracket. If unspecified, the
	results are implementation-dependent. The line-shape
	attribute is used to specify whether the bracket is straight
	or in the older curved or slurred style. It is straight by
	default.
	
	Whereas a time-modification element shows how the cumulative,
	sounding effect of tuplets and double-note tremolos compare to
	the written note type, the tuplet element describes how this
	is displayed. The tuplet element also provides more detailed
	representation information than the time-modification element,
	and is needed to represent nested tuplets and other complex
	tuplets accurately. The tuplet-actual and tuplet-normal
	elements provide optional full control over tuplet
	specifications. Each allows the number and note type
	(including dots) describing a single tuplet. If any of
	these elements are absent, their values are based on the
	time-modification element.
	
	The show-number attribute is used to display either the
	number of actual notes, the number of both actual and
	normal notes, or neither. It is actual by default. The
	show-type attribute is used to display either the actual
	type, both the actual and normal types, or neither. It is
	none by default.
-->
<!ELEMENT tuplet (tuplet-actual?, tuplet-normal?)>
<!ATTLIST tuplet
    type %start-stop; #REQUIRED
    number %number-level; #IMPLIED
    bracket %yes-no; #IMPLIED
    show-number (actual | both | none) #IMPLIED
    show-type (actual | both | none) #IMPLIED
    %line-shape;
    %position;
    %placement;
>
<!ELEMENT tuplet-actual (tuplet-number?,
	tuplet-type?, tuplet-dot*)>
<!ELEMENT tuplet-normal (tuplet-number?,
	tuplet-type?, tuplet-dot*)>
<!ELEMENT tuplet-number (#PCDATA)>
<!ATTLIST tuplet-number
    %font;
    %color;
>
<!ELEMENT tuplet-type (#PCDATA)>
<!ATTLIST tuplet-type
    %font;
    %color;
>
<!ELEMENT tuplet-dot EMPTY>
<!ATTLIST tuplet-dot
    %font;
    %color;
>

<!--
	Glissando and slide elements both indicate rapidly moving
	from one pitch to the other so that individual notes are not
	discerned. The distinction is similar to that between NIFF's
	glissando and portamento elements. A glissando sounds the
	half notes in between the slide and defaults to a wavy line.
	A slide is continuous between two notes and defaults to a
	solid line. The optional text for a glissando or slide is
	printed alongside the line.
-->
<!ELEMENT glissando (#PCDATA)>
<!ATTLIST glissando
    type %start-stop; #REQUIRED
    number %number-level; "1"
    %line-type;
    %dashed-formatting; 
    %print-style; 
>
<!ELEMENT slide (#PCDATA)>
<!ATTLIST slide
    type %start-stop; #REQUIRED
    number %number-level; "1"
    %line-type; 
    %dashed-formatting; 
    %print-style; 
    %bend-sound;
>

<!--
	The other-notation element is used to define any notations
	not yet in the MusicXML format. This allows extended
	representation, though without application interoperability.
	It handles notations where more specific extension elements
	such as other-dynamics and other-technical are not
	appropriate.
-->
<!ELEMENT other-notation (#PCDATA)>
<!ATTLIST other-notation
    type %start-stop-single; #REQUIRED
    number %number-level; "1"
    %print-object;
    %print-style; 
    %placement;
>

<!--
	Ornaments can be any of several types, followed optionally
	by accidentals. The accidental-mark element's content is
	represented the same as an accidental element, but with a
	different name to reflect the different musical meaning.
-->
<!ELEMENT ornaments
	(((trill-mark | turn | delayed-turn | inverted-turn |
	   delayed-inverted-turn | vertical-turn | shake |
	   wavy-line | mordent | inverted-mordent | schleifer |
	   tremolo | other-ornament), accidental-mark*)*)>
<!ELEMENT trill-mark EMPTY>
<!ATTLIST trill-mark
    %print-style; 
    %placement; 
    %trill-sound; 
>

<!--
	The turn and delayed-turn elements are the normal turn
	shape which goes up then down. The inverted-turn and
	delayed-inverted-turn elements have the shape which goes
	down and then up. The delayed-turn and delayed-inverted-turn
	elements indicate turns that are delayed until the end of the
	current note. The vertical-turn element has the shape
	arranged vertically going from upper left to lower right.
	If the slash attribute is yes, then a vertical line is used
	to slash the turn; it is no by default.
-->
<!ELEMENT turn EMPTY>
<!ATTLIST turn
    %print-style; 
    %placement; 
    %trill-sound; 
    slash %yes-no; #IMPLIED
>
<!ELEMENT delayed-turn EMPTY>
<!ATTLIST delayed-turn
    %print-style; 
    %placement; 
    %trill-sound; 
    slash %yes-no; #IMPLIED
>
<!ELEMENT inverted-turn EMPTY>
<!ATTLIST inverted-turn
    %print-style; 
    %placement; 
    %trill-sound; 
    slash %yes-no; #IMPLIED
>
<!ELEMENT delayed-inverted-turn EMPTY>
<!ATTLIST delayed-inverted-turn
    %print-style; 
    %placement; 
    %trill-sound; 
    slash %yes-no; #IMPLIED
>
<!ELEMENT vertical-turn EMPTY>
<!ATTLIST vertical-turn
    %print-style; 
    %placement; 
    %trill-sound; 
>

<!ELEMENT shake EMPTY>
<!ATTLIST shake
    %print-style; 
    %placement; 
    %trill-sound; 
>

<!--
	The wavy-line element is defined in the common.mod file,
	as it applies to more than just note elements.
-->

<!-- 
	The long attribute for the mordent and inverted-mordent
	elements is "no" by default. The mordent element represents
	the sign with the vertical line; the inverted-mordent
	element represents the sign without the vertical line.
	The approach and departure attributes are used for compound
	ornaments, indicating how the beginning and ending of the
	ornament look relative to the main part of the mordent.
-->
<!ELEMENT mordent EMPTY>
<!ATTLIST mordent
    long %yes-no; #IMPLIED
    approach %above-below; #IMPLIED
    departure %above-below; #IMPLIED
    %print-style; 
    %placement; 
    %trill-sound; 
>
<!ELEMENT inverted-mordent EMPTY>
<!ATTLIST inverted-mordent
    long %yes-no; #IMPLIED
    approach %above-below; #IMPLIED
    departure %above-below; #IMPLIED
    %print-style; 
    %placement; 
    %trill-sound; 
>

<!--
	The name for this ornament is based on the German,
	to avoid confusion with the more common slide element
	defined earlier.
-->
<!ELEMENT schleifer EMPTY>
<!ATTLIST schleifer
    %print-style; 
    %placement; 
>

<!--
	The tremolo ornament can be used to indicate either 
	single-note or double-note tremolos. Single-note tremolos
	use the single type, while double-note tremolos use the
	start and stop types. The default is "single" for
	compatibility with Version 1.1. The text of the element
	indicates the number of tremolo marks and is an integer
	from 0 to 8. Note that the number of attached beams is
	not included in this value, but is represented separately
	using the beam element.

	When using double-note tremolos, the duration of each note
	in the tremolo should correspond to half of the notated type
	value. A time-modification element should also be added with
	an actual-notes value of 2 and a normal-notes value of 1. If
	used within a tuplet, this 2/1 ratio should be multiplied by
	the existing tuplet ratio.

	Using repeater beams for indicating tremolos is deprecated as
	of MusicXML 3.0.
-->
<!ELEMENT tremolo (#PCDATA)>
<!ATTLIST tremolo
    type %start-stop-single; "single"
    %print-style; 
    %placement; 
>

<!--
	The other-ornament element is used to define any ornaments
	not yet in the MusicXML format. This allows extended
	representation, though without application interoperability.
-->
<!ELEMENT other-ornament (#PCDATA)>
<!ATTLIST other-ornament
    %print-style; 
    %placement; 
>

<!--
	An accidental-mark can be used as a separate notation or
	as part of an ornament. When used in an ornament, position
	and placement are relative to the ornament, not relative to
	the note.
-->
<!ELEMENT accidental-mark (#PCDATA)>
<!ATTLIST accidental-mark
    %print-style; 
    %placement; 
>

<!--
	Technical indications give performance information for
	individual instruments.
-->
<!ELEMENT technical
	((up-bow | down-bow | harmonic | open-string |
	  thumb-position | fingering | pluck | double-tongue |
	  triple-tongue | stopped | snap-pizzicato | fret |
	  string | hammer-on | pull-off | bend | tap | heel |
	  toe | fingernails | hole | arrow | handbell | 
	  other-technical)*)>

<!--
	The up-bow element represents the symbol that is used both
	for up-bowing on bowed instruments, and up-stroke on plucked
	instruments.
-->
<!ELEMENT up-bow EMPTY>
<!ATTLIST up-bow
    %print-style; 
    %placement; 
>

<!--
	The down-bow element represents the symbol that is used both
	for down-bowing on bowed instruments, and down-stroke on
	plucked instruments.
-->
<!ELEMENT down-bow EMPTY>
<!ATTLIST down-bow
    %print-style; 
    %placement; 
>

<!--
	The harmonic element indicates natural and artificial
	harmonics. Natural harmonics usually notate the base
	pitch rather than the sounding pitch. Allowing the type
	of pitch to be specified, combined with controls for
	appearance/playback differences, allows both the notation
	and the sound to be represented. Artificial harmonics can
	add a notated touching-pitch; the pitch or fret at which
	the string is touched lightly to produce the harmonic.
	Artificial pinch harmonics will usually not notate a
	touching pitch. The attributes for the harmonic element
	refer to the use of the circular harmonic symbol, typically
	but not always used with natural harmonics.
-->
<!ELEMENT harmonic
	((natural | artificial)?, 
	 (base-pitch | touching-pitch | sounding-pitch)?)>
<!ATTLIST harmonic
    %print-object;
    %print-style; 
    %placement; 
>
<!ELEMENT natural EMPTY>
<!ELEMENT artificial EMPTY>
<!ELEMENT base-pitch EMPTY>
<!ELEMENT touching-pitch EMPTY>
<!ELEMENT sounding-pitch EMPTY>

<!--
	The open-string element represents the zero-shaped 
	open string symbol.
-->
<!ELEMENT open-string EMPTY>
<!ATTLIST open-string
    %print-style; 
    %placement; 
>

<!--
	The thumb-position element represents the thumb position
	symbol. This is a circle with a line, where the line does
	not come within the circle. It is distinct from the snap
	pizzicato symbol, where the line comes inside the circle.
-->
<!ELEMENT thumb-position EMPTY>
<!ATTLIST thumb-position
    %print-style; 
    %placement; 
>

<!--
	The pluck element is used to specify the plucking fingering
	on a fretted instrument, where the fingering element refers
	to the fretting fingering. Typical values are p, i, m, a for
	pulgar/thumb, indicio/index, medio/middle, and anular/ring
	fingers.
-->
<!ELEMENT pluck (#PCDATA)>
<!ATTLIST pluck
    %print-style; 
    %placement; 
>

<!--
	The double-tongue element represents the double tongue symbol
	(two dots arranged horizontally).
-->
<!ELEMENT double-tongue EMPTY>
<!ATTLIST double-tongue
    %print-style; 
    %placement; 
>

<!--
	The triple-tongue element represents the triple tongue symbol
	(three dots arranged horizontally).
-->
<!ELEMENT triple-tongue EMPTY>
<!ATTLIST triple-tongue
    %print-style; 
    %placement; 
>

<!--
	The stopped element represents the stopped symbol, which looks
	like a plus sign.
-->
<!ELEMENT stopped EMPTY>
<!ATTLIST stopped
    %print-style; 
    %placement; 
>

<!--
	The snap-pizzicato element represents the snap pizzicato
	symbol. This is a circle with a line, where the line comes
	inside the circle. It is distinct from the thumb-position
	symbol, where the line does not come inside the circle.
-->
<!ELEMENT snap-pizzicato EMPTY>
<!ATTLIST snap-pizzicato
    %print-style; 
    %placement; 
>

<!--
	The hammer-on and pull-off elements are used in guitar
	and fretted instrument notation. Since a single slur
	can be marked over many notes, the hammer-on and pull-off
	elements are separate so the individual pair of notes can
	be specified. The element content can be used to specify
	how the hammer-on or pull-off should be notated. An empty
	element leaves this choice up to the application.
-->
<!ELEMENT hammer-on (#PCDATA)>
<!ATTLIST hammer-on
    type %start-stop; #REQUIRED
    number %number-level; "1"
    %print-style;
    %placement;
>
<!ELEMENT pull-off (#PCDATA)>
<!ATTLIST pull-off
    type %start-stop; #REQUIRED
    number %number-level; "1"
    %print-style;
    %placement;
>

<!--
	The bend element is used in guitar and tablature. The
	bend-alter element indicates the number of steps in the
	bend, similar to the alter element. As with the alter
	element, numbers like 0.5 can be used to indicate
	microtones. Negative numbers indicate pre-bends or
	releases; the pre-bend and release elements are used
	to distinguish what is intended. A with-bar element
	indicates that the bend is to be done at the bridge
	with a whammy or vibrato bar. The content of the
	element indicates how this should be notated.
-->
<!ELEMENT bend
	(bend-alter, (pre-bend | release)?, with-bar?)>
<!ATTLIST bend
    %print-style; 
    %bend-sound;
>
<!ELEMENT bend-alter (#PCDATA)>
<!ELEMENT pre-bend EMPTY>
<!ELEMENT release EMPTY>
<!ELEMENT with-bar (#PCDATA)>
<!ATTLIST with-bar
    %print-style; 
    %placement; 
>

<!--
	The tap element indicates a tap on the fretboard. The
	element content allows specification of the notation;
	+ and T are common choices. If empty, the display is
	application-specific.
-->
<!ELEMENT tap (#PCDATA)>
<!ATTLIST tap
    %print-style; 
    %placement; 
>

<!-- 
	The heel and toe element are used with organ pedals. The
	substitution value is "no" if the attribute is not present.
-->
<!ELEMENT heel EMPTY>
<!ATTLIST heel
    substitution %yes-no; #IMPLIED
    %print-style; 
    %placement; 
>
<!ELEMENT toe EMPTY>
<!ATTLIST toe
    substitution %yes-no; #IMPLIED
    %print-style; 
    %placement; 
>

<!-- 
	The fingernails element is used in notation for harp and
	other plucked string instruments.
-->
<!ELEMENT fingernails EMPTY>
<!ATTLIST fingernails
    %print-style; 
    %placement; 
>

<!--
	The hole element represents the symbols used for woodwind
	and brass fingerings as well as other notations. The content
	of the optional hole-type element indicates what the hole
	symbol represents in terms of instrument fingering or other
	techniques. The hole-closed element represents whether the
	hole is closed, open, or half-open. Valid element values are
	yes, no, and half. The optional location attribute indicates
	which portion of the hole is filled in when the element value
	is half. The optional hole-shape element indicates the shape
	of the hole symbol; the default is a circle.
-->
<!ELEMENT hole (hole-type?, hole-closed, hole-shape?)>
<!ATTLIST hole
    %print-style; 
    %placement; 
>
<!ELEMENT hole-type (#PCDATA)>
<!ELEMENT hole-closed (#PCDATA)>
<!ATTLIST hole-closed
    location (right | bottom | left | top) #IMPLIED 
>
<!ELEMENT hole-shape (#PCDATA)>

<!-- 
	The arrow element represents an arrow used for a musical
	technical indication. Straight arrows are represented with
	an arrow-direction element and an optional arrow-style
	element. Circular arrows are represented with a
	circular-arrow element. Descriptive values use Unicode
	arrow terminology.

	Values for the arrow-direction element are left, up, right,
	down, northwest, northeast, southeast, southwest, left right,
	up down, northwest southeast, northeast southwest, and other.

	Values for the arrow-style element are single, double,
	filled, hollow, paired, combined, and other. Filled and
	hollow arrows indicate polygonal single arrows. Paired
	arrows are duplicate single arrows in the same direction.
	Combined arrows apply to double direction arrows like
	left right, indicating that an arrow in one direction
	should be combined with an arrow in the other direction.

	Values for the circular-arrow element are clockwise and
	anticlockwise.
-->
<!ELEMENT arrow
	((arrow-direction, arrow-style?) | circular-arrow)>
<!ATTLIST arrow
    %print-style; 
    %placement; 
>
<!ELEMENT arrow-direction (#PCDATA)>
<!ELEMENT arrow-style (#PCDATA)>
<!ELEMENT circular-arrow (#PCDATA)>

<!-- 
	The handbell element represents notation for various
	techniques used in handbell and handchime music. Valid
	values are damp, echo, gyro, hand martellato, mallet lift,
	mallet table, martellato, martellato lift, 
	muted martellato, pluck lift, and swing.
-->
<!ELEMENT handbell (#PCDATA)>
<!ATTLIST handbell
    %print-style; 
    %placement; 
>

<!--
	The other-technical element is used to define any technical
	indications not yet in the MusicXML format. This allows
	extended representation, though without application
	interoperability.
-->
<!ELEMENT other-technical (#PCDATA)>
<!ATTLIST other-technical
    %print-style; 
    %placement; 
>

<!--
	Articulations and accents are grouped together here.
-->
<!ELEMENT articulations
	((accent | strong-accent | staccato | tenuto |
	  detached-legato | staccatissimo | spiccato |
	  scoop | plop | doit | falloff | breath-mark | 
	  caesura | stress | unstress | other-articulation)*)>

<!ELEMENT accent EMPTY>
<!ATTLIST accent
    %print-style; 
    %placement; 
>
<!ELEMENT strong-accent EMPTY>
<!ATTLIST strong-accent
    %print-style; 
    %placement; 
    type %up-down; "up"
>

<!-- 
	The staccato element is used for a dot articulation, as
	opposed to a stroke or a wedge.
-->
<!ELEMENT staccato EMPTY>
<!ATTLIST staccato
    %print-style; 
    %placement; 
>
<!ELEMENT tenuto EMPTY>
<!ATTLIST tenuto
    %print-style; 
    %placement; 
>
<!ELEMENT detached-legato EMPTY>
<!ATTLIST detached-legato
    %print-style; 
    %placement; 
>

<!--
	The staccatissimo element is used for a wedge articulation,
	as opposed to a dot or a stroke.
-->
<!ELEMENT staccatissimo EMPTY>
<!ATTLIST staccatissimo
    %print-style; 
    %placement; 
>

<!--
	The spiccato element is used for a stroke articulation, as
	opposed to a dot or a wedge.
-->
<!ELEMENT spiccato EMPTY>
<!ATTLIST spiccato
    %print-style; 
    %placement; 
>

<!-- 
	The scoop, plop, doit, and falloff elements are
	indeterminate slides attached to a single note.
	Scoops and plops come before the main note, coming
	from below and above the pitch, respectively. Doits
	and falloffs come after the main note, going above
	and below the pitch, respectively.
-->
<!ELEMENT scoop EMPTY>
<!ATTLIST scoop
    %line-shape;
    %line-type;
    %dashed-formatting;
    %print-style; 
    %placement;
>
<!ELEMENT plop EMPTY>
<!ATTLIST plop
    %line-shape;
    %line-type;
    %dashed-formatting;
    %print-style; 
    %placement; 
>
<!ELEMENT doit EMPTY>
<!ATTLIST doit
    %line-shape;
    %line-type;
    %dashed-formatting;
    %print-style; 
    %placement;
>
<!ELEMENT falloff EMPTY>
<!ATTLIST falloff
    %line-shape;
    %line-type;
    %dashed-formatting;
    %print-style; 
    %placement; 
>

<!--
	The breath-mark element may have a text value to
	indicate the symbol used for the mark. Valid values are
	comma, tick, and an empty string.
-->
<!ELEMENT breath-mark (#PCDATA)>
<!ATTLIST breath-mark
    %print-style; 
    %placement; 
>

<!ELEMENT caesura EMPTY>
<!ATTLIST caesura
    %print-style; 
    %placement; 
>
<!ELEMENT stress EMPTY>
<!ATTLIST stress
    %print-style; 
    %placement; 
>
<!ELEMENT unstress EMPTY>
<!ATTLIST unstress
    %print-style; 
    %placement; 
>

<!--
	The other-articulation element is used to define any
	articulations not yet in the MusicXML format. This allows
	extended representation, though without application
	interoperability.
-->
<!ELEMENT other-articulation (#PCDATA)>
<!ATTLIST other-articulation
    %print-style; 
    %placement; 
>

<!--
	The dynamics and fermata elements are defined in the
	common.mod file as they apply to more than just note
	elements.
-->

<!--
	The arpeggiate element indicates that this note is part of
	an arpeggiated chord. The number attribute can be used to
	distinguish between two simultaneous chords arpeggiated
	separately (different numbers) or together (same number).
	The up-down attribute is used if there is an arrow on the
	arpeggio sign. By default, arpeggios go from the lowest to
	highest note.
-->
<!ELEMENT arpeggiate EMPTY>
<!ATTLIST arpeggiate
    number %number-level; #IMPLIED
    direction %up-down; #IMPLIED
    %position; 
    %placement;
    %color; 
>

<!-- 
	The non-arpeggiate element indicates that this note is at
	the top or bottom of a bracket indicating to not arpeggiate
	these notes. Since this does not involve playback, it is
	only used on the top or bottom notes, not on each note
	as for the arpeggiate element.
-->
<!ELEMENT non-arpeggiate EMPTY>
<!ATTLIST non-arpeggiate
    type %top-bottom; #REQUIRED
    number %number-level; #IMPLIED
    %position; 
    %placement;
    %color; 
>

<!--
	Text underlays for lyrics, based on Humdrum with support
	for other formats. The lyric number indicates multiple
	lines, though a name can be used as well (as in Finale's
	verse/chorus/section specification). Word extensions are
	represented using the extend element. Hyphenation is 
	indicated by the syllabic element, which can be single, 
	begin, end, or middle. These represent single-syllable
	words, word-beginning syllables, word-ending syllables,
	and mid-word syllables. Multiple syllables on a single
	note are separated by elision elements. A hyphen in the
	text element should only be used for an actual hyphenated
	word. Two text elements that are not separated by an
	elision element are part of the same syllable, but may have
	different text formatting.

	Humming and laughing representations are taken from
	Humdrum. The end-line and end-paragraph elements come
	from RP-017 for Standard MIDI File Lyric meta-events;
	they help facilitate lyric display for Karaoke and
	similar applications. Language names for text elements
	come from ISO 639, with optional country subcodes from
	ISO 3166. Justification is center by default; placement is
	below by default. The print-object attribute can override
	a note's print-lyric attribute in cases where only some
	lyrics on a note are printed, as when lyrics for later verses
	are printed in a block of text rather than with each note.
-->
<!ELEMENT lyric
	((((syllabic?, text),
	   (elision?, syllabic?, text)*, extend?) |
	   extend | laughing | humming),
	  end-line?, end-paragraph?, %editorial;)>
<!ATTLIST lyric
    number NMTOKEN #IMPLIED
    name CDATA #IMPLIED
    %justify;
    %position;
    %placement;
    %color;
    %print-object;
>

<!ELEMENT text (#PCDATA)>
<!ATTLIST text
    %font;
    %color;
    %text-decoration;
    %text-rotation;
    %letter-spacing;
    xml:lang NMTOKEN #IMPLIED
    %text-direction;
>
<!ELEMENT syllabic (#PCDATA)>

<!--
	The elision element text specifies the symbol used to
	display the elision. Common values are a no-break space
	(Unicode 00A0), an underscore (Unicode 005F), or an undertie
	(Unicode 203F).
-->
<!ELEMENT elision (#PCDATA)>
<!ATTLIST elision
    %font;
    %color;
>

<!--
	The extend element represents lyric word extension / 
	melisma lines as well as figured bass extensions. The
	optional type and position attributes are added in
	Version 3.0 to provide better formatting control.
-->
<!ELEMENT extend EMPTY>
<!ATTLIST extend
    type %start-stop-continue; #IMPLIED
    %print-style;
>

<!ELEMENT laughing EMPTY>
<!ELEMENT humming EMPTY>
<!ELEMENT end-line EMPTY>
<!ELEMENT end-paragraph EMPTY>

<!--
	Figured bass elements take their position from the first
	regular note (not a grace note or chord note) that follows
	in score order. The optional duration element is used to
	indicate changes of figures under a note.	

	Figures are ordered from top to bottom. A figure-number is a
	number. Values for prefix and suffix include the accidental
	values sharp, flat, natural, double-sharp, flat-flat, and
	sharp-sharp. Suffixes include both symbols that come after
	the figure number and those that overstrike the figure number.
	The suffix value slash is used for slashed numbers indicating
	chromatic alteration. The orientation and display of the slash
	usually depends on the figure number. The prefix and suffix
	elements may contain additional values for symbols specific
	to particular figured bass styles. The value of parentheses
	is "no" if not present.
-->
<!ELEMENT figured-bass (figure+, duration?, %editorial;)>
<!ATTLIST figured-bass
    %print-style; 
    %printout;
    parentheses %yes-no; #IMPLIED
>
<!ELEMENT figure (prefix?, figure-number?, suffix?, extend?)>
<!ELEMENT prefix (#PCDATA)>
<!ATTLIST prefix
    %print-style;
>
<!ELEMENT figure-number (#PCDATA)>
<!ATTLIST figure-number
    %print-style;
>
<!ELEMENT suffix (#PCDATA)>
<!ATTLIST suffix
    %print-style;
>

<!--
	The backup and forward elements are required to coordinate
	multiple voices in one part, including music on multiple
	staves. The forward element is generally used within voices
	and staves, while the backup element is generally used to
	move between voices and staves. Thus the backup element
	does not include voice or staff elements. Duration values
	should always be positive, and should not cross measure
	boundaries or mid-measure changes in the divisions value.
-->
<!ELEMENT backup (duration, %editorial;)>
<!ELEMENT forward
	(duration, %editorial-voice;, staff?)>
