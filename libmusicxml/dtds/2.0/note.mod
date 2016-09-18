<!--
	MusicXML™ note.mod module

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
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
	 staff?, beam*, notations*, lyric*)>

<!--
	The position and printout entities for printing suggestions
	are defined in the common.mod file.
	
	The dynamics and end-dynamics attributes correspond to
	MIDI 1.0's Note On and Note Off velocities, respectively.
	They are expressed in terms of percentages of the default
	forte value (90 for MIDI 1.0). The attack and release
	attributes are used to alter the staring and stopping time
	of the note from when it would otherwise occur based on
	the flow of durations - information that is specific to a
	performance. They are expressed in terms of divisions,
	either positive or negative. A note that starts a tie should
	not have a release attribute, and a note that stops a tie
	should not have an attack attribute. If a note is played
	only one time through a repeat, the time-only attribute
	shows which time to play the note. The pizzicato attribute
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
    time-only CDATA #IMPLIED
    pizzicato %yes-no; #IMPLIED
>

<!--
	Pitch is represented as a combination of the step of the
	diatonic scale, the chromatic alteration, and the octave.
	The step element uses the English letters A through G. 
	The alter element represents chromatic alteration in
	number of semitones (e.g., -1 for flat, 1 for sharp).
	Decimal values like 0.5 (quarter tone sharp) may be 
	used for microtones. The octave element is represented
	by the numbers 0 to 9, where 4 indicates the octave
	started by middle C.
-->
<!ELEMENT pitch (step, alter?, octave)>
<!ELEMENT step (#PCDATA)>
<!ELEMENT alter (#PCDATA)>
<!ELEMENT octave (#PCDATA)>

<!--
	The cue and grace elements indicate the presence of
	cue and grace notes. The slash attribute for a grace
	note is yes for slashed eighth notes. The other grace
	note attributes come from MuseData sound suggestions.
	Steal-time-previous indicates the percentage of time
	to steal from the previous note for the grace note.
	Steal-time-following indicates the percentage of time
	to steal from the following note for the grace note.
	Make-time indicates to make time, not steal time; the
	units are in real-time divisions for the grace note. 
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
	line of the staff, generally used for one-line staffs.
-->
<!ELEMENT unpitched ((display-step, display-octave)?)>
<!ELEMENT display-step (#PCDATA)>
<!ELEMENT display-octave (#PCDATA)>

<!--
	The rest element indicates notated rests or silences.
	Rest are usually empty, but placement on the staff can
	be specified using display-step and display-octave
	elements.
-->
<!ELEMENT rest ((display-step, display-octave)?)>

<!--
	Duration is a positive number specified in division units.
	This is the intended duration vs. notated duration (for
	instance, swing eighths vs. even eighths, or differences
	in dotted notes in Baroque-era music). Differences in
	duration specific to an interpretation or performance
	should use the note element's attack and release
	attributes. 

	The tie element indicates that a tie begins or ends with
	this note. The tie element indicates sound; the tied
	element indicates notation.
-->
<!ELEMENT duration (#PCDATA)>
<!ELEMENT tie EMPTY>
<!ATTLIST tie
    type %start-stop; #REQUIRED
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
	shortest to longest) are 256th, 128th, 64th, 32nd, 16th,
	eighth, quarter, half, whole, breve, and long. The size
	attribute indicates full, cue, or large size, with full
	the default for regular notes and cue the default for
	cue and grace notes.
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
	three-quarters-flat, and three-quarters-sharp. Editorial
	and cautionary indications are indicated by attributes.
	Values for these attributes are "no" if not present.
	Specific graphic display such as parentheses, brackets,
	and size are controlled by the level-display entity 
	defined in the common.mod file.
-->
<!ELEMENT accidental (#PCDATA)>
<!ATTLIST accidental
    cautionary %yes-no; #IMPLIED
    editorial %yes-no; #IMPLIED
    %level-display;
    %print-style;
>

<!--
	Time modification indicates tuplets and other durational
	changes. The child elements are defined in the common.mod
	file.
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
	it are ignored.
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
	back slashed, normal, cluster, or none. For shape note
	music, the element values do, re, mi, fa, so, la, and ti
	are used, corresponding to Aikin's 7-shape system.

	The arrow shapes differ from triangle and inverted triangle
	by being centered on the stem. Slashed and back slashed 
	notes include both the normal notehead and a slash. The 
	triangle shape has the tip of the triangle pointing up;
	the inverted triangle shape has the tip of the triangle 
	pointing down.
	
	For the enclosed shapes, the default is to be hollow for
	half notes and longer, and filled otherwise. The filled
	attribute can be set to change this if needed.
	
	If the parentheses attribute is set to yes, the notehead
	is parenthesized. It is no by default.
-->
<!ELEMENT notehead (#PCDATA)>
<!ATTLIST notehead
    filled %yes-no; #IMPLIED
    parentheses %yes-no; #IMPLIED
    %font;
    %color;
>

<!--
	Beam types include begin, continue, end, forward hook,
	and backward hook. In MuseData, up to six concurrent
	beams are available to cover up to 256th notes. This
	seems sufficient so we use an enumerated type defined
	in the common.mod file. The repeater attribute, used for
	tremolos, needs to be specified with a "yes" value for each
	beam using it. Beams that have a begin value can also have 
	a fan attribute to indicate accelerandos and ritardandos 
	using fanned beams. The fan attribute may also be used
	with a continue value if the fanning direction changes
	on that note. The value is "none" if not specified.
	
	Note that the beam number does not distinguish sets of
	beams that overlap, as it does for slur and other elements.
	Beaming groups are distinguished by being in different
	voices and/or the presence or absence of grace and cue
	elements.
-->
<!ELEMENT beam (#PCDATA)>
<!ATTLIST beam
    number %beam-level; "1"
    repeater %yes-no; #IMPLIED
    fan (accel | rit | none) #IMPLIED
    %color;
>

<!--
	Notations are musical notations, not XML notations.
	Multiple notations are allowed in order to represent
	multiple editorial levels. The set of notations will be
	refined and expanded over time, especially to handle
	more instrument-specific technical notations.
-->
<!ELEMENT notations
	(%editorial;, 
	 (tied | slur | tuplet | glissando | slide | 
	  ornaments | technical | articulations | dynamics |
	  fermata | arpeggiate | non-arpeggiate | 
	  accidental-mark | other-notation)*)>

<!ELEMENT tied EMPTY>
<!ATTLIST tied
    type %start-stop; #REQUIRED
    number %number-level; #IMPLIED
    %line-type;
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
	
	Whereas a time-modification element shows how the
	cumulative, sounding effect of tuplets compare to the
	written note type, the tuplet element describes how this
	is displayed. The tuplet-actual and tuplet-normal elements
	provide optional full control over tuplet specifications.
	Each allows the number and note type (including dots)
	describing a single tuplet. If any of these elements are
	absent, their values are based on the time-modification
	element.
	
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
    %print-style; 
>
<!ELEMENT slide (#PCDATA)>
<!ATTLIST slide
    type %start-stop; #REQUIRED
    number %number-level; "1"
    %line-type; 
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
	   shake | wavy-line | mordent | inverted-mordent | 
	   schleifer | tremolo | other-ornament), 
	   accidental-mark*)*)>
<!ELEMENT trill-mark EMPTY>
<!ATTLIST trill-mark
    %print-style; 
    %placement; 
    %trill-sound; 
>

<!--
	The turn and delayed-turn elements are the normal turn
	shape which goes up then down. The delayed-turn element
	indicates a turn that is delayed until the end of the
	current note. The inverted-turn element has the shape
	which goes down and then up.
-->
<!ELEMENT turn EMPTY>
<!ATTLIST turn
    %print-style; 
    %placement; 
    %trill-sound; 
>
<!ELEMENT delayed-turn EMPTY>
<!ATTLIST delayed-turn
    %print-style; 
    %placement; 
    %trill-sound; 
>
<!ELEMENT inverted-turn EMPTY>
<!ATTLIST inverted-turn
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
-->
<!ELEMENT mordent EMPTY>
<!ATTLIST mordent
    long %yes-no; #IMPLIED
    %print-style; 
    %placement; 
    %trill-sound; 
>
<!ELEMENT inverted-mordent EMPTY>
<!ATTLIST inverted-mordent
    long %yes-no; #IMPLIED
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
	While using repeater beams is the preferred method for
	indicating tremolos, often playback and display are not
	well-enough integrated in an application to make that
	feasible. The tremolo ornament can be used to indicate
	either single-note or double-note tremolos. Single-note
	tremolos use the single type, while double-note tremolos
	use the start and stop types. The default is "single" for
	compatibility with Version 1.1. The text of the element
	indicates the number of tremolo marks and is an integer
	from 0 to 6. Note that the number of attached beams is
	not included in this value, but is represented separately
	using the beam element.
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
	  toe | fingernails | other-technical)*)>

<!--
	The up-bow and down-bow elements represent the symbol
	that is used both for bowing indications on bowed
	instruments, and up-stroke / down-stoke indications
	on plucked instruments.
-->
<!ELEMENT up-bow EMPTY>
<!ATTLIST up-bow
    %print-style; 
    %placement; 
>
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

<!ELEMENT open-string EMPTY>
<!ATTLIST open-string
    %print-style; 
    %placement; 
>
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

<!ELEMENT double-tongue EMPTY>
<!ATTLIST double-tongue
    %print-style; 
    %placement; 
>
<!ELEMENT triple-tongue EMPTY>
<!ATTLIST triple-tongue
    %print-style; 
    %placement; 
>
<!ELEMENT stopped EMPTY>
<!ATTLIST stopped
    %print-style; 
    %placement; 
>
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
	The fingernails element is used in harp notation.
-->
<!ELEMENT fingernails EMPTY>
<!ATTLIST fingernails
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
    %print-style; 
    %placement;
>
<!ELEMENT plop EMPTY>
<!ATTLIST plop
    %line-shape;
    %line-type;
    %print-style; 
    %placement; 
>
<!ELEMENT doit EMPTY>
<!ATTLIST doit
    %line-shape;
    %line-type;
    %print-style; 
    %placement;
>
<!ELEMENT falloff EMPTY>
<!ATTLIST falloff
    %line-shape;
    %line-type;
    %print-style; 
    %placement; 
>

<!ELEMENT breath-mark EMPTY>
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
	below by default.
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
	In Version 2.0, the elision element text is used to specify
	the symbol used to display the elision. Common values
	are a no-break space (Unicode 00A0), an underscore
	(Unicode 005F), or an undertie (Unicode 203F).
-->
<!ELEMENT elision (#PCDATA)>
<!ATTLIST elision
    %font;
    %color;
>
<!ELEMENT extend EMPTY>
<!ATTLIST extend
    %font;
    %color;
>
<!ELEMENT laughing EMPTY>
<!ELEMENT humming EMPTY>
<!ELEMENT end-line EMPTY>
<!ELEMENT end-paragraph EMPTY>

<!--
	Figured bass elements take their position from the first
	regular note that follows. Figures are ordered from top to
	bottom. A figure-number is a number. Values for prefix and
	suffix include the accidental values sharp, flat, natural,
	double-sharp, flat-flat, and sharp-sharp. Suffixes include
	both symbols that come after the figure number and those 
	that overstrike the figure number. The suffix value slash
	is used for slashed numbers indicating chromatic alteration.
	The orientation and display of the slash usually depends on
	the figure number. The prefix and suffix elements may
	contain additional values for symbols specific to particular
	figured bass styles. The value of parentheses is "no" if not
	present.
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
	boundaries.
-->
<!ELEMENT backup (duration, %editorial;)>
<!ELEMENT forward
	(duration, %editorial-voice;, staff?)>
