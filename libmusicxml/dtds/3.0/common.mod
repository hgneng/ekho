<!--
	MusicXML™ common.mod module

	Version 3.0
	
	Copyright © 2004-2011 MakeMusic, Inc.
	http://www.makemusic.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Public License Version 3.0,
	available from:
	
		http://www.musicxml.org/dtds/license.html
-->

<!--
	This file contains entities and elements that are common
	across multiple DTD modules. In particular, several elements
	here are common across both notes and measures.
-->

<!-- Entities -->

<!--
	If greater ASCII compatibility is desired, entity references
	may be used instead of the direct Unicode characters.
	Currently we include ISO Latin-1 for Western European
	characters and ISO Latin-2 for Central European characters.
	These files are local copies of the W3C entities located at:

		http://www.w3.org/2003/entities/
-->
<!ENTITY % isolat1 PUBLIC 
	"ISO 8879:1986//ENTITIES Added Latin 1//EN//XML"
	"isolat1.ent">
%isolat1;
<!ENTITY % isolat2 PUBLIC 
	"ISO 8879:1986//ENTITIES Added Latin 2//EN//XML"
	"isolat2.ent">
%isolat2;

<!--
	Data type entities. The ones that resolve to strings show
	intent for how data is formatted and used.
-->

<!--
	Calendar dates are represented yyyy-mm-dd format, following
	ISO 8601.
-->
<!ENTITY % yyyy-mm-dd "(#PCDATA)">

<!--
	The tenths entity is a number representing tenths of
	interline space (positive or negative) for use in
	attributes. The layout-tenths entity is the same for
	use in elements. Both integer and decimal values are 
	allowed, such as 5 for a half space and 2.5 for a 
	quarter space. Interline space is measured from the
	middle of a staff line.
-->
<!ENTITY % tenths "CDATA">
<!ENTITY % layout-tenths "(#PCDATA)">

<!--
	The start-stop and start-stop-continue entities are used 
	for musical elements that can either start or stop, such 
	as slurs, tuplets, and wedges. The start-stop-continue
	entity is used when there is a need to refer to an
	intermediate point in the symbol, as for complex slurs
	or for specifying formatting of symbols across system
	breaks. The start-stop-single entity is used when the same
	element is used for multi-note and single-note notations,
	as for tremolos.

	The values of start, stop, and continue refer to how an
	element appears in musical score order, not in MusicXML
	document order. An element with a stop attribute may
	precede the corresponding element with a start attribute
	within a MusicXML document. This is particularly common
	in multi-staff music. For example, the stopping point for
	a slur may appear in staff 1 before the starting point for
	the slur appears in staff 2 later in the document.
-->
<!ENTITY % start-stop "(start | stop)">
<!ENTITY % start-stop-continue "(start | stop | continue)">
<!ENTITY % start-stop-single "(start | stop | single)">

<!--
	The yes-no entity is used for boolean-like attributes.
-->
<!ENTITY % yes-no "(yes | no)">

<!--
	The yes-no-number entity is used for attributes that can
	be either boolean or numeric values. Values can be "yes",
	"no", or numbers.
-->
<!ENTITY % yes-no-number "NMTOKEN">

<!--
	The symbol-size entity is used to indicate full vs.
	cue-sized vs. oversized symbols. The large value
	for oversized symbols was added in version 1.1.
-->
<!ENTITY % symbol-size "(full | cue | large)">

<!--
	The above-below type is used to indicate whether one
	element appears above or below another element.
-->
<!ENTITY % above-below "(above | below)">

<!--
	The up-down entity is used for arrow direction,
	indicating which way the tip is pointing.
-->
<!ENTITY % up-down "(up | down)">

<!--
	The top-bottom entity is used to indicate the top or
	bottom part of a vertical shape like non-arpeggiate.
-->
<!ENTITY % top-bottom "(top | bottom)">

<!--
	The left-right entity is used to indicate whether one
	element appears to the left or the right of another
	element.
-->
<!ENTITY % left-right "(left | right)">

<!--
	The number-of-lines entity is used to specify the
	number of lines in text decoration attributes.
-->
<!ENTITY % number-of-lines "(0 | 1 | 2 | 3)">

<!--
	The enclosure-shape entity describes the shape and 
	presence / absence of an enclosure around text. A bracket
	enclosure is similar to a rectangle with the bottom line
	missing, as is common in jazz notation.
-->
<!ENTITY % enclosure-shape 
	"(rectangle | square | oval | circle | 
	  bracket | triangle | diamond | none)">

<!--
	Slurs, tuplets, and many other features can be
	concurrent and overlapping within a single musical
	part. The number-level attribute distinguishes up to
	six concurrent objects of the same type. A reading
	program should be prepared to handle cases where
	the number-levels stop in an arbitrary order.
	Different numbers are needed when the features
	overlap in MusicXML document order. When a number-level
	value is implied, the value is 1 by default.
-->
<!ENTITY % number-level "(1 | 2 | 3 | 4 | 5 | 6)">

<!--
	The MusicXML format supports eight levels of beaming, up
	to 1024th notes. Unlike the number-level attribute, the
	beam-level attribute identifies concurrent beams in a beam
	group. It does not distinguish overlapping beams such as
	grace notes within regular notes, or beams used in different
	voices.
-->
<!ENTITY % beam-level "(1 | 2 | 3 | 4 | 5 | 6 | 7 | 8)">

<!--
	Common structures for formatting attribute definitions. 
-->

<!--
	The position attributes are based on MuseData print
	suggestions. For most elements, any program will compute
	a default x and y position. The position attributes let
	this be changed two ways. 

	The default-x and default-y attributes change the
	computation of the default position. For most elements,
	the origin is changed relative to the left-hand side of
	the note or the musical position within the bar (x) and
	the top line of the staff (y).

	For the following elements, the default-x value changes
	the origin relative to the start of the current measure:

		- note
		- figured-bass
		- harmony
		- link
		- directive
		- measure-numbering
		- all descendants of the part-list element
		- all children of the direction-type element

	This origin is from the start of the entire measure,
	at either the left barline or the start of the system.

	When the default-x attribute is used within a child element
	of the part-name-display, part-abbreviation-display, 
	group-name-display, or group-abbreviation-display elements,
	it changes the origin relative to the start of the first 
	measure on the system. These values are used when the current
	measure or a succeeding measure starts a new system. The same 
	change of origin is used for the group-symbol element.

	For the note, figured-bass, and harmony elements, the
	default-x value is considered to have adjusted the musical
	position within the bar for its descendant elements.

	Since the credit-words and credit-image elements are not
	related to a measure, in these cases the default-x and
	default-y attributes adjust the origin relative to the
	bottom left-hand corner of the specified page.

	The relative-x and relative-y attributes change the position 
	relative to the default position, either as computed by the
	individual program, or as overridden by the default-x and
	default-y attributes.
	
	Positive x is right, negative x is left; positive y is up,
	negative y is down. All units are in tenths of interline
	space. For stems, positive relative-y lengthens a stem
	while negative relative-y shortens it.

	The default-x and default-y position attributes provide
	higher-resolution positioning data than related features
	such as the placement attribute and the offset element.
	Applications reading a MusicXML file that can understand
	both features should generally rely on the default-x and
	default-y attributes for their greater accuracy. For the
	relative-x and relative-y attributes, the offset element,
	placement attribute, and directive attribute provide
	context for the relative position information, so the two
	features should be interpreted together.

	As elsewhere in the MusicXML format, tenths are the global
	tenths defined by the scaling element, not the local tenths
	of a staff resized by the staff-size element.
-->
<!ENTITY % position
	"default-x     %tenths;    #IMPLIED
	 default-y     %tenths;    #IMPLIED
	 relative-x    %tenths;    #IMPLIED
	 relative-y    %tenths;    #IMPLIED">

<!--
	The placement attribute indicates whether something is
	above or below another element, such as a note or a
	notation. 
-->
<!ENTITY % placement
	"placement %above-below; #IMPLIED">

<!--
	The orientation attribute indicates whether slurs and
	ties are overhand (tips down) or underhand (tips up).
	This is distinct from the placement entity used by any
	notation type.
-->
<!ENTITY % orientation
	"orientation (over | under) #IMPLIED">

<!--
	The directive entity changes the default-x position 
	of a direction. It indicates that the left-hand side of the
	direction is aligned with the left-hand side of the time
	signature. If no time signature is present, it is aligned
	with the left-hand side of the first music notational
	element in the measure. If a default-x, justify, or halign
	attribute is present, it overrides the directive entity.
-->
<!ENTITY % directive
	"directive  %yes-no;  #IMPLIED">
	
<!--
	The bezier entity is used to indicate the curvature of
	slurs and ties, representing the control points for a 
	cubic bezier curve. For ties, the bezier entity is 
	used with the tied element.

	Normal slurs, S-shaped slurs, and ties need only two 
	bezier points: one associated with the start of the slur 
	or tie, the other with the stop. Complex slurs and slurs 
	divided over system breaks can specify additional 
	bezier data at slur elements with a continue type.
	
	The bezier-offset, bezier-x, and bezier-y attributes
	describe the outgoing bezier point for slurs and ties 
	with a start type, and the incoming bezier point for
	slurs and ties with types of stop or continue. The 
	attributes bezier-offset2, bezier-x2, and bezier-y2 
	are only valid with slurs of type continue, and 
	describe the outgoing bezier point.
	
	The bezier-offset and bezier-offset2 attributes are
	measured in terms of musical divisions, like the offset
	element. These are the recommended attributes for
	specifying horizontal position. The other attributes
	are specified in tenths, relative to any position 
	settings associated with the slur or tied element.
-->
<!ENTITY % bezier
	"bezier-offset  CDATA     #IMPLIED
	 bezier-offset2 CDATA     #IMPLIED
	 bezier-x       %tenths;  #IMPLIED
	 bezier-y       %tenths;  #IMPLIED
	 bezier-x2      %tenths;  #IMPLIED
	 bezier-y2      %tenths;  #IMPLIED">

<!--
	The font entity gathers together attributes for
	determining the font within a directive or direction.
	They are based on the text styles for Cascading
	Style Sheets. The font-family is a comma-separated list
	of font names. These can be specific font styles such
	as Maestro or Opus, or one of several generic font styles:
	music, engraved, handwritten, text, serif, sans-serif,
	handwritten, cursive, fantasy, and monospace. The music,
	engraved, and handwritten values refer to music fonts;
	the rest refer to text fonts. The fantasy style refers to
	decorative text such as found in older German-style
	printing. The font-style can be normal or italic. The
	font-size can be one of the CSS sizes (xx-small, x-small,
	small, medium, large, x-large, xx-large) or a numeric
	point size. The font-weight can be normal or bold. The
	default is application-dependent, but is a text font vs.
	a music font.
-->
<!ENTITY % font
	"font-family  CDATA  #IMPLIED
	 font-style   CDATA  #IMPLIED
	 font-size    CDATA  #IMPLIED
	 font-weight  CDATA  #IMPLIED">
	
<!--
	The color entity indicates the color of an element.
	Color may be represented as hexadecimal RGB triples,
	as in HTML, or as hexadecimal ARGB tuples, with the
	A indicating alpha of transparency. An alpha value
	of 00 is totally transparent; FF is totally opaque.
	If RGB is used, the A value is assumed to be FF. 

	For instance, the RGB value "#800080" represents
	purple. An ARGB value of "#40800080" would be a
	transparent purple.

	As in SVG 1.1, colors are defined in terms of the
	sRGB color space (IEC 61966).
-->
<!ENTITY % color
	"color CDATA #IMPLIED">

<!--
	The text-decoration entity is based on the similar
	feature in XHTML and CSS. It allows for text to
	be underlined, overlined, or struck-through. It
	extends the CSS version by allow double or
	triple lines instead of just being on or off.
-->
<!ENTITY % text-decoration
	"underline  %number-of-lines;  #IMPLIED
	 overline  %number-of-lines;   #IMPLIED
	 line-through  %number-of-lines;   #IMPLIED">
	
<!--
	The justify entity is used to indicate left, center, or
	right justification. The default value varies for different
	elements. For elements where the justify attribute is present
	but the halign attribute is not, the justify attribute
	indicates horizontal alignment as well as justification.
-->
<!ENTITY % justify
	"justify (left | center | right) #IMPLIED">

<!--
	In cases where text extends over more than one line, 
	horizontal alignment and justify values can be different.
	The most typical case is for credits, such as:

		Words and music by
		  Pat Songwriter

	Typically this type of credit is aligned to the right,
	so that the position information refers to the right-
	most part of the text. But in this example, the text 
	is center-justified, not right-justified.

	The halign attribute is used in these situations. If it 
	is not present, its value is the same as for the justify
	attribute.
-->
<!ENTITY % halign
	"halign (left | center | right) #IMPLIED">

<!--
	The valign entity is used to indicate vertical
	alignment to the top, middle, bottom, or baseline 
	of the text. Defaults are implementation-dependent.
-->
<!ENTITY % valign
	"valign (top | middle | bottom | baseline) #IMPLIED">

<!--
	The valign-image entity is used to indicate vertical
	alignment for images and graphics, so it removes the
	baseline value. Defaults are implementation-dependent.
-->
<!ENTITY % valign-image
	"valign (top | middle | bottom) #IMPLIED">

<!--
	The letter-spacing entity specifies text tracking.
	Values are either "normal" or a number representing
	the number of ems to add between each letter. The
	number may be negative in order to subtract space.
	The default is normal, which allows flexibility of
	letter-spacing for purposes of text justification.
-->
<!ENTITY % letter-spacing
	"letter-spacing CDATA #IMPLIED">

<!--
	The line-height entity specified text leading. Values
	are either "normal" or a number representing the
	percentage of the current font height  to use for 
	leading. The default is "normal". The exact normal 
	value is implementation-dependent, but values 
	between 100 and 120 are recommended.
-->
<!ENTITY % line-height
	"line-height CDATA #IMPLIED">

<!--
	The text-direction entity is used to adjust and override
	the Unicode bidirectional text algorithm, similar to the
	W3C Internationalization Tag Set recommendation. Values
	are ltr (left-to-right embed), rtl (right-to-left embed),
	lro (left-to-right bidi-override), and rlo (right-to-left
	bidi-override). The default value is ltr. This entity
	is typically used by applications that store text in
	left-to-right visual order rather than logical order.
	Such applications can use the lro value to better
	communicate with other applications that more fully
	support bidirectional text.
-->
<!ENTITY % text-direction
	"dir (ltr | rtl | lro | rlo) #IMPLIED">

<!--
	The text-rotation entity is used to rotate text
	around the alignment point specified by the
	halign and valign entities. The value is a number
	ranging from -180 to 180. Positive values are
	clockwise rotations, while negative values are
	counter-clockwise rotations.
-->
<!ENTITY % text-rotation
	"rotation CDATA #IMPLIED">

<!--
	The enclosure entity is used to specify the
	formatting of an enclosure around text or symbols.
-->
<!ENTITY % enclosure
	"enclosure %enclosure-shape; #IMPLIED">

<!--
	The print-style entity groups together the most popular
	combination of printing attributes: position, font, and
	color.
-->
<!ENTITY % print-style
	"%position;
	 %font;
	 %color;">

<!--
	The print-style-align entity adds the halign and valign
	attributes to the position, font, and color attributes.
-->
<!ENTITY % print-style-align
	"%print-style;
	 %halign;
	 %valign;">

<!--
	The line-shape entity is used to distinguish between
	straight and curved lines. The line-type entity
	distinguishes between solid, dashed, dotted, and
	wavy lines.
-->
<!ENTITY % line-shape
	"line-shape (straight | curved) #IMPLIED">

<!ENTITY % line-type
	"line-type (solid | dashed | dotted | wavy) #IMPLIED">

<!--
	The dashed-formatting entity represents the length of
	dashes and spaces in a dashed line. Both the dash-length
	and space-length attributes are represented in tenths.
	These attributes are ignored if the corresponding 
	line-type attribute is not dashed.
-->
<!ENTITY % dashed-formatting
	"dash-length   %tenths;  #IMPLIED
	 space-length  %tenths;  #IMPLIED">

<!--
	The printout entity is based on MuseData print
	suggestions. They allow a way to specify not to print
	print an object (e.g. note or rest), its augmentation
	dots, or its lyrics. This is especially useful for notes 
	that overlap in different voices, or for chord sheets
	that contain lyrics and chords but no melody. For wholly
	invisible notes, such as those providing sound-only data,
	the attribute for print-spacing may be set to no so that
	no space is left for this note. The print-spacing value
	is only used if no note, dot, or lyric is being printed.

	By default, all these attributes are set to yes. If 
	print-object is set to no, print-dot and print-lyric are
	interpreted to also be set to no if they are not present.
-->
<!ENTITY % print-object
	"print-object  %yes-no;  #IMPLIED">

<!ENTITY % print-spacing
	"print-spacing %yes-no;  #IMPLIED">

<!ENTITY % printout
	"%print-object;
	 print-dot     %yes-no;  #IMPLIED
	 %print-spacing;
	 print-lyric   %yes-no;  #IMPLIED">

<!--
	The text-formatting entity contains the common formatting 
	attributes for text elements. Default values may differ
	across the elements that use this entity.
-->
<!ENTITY % text-formatting
	"%justify;
	 %print-style-align;
	 %text-decoration;
	 %text-rotation;
	 %letter-spacing;
	 %line-height;
	 xml:lang NMTOKEN #IMPLIED
	 xml:space (default | preserve) #IMPLIED
	 %text-direction;
	 %enclosure;">

<!--
	The level-display entity allows specification of three 
	common ways to indicate editorial indications: putting
	parentheses or square brackets around a symbol, or making
	the symbol a different size. If not specified, they are
	left to application defaults. It is used by the level and
	accidental elements.
-->
<!ENTITY % level-display
	"parentheses %yes-no;       #IMPLIED
	 bracket     %yes-no;       #IMPLIED
	 size        %symbol-size;  #IMPLIED">

<!--
	Common structures for playback attribute definitions. 
-->

<!--
	The trill-sound entity includes attributes used to guide
	the sound of trills, mordents, turns, shakes, and wavy
	lines, based on MuseData sound suggestions. The default
	choices are:
	
		start-note = "upper"
		trill-step = "whole"
		two-note-turn = "none"
		accelerate = "no"
		beats = "4" (minimum of "2").
	
	Second-beat and last-beat are percentages for landing on
	the indicated beat, with defaults of 25 and 75 respectively.
	
	For mordent and inverted-mordent elements, the defaults
	are different:
	
		The default start-note is "main", not "upper".
		The default for beats is "3", not "4".
		The default for second-beat is "12", not "25".
		The default for last-beat is "24", not "75".
-->
<!ENTITY % trill-sound
	"start-note    (upper | main | below)  #IMPLIED
	 trill-step    (whole | half | unison) #IMPLIED
	 two-note-turn (whole | half | none)   #IMPLIED
	 accelerate    %yes-no; #IMPLIED
	 beats         CDATA    #IMPLIED
	 second-beat   CDATA    #IMPLIED
	 last-beat     CDATA    #IMPLIED">

<!--
	The bend-sound entity is used for bend and slide elements,
	and is similar to the trill-sound. Here the beats element
	refers to the number of discrete elements (like MIDI pitch
	bends) used to represent a continuous bend or slide. The
	first-beat indicates the percentage of the direction for
	starting a bend; the last-beat the percentage for ending it.
	The default choices are:
	
		accelerate = "no"
		beats = "4" (minimum of "2")
		first-beat = "25"
		last-beat = "75"
-->
<!ENTITY % bend-sound
	"accelerate    %yes-no; #IMPLIED
	 beats         CDATA    #IMPLIED
	 first-beat    CDATA    #IMPLIED
	 last-beat     CDATA    #IMPLIED">

<!--
	The time-only entity is used to indicate that a particular
	playback-related element only applies particular times through
	a repeated section. The value is a comma-separated list of
	positive integers arranged in ascending order, indicating which
	times through the repeated section that the element applies.
-->
<!ENTITY % time-only
	"time-only CDATA #IMPLIED">

<!--
	Common structures for other attribute definitions. 
-->

<!--
	The document-attributes entity is used to specify the
	attributes for an entire MusicXML document. Currently
	this is used for the version attribute.

	The version attribute was added in Version 1.1 for the
	score-partwise and score-timewise documents, and in
	Version 2.0 for opus documents. It provides an easier 
	way to get version information than through the MusicXML
	public ID. The default value is 1.0 to make it possible
	for programs that handle later versions to distinguish
	earlier version files reliably. Programs that write
	MusicXML 1.1 or later files should set this attribute.
-->
<!ENTITY % document-attributes "version  CDATA  '1.0'">

<!--
	Common structures for element definitions. 
-->

<!--
	Two entities for editorial information in notes. These
	entities, and their elements defined below, are used
	across all the different component DTD modules.
-->
<!ENTITY % editorial "(footnote?, level?)">
<!ENTITY % editorial-voice "(footnote?, level?, voice?)">

<!-- Elements -->

<!--
	Footnote and level are used to specify editorial
	information, while voice is used to distinguish between
	multiple voices (what MuseData calls tracks) in individual
	parts. These elements are used throughout the different
	MusicXML DTD modules. If the reference attribute for the
	level element is yes, this indicates editorial information
	that is for display only and should not affect playback.
	For instance, a modern edition of older music may set
	reference="yes" on the attributes containing the music's
	original clef, key, and time signature. It is no by default.
-->
<!ELEMENT footnote (#PCDATA)>
<!ATTLIST footnote
	%text-formatting;
>
<!ELEMENT level (#PCDATA)>
<!ATTLIST level
    reference %yes-no; #IMPLIED
    %level-display;
>
<!ELEMENT voice (#PCDATA)>

<!--
	Fermata and wavy-line elements can be applied both to
	notes and to measures, so they are defined here. Wavy
	lines are one way to indicate trills; when used with a
	measure element, they should always have type="continue"
	set. The fermata text content represents the shape of the
	fermata sign and may be normal, angled, or square.
	An empty fermata element represents a normal fermata.
	The fermata type is upright if not specified.
-->
<!ELEMENT fermata  (#PCDATA)>
<!ATTLIST fermata
    type (upright | inverted) #IMPLIED
    %print-style;
>
<!ELEMENT wavy-line EMPTY>
<!ATTLIST wavy-line
    type %start-stop-continue; #REQUIRED
    number %number-level; #IMPLIED
    %position;
    %placement; 
    %color;
    %trill-sound; 
>

<!--
	Staff assignment is only needed for music notated on
	multiple staves. Used by both notes and directions. Staff
	values are numbers, with 1 referring to the top-most staff
	in a part.
-->
<!ELEMENT staff (#PCDATA)>

<!--
	Segno and coda signs can be associated with a measure
	or a general musical direction. These are visual
	indicators only; a sound element is needed to guide
	playback applications reliably.
-->
<!ELEMENT segno EMPTY>
<!ATTLIST segno
    %print-style-align; 
>

<!ELEMENT coda EMPTY>
<!ATTLIST coda
    %print-style-align; 
>

<!--
	These elements are used both in the time-modification and
	metronome-tuplet elements. The actual-notes element
	describes how many notes are played in the time usually
	occupied by the number of normal-notes. If the normal-notes
	type is different than the current note type (e.g., a 
	quarter note within an eighth note triplet), then the
	normal-notes type (e.g. eighth) is specified in the
	normal-type and normal-dot elements. The content of the
	actual-notes and normal-notes elements ia a non-negative
	integer.
-->
<!ELEMENT actual-notes (#PCDATA)>
<!ELEMENT normal-notes (#PCDATA)>
<!ELEMENT normal-type (#PCDATA)>
<!ELEMENT normal-dot EMPTY>

<!--
	Dynamics can be associated either with a note or a general
	musical direction. To avoid inconsistencies between and
	amongst the letter abbreviations for dynamics (what is sf
	vs. sfz, standing alone or with a trailing dynamic that is
	not always piano), we use the actual letters as the names
	of these dynamic elements. The other-dynamics element
	allows other dynamic marks that are not covered here, but
	many of those should perhaps be included in a more general
	musical direction element. Dynamics may also be combined as
	in <sf/><mp/>.
	
	These letter dynamic symbols are separated from crescendo,
	decrescendo, and wedge indications. Dynamic representation
	is inconsistent in scores. Many things are assumed by the
	composer and left out, such as returns to original dynamics.
	Systematic representations are quite complex: for example,
	Humdrum has at least 3 representation formats related to
	dynamics. The MusicXML format captures what is in the score,
	but does not try to be optimal for analysis or synthesis of
	dynamics.
-->
<!ELEMENT dynamics ((p | pp | ppp | pppp | ppppp | pppppp |
	f | ff | fff | ffff | fffff | ffffff | mp | mf | sf |
	sfp | sfpp | fp | rf | rfz | sfz | sffz | fz | 
	other-dynamics)*)>
<!ATTLIST dynamics
    %print-style-align; 
    %placement;
    %text-decoration; 
    %enclosure;
>
<!ELEMENT p EMPTY>
<!ELEMENT pp EMPTY>
<!ELEMENT ppp EMPTY>
<!ELEMENT pppp EMPTY>
<!ELEMENT ppppp EMPTY>
<!ELEMENT pppppp EMPTY>
<!ELEMENT f EMPTY>
<!ELEMENT ff EMPTY>
<!ELEMENT fff EMPTY>
<!ELEMENT ffff EMPTY>
<!ELEMENT fffff EMPTY>
<!ELEMENT ffffff EMPTY>
<!ELEMENT mp EMPTY>
<!ELEMENT mf EMPTY>
<!ELEMENT sf EMPTY>
<!ELEMENT sfp EMPTY>
<!ELEMENT sfpp EMPTY>
<!ELEMENT fp EMPTY>
<!ELEMENT rf EMPTY>
<!ELEMENT rfz EMPTY>
<!ELEMENT sfz EMPTY>
<!ELEMENT sffz EMPTY>
<!ELEMENT fz EMPTY>
<!ELEMENT other-dynamics (#PCDATA)>

<!--
	The fret, string, and fingering elements can be used either
	in a technical element for a note or in a frame element as
	part of a chord symbol.
-->

<!--
	Fingering is typically indicated 1,2,3,4,5. Multiple
	fingerings may be given, typically to substitute
	fingerings in the middle of a note. The substitution
	and alternate values are "no" if the attribute is 
	not present. For guitar and other fretted instruments,
	the fingering element represents the fretting finger;
	the pluck element represents the plucking finger.
-->
<!ELEMENT fingering (#PCDATA)>
<!ATTLIST fingering
    substitution %yes-no; #IMPLIED
    alternate %yes-no; #IMPLIED
    %print-style; 
    %placement;
>

<!--
	Fret and string are used with tablature notation and chord
	symbols. Fret numbers start with 0 for an open string and
	1 for the first fret. String numbers start with 1 for the
	highest string. The string element can also be used in
	regular notation.
-->
<!ELEMENT fret (#PCDATA)>
<!ATTLIST fret
    %font;
    %color; 
>
<!ELEMENT string (#PCDATA)>
<!ATTLIST string
    %print-style;
    %placement;
>

<!--
	The tuning-step, tuning-alter, and tuning-octave elements
	are represented like the step, alter, and octave elements,
	with different names to reflect their different function.
	They are used in the staff-tuning and accord elements.
-->
<!ELEMENT tuning-step (#PCDATA)>
<!ELEMENT tuning-alter (#PCDATA)>
<!ELEMENT tuning-octave (#PCDATA)>

<!--
	The display-text element is used for exact formatting of
	multi-font text in element in display elements such as
	part-name-display. Language is Italian ("it") by default.
	Enclosure is none by default.
-->
<!ELEMENT display-text (#PCDATA)>
<!ATTLIST display-text
    %text-formatting;
>
<!--
	The accidental-text element is used for exact formatting of
	accidentals in display elements such as part-name-display.
	Values are the same as for the accidental element.
	Enclosure is none by default.
-->
<!ELEMENT accidental-text (#PCDATA)>
<!ATTLIST accidental-text
    %text-formatting;
>

<!--
	The part-name-display and part-abbreviation-display 
	elements are used in both the score.mod and direction.mod
	files. They allow more precise control of how part names
	and abbreviations appear throughout a score. The
	print-object attributes can be used to determine what,
	if anything, is printed at the start of each system.
	Formatting specified in the part-name-display and
	part-abbreviation-display elements override the formatting
	specified in the part-name and part-abbreviation elements,
	respectively.
-->
<!ELEMENT part-name-display
	((display-text | accidental-text)*)>
<!ATTLIST part-name-display
    %print-object;
>
<!ELEMENT part-abbreviation-display
	((display-text | accidental-text)*)>
<!ATTLIST part-abbreviation-display
    %print-object;
>

<!--
	The midi-device content corresponds to the DeviceName
	meta event in Standard MIDI Files. The optional port
	attribute is a number from 1 to 16 that can be used
	with the unofficial MIDI port (or cable) meta event.
	Unlike the DeviceName meta event, there can be
	multiple midi-device elements per MusicXML part
	starting in MusicXML 3.0. The optional id attribute
	refers to the score-instrument assigned to this
	device. If missing, the device assignment affects
	all score-instrument elements in the score-part.
-->
<!ELEMENT midi-device (#PCDATA)>
<!ATTLIST midi-device
    port CDATA #IMPLIED
    id IDREF #IMPLIED
>

<!--
	The midi-instrument element can be a part of either
	the score-instrument element at the start of a part,
	or the sound element within a part. The id attribute
	refers to the score-instrument affected by the change.
-->
<!ELEMENT midi-instrument
	(midi-channel?, midi-name?, midi-bank?, midi-program?,
	 midi-unpitched?, volume?, pan?, elevation?)>
<!ATTLIST midi-instrument
    id IDREF #REQUIRED
>

<!-- 
	MIDI 1.0 channel numbers range from 1 to 16.
-->
<!ELEMENT midi-channel (#PCDATA)>

<!--
	MIDI names correspond to ProgramName meta-events within
	a Standard MIDI File.
-->
<!ELEMENT midi-name (#PCDATA)>

<!-- MIDI 1.0 bank numbers range from 1 to 16,384. -->
<!ELEMENT midi-bank (#PCDATA)>

<!-- MIDI 1.0 program numbers range from 1 to 128. -->
<!ELEMENT midi-program (#PCDATA)>

<!--
	For unpitched instruments, specify a MIDI 1.0 note number
	ranging from 1 to 128. It is usually used with MIDI banks for
	percussion. Note that MIDI 1.0 note numbers are generally
	specified from 0 to 127 rather than the 1 to 128 numbering
	used in this element.
-->
<!ELEMENT midi-unpitched (#PCDATA)>

<!-- 
	The volume value is a percentage of the maximum
	ranging from 0 to 100, with decimal values allowed.
	This corresponds to a scaling value for the MIDI 1.0
	channel volume controller.
 -->
<!ELEMENT volume (#PCDATA)>

<!-- 
	Pan and elevation allow placing of sound in a 3-D space
	relative to the listener. Both are expressed in degrees
	ranging from -180 to 180. For pan, 0 is straight ahead,
	-90 is hard left, 90 is hard right, and -180 and 180
	are directly behind the listener. For elevation, 0 is
	level with the listener, 90 is directly above, and -90
	is directly below.
-->
<!ELEMENT pan (#PCDATA)>
<!ELEMENT elevation (#PCDATA)>

<!-- 
	The play element, new in Version 3.0, specifies playback
	techniques to be used in conjunction with the instrument-sound
	element. When used as part of a sound element, it applies to
	all notes going forward in score order. In multi-instrument
	parts, the affected instrument should be specified using the
	id attribute. When used as part of a note element, it applies
	to the current note only.
-->
<!ELEMENT play ((ipa | mute | semi-pitched | other-play)*)>
<!ATTLIST play
    id IDREF #IMPLIED
>

<!-- 
	The ipa element represents International Phonetic Alphabet
	(IPA) sounds for vocal music. String content is limited to
	IPA 2005 symbols represented in Unicode 6.0.
-->
<!ELEMENT ipa (#PCDATA)>

<!-- 
	The mute element represents muting for different instruments,
	including brass, winds, and strings. The on and off values
	are used for undifferentiated mutes. The remaining values
	represent specific mutes: straight, cup, harmon-no-stem, 
	harmon-stem, bucket, plunger, hat, solotone, practice,
	stop-mute, stop-hand, echo, and palm.
-->
<!ELEMENT mute (#PCDATA)>

<!-- 
	The semi-pitched element represents categories of indefinite
	pitch for percussion instruments. Values are high, medium-high,
	medium, medium-low, low, and very-low.
-->
<!ELEMENT semi-pitched (#PCDATA)>

<!-- 
	The other-play element represents other types of playback. The
	required type attribute indicates the type of playback to which
	the element content applies.
-->
<!ELEMENT other-play (#PCDATA)>
<!ATTLIST other-play
    type CDATA #REQUIRED
>
