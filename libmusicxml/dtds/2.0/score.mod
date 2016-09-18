<!--
	MusicXML™ score.mod module

	Version 2.0 - 10 July 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	Works and movements are optionally identified by number
	and title. The work element also may indicate a link
	to the opus document that composes multiple movements
	into a collection.
-->
<!ELEMENT work (work-number?, work-title?, opus?)>
<!ELEMENT work-number (#PCDATA)>
<!ELEMENT work-title (#PCDATA)>

<!ELEMENT opus EMPTY>
<!ATTLIST opus
    %link-attributes; 
>

<!ELEMENT movement-number (#PCDATA)>
<!ELEMENT movement-title (#PCDATA)>

<!--
	Collect score-wide defaults. This includes scaling
	and layout, defined in layout.mod, and default values
	for the music font, word font, lyric font, and 
	lyric language. The number and name attributes in
	lyric-font and lyric-language elements are typically
	used when lyrics are provided in multiple languages.
	If the number and name attributes are omitted, the 
	lyric-font and lyric-language values apply to all 
	numbers and names.
-->
<!ELEMENT defaults (scaling?, page-layout?,
	system-layout?, staff-layout*, appearance?, 
	music-font?, word-font?, lyric-font*, lyric-language*)>

<!ELEMENT music-font EMPTY>
<!ATTLIST music-font
    %font;
>
<!ELEMENT word-font EMPTY>
<!ATTLIST word-font
    %font;
>
<!ELEMENT lyric-font EMPTY>
<!ATTLIST lyric-font
    number NMTOKEN #IMPLIED
    name CDATA #IMPLIED
    %font;
>
<!ELEMENT lyric-language EMPTY>
<!ATTLIST lyric-language
    number NMTOKEN #IMPLIED
    name CDATA #IMPLIED
    xml:lang NMTOKEN #REQUIRED
>

<!--
	Credit elements refer to the title, composer, arranger,
	lyricist, copyright, dedication, and other text that usually
	appears on the first page of a score. The credit-words
	and credit-image elements are similar to the words and
	image elements for directions. However, since the 
	credit is not part of a measure, the default-x and 
	default-y attributes adjust the origin relative to the 
	bottom left-hand corner of the first page. The 
	enclosure for credit-words is none by default.

	By default, a series of credit-words elements within a
	single credit element follow one another in sequence
	visually. Non-positional formatting attributes are carried
	over from the previous element by default.

	The page attribute for the credit element, new in Version
	2.0, specifies the page number where the credit should
	appear. This is an integer value that starts with 1 for the
	first page. Its value is 1 by default. Since credits occur
	before the music, these page numbers do not refer to the
	page numbering specified by the print element's page-number
	attribute.

	In the initial release of Version 2.0, the credit element
	had a non-deterministic definition. The current credit
	element definition has the same meaning, but avoids the
	validity errors arising from a non-deterministic definition.
-->
<!ELEMENT credit
	(link*, bookmark*, 
	(credit-image | 
	 (credit-words, (link*, bookmark*, credit-words)*)))>
<!ATTLIST credit
    page NMTOKEN #IMPLIED
>
<!ELEMENT credit-words (#PCDATA)>
<!ATTLIST credit-words
    %text-formatting;
>
<!ELEMENT credit-image EMPTY>
<!ATTLIST credit-image
    source CDATA #REQUIRED
    type CDATA #REQUIRED
    %position; 
    %halign;
    %valign-image;
>

<!--
	The part-list identifies the different musical parts in
	this movement. Each part has an ID that is used later
	within the musical data. Since parts may be encoded
	separately and combined later, identification elements
	are present at both the score and score-part levels.
	There must be at least one score-part, combined as
	desired with part-group elements that indicate braces
	and brackets. Parts are ordered from top to bottom in
	a score based on the order in which they appear in the
	part-list.
	
	Each MusicXML part corresponds to a track in a Standard
	MIDI Format 1 file. The score-instrument elements are
	used when there are multiple instruments per track.
	The midi-device element is used to make a MIDI device
	or port assignment for the given track. Initial
	midi-instrument assignments may be made here as well.

	The part-name and part-abbreviation elements are defined
	in the common.mod file, as they can be used within both the
	part-list and print elements.
-->
<!ELEMENT part-list (part-group*, score-part,
	(part-group | score-part)*)>
<!ELEMENT score-part (identification?,
	part-name, part-name-display?,
	part-abbreviation?, part-abbreviation-display?, 
	group*, score-instrument*, midi-device?, midi-instrument*)>
<!ATTLIST score-part
    id ID #REQUIRED
>

<!--
	The part-name indicates the full name of the musical part.
	The part-abbreviation indicates the abbreviated version of
	the name of the musical part. The part-name will often
	precede the first system, while the part-abbreviation will
	precede the other systems. The formatting attributes for
	these elements are deprecated in Version 2.0 in favor of
	the new part-name-display and part-abbreviation-display
	elements. These are defined in the common.mod file as they
	are used in both the part-list and print elements. They
	provide more complete formatting control for how part names
	and abbreviations appear in a score.
-->
<!ELEMENT part-name (#PCDATA)>
<!ATTLIST part-name
    %print-style;
    %print-object;
    %justify;
>
<!ELEMENT part-abbreviation (#PCDATA)>
<!ATTLIST part-abbreviation
    %print-style;
    %print-object;
    %justify;
>
<!--
	The part-group element indicates groupings of parts in the
	score, usually indicated by braces and brackets. Braces
	that are used for multi-staff parts should be defined in
	the attributes element for that part. The part-group start
	element appears before the first score-part in the group.
	The part-group stop element appears after the last 
	score-part in the group.
	
	The number attribute is used to distinguish overlapping
	and nested part-groups, not the sequence of groups. As
	with parts, groups can have a name and abbreviation.
	Formatting attributes for group-name and group-abbreviation
	are deprecated in Version 2.0 in favor of the new
	group-name-display and group-abbreviation-display elements.
	Formatting specified in the group-name-display and
	group-abbreviation-display elements overrides formatting
	specified in the group-name and group-abbreviation
	elements, respectively.

	The group-symbol element indicates how the symbol for
	a group is indicated in the score. Values include none,
	brace, line, and bracket; the default is none. The 
	group-barline element indicates if the group should have
	common barlines. Values can be yes, no, or Mensurstrich.
	The group-time element indicates that the displayed time
	signatures should stretch across all parts and staves in
	the group. Values for the child elements are ignored at
	the stop of a group. 

	A part-group element is not needed for a single multi-staff
	part. By default, multi-staff parts include a brace symbol
	and (if appropriate given the bar-style) common barlines.
	The symbol formatting for a multi-staff part can be more
	fully specified using the part-symbol element, defined in
	the attributes.mod file.
-->
<!ELEMENT part-group (group-name?, group-name-display?,
	group-abbreviation?, group-abbreviation-display?,
	group-symbol?, group-barline?, group-time?, %editorial;)>
<!ATTLIST part-group
    type %start-stop; #REQUIRED
    number CDATA "1"
>

<!ELEMENT group-name (#PCDATA)>
<!ATTLIST group-name
    %print-style;
    %justify;
>
<!ELEMENT group-name-display
	((display-text | accidental-text)*)>
<!ATTLIST group-name-display
    %print-object;
>
<!ELEMENT group-abbreviation (#PCDATA)>
<!ATTLIST group-abbreviation
    %print-style;
    %justify;
>
<!ELEMENT group-abbreviation-display
	((display-text | accidental-text)*)>
<!ATTLIST group-abbreviation-display
    %print-object;
>

<!ELEMENT group-symbol (#PCDATA)>
<!ATTLIST group-symbol
    %position;
    %color;
>

<!ELEMENT group-barline (#PCDATA)>
<!ATTLIST group-barline
    %color;
>
<!ELEMENT group-time EMPTY>

<!--
	The score-instrument element allows for multiple
	instruments per score-part. As with the score-part
	element, each score-instrument has a required ID
	attribute, a name, and an optional abbreviation. The
	instrument-name and instrument-abbreviation are
	typically used within a software application, rather
	than appearing on the printed page of a score.

	A score-instrument element is also required if the
	score specifies MIDI 1.0 channels, banks, or programs.
	An initial midi-instrument assignment can also
	be made here. MusicXML software should be able to
	automatically assign reasonable channels and 
	instruments without these elements in simple cases,
	such as where part names match General MIDI
	instrument names.

	The solo and ensemble elements are new as of Version
	2.0. The solo element is present if performance is
	intended by a solo instrument. The ensemble element
	is present if performance is intended by an ensemble
	such as an orchestral section. The text of the 
	ensemble element contains the size of the section,
	or is empty if the ensemble size is not specified.
	
	The midi-instrument element is defined in the common.mod
	file, as it can be used within both the score-part and
	sound elements.
-->
<!ELEMENT score-instrument
	(instrument-name, instrument-abbreviation?,
	 (solo | ensemble)?)>
<!ATTLIST score-instrument
    id ID #REQUIRED
>
<!ELEMENT instrument-name (#PCDATA)>
<!ELEMENT instrument-abbreviation (#PCDATA)>
<!ELEMENT solo EMPTY>
<!ELEMENT ensemble (#PCDATA)>

<!--
	The midi-device content corresponds to the DeviceName
	meta event in Standard MIDI Files. The optional port
	attribute is a number from 1 to 16 that can be used
	with the unofficial MIDI port (or cable) meta event.
-->
<!ELEMENT midi-device (#PCDATA)>
<!ATTLIST midi-device
    port CDATA #IMPLIED
>

<!--
	The group element allows the use of different versions of
	the part for different purposes. Typical values include
	score, parts, sound, and data. Ordering information that is
	directly encoded in MuseData can be derived from the
	ordering within a MusicXML score or opus.
-->
<!ELEMENT group (#PCDATA)>

<!--
	Here is the basic musical data that is either associated
	with a part or a measure, depending on whether partwise
	or timewise hierarchy is used.
-->
<!ENTITY % music-data
	"(note | backup | forward | direction | attributes |
	  harmony | figured-bass | print | sound | barline | 
	  grouping | link | bookmark)*">

<!--
	The score-header entity contains basic score metadata
	about the work and movement, score-wide defaults for
	layout and fonts, credits that appear on the first page,
	and the part list. 
-->
<!ENTITY % score-header
	"(work?, movement-number?, movement-title?,
	  identification?, defaults?, credit*, part-list)">

<!--
	The score is the root element for the DTD. It includes
	the score-header entity, followed either by a series of
	parts with measures inside (score-partwise) or a series
	of measures with parts inside (score-timewise). Having
	distinct top-level elements for partwise and timewise
	scores makes it easy to ensure that an XSLT stylesheet
	does not try to transform a document already in the
	desired format. The document-attributes entity includes the
	version attribute and is defined in the common.mod file.
-->
<![ %partwise; [
<!ELEMENT score-partwise (%score-header;, part+)>
<!ATTLIST score-partwise
    %document-attributes;
>	
<!ELEMENT part (measure+)>
<!ELEMENT measure (%music-data;)>
]]>
<![ %timewise; [
<!ELEMENT score-timewise (%score-header;, measure+)>
<!ATTLIST score-timewise
    %document-attributes;
>	
<!ELEMENT measure (part+)>
<!ELEMENT part (%music-data;)>
]]>
<!--
	In either format, the part element has an id attribute that
	is an IDREF back to a score-part in the part-list. Measures
	have a required number attribute (going from partwise to
	timewise, measures are grouped via the number).
-->
<!ATTLIST part
    id IDREF #REQUIRED
>
<!--
	The implicit attribute is set to "yes" for measures where
	the measure number should never appear, such as pickup
	measures and the last half of mid-measure repeats. The
	value is "no" if not specified.
	
	The non-controlling attribute is intended for use in
	multimetric music like the Don Giovanni minuet. If set
	to "yes", the left barline in this measure does not
	coincide with the left barline of measures in other
	parts. The value is "no" if not specified. 

	In partwise files, the number attribute should be the same
	for measures in different parts that share the same left
	barline. While the number attribute is often numeric, it
	does not have to be. Non-numeric values are typically used
	together with the implicit or non-controlling attributes
	being set to "yes". For a pickup measure, the number
	attribute is typically set to "0" and the implicit attribute
	is typically set to "yes". Further details about measure
	numbering can be defined using the measure-numbering
	element defined in the direction.mod file

	Measure width is specified in tenths. These are the
	global tenths specified in the scaling element, not
	local tenths as modified by the staff-size element.
-->
<!ATTLIST measure
    number CDATA #REQUIRED
    implicit %yes-no; #IMPLIED
    non-controlling %yes-no; #IMPLIED
    width %tenths; #IMPLIED
>
