<!--
	MusicXML™ layout.mod module

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	Version 1.1 of the MusicXML format added layout information
	for pages, systems, staffs, and measures. These layout
	elements joined the print and sound elements in providing
	formatting data as elements rather than attributes.

	Everything is measured in tenths of staff space. Tenths are
	then scaled to millimeters within the scaling element, used
	in the defaults element at the start of a score. Individual
	staves can apply a scaling factor to adjust staff size.
	When a MusicXML element or attribute refers to tenths,
	it means the global tenths defined by the scaling element,
	not the local tenths as adjusted by the staff-size element.
-->

<!-- Elements -->

<!--
	Margins, page sizes, and distances are all measured in
	tenths to keep MusicXML data in a consistent coordinate
	system as much as possible. The translation to absolute
	units is done in the scaling element, which specifies
	how many millimeters are equal to how many tenths. For
	a staff height of 7 mm, millimeters would be set to 7
	while tenths is set to 40. The ability to set a formula
	rather than a single scaling factor helps avoid roundoff
	errors.
-->
<!ELEMENT scaling (millimeters, tenths)>
<!ELEMENT millimeters (#PCDATA)>
<!ELEMENT tenths %layout-tenths;>

<!-- 
	Margin elements are included within many of the larger
	layout elements.
-->
<!ELEMENT left-margin %layout-tenths;>
<!ELEMENT right-margin %layout-tenths;>
<!ELEMENT top-margin %layout-tenths;>
<!ELEMENT bottom-margin %layout-tenths;>

<!--
	Page layout can be defined both in score-wide defaults
	and in the print element. Page margins are specified either
	for both even and odd pages, or via separate odd and even
	page number values. The type is not needed when used as
	part of a print element. If omitted when used in the
	defaults element, "both" is the default.
-->
<!ELEMENT page-layout ((page-height, page-width)?, 
	(page-margins, page-margins?)?)>
<!ELEMENT page-height %layout-tenths;>
<!ELEMENT page-width %layout-tenths;>
<!ELEMENT page-margins (left-margin, right-margin, 
	top-margin, bottom-margin)>
<!ATTLIST page-margins
    type (odd | even | both) #IMPLIED
>

<!--
	System layout includes left and right margins and the
	vertical distance from the previous system. Margins are
	relative to the page margins. Positive values indent and
	negative values reduce the margin size. The system
	distance is measured from the bottom line of the previous
	system to the top line of the current system. It is ignored
	for the first system on a page. The top system distance
	is measured from the page's top margin to the top line
	of the first system. It is ignored for all but the first
	system on a page.

	Sometimes the sum of measure widths in a system may not
	equal the system width specified by the layout elements due
	to roundoff or other errors. The behavior when reading
	MusicXML files in these cases is application-dependent.
	For instance, applications may find that the system layout
	data is more reliable than the sum of the measure widths,
	and adjust the measure widths accordingly.
-->
<!ELEMENT system-layout
	(system-margins?, system-distance?, top-system-distance?)>
<!ELEMENT system-margins (left-margin, right-margin)>
<!ELEMENT system-distance %layout-tenths;>
<!ELEMENT top-system-distance %layout-tenths;>

<!--
	Staff layout includes the vertical distance from the bottom
	line of the previous staff in this system to the top line
	of the staff specified by the number attribute. The
	optional number attribute refers to staff numbers within
	the part, from top to bottom on the system. A value of 1
	is assumed if not present. When used in the defaults
	element, the values apply to all parts. This value is
	ignored for the first staff in a system.
-->
<!ELEMENT staff-layout (staff-distance?)>
<!ELEMENT staff-distance %layout-tenths;>
<!ATTLIST staff-layout
    number CDATA #IMPLIED
>

<!--
	Measure layout includes the horizontal distance from the
	previous measure. This value is only used for systems
	where there is horizontal whitespace in the middle of a
	system, as in systems with codas. To specify the measure
	width, use the width attribute of the measure element.
-->
<!ELEMENT measure-layout (measure-distance?)>
<!ELEMENT measure-distance %layout-tenths;>

<!--
	The appearance element controls general graphical
	settings for the music's final form appearance on a
	printed page of display. Currently this includes support
	for line widths and definitions for note sizes, plus an
	extension element for other aspects of appearance.

	The line-width element indicates the width of a line type
	in tenths. The type attribute defines what type of line is
	being defined. Values include beam, bracket, dashes,
	enclosure, ending, extend, heavy barline, leger,
	light barline, octave shift, pedal, slur middle, slur tip,
	staff, stem, tie middle, tie tip, tuplet bracket, and
	wedge. The text content is expressed in tenths.

	The note-size element indicates the percentage of the
	regular note size to use for notes with a cue and large
	size as defined in the type element. The grace type is
	used for notes of cue size that that include a grace
	element. The cue type is used for all other notes with 
	cue size, whether defined explicitly or implicitly via a 
	cue element. The large type is used for notes of large
	size. The text content represent the numeric percentage.
	A value of 100 would be identical to the size of a regular
	note as defined by the music font.

	The other-appearance element is used to define any
	graphical settings not yet in the current version of the
	MusicXML format. This allows extended representation,
	though without application interoperability.
-->
<!ELEMENT appearance
	(line-width*, note-size*, other-appearance*)>
<!ELEMENT line-width %layout-tenths;>
<!ATTLIST line-width
    type CDATA #REQUIRED
>
<!ELEMENT note-size (#PCDATA)>
<!ATTLIST note-size
    type (cue | grace | large) #REQUIRED
>
<!ELEMENT other-appearance (#PCDATA)>
<!ATTLIST other-appearance
    type CDATA #REQUIRED
>
