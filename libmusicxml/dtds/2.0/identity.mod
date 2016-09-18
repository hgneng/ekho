<!--
	MusicXML™ identity.mod module

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	The identify DTD module contains the identification
	element and its children, containing metadata about a
	score.
-->

<!-- Elements -->

<!--
	Identification contains basic metadata about the score.
	It includes the information in MuseData headers that
	may apply at a score-wide, movement-wide, or part-wide
	level. The creator, rights, source, and relation elements
	are based on Dublin Core.
-->
<!ELEMENT identification (creator*, rights*, encoding?,
	source?, relation*, miscellaneous?)>

<!--
	The creator element is borrowed from Dublin Core. It is
	used for the creators of the score. The type attribute is
	used to distinguish different creative contributions. Thus,
	there can be multiple creators within an identification.
	Standard type values are composer, lyricist, and arranger.
	Other type values may be used for different types of
	creative roles. The type attribute should usually be used
	even if there is just a single creator element. The MusicXML
	format does not use the creator / contributor distinction
	from Dublin Core.
-->
<!ELEMENT creator (#PCDATA)>
<!ATTLIST creator
    type CDATA #IMPLIED
>

<!--
	Rights is borrowed from Dublin Core. It contains
	copyright and other intellectual property notices.
	Words, music, and derivatives can have different types,
	so multiple rights tags with different type attributes
	are supported. Standard type values are music, words,
	and arrangement, but other types may be used. The
	type attribute is only needed when there are multiple
	rights elements.
-->
<!ELEMENT rights (#PCDATA)>
<!ATTLIST rights
    type CDATA #IMPLIED
>

<!--
	Encoding contains information about who did the digital
	encoding, when, with what software, and in what aspects.
	Standard type values for the encoder element are music,
	words, and arrangement, but other types may be used. The
	type attribute is only needed when there are multiple
	encoder elements.

	The supports element indicates if the encoding supports
	a particular MusicXML element. This is recommended for
	elements like beam, stem, and accidental, where the
	absence of an element is ambiguous if you do not know
	if the encoding supports that element. For Version 2.0,
	the supports element is expanded to allow programs to
	indicate support for particular attributes or particular
	values. This lets applications communicate, for example,
	that all system and/or page breaks are contained in the
	MusicXML file.
-->
<!ELEMENT encoding ((encoding-date | encoder | software |
	encoding-description | supports)*)>
<!ELEMENT encoding-date %yyyy-mm-dd;>
<!ELEMENT encoder (#PCDATA)>
<!ATTLIST encoder
    type CDATA #IMPLIED
>
<!ELEMENT software (#PCDATA)>
<!ELEMENT encoding-description (#PCDATA)>
<!ELEMENT supports EMPTY>
<!ATTLIST supports
    type %yes-no; #REQUIRED
    element CDATA #REQUIRED
    attribute CDATA #IMPLIED
    value CDATA #IMPLIED
>

<!--
	The source for the music that is encoded. This is similar
	to the Dublin Core source element.
-->
<!ELEMENT source (#PCDATA)>

<!--
	A related resource for the music that is encoded. This is
	similar to the Dublin Core relation element. Standard type
	values are music, words, and arrangement, but other
	types may be used.
-->
<!ELEMENT relation (#PCDATA)>
<!ATTLIST relation
    type CDATA #IMPLIED
>

<!--
	If a program has other metadata not yet supported in the
	MusicXML format, it can go in the miscellaneous area.
-->
<!ELEMENT miscellaneous (miscellaneous-field*)>
<!ELEMENT miscellaneous-field (#PCDATA)>
<!ATTLIST miscellaneous-field
    name CDATA #REQUIRED
>
