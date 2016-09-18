<?xml version="1.0" encoding="UTF-8"?>

<!--
	MusicXML™ to20.xsl stylesheet
	
	Version 3.0
	
	Copyright © 2004-2011 MakeMusic, Inc.
	http://www.makemusic.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Public License Version 3.0,
	available from:
	
		http://www.musicxml.org/dtds/license.html
-->

<!--
	To20.xsl converts from MusicXML 3.0 to 2.0 for
	compatibility with older products.
-->

<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!--
    XML output, with a DOCTYPE refering the partwise DTD.
    Here we use the full Internet URL.
  -->

  <xsl:output method="xml" indent="yes" encoding="UTF-8"
	omit-xml-declaration="no" standalone="no"
	doctype-system="http://www.musicxml.org/dtds/partwise.dtd"
	doctype-public="-//Recordare//DTD MusicXML 2.0 Partwise//EN" />

  <!--
    For the root, only look for score-partwise. Anything else 
    as a root element gets ignored.
  -->  
  <xsl:template match="/">
    <xsl:apply-templates select="./score-partwise"/>
  </xsl:template>

  <!--
    Transformations that remove post-2.0 elements and 
    attributes.
  -->
  
  <!-- Additions in note.mod -->
  <xsl:template 
    match="notehead-text |
			tied/@dash-length | tied/@space-length |
			slur/@dash-length | slur/@space-length |
			glissando/@dash-length | glissando/@space-length |
			slide/@dash-length | slide/@space-length |
			scoop/@dash-length | scoop/@space-length |
			plop/@dash-length | plop/@space-length |
			doit/@dash-length | doit/@space-length |
			falloff/@dash-length | falloff/@space-length |
			extend/@default-x | extend/@default-y |
			extend/@relative-x | extend/@relative-y |
			extend/@type | rest/@measure | 
			breath-mark/text() | tie/@time-only |
			turn/@slash | delayed-turn/@slash |
			inverted-turn/@slash | delayed-inverted-turn |
			vertical-turn | hole | arrow | handbell | 
			mordent/@approach | inverted-mordent/@approach |
			mordent/@departure | inverted-mordent/@departure |
			notations/@print-object | lyric/@print-object"/>

  <!--
    Remove accidental and accidental-mark elements with 
    new values.
  -->
  <xsl:template 
    match="accidental[. = 'sharp-down' or . = 'sharp-up' or 
				. = 'natural-down' or . = 'natural-up' or 
				. = 'flat-down' or . = 'flat-up' or
				. = 'triple-sharp' or . = 'triple-flat' or
				. = 'slash-quarter-sharp' or . = 'slash-sharp' or
				. = 'slash-flat' or . = 'double-slash-flat' or
				. = 'sharp-1' or . = 'sharp-2' or
				. = 'sharp-3' or . = 'sharp-5' or
				. = 'flat-1' or . = 'flat-2' or
				. = 'flat-3' or . = 'flat-4' or
				. = 'sori' or . = 'koron']"/>

  <xsl:template 
    match="accidental-mark[. = 'sharp-down' or . = 'sharp-up' or 
				. = 'natural-down' or . = 'natural-up' or 
				. = 'flat-down' or . = 'flat-up' or
				. = 'triple-sharp' or . = 'triple-flat' or
				. = 'slash-quarter-sharp' or . = 'slash-sharp' or
				. = 'slash-flat' or . = 'double-slash-flat' or
				. = 'sharp-1' or . = 'sharp-2' or
				. = 'sharp-3' or . = 'sharp-5' or
				. = 'flat-1' or . = 'flat-2' or
				. = 'flat-3' or . = 'flat-4' or
				. = 'sori' or . = 'koron']"/>

  <!-- Remove type elements with new values. -->
  <xsl:template 
    match="type[. = '1024th' or . = '512th' or 
				. = 'maxima']"/> 

  <!-- Remove notehead elements with new values. -->
  <xsl:template 
    match="notehead[. = 'fa up' or . = 'circle dot' or 
					. = 'left triangle' or . = 'rectangle']"/> 

  <!-- Remove beam element with new number values -->
  <xsl:template 
    match="beam[@number[. = '7' or . = '8']]"/>

  <!-- Remove tremolo element with new number values -->
  <xsl:template 
    match="tremolo[. = '7' or . = '8']"/>

  <!-- 
    Remove tied elements with new continue value for the 
	for the type attribute.
  -->
  <xsl:template 
    match="tied[@type[. = 'continue']]"/>

  <!-- Additions in attributes.mod -->

  <xsl:template 
    match="key-accidental | interchangeable | 
			senza-misura/text() | transpose/@number |
			time/@halign | time/@valign |
			time/@separator | clef/@after-barline"/>

  <!-- Remove cancel location attribute with new values -->
  <xsl:template 
    match="cancel/@location[. = 'before-barline']"/>

  <!-- Remove time symbol attribute with new values -->
  <xsl:template 
    match="time/@symbol[. = 'note' or . = 'dotted-note']"/>

  <!-- Remove all but first transpose element -->
  <xsl:template 
    match="attributes/transpose[position() > 1]"/>

  <!-- Remove jianpu clefs altogether -->
  <xsl:template 
    match="clef[//sign[.='jianpu']]"/>

  <!-- Remove part-symbol element with new square value -->
  <xsl:template 
    match="part-symbol[.='square']"/>

  <!-- Additions in barline.mod -->
  <xsl:template 
    match="repeat/@winged"/>

  <!-- Additions in common.mod -->

  <xsl:template 
    match="dynamics/@halign | dynamics/@valign |
			dynamics/@underline | dynamics/@overline |
			dynamics/@line-through | dynamics/@enclosure |
			coda/@halign | coda/@valign |
			segno/@halign | segno/@valign |
			midi-device/@id | play | @xml:space"/>
  
  <!-- 
    Remove display-text and accidental-text enclosure 
	attributes that have values of square, circle, or bracket.
  -->
  <xsl:template 
    match="display-text/@enclosure[. = 'square' or . = 'circle' or
								   . = 'bracket' or . = 'triangle' or
								   . = 'diamond'] "/>
  <xsl:template 
    match="accidental-text/@enclosure[. = 'square' or . = 'circle' or
									  . = 'bracket' or . = 'triangle' or
									  . = 'diamond'] "/>

  <!-- Remove accidental-text elements with new values. -->
  <xsl:template 
    match="accidental-text[. = 'sharp-down' or . = 'sharp-up' or 
				. = 'natural-down' or . = 'natural-up' or 
				. = 'flat-down' or . = 'flat-up' or
				. = 'triple-sharp' or . = 'triple-flat' or
				. = 'slash-quarter-sharp' or . = 'slash-sharp' or
				. = 'slash-flat' or . = 'double-slash-flat' or
				. = 'sharp-1' or . = 'sharp-2' or
				. = 'sharp-3' or . = 'sharp-5' or
				. = 'flat-1' or . = 'flat-2' or
				. = 'flat-3' or . = 'flat-4' or
				. = 'sori' or . = 'koron']"/>

  <!-- Additions in direction.mod -->

  <xsl:template 
    match="rehearsal/@justify | rehearsal/@halign |
			rehearsal/@valign | rehearsal/@letter-spacing |
			rehearsal/@line-height | 
			wedge/@line-type | wedge/@niente |
			wedge/@dash-length | wedge/@space-length |
			bracket/@dash-length | bracket/@space-length |
			dashes/@dash-length | dashes/@space-length |
			octave-shift/@dash-length | octave-shift/@space-length |
			accordion-registration/@halign | accordion-registration/@valign |
			damp/@halign | damp/@valign |
			damp-all/@halign | damp-all/@valign |
			eyeglasses/@halign | eyeglasses/@valign |
			harp-pedals/@halign | harp-pedals/@valign |
			pedal/@halign | pedal/@valign | pedal/@sign |
			other-direction/@halign | other-direction/@valign |
			measure-numbering/@halign | measure-numbering/@valign |
			metronome/@halign | metronome/@valign |
			metronome/@justify | frame/@unplayed |
			degree-value/@symbol | sound/midi-device"/>

  <!-- 
    For safety, remove entire direction that has a new 
	continue value for the type attribute.
  -->
  <xsl:template 
    match="direction[direction-type[wedge[@type[. = 'continue']]]]"/>

  <xsl:template 
    match="direction[direction-type[dashes[@type[. = 'continue']]]]"/>

  <xsl:template 
    match="direction[direction-type[bracket[@type[. = 'continue']]]]"/>

  <xsl:template 
    match="direction[direction-type[octave-shift[@type[. = 'continue']]]]"/>

  <xsl:template 
    match="direction[direction-type[pedal[@type[. = 'continue']]]]"/>

  <!-- 
    For safety, remove entire direction that has a new
    MusicXML 3.0 direction-type child.
  -->
  <xsl:template 
    match="direction[direction-type[string-mute]]"/>

  <xsl:template 
    match="direction[direction-type[principal-voice]]"/>

  <xsl:template 
    match="direction[direction-type[percussion]]"/>

  <!-- 
    Remove rehearsal enclosure attributes that have values
	of rectangle, oval, bracket, triangle, or diamond.
  -->
  <xsl:template 
    match="rehearsal/@enclosure[. = 'rectangle' or . = 'oval' or
							. = 'bracket' or . = 'triangle' or
							. = 'diamond'] "/>

  <!-- 
    Remove words enclosure attributes that have values
	of square, circle, bracket, triangle, or diamond.
  -->
  <xsl:template 
    match="words/@enclosure[. = 'square' or . = 'circle' or
							. = 'bracket' or . = 'triangle' or
							. = 'diamond'] "/>

  <!-- Remove metronome-beam element with new number values -->
  <xsl:template 
    match="metronome-beam[@number[. = '7' or . = '8']]"/>

  <!-- Additions in layout.mod -->
  <xsl:template 
    match="distance | system-dividers"/>
  
  <!-- Additions in score.mod -->

  <xsl:template 
    match="credit-type | instrument-sound | virtual-instrument"/>
  
  <!-- Remove all but first midi-device element -->
  <xsl:template 
    match="score-part/midi-device[position() > 1]"/>

  <!-- Remove group-symbol element with new square value -->
  <xsl:template 
    match="group-symbol[.='square']"/>

  <!-- 
    Remove credit-words enclosure attributes that have
	values of square, circle, bracket, triangle, or diamond.
  -->
  <xsl:template 
    match="credit-words/@enclosure[. = 'square' or . = 'circle' or
								   . = 'bracket' or . = 'triangle' or
								   . = 'diamond'] "/>

  <!--
    Convert score version attribute to 2.0
  -->
  <xsl:template 
    match="score-partwise/@version | score-timewise/@version">
    <xsl:attribute name="version">2.0</xsl:attribute>
  </xsl:template>

  <!--
    The identity transformation. Used for everything that
    stays the same in 2.0.
  -->

  <xsl:template match="text()">
    <xsl:value-of select="." />
  </xsl:template>
  
  <!--
    Whitespace within an xsl:copy could cause problems with 
    empty elements.
  -->
  <xsl:template match="*|@*|comment()|processing-instruction()">
    <xsl:copy><xsl:apply-templates
        select="*|@*|comment()|processing-instruction()|text()"
    /></xsl:copy>
  </xsl:template>

</xsl:stylesheet>
