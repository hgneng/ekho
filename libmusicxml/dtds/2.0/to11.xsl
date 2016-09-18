<?xml version="1.0" encoding="UTF-8"?>

<!--
	MusicXML™ to11.xsl stylesheet
	
	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	To11.xsl converts from MusicXML 2.0 to 1.1 for
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
	doctype-public="-//Recordare//DTD MusicXML 1.1 Partwise//EN" />

  <!--
    For the root, only look for score-partwise. Anything else 
    as a root element gets ignored.
  -->  
  <xsl:template match="/">
    <xsl:apply-templates select="./score-partwise"/>
  </xsl:template>

  <!--
    Transformations that remove post-1.1 elements and 
    attributes.
  -->
  
  <!-- Additions in note.mod -->
  <xsl:template 
    match="dot/@placement | tremolo/@type |
			harmonic/@print-object | 
			other-notation/@print-object |
			stress | unstress | inverted-turn | 
			pluck/text() | elision/text()"/>
    
  <!--
    If there is no elision element, remove all but the first
    text and syllabic elements.
  -->  
  <xsl:template 
    match="lyric[not (elision)]/text[position() > 1] |
		lyric[not (elision)]/syllabic[position() > 1]"/>

  <!-- Remove other-notation elements with a single type. -->
  <xsl:template 
    match="other-notation[@type = 'single']"/>

  <!-- Additions in attributes.mod -->
  <xsl:template 
    match="key/@font-family | key/@font-style |
			key/@font-size | key/@font-weight |
			key/@default-x | key/@default-y |
			key/@relative-x | key/@relative-y |
			time/@font-family | time/@font-style |
			time/@font-size | time/@font-weight |
			time/@default-x | time/@default-y |
			time/@relative-x | time/@relative-y |
			clef/@font-family | clef/@font-style |
			clef/@font-size | clef/@font-weight |
			clef/@default-x | clef/@default-y |
			clef/@relative-x | clef/@relative-y | clef/@size |
			key/@print-object | clef/@print-object |
			cancel/@location | part-symbol | key-octave |
			slash-type | slash-dot"/>

  <!-- Remove additional clefs altogether -->
  <xsl:template 
    match="clef[@additional]"/>

  <!-- Remove all but first key and time element -->
  <xsl:template 
    match="attributes/key[position() > 1] | 
			attributes/time[position() > 1]"/>

  <!-- Additions in barline.mod -->
  <xsl:template 
    match="ending/@print-object | ending/@text-x |
			ending/@text-y | barline/@segno |
			barline/@coda | barline/@divisions"/>

  <xsl:template 
    match="bar-style[(. = 'tick') or (. = 'short')]"/>

  <!-- Additions in common.mod -->
  <xsl:template 
    match="volume | pan | elevation | @directive |
			part-name-display | part-abbreviation-display | 
			@rotation | @dir | @letter-spacing | @line-height |
			@overline | @underline | @line-through |
			footnote/@xml:lang | footnote/@enclosure |
			fermata/text()"/>
  
  <!-- Additions in identity.mod -->
  <xsl:template 
    match="relation"/>

  <!--
    Remove a supports element that contains an attribute or
    value attribute
  -->
  <xsl:template 
    match="supports[@attribute | @value]"/>

  <!-- Additions in layout.mod -->
  <xsl:template 
    match="appearance"/>

  <!-- 
    Remove a page-layout element that does not have a
    page-height child
  -->
  <xsl:template 
    match="page-layout[not (page-height)]"/>
  
  <!-- Additions in direction.mod -->
  <xsl:template 
    match="offset/@sound | sound/offset |
			measure-numbering | print/@blank-page |
			print/part-name | print/part-abbreviation |
			root-step/@* | root-alter/@* |
			bass-step/@* | bass-alter/@* |
			function/@* | inversion/@* | 
			kind/@default-x | kind/@default-y |
			kind/@relative-x | kind/@relative-y |
			kind/@font-family | kind/@font-style |
			kind/@font-size | kind/@font-weight | 
			kind/@color | kind/@halign | kind/@valign |
			degree-value/@* | degree-alter/@* | 
			degree-type/@* | per-minute/@* |
			frame/@halign | frame/@valign |
			first-fret/@* | other-direction/@print-object"/>

  <!-- 
    For safety, remove entire direction that has a new
    MusicXML 2.0 direction-type child, or a new style
	of metronome element.
  -->
  <xsl:template 
    match="direction[direction-type[image]]"/>

  <xsl:template 
    match="direction[direction-type[accordion-registration]]"/>

  <xsl:template 
    match="direction[direction-type[metronome[metronome-note]]]"/>

  <!-- 
    Remove pedal attributes that have values other than 
	yes or no.
  -->
  <xsl:template 
    match="@damper-pedal[. != 'yes' and . != 'no'] "/>
  <xsl:template 
    match="@soft-pedal[. != 'yes' and . != 'no'] "/>
  <xsl:template 
    match="@sostenuto-pedal[. != 'yes' and . != 'no'] "/>

  <!-- 
    Remove grouping type attributes that have values other than 
	start or stop.
  -->
  <xsl:template 
    match="grouping/@typel[. != 'start' and . != 'stop'] "/>

  <!-- Additions in link.mod -->
  <xsl:template 
    match="link/@element | link/@position | 
			bookmark/@element | bookmark/@position"/>

  <!-- Additions in score.mod -->
  <xsl:template 
    match="group-name-display | group-abbreviation-display |
			group-time | solo | ensemble | credit[credit-image] |
			credit/link | credit/bookmark"/>
  
  <!-- Remove a credit that is not on page 1 -->
  <xsl:template 
    match="credit[@page != '1']"/>

  <!-- Remove the page attribute for page 1 credits -->
  <xsl:template 
    match="credit/@page[. = '1']"/>
  
<!--
    Convert score version attribute to 1.1
  -->
  <xsl:template 
    match="score-partwise/@version | score-timewise/@version">
    <xsl:attribute name="version">1.1</xsl:attribute>
  </xsl:template>

  <!--
    Do not copy text for elision, pluck, or fermata elements.
  -->
  <xsl:template match="elision | pluck | fermata">
    <xsl:copy><xsl:apply-templates
        select="*|@*|comment()|processing-instruction()"
    /></xsl:copy>
  </xsl:template>

  <!--
    The identity transformation. Used for everything that
    stays the same in 1.1.
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
