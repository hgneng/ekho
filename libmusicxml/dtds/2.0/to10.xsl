<?xml version="1.0" encoding="UTF-8"?>

<!--
	MusicXML™ to10.xsl stylesheet
	
	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	To10.xsl converts from MusicXML 1.1 to 1.0 for
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
	doctype-public="-//Recordare//DTD MusicXML 1.0 Partwise//EN" />

  <!--
    For the root, only look for score-partwise. Anything else 
    as a root element gets ignored.
  -->  
  <xsl:template match="/">
    <xsl:apply-templates select="./score-partwise"/>
  </xsl:template>

  <!--
    Transformations that remove post-1.0 elements and attributes.
  -->
  
  <!-- Additions in note.dtd -->
  <xsl:template 
    match="tuplet-number/@* | tuplet-type/@* | tuplet-dot/@* |
           tuplet/@line-shape | pluck | tremolo |
           tied/@bezier-offset | tied/@bezier-offset2 |
           tied/@bezier-x | tied/@bezier-x2 |
           tied/@bezier-y | tied/@bezier-y2 |
           heel/@substitution | toe/@substitution |
           lyric/@justify | lyric/@placement |
           lyric/@default-x | lyric/@default-y |
           lyric/@relative-x | lyric/@relative-y |
           figured-bass/@parentheses | elision/@* | extend/@* |
           prefix/@* | figure-number/@* / suffix/@* | figure/extend"/>

  <!--
    Convert parentheses or bracket attributes on accidental
    elements into the editorial attribute from MusicXML 1.0.
  -->
  <xsl:template match="accidental/@parentheses | accidental/@bracket">
    <xsl:attribute name="editorial">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  
  <!-- Additions in attributes.dtd -->
  <xsl:template 
    match="staff-size | key/@number | time/@number | 
           staff-details/@print-object | staff-details/@print-spacing |
           directive/@relative-x | directive/@relative-y |
           directive/@default-x | directive/@default-y |
           time/@print-object"/>
  
  <!-- Additions in barline.dtd -->
  <xsl:template 
    match="ending/@relative-x | ending/@relative-y |
           ending/@default-x | ending/@default-y |
           ending/@end-length | ending/text()"/>

  <!-- Additions in common.dtd -->
  <xsl:template
    match="@color | @print-lyric | level/@reference |
           fret/@* | string/@* | footnote/@*"/>
  <xsl:template match="@size[.='large']"/>
  
  <!-- 
    MusicXML 1.1 makes much greater use of font attributes.
    The more general rule strips the font attributes from
    most elements. The more specific rule, which takes 
    priority, keeps them for the elements where they were
    used in MusicXML 1.0.
  -->
  <xsl:template
    match="@font-family | @font-style | @font-size | @font-weight"/>

  <xsl:template
    match="words/@font-family | words/@font-style |
           words/@font-size | words/@font-weight |
           rehearsal/@font-family | rehearsal/@font-style |
           rehearsal/@font-size | rehearsal/@font-weight |
           directive/@font-family | directive/@font-style |
           directive/@font-size | directive/@font-weight |
           text/@font-family | text/@font-style |
           text/@font-size | text/@font-weight |
           hammer-on/@font-family | hammer-on/@font-style |
           hammer-on/@font-size | hammer-on/@font-weight |
           pull-off/@font-family | pull-off/@font-style |
           pull-off/@font-size | pull-off/@font-weight |
           tap/@font-family | tap/@font-style |
           tap/@font-size | tap/@font-weight">
    <xsl:copy><xsl:apply-templates select="*|@*"/></xsl:copy>
  </xsl:template>

  <!-- Additions in layout.dtd -->
  <xsl:template 
    match="scaling | page-layout | system-layout |
           staff-layout | measure-layout"/>
  
  <!-- Additions in direction.dtd -->
  <!-- 
    For safety, remove entire direction that has a new
    MusicXML 1.1 direction-type child.
  -->
  <xsl:template 
    match="direction[//pedal[@type='.change'] |
		//harp-pedals | //scordatura]"/>

  <xsl:template
    match="barre | kind/@* | frame/@* | degree/@* |
           harmony/offset | harmony/staff |
           words/@halign | words/@valign | words/@enclosure | 
           rehearsal/@xml:lang | rehearsal/@enclosure |
           print/@page-number"/>

  <!-- Additions in link.dtd -->
  <xsl:template 
    match="link/@relative-x | link/@relative-y |
           link/@default-x | link/@default-y"/>

  <!-- Additions in score.dtd -->
  <xsl:template 
    match="defaults | credit | measure/@width |
           part-name/@* | part-abbreviation/@* |
           group-name/@* | group-abbreviation/@* |
           group-symbol/@* | group-barline/@* |
           score-partwise/@* | score-timewise/@*"/>

  <!--
    Do not copy text for glissando or slide elements.
  -->
  <xsl:template match="glissando | slide">
    <xsl:copy><xsl:apply-templates
        select="*|@*|comment()|processing-instruction()"
    /></xsl:copy>
  </xsl:template>

  <!--
    The identity transformation. Used for everything that
    stays the same in 1.0.
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
