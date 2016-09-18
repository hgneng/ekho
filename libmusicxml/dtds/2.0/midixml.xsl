<?xml version="1.0" encoding="UTF-8"?>

<!--
	MIDI XML midixml.xsl for sorting MIDI files with
	absolute timestamps.

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!--
	XSL Stylesheet for sorting MIDI XML by absolute timestamp.
-->

<xsl:stylesheet
	version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


  <!--
	The identity transformation. Used for everything aside
	from the track element.
  -->

  <xsl:template match="text()">
    <xsl:value-of select="." />
  </xsl:template>
  
  <xsl:template match="*|@*|comment()|processing-instruction()">
    <xsl:copy>
      <xsl:apply-templates
        select="*|@*|comment()|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:template>

  <!--
	When we get to a Track, sort all events within the track
	by their Absolute timestamp.
  -->

  <xsl:template match="Track">
    <xsl:element name="Track">
      <xsl:apply-templates>
        <xsl:sort data-type="number" select="Absolute" />
      </xsl:apply-templates>
    </xsl:element>
  </xsl:template>
 
</xsl:stylesheet>