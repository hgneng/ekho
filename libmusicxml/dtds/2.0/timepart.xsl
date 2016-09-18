<?xml version="1.0" encoding="UTF-8"?>

<!--
	MusicXML™ timepart.xsl stylesheet

	Version 2.0 - 18 June 2007
	
	Copyright © 2004-2007 Recordare LLC.
	http://www.recordare.com/
	
	This MusicXML™ work is being provided by the copyright
	holder under the MusicXML Document Type Definition 
	Public License Version 2.0, available from:
	
		http://www.recordare.com/dtds/license.html
-->

<!-- 
	Timepart.xsl is an XSLT stylesheet for transforming
	timewise MusicXML scores into partwise scores. Thus
	instead of having parts included within each measure,
	the transformed score includes measures within each part.
	This type of transformation allows the 2-dimensional
	nature of a musical score to be adequately represented
	within a hierarchical format like XML.
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
    For the root, only look for score-partwise and
    score-timewise. Anything else as a root element gets
    ignored.
  -->  
  <xsl:template match="/">
    <xsl:apply-templates select="./score-partwise"/>
    <xsl:apply-templates select="./score-timewise"/>
  </xsl:template>

  <!--
    If we have a partwise score, we really shouldn't be
    applying this stylesheet. Copy everthing as-is without
    triggering templates.
  -->
  <xsl:template match="score-partwise">
    <xsl:copy-of select="." />
  </xsl:template>

  <!-- The identity transformation. Used by default. -->
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

  <!--
    We need to take control at the document element level
    explicitly to redo the tree. The header elements should
    all be copied. After that, we loop through the parts in
    measure 1, and create the part elements (for measure 1
    and all others) from within that loop.
  -->
  <xsl:template match="score-timewise">
	
    <!-- Create the score-partwise element. -->
    <xsl:element name="score-partwise">
		
      <!--
        Copy the seven score header elements and their
        children. The DTD specifies that these occur, if
        present, in a fixed order.
      -->
      <xsl:apply-templates select="@version[.!='1.0']"/>
      <xsl:apply-templates select="work"/>
      <xsl:apply-templates select="movement-number"/>
      <xsl:apply-templates select="movement-title"/>
      <xsl:apply-templates select="identification"/>
      <xsl:apply-templates select="defaults"/>
      <xsl:apply-templates select="credit"/>
      <xsl:apply-templates select="part-list"/>
			
      <!--
        Now loop through all parts in the first measure.
      -->
      <xsl:for-each select="measure[1]/part">
			
        <!--
          Bind part ID to a variable for use throughout the
          loop, including inner loop where we will lose the
          immediate context.
        -->
        <xsl:variable name="part-id">
          <xsl:value-of select="@id"/>
        </xsl:variable>
				
        <!-- Create the part element. -->
        <xsl:element name="part">
				
          <!--
            Now we need to copy the part id attribute.
          -->
          <xsl:copy-of select="@id" />
					
          <!--
            Now for the inner loop. We go back to the root
            ancestor, and loop through each measure and
            part, looking for the ones that match the part
            ID, and add it here.
						
            This is inefficient. but it provides a working
            starting point.
          -->
          <xsl:for-each select="../../measure/part">
            <xsl:if test="@id=$part-id">
						
              <!-- Create the measure element. -->
              <xsl:element name="measure">
							
                <!--
                  Copy the attributes from the parent 
                  measure element. 
                -->
                <xsl:attribute name="number">
                  <xsl:value-of select="parent::measure/@number"/>
                </xsl:attribute>
                <xsl:if test="parent::measure/@implicit[. = 'yes']">
                  <xsl:attribute name="implicit">
                    <xsl:value-of select="parent::measure/@implicit"/>
                  </xsl:attribute>
                </xsl:if>
                <xsl:if test="parent::measure/@non-controlling[. = 'yes']">
                  <xsl:attribute name="non-controlling">
                    <xsl:value-of
                      select="parent::measure/@non-controlling"/>
                  </xsl:attribute>
                </xsl:if>
                <xsl:if test="parent::measure/@width">
                  <xsl:attribute name="width">
                    <xsl:value-of
                      select="parent::measure/@width"/>
                  </xsl:attribute>
                </xsl:if>
								
                <!--
                  Now copy all the descendants using
                  identity transforms.
                -->
                <xsl:apply-templates />
              </xsl:element>
            </xsl:if>
          </xsl:for-each>
					
        </xsl:element>
      </xsl:for-each>
    </xsl:element>       
  </xsl:template>
</xsl:stylesheet>