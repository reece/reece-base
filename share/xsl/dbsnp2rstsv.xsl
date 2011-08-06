<?xml version='1.0'  encoding="ISO-8859-1" ?>

<!--

dbsnp2rstsv.xsl - transform dbsnp xml to TSV file of rs SNP info

This file is based on dbsnp2rdf.xsl by Pierre Lindenbaum
http://plindenbaum.blogspot.com/2010/02/processing-large-xml-documents-with.html

  curl ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/XML/ds_chMT.xml.gz \
  | gzip -cd \
  | xsltproc -\-novalid dbsnp2rstsv.xsl - \
  >snps-MT.tsv

-->

<!DOCTYPE xsl:stylesheet [
          <!ENTITY rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#">
          <!ENTITY xsd "http://www.w3.org/2001/XMLSchema">
          <!ENTITY tab "&#009;">
          <!ENTITY nl  "&#010;">
          ]>

<xsl:stylesheet
	xmlns:s="http://www.ncbi.nlm.nih.gov/SNP/docsum"
	xmlns:rdf='&rdf;'
	xmlns:xsd='&xsd;'
	xmlns:dc="http://purl.org/dc/elements/1.1/"
	xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
	xmlns:o="urn:void:dbsnp"
	version='1.0'
	>

  <xsl:output method="text" indent="yes" encoding="UTF-8" />

  <xsl:template match="/">
	<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="s:ExchangeSet">
	<xsl:apply-templates select="s:Rs" />
  </xsl:template>

  <xsl:template match="s:Rs">
	<xsl:variable name="rsId" select="@rsId"/>
	
	<xsl:variable name="allele" select="s:Sequence/s:Observed/text()"/>

	<xsl:variable name="hgvs_tags">
	  <!-- XSLT2.0: xsl:value-of select="string-join("/hgvs/text()", ',')"/ -->
	  <!-- XSLT2.0: xsl:value-of separator="," select="(s:hgvs)[text()]"/ -->
	  <xsl:for-each select="s:hgvs">
        <xsl:value-of select="text()"/>
        <xsl:if test="not(position() = last())"><xsl:text>&#032;</xsl:text></xsl:if>
	  </xsl:for-each>
	</xsl:variable>

	<xsl:for-each select="s:Assembly[@genomeBuild='37_1' and @groupLabel='GRCh37' and @current='true']">
	  <xsl:variable name="genomeBuild" select="@genomeBuild"/>
	  <xsl:variable name="groupLabel" select="@groupLabel"/>
	  <xsl:for-each select="s:Component[@chromosome]">
		<xsl:variable name="chromosome" select="@chromosome"/>
		<xsl:for-each select="s:MapLoc[@physMapInt and @leftContigNeighborPos and @rightContigNeighborPos and  @orient]">
		  <xsl:variable name="len" select="number(@rightContigNeighborPos)  - number(@leftContigNeighborPos) -1"/>
		  <xsl:variable name="chromStart">
			<xsl:choose>
			  <xsl:when test="$len=0">
				<xsl:value-of select="number(@physMapInt)+1"/>
			  </xsl:when>
			  <xsl:otherwise>
				<xsl:value-of select="number(@physMapInt)"/>
			  </xsl:otherwise>
			</xsl:choose>
		  </xsl:variable>
		  <xsl:variable name="chromEnd" select="$chromStart + $len"/>
		  <xsl:variable name="strand">
			<xsl:choose>
			  <xsl:when test="@orient='forward'"><xsl:text>+</xsl:text></xsl:when>
			  <xsl:when test="@orient='reverse'"><xsl:text>-</xsl:text></xsl:when>
			  <xsl:otherwise><xsl:value-of select="@orient"/></xsl:otherwise>
			</xsl:choose>
		  </xsl:variable>
		  
		  <!-- output one row for each rs and mapping data that meets criteria
			   (expect only 1 currently) -->
		  <xsl:value-of select="concat('rs',$rsId)"/>
		  
		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="concat($groupLabel,'/',$genomeBuild)"/>
		  
		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$chromosome"/>
		  
		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$strand"/>
		  
		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$chromStart"/>
		  
		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$chromEnd"/>

		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$len"/>

		  <!-- URL is easy enough to construct from rsid, so skip it
		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="concat('http://www.ncbi.nlm.nih.gov/snp/',$rsId)"/>
		  -->

		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$allele"/>

		  <xsl:text>&#009;</xsl:text>
		  <xsl:value-of select="$hgvs_tags"/>

		  <xsl:text>&#010;</xsl:text>

		</xsl:for-each>
	  </xsl:for-each>
	</xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
