<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:import href="../../../../../tools/boostbook/xsl/docbook.xsl"/>

  <xsl:output method="xml"
              doctype-public="-//OASIS//DTD DocBook XML V4.2//EN"
              doctype-system="http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd"/>

  <xsl:param name="boost.root" select="'../../../..'"/>
  <xsl:param name="snippet.dir" select="'./snippet'"/>
  <xsl:param name="example.dir" select="'./example'"/>
  
<!-- *********************************************************************** -->
<!-- *********************************************************************** -->
<!-- *********************************************************************** -->

  <xsl:template match="annotations/annotation" mode="area-spec">
   <area id="{@id}-co" linkends="{@id}" coords="{@coords}"/>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="annotations/annotation" mode="area-descr">
   <callout arearefs="{@id}-co" id="{@id}">
    <xsl:apply-templates/>
   </callout>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-example" mode="content">
    <programlisting><xi:include href="{$example.dir}/{@name}.cpp" parse="text" xmlns:xi="http://www.w3.org/2001/XInclude">
     <xi:fallback xmlns:xi='http://www.w3.org/2001/XInclude'>
      <simpara><emphasis>FIXME:  MISSING XINCLUDE CONTENT</emphasis></simpara>
     </xi:fallback>
     </xi:include></programlisting>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-example">
   <example id="{../@id}.{@name}">
    <xsl:copy-of select="title|para|simpara"/>

    <xsl:choose>
     <xsl:when test="annotations">
      <programlistingco>
       <areaspec units="linecolumn">
        <xsl:apply-templates select ="annotations/annotation" mode="area-spec"/>
       </areaspec>

       <xsl:apply-templates select ="." mode="content"/>

       <simplelist type='horiz' columns='5'>
        <member><literal><ulink url="../src/examples/{@name}.cpp">Source code</ulink></literal></member>
        <member> | </member>
        <member><literal><toggle linkend="{@name}-annot" on-label="Show annotations" off-label="Hide annotations"/></literal></member>
        <member> | </member>
        <member><literal><toggle linkend="{@name}-output" on-label="Show output" off-label="Hide output"/></literal></member>
       </simplelist>

       <calloutlist html-id="{@name}-annot" html-class="example-annot">
        <xsl:apply-templates select ="annotations/annotation" mode="area-descr"/>
       </calloutlist>

      </programlistingco>
     </xsl:when>

     <xsl:otherwise>
      <xsl:apply-templates select ="." mode="content"/>

      <simplelist type='horiz' columns='3'>
       <member><literal><ulink url="../src/examples/{@name}.cpp">Source code</ulink></literal></member>
       <member> | </member>
       <member><literal><toggle linkend="{@name}-output" on-label="Show output" off-label="Hide output"/></literal></member>
      </simplelist>
     </xsl:otherwise>

    </xsl:choose>

    <screen html-id="{@name}-output" html-class="example-output"><xi:include href="{$example.dir}/{@name}.output" parse="text" xmlns:xi="http://www.w3.org/2001/XInclude">
      <xi:fallback xmlns:xi='http://www.w3.org/2001/XInclude'><simpara><emphasis>FIXME:  MISSING XINCLUDE CONTENT</emphasis></simpara></xi:fallback>
      </xi:include></screen>
   </example>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-snippet" mode="content">
    <programlisting><xi:include href="{$snippet.dir}/{@name}.cpp" parse="text" xmlns:xi="http://www.w3.org/2001/XInclude">
     <xi:fallback xmlns:xi='http://www.w3.org/2001/XInclude'>
      <simpara><emphasis>FIXME:  MISSING XINCLUDE CONTENT</emphasis></simpara>
     </xi:fallback>
     </xi:include></programlisting>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-snippet">
   <xsl:choose>
    <xsl:when test="annotations">
     <programlistingco>
      <areaspec units="linecolumn">
       <xsl:apply-templates select ="annotations/annotation" mode="area-spec"/>
      </areaspec>

      <xsl:apply-templates select ="." mode="content"/>

      <calloutlist>
       <xsl:apply-templates select ="annotations/annotation" mode="area-descr"/>
      </calloutlist>

     </programlistingco>
    </xsl:when>

    <xsl:otherwise>
     <xsl:apply-templates select ="." mode="content"/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-parameter-reference">
   <inline-reference id="{@id}" curr_entry_var="{generate-id()}">
    <xsl:apply-templates/>
   </inline-reference>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="inline-reference">
   <inline-reference id="{@id}" curr_entry_var="{generate-id()}">
    <xsl:apply-templates select="*"/>
   </inline-reference>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-parameter-reference/refentry">
   <refentry name="{@name}">
    <segmentedlist>
     <?dbhtml list-presentation="list"?>

     <segtitle>Parameter Name</segtitle>
     <segtitle>Environment variable name</segtitle>
     <segtitle>Command line argument name</segtitle>
     <segtitle>Acceptable Values</segtitle>
     <segtitle>Description</segtitle>

     <seglistitem>
      <seg><emphasis><xsl:value-of select="name"/></emphasis></seg>
      <seg><varname><xsl:value-of select="env"/></varname></seg>
      <seg><parameter class='command'><xsl:value-of select="cla"/></parameter></seg>
      <seg><xsl:copy-of select="vals/*"/></seg>
      <seg><xsl:copy-of select="descr/*"/></seg>
     </seglistitem>
    </segmentedlist>
   </refentry>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="btl-equation">
   <informaltable tabstyle="equation" id="{../@id}.eq.{@index}">
    <tgroup cols="2">
     <tbody><row>
      <entry>
       <informalequation>
        <mathphrase>
         <xsl:copy-of select="node()|text()"/>
        </mathphrase>
       </informalequation>
      </entry>
      <entry role="index">(<emphasis role="bold"><xsl:value-of select="@index"/></emphasis>)</entry>
     </row></tbody>
    </tgroup>
   </informaltable>
  </xsl:template>

<!-- *********************************************************************** -->

  <!-- TO FIX: correct formatting? -->
  <xsl:template match="inline-synopsis">
   <programlisting html-class="inline-synopsis">
    <xsl:if test="@id">
     <xsl:attribute name="id">
       <xsl:value-of select="@id"/>
     </xsl:attribute>
    </xsl:if>
    <xsl:apply-templates select="*" mode="inline-synopsis"/>
   </programlisting>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template name="generate.id">
   <xsl:param name="node" select="."/>

   <xsl:choose>
    <xsl:when test="$node/@ref-id">
     <xsl:value-of select="$node/@ref-id"/>
    </xsl:when>
    <xsl:when test="ancestor::class-specialization|ancestor::struct-specialization|ancestor::union-specialization">
     <xsl:value-of select="generate-id(.)"/>
     <xsl:text>-bb</xsl:text>
    </xsl:when>
    <xsl:otherwise>
     <xsl:apply-templates select="$node" mode="generate.id"/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match="macro" mode="inline-synopsis">
   <xsl:text>&#10;</xsl:text>

   <xsl:choose>
    <xsl:when test="@ref-id='none'">
     <xsl:value-of select="@name"/>
    </xsl:when>
    <xsl:otherwise>
     <xsl:call-template name="link-or-anchor">
      <xsl:with-param name="to" select="@name"/>
      <xsl:with-param name="text" select="@name"/>
      <xsl:with-param name="link-type" select="'anchor'"/>
     </xsl:call-template>
    </xsl:otherwise>
   </xsl:choose>

   <xsl:if test="@kind='functionlike'">
    <xsl:text>(</xsl:text>
    <xsl:for-each select="macro-parameter">
     <xsl:if test="position() &gt; 1">
      <xsl:text>, </xsl:text>
     </xsl:if>
     <emphasis><xsl:value-of select="@name"/></emphasis>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
   </xsl:if>
  </xsl:template>

<!-- *********************************************************************** -->

</xsl:stylesheet>
