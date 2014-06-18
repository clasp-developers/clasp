<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:import
    href="http://docbook.sourceforge.net/release/xsl/current/html/chunktoc.xsl"/>

  <xsl:import
    href="http://docbook.sourceforge.net/release/xsl/current/html/math.xsl"/>
  
  <xsl:import href="../../../../../tools/boostbook/xsl/chunk-common.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/docbook-layout.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/navbar.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/admon.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/xref.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/relative-href.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/callout.xsl"/>
  <xsl:import href="../../../../../tools/boostbook/xsl/html-base.xsl"/>
  
  <xsl:param name = "boost.root" select = "'../../../..'"/>
  <xsl:param name = "callout.graphics" select = "'0'"/>

  <xsl:param name   = "callout.graphics.path"
             select = "concat($boost.root, '/doc/html/images/')"/>

  <xsl:param name = "html.stylesheet" select = "'../style/style.css'"/>

  <xsl:param name = "chunk.fast" select = "1"/>
  <xsl:param name = "chunk.separate.lots" select = "1"/>
  <xsl:param name = "chunk.toc" select = "btl-toc.xml"/>
  <xsl:param name = "manual.toc" select = "btl-toc.xml"/>

  <xsl:param name = "use.id.as.filename" select = "1"/>

  <xsl:param name = "chapter.autolabel" select = "0"/>
  <xsl:param name = "section.autolabel" select = "0"/>

  <xsl:param name = "variablelist.as.table" select = "1"/>
  <xsl:param name = "variablelist.term.break.after" select = "1"/>

  <xsl:param name = "runinhead.default.title.end.punct">:<br/></xsl:param>

  <xsl:param name = "generate.toc">
  book      toc,title
  chapter   toc,title
  part      toc,title
  section   toc,title
  qandaset  toc
  </xsl:param>

  <xsl:param name="generate.section.toc.level" select="2"/>

  <xsl:template match="itemizedlist">
    <div>
      <xsl:apply-templates select="." mode="class.attribute"/>
      <xsl:call-template name="anchor"/>
      <xsl:if test="title">
        <xsl:call-template name="formal.object.heading"/>
      </xsl:if>

      <!-- Preserve order of PIs and comments -->
      <xsl:apply-templates
          select="*[not(self::listitem
                  or self::title
                  or self::titleabbrev)]
                |comment()[not(preceding-sibling::listitem)]
                |processing-instruction()[not(preceding-sibling::listitem)]"/>

      <ul>
        <xsl:if test="@role">
          <xsl:attribute name="class">
            <xsl:value-of select="@role"/>
          </xsl:attribute>
        </xsl:if>

        <xsl:if test="$css.decoration != 0">
          <xsl:attribute name="type">
            <xsl:call-template name="list.itemsymbol"/>
          </xsl:attribute>
        </xsl:if>

        <xsl:if test="@spacing='compact'">
          <xsl:attribute name="compact">
            <xsl:value-of select="@spacing"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:apply-templates
              select="listitem
                    |comment()[preceding-sibling::listitem]
                    |processing-instruction()[preceding-sibling::listitem]"/>
      </ul>
    </div>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "seglistitem">
   <xsl:call-template name = "anchor"/>
   <table class="seglistitem">
     <xsl:apply-templates/>
   </table>
   <br/>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "seg">
   <xsl:variable name = "segnum" select = "count(preceding-sibling::seg)+1"/>
   <xsl:variable name = "seglist" select = "ancestor::segmentedlist"/>
   <xsl:variable name = "segtitles" select = "$seglist/segtitle"/>

   <tr class="seg">
    <td>
     <strong><nobr>
      <span class="segtitle">
        <xsl:apply-templates select = "$segtitles[$segnum=position()]" mode = "segtitle-in-seg"/>
      </span>
     </nobr></strong>
    </td>
    <td>
     <xsl:text>: </xsl:text>
    </td>
    <td>
     <xsl:apply-templates/>
    </td>
   </tr>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "userinput"><span class="userinput">&lt;<xsl:apply-templates/>&gt;</span></xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "toggle">
   <xsl:variable name = "toggle-id" select = "generate-id()"/>

   <a href="#" target="_top" id="{$toggle-id}" >
    <xsl:attribute name = "onclick">toggle_element( '<xsl:value-of select = "@linkend"/>', '<xsl:value-of select = "$toggle-id"/>', '<xsl:value-of select = "@on-label"/>', '<xsl:value-of select = "@off-label"/>' ); return false;</xsl:attribute>
    <xsl:value-of select = "@on-label"/>
   </a>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template name = "user.head.content">
   <xsl:param name = "node" select = "."/>
   <script language="JavaScript1.2">
    <xsl:attribute name = "src">
     <xsl:call-template name = "href.target.relative">
      <xsl:with-param name = "target" select = "'../js/boost-test.js'"/>
     </xsl:call-template>
    </xsl:attribute>
   </script>

  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "*" mode = "class.attribute">
    <xsl:param name = "class" select = "local-name(.)"/>

    <xsl:choose>

     <xsl:when test="@html-class">
      <xsl:attribute name = "class">
       <xsl:value-of select = "@html-class"/>
      </xsl:attribute>
     </xsl:when>

     <xsl:otherwise>
      <xsl:attribute name = "class">
       <xsl:value-of select = "$class"/>
      </xsl:attribute>
     </xsl:otherwise>

    </xsl:choose>

    <!-- TO FIX: is there a better way? -->
    <xsl:if test="@html-id">
     <xsl:attribute name = "id">
      <xsl:value-of select = "@html-id"/>
     </xsl:attribute>
    </xsl:if>

  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "inline-reference/refentry" mode = "index">
   <xsl:variable name = "curr_entry_var">
     <xsl:value-of select = "ancestor::inline-reference/@curr_entry_var"/>
   </xsl:variable>

   <xsl:variable name = "targ_id">
     <xsl:value-of select = "../@id"/>.<xsl:value-of select = "@name"/>
   </xsl:variable>

   <li><a href="#" onclick="{$curr_entry_var} = select_form_page( '{$targ_id}', {$curr_entry_var} ); return false;"><xsl:value-of select = "@name"/></a></li>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "inline-reference/refentry" mode = "entry">
   <xsl:variable name = "targ_id">
     <xsl:value-of select = "../@id"/>.<xsl:value-of select = "@name"/>
   </xsl:variable>

   <div class="entry" id="{$targ_id}">
    <xsl:apply-templates select = "*"/>
   </div>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "inline-reference/refentry/seealso">
   <span class="inline-ref-see-also"><xsl:text>See also: </xsl:text></span>
   <xsl:apply-templates select = "node()|text()"/>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "inline-reference//ref">
   <xsl:variable name = "curr_entry_var">
     <xsl:value-of select = "ancestor::inline-reference/@curr_entry_var"/>
   </xsl:variable>

   <xsl:variable name = "targ">
     <xsl:value-of select = "."/>
   </xsl:variable>

   <xsl:variable name = "targ_id">
     <xsl:value-of select = "ancestor::inline-reference/@id"/>.<xsl:value-of select = "$targ"/>
   </xsl:variable>

   <a href="#" onclick="{$curr_entry_var} = select_form_page( '{$targ_id}', {$curr_entry_var} ); return false;"><xsl:value-of select = "$targ"/></a>

  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template match = "inline-reference">
   <script language="JavaScript1.2">
     var <xsl:value-of select = "@curr_entry_var"/>;
   </script>

   <table>
    <xsl:apply-templates select = "." mode = "class.attribute"/>
    <tr>
     <td class="index" valign="top">
      <ul>
       <xsl:apply-templates select ="refentry" mode = "index"/>
      </ul>
     </td>

     <td class="content" valign="top">
      <xsl:apply-templates select ="refentry" mode = "entry"/>
     </td>
    </tr>
   </table>
  </xsl:template>

<!-- *********************************************************************** -->

  <xsl:template name = "navig.link">
   <xsl:param name = "targ"/>
   <xsl:param name = "direction"/>
   <xsl:param name = "accesskey"/>
   <xsl:param name = "context" select = "."/>
   
   <xsl:if test = "count($targ)>0">
    <xsl:variable name = "href-target">
     <xsl:call-template name = "href.target">
      <xsl:with-param name = "object" select = "$targ"/>
      <xsl:with-param name = "context" select = "$context"/>     
     </xsl:call-template>
    </xsl:variable>

    <a>
     <xsl:if test="$accesskey">
      <xsl:attribute name = "accesskey" select = "$acceskey"/>
     </xsl:if>
     <xsl:attribute name = "href">
      <!--xsl:message>
0000 <xsl:value-of select = "$href-target"/>
      </xsl:message-->

     <xsl:value-of select = "$href-target"/>
     </xsl:attribute>

     <xsl:call-template name = "navig.content">
      <xsl:with-param name = "direction" select = "$direction"/>
     </xsl:call-template>
    </a>
   </xsl:if>
  </xsl:template>

 <!-- *********************************************************************** -->

 <xsl:template match = "*" mode = "navig.location-path">
  <xsl:param name = "home"/>
  <xsl:param name = "next"/>
  <xsl:param name = "context" select = "."/>
  <xsl:param name = "leaf" select = "1"/>

  <xsl:variable name = "node" select = "."/>

  <xsl:choose>
   <xsl:when test="($node) and ($node != $home)">
    <xsl:apply-templates select = "parent::*" mode="navig.location-path">
     <xsl:with-param name = "home" select = "$home"/>
     <xsl:with-param name = "next" select = "$next"/>
     <xsl:with-param name = "context" select = "$context"/>
     <xsl:with-param name = "leaf" select = "0"/>
    </xsl:apply-templates>

    <xsl:variable name = "text">
     <xsl:choose>
      <xsl:when test="$node/titleabbrev">
       <xsl:value-of select = "$node/titleabbrev"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select = "$node/title"/>
      </xsl:otherwise>
     </xsl:choose>
    </xsl:variable>

    <xsl:choose>
     <xsl:when test="$leaf">
      <b><xsl:value-of select = "$text"/></b>
     </xsl:when>
     <xsl:otherwise>
      <xsl:call-template name = "navig.link">
      <xsl:with-param name = "direction" select = "$text"/>
      <xsl:with-param name = "targ"      select = "$node"/>
      <xsl:with-param name = "context"   select = "$context"/>     
     </xsl:call-template>
    </xsl:otherwise>
    </xsl:choose>
   
    <xsl:variable name = "next-sibling" select = "following-sibling::*[1]" />

    <xsl:choose>
     <xsl:when test = "$next-sibling and ($leaf = 0 or $next-sibling != $next)">
      <a>
       <xsl:attribute name = "href">
        <xsl:call-template name = "href.target">
         <xsl:with-param name = "object" select = "$next-sibling"/>
         <xsl:with-param name = "context" select = "$context"/>     
        </xsl:call-template>
       </xsl:attribute>
      &gt;
      </a>
     </xsl:when>
     <xsl:when test = "$leaf = 0">
      <xsl:text> &gt; </xsl:text>
     </xsl:when>
    </xsl:choose>

   </xsl:when>
   <xsl:when test="$leaf = 0"><xsl:text> &gt; </xsl:text></xsl:when>
  </xsl:choose>
 </xsl:template>

 <!-- *********************************************************************** -->

  <xsl:template name = "header.navigation">
   <xsl:param name = "prev" select = "/foo"/>
   <xsl:param name = "next" select = "/foo"/>
   <xsl:param name = "nav.context"/>

   <xsl:variable name = "home" select = "/*[1]"/>
   <xsl:variable name = "up"   select = "parent::*"/>
   <xsl:variable name = "boost.test.image.src" select = "concat($boost.root, '/libs/test/doc/img/boost.test.logo.png')"/>

   <table width = "100%">
    <tr>
     <td width="10%">
      <a>
       <xsl:attribute name = "href">
        <xsl:call-template name = "href.target">
         <xsl:with-param name = "object" select = "$home"/>
        </xsl:call-template>
       </xsl:attribute>

       <img alt="Home" width="229" height="61" border="0">
        <xsl:attribute name = "src">
         <xsl:call-template name = "href.target.relative">
          <xsl:with-param name = "target" select = "$boost.test.image.src"/>
         </xsl:call-template>
        </xsl:attribute>
       </img>
      </a>
     </td>
     <td valign = "middle" align="left">
      <xsl:apply-templates select = "." mode="navig.location-path">
       <xsl:with-param name = "home" select = "$home"/>
       <xsl:with-param name = "next" select = "$next"/>
      </xsl:apply-templates>
     </td>
     <td>
      <div class = "spirit-nav">
       <xsl:call-template name = "navig.link">
        <xsl:with-param name = "direction" select = "'prev'"/>
        <xsl:with-param name = "targ"      select = "$prev"/>
        <xsl:with-param name = "accesskey" select = "p"/>
       </xsl:call-template>
       <xsl:call-template name = "navig.link">
        <xsl:with-param name = "direction" select = "'next'"/>
        <xsl:with-param name = "targ"      select = "$next"/>
        <xsl:with-param name = "accesskey" select = "n"/>
       </xsl:call-template>
      </div>
     </td>
    </tr>
   </table>
   <hr/>
  </xsl:template>

<!-- *********************************************************************** -->

</xsl:stylesheet>
