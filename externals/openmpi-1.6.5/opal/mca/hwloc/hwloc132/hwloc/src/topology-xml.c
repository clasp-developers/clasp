/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2011 inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux 1
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

#include <assert.h>
#include <strings.h>

#ifdef HWLOC_HAVE_LIBXML2
#include <libxml/parser.h>
#include <libxml/tree.h>
#endif

static int
hwloc__xml_verbose(void)
{
  static int first = 1;
  static int verbose = 0;
  if (first) {
    char *env = getenv("HWLOC_XML_VERBOSE");
    if (env)
      verbose = atoi(env);
    first = 0;
  }
  return verbose;
}

#ifdef HWLOC_HAVE_LIBXML2
static void hwloc_libxml2_error_callback(void * ctx __hwloc_attribute_unused, const char * msg __hwloc_attribute_unused, ...) { /* do nothing */ }

static void
hwloc_libxml2_disable_stderrwarnings(void)
{
  static int first = 1;
  if (first) {
    xmlSetGenericErrorFunc(NULL, hwloc__xml_verbose() ? xmlGenericError : hwloc_libxml2_error_callback);
    first = 0;
  }
}
#endif

/***********************************
 ******** Backend Init/Exit ********
 ***********************************/

/* this can be the first XML call */
int
hwloc_backend_xml_init(struct hwloc_topology *topology, const char *xmlpath, const char *xmlbuffer, int xmlbuflen)
{
#ifdef HWLOC_HAVE_LIBXML2
  char *env = getenv("HWLOC_NO_LIBXML_IMPORT");
  if (!env || !atoi(env)) {
    xmlDoc *doc = NULL;

    LIBXML_TEST_VERSION;
    hwloc_libxml2_disable_stderrwarnings();

    errno = 0; /* set to 0 so that we know if libxml2 changed it */

    if (xmlpath)
      doc = xmlReadFile(xmlpath, NULL, 0);
    else if (xmlbuffer)
      doc = xmlReadMemory(xmlbuffer, xmlbuflen, "", NULL, 0);

    if (!doc) {
      if (!errno)
	/* libxml2 read the file fine, but it got an error during parsing */
      errno = EINVAL;
      return -1;
    }

    topology->backend_params.xml.buffer = NULL;
    topology->backend_params.xml.doc = doc;
  } else
#endif /* HWLOC_HAVE_LIBXML2 */
  if (xmlbuffer) {
    topology->backend_params.xml.buffer = malloc(xmlbuflen);
    memcpy(topology->backend_params.xml.buffer, xmlbuffer, xmlbuflen);
  } else {
    FILE * file;
    size_t buflen = 4096, offset, readlen;
    char *buffer = malloc(buflen+1);
    size_t ret;

    if (!strcmp(xmlpath, "-"))
      xmlpath = "/dev/stdin";

    file = fopen(xmlpath, "r");
    if (!file)
      return -1;

    offset = 0; readlen = buflen;
    while (1) {
      ret = fread(buffer+offset, 1, readlen, file);

      offset += ret;
      buffer[offset] = 0;

      if (ret != readlen)
        break;

      buflen *= 2;
      buffer = realloc(buffer, buflen+1);
      readlen = buflen/2;
    }

    fclose(file);

    topology->backend_params.xml.buffer = buffer;
    buflen = offset+1;
  }

  topology->is_thissystem = 0;
  assert(topology->backend_type == HWLOC_BACKEND_NONE);
  topology->backend_type = HWLOC_BACKEND_XML;

  return 0;
}

/* this canNOT be the first XML call */
void
hwloc_backend_xml_exit(struct hwloc_topology *topology)
{
#ifdef HWLOC_HAVE_LIBXML2
  char *env = getenv("HWLOC_NO_LIBXML_IMPORT");
  if (!env || !atoi(env)) {
    xmlFreeDoc((xmlDoc*)topology->backend_params.xml.doc);
  } else
#endif
  {
    assert(topology->backend_params.xml.buffer);
    free(topology->backend_params.xml.buffer);
  }
  assert(topology->backend_type == HWLOC_BACKEND_XML);
  topology->backend_type = HWLOC_BACKEND_NONE;
}

/************************************************
 ********* XML import (common routines) *********
 ************************************************/

static void
hwloc__xml_import_object_attr(struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *obj,
			      const char *name, const char *value)
{
  if (!strcmp(name, "type")) {
    /* already handled */
    return;
  }

  else if (!strcmp(name, "os_level"))
    obj->os_level = strtoul(value, NULL, 10);
  else if (!strcmp(name, "os_index"))
    obj->os_index = strtoul(value, NULL, 10);
  else if (!strcmp(name, "cpuset")) {
    obj->cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->cpuset, value);
  } else if (!strcmp(name, "complete_cpuset")) {
    obj->complete_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->complete_cpuset,value);
  } else if (!strcmp(name, "online_cpuset")) {
    obj->online_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->online_cpuset, value);
  } else if (!strcmp(name, "allowed_cpuset")) {
    obj->allowed_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->allowed_cpuset, value);
  } else if (!strcmp(name, "nodeset")) {
    obj->nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->nodeset, value);
  } else if (!strcmp(name, "complete_nodeset")) {
    obj->complete_nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->complete_nodeset, value);
  } else if (!strcmp(name, "allowed_nodeset")) {
    obj->allowed_nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_sscanf(obj->allowed_nodeset, value);
  } else if (!strcmp(name, "name"))
    obj->name = strdup(value);

  else if (!strcmp(name, "cache_size")) {
    unsigned long long lvalue = strtoull(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.size = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_size attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "cache_linesize")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.linesize = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_linesize attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "cache_associativity")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    if (obj->type == HWLOC_OBJ_CACHE)
      obj->attr->cache.associativity = lvalue;
    else if (hwloc__xml_verbose())
      fprintf(stderr, "ignoring cache_associativity attribute for non-cache object type\n");
  }

  else if (!strcmp(name, "local_memory"))
    obj->memory.local_memory = strtoull(value, NULL, 10);

  else if (!strcmp(name, "depth")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_CACHE:
	obj->attr->cache.depth = lvalue;
	break;
      case HWLOC_OBJ_GROUP:
	obj->attr->group.depth = lvalue;
	break;
      case HWLOC_OBJ_BRIDGE:
	obj->attr->bridge.depth = lvalue;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring depth attribute for object type without depth\n");
	break;
    }
  }

  else if (!strcmp(name, "pci_busid")) {
    switch (obj->type) {
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_BRIDGE: {
      unsigned domain, bus, dev, func;
      if (sscanf(value, "%04x:%02x:%02x.%01x",
		 &domain, &bus, &dev, &func) != 4) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid pci_busid format string %s\n", value);
      } else {
	obj->attr->pcidev.domain = domain;
	obj->attr->pcidev.bus = bus;
	obj->attr->pcidev.dev = dev;
	obj->attr->pcidev.func = func;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring pci_busid attribute for non-PCI object\n");
      break;
    }
  }

  else if (!strcmp(name, "pci_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_BRIDGE: {
      unsigned classid, vendor, device, subvendor, subdevice, revision;
      if (sscanf(value, "%04x [%04x:%04x] [%04x:%04x] %02x",
		 &classid, &vendor, &device, &subvendor, &subdevice, &revision) != 6) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid pci_type format string %s\n", value);
      } else {
	obj->attr->pcidev.class_id = classid;
	obj->attr->pcidev.vendor_id = vendor;
	obj->attr->pcidev.device_id = device;
	obj->attr->pcidev.subvendor_id = subvendor;
	obj->attr->pcidev.subdevice_id = subdevice;
	obj->attr->pcidev.revision = revision;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring pci_type attribute for non-PCI object\n");
      break;
    }
  }

  else if (!strcmp(name, "pci_link_speed")) {
    switch (obj->type) {
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_BRIDGE: {
      obj->attr->pcidev.linkspeed = atof(value);
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring pci_link_speed attribute for non-PCI object\n");
      break;
    }
  }

  else if (!strcmp(name, "bridge_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_BRIDGE: {
      unsigned upstream_type, downstream_type;
      if (sscanf(value, "%u-%u", &upstream_type, &downstream_type) != 2) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid bridge_type format string %s\n", value);
      } else {
	obj->attr->bridge.upstream_type = upstream_type;
	obj->attr->bridge.downstream_type = downstream_type;
      };
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring bridge_type attribute for non-bridge object\n");
      break;
    }
  }

  else if (!strcmp(name, "bridge_pci")) {
    switch (obj->type) {
    case HWLOC_OBJ_BRIDGE: {
      unsigned domain, secbus, subbus;
      if (sscanf(value, "%04x:[%02x-%02x]",
		 &domain, &secbus, &subbus) != 3) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid bridge_pci format string %s\n", value);
      } else {
	obj->attr->bridge.downstream.pci.domain = domain;
	obj->attr->bridge.downstream.pci.secondary_bus = secbus;
	obj->attr->bridge.downstream.pci.subordinate_bus = subbus;
      }
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring bridge_pci attribute for non-bridge object\n");
      break;
    }
  }

  else if (!strcmp(name, "osdev_type")) {
    switch (obj->type) {
    case HWLOC_OBJ_OS_DEVICE: {
      unsigned osdev_type;
      if (sscanf(value, "%u", &osdev_type) != 1) {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring invalid osdev_type format string %s\n", value);
      } else
	obj->attr->osdev.type = osdev_type;
      break;
    }
    default:
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring osdev_type attribute for non-osdev object\n");
      break;
    }
  }




  /*************************
   * deprecated (from 1.0)
   */
  else if (!strcmp(name, "dmi_board_vendor")) {
    hwloc_obj_add_info(obj, "DMIBoardVendor", strdup(value));
  }
  else if (!strcmp(name, "dmi_board_name")) {
    hwloc_obj_add_info(obj, "DMIBoardName", strdup(value));
  }

  /*************************
   * deprecated (from 0.9)
   */
  else if (!strcmp(name, "memory_kB")) {
    unsigned long long lvalue = strtoull(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_CACHE:
	obj->attr->cache.size = lvalue << 10;
	break;
      case HWLOC_OBJ_NODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	obj->memory.local_memory = lvalue << 10;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring memory_kB attribute for object type without memory\n");
	break;
    }
  }
  else if (!strcmp(name, "huge_page_size_kB")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_NODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	if (!obj->memory.page_types) {
	  obj->memory.page_types = malloc(sizeof(*obj->memory.page_types));
	  obj->memory.page_types_len = 1;
	}
	obj->memory.page_types[0].size = lvalue << 10;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring huge_page_size_kB attribute for object type without huge pages\n");
	break;
    }
  }
  else if (!strcmp(name, "huge_page_free")) {
    unsigned long lvalue = strtoul(value, NULL, 10);
    switch (obj->type) {
      case HWLOC_OBJ_NODE:
      case HWLOC_OBJ_MACHINE:
      case HWLOC_OBJ_SYSTEM:
	if (!obj->memory.page_types) {
	  obj->memory.page_types = malloc(sizeof(*obj->memory.page_types));
	  obj->memory.page_types_len = 1;
	}
	obj->memory.page_types[0].count = lvalue;
	break;
      default:
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring huge_page_free attribute for object type without huge pages\n");
	break;
    }
  }
  /*
   * end of deprecated (from 0.9)
   *******************************/



  else if (hwloc__xml_verbose())
    fprintf(stderr, "ignoring unknown object attribute %s\n", name);
}


/* NO-libxml helper: skip spaces until the next interesting char */
static char *
hwloc__nolibxml_import_ignore_spaces(char *buffer)
{
  return buffer + strspn(buffer, " \t\n");
}

typedef struct hwloc__xml_import_state_s {
  struct hwloc__xml_import_state_s *parent;
  int use_libxml;

  /* only useful if not using libxml */
  char *tagbuffer; /* buffer containing the next tag */
  char *attrbuffer; /* buffer containing the next attribute of the current node */
  char *tagname; /* tag name of the current node */
  int closed; /* set if the current node is auto-closing */

  /* only useful if using libxml */
#ifdef HWLOC_HAVE_LIBXML2
  xmlNode *libxml_node; /* current libxml node, always valid */
  xmlNode *libxml_child; /* last processed child, or NULL if none yet */
  xmlAttr *libxml_attr; /* last processed attribute, or NULL if none yet */
#endif
} * hwloc__xml_import_state_t;

static int
hwloc__xml_import_next_attr(hwloc__xml_import_state_t state, char **namep, char **valuep)
{
  if (state->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    xmlAttr *attr;
    if (state->libxml_attr)
      attr = state->libxml_attr->next;
    else
      attr = state->libxml_node->properties;
    for (; attr; attr = attr->next)
      if (attr->type == XML_ATTRIBUTE_NODE) {
	/* use the first valid attribute content */
	xmlNode *subnode;
	for (subnode = attr->children; subnode; subnode = subnode->next) {
	  if (subnode->type == XML_TEXT_NODE) {
	    if (subnode->content && subnode->content[0] != '\0' && subnode->content[0] != '\n') {
	      *namep = (char *) attr->name;
	      *valuep = (char *) subnode->content;
	      state->libxml_attr = attr;
	      return 0;
	    }
	  } else {
	    if (hwloc__xml_verbose())
	      fprintf(stderr, "ignoring unexpected xml attr node type %u\n", subnode->type);
	  }
	}
      } else {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring unexpected xml attr type %u\n", attr->type);
      }
#else
    assert(0);
#endif
    return -1;
  } else {
    int namelen;
    size_t len, escaped;
    char *buffer, *value, *end;

    /* find the beginning of an attribute */
    buffer = hwloc__nolibxml_import_ignore_spaces(state->attrbuffer);
    namelen = strspn(buffer, "abcdefghijklmnopqrstuvwxyz_");
    if (buffer[namelen] != '=' || buffer[namelen+1] != '\"')
      return -1;
    buffer[namelen] = '\0';
    *namep = buffer;

    /* find the beginning of its value, and unescape it */
    *valuep = value = buffer+namelen+2;
    len = 0; escaped = 0;
    while (value[len+escaped] != '\"') {
      if (value[len+escaped] == '&') {
	if (!strcmp(&value[1+len+escaped], "#10;")) {
	  escaped += 4;
	  value[1+len] = '\n';
	} else if (!strcmp(&value[1+len+escaped], "#13;")) {
	  escaped += 4;
	  value[1+len] = '\r';
	} else if (!strcmp(&value[1+len+escaped], "#9;")) {
	  escaped += 3;
	  value[1+len] = '\t';
	} else if (!strcmp(&value[1+len+escaped], "quot;")) {
	  escaped += 5;
	  value[1+len] = '\"';
	} else if (!strcmp(&value[1+len+escaped], "lt;")) {
	  escaped += 3;
	  value[1+len] = '<';
	} else if (!strcmp(&value[1+len+escaped], "gt;")) {
	  escaped += 3;
	  value[1+len] = '>';
	} else if (!strcmp(&value[1+len+escaped], "amp;")) {
	  escaped += 4;
	  value[1+len] = '&';
	} else {
	  return -1;
	}
	value[len] = value[len+escaped];
      }
      len++;
      if (value[len+escaped] == '\0')
	return -1;
    }
    value[len] = '\0';

    /* find next attribute */
    end = &value[len+escaped+1]; /* skip the ending " */
    state->attrbuffer = hwloc__nolibxml_import_ignore_spaces(end);
    return 0;
  }
}

static int
hwloc__xml_import_find_child(hwloc__xml_import_state_t state,
			     hwloc__xml_import_state_t childstate,
			     char **tagp)
{
  childstate->parent = state;
  childstate->use_libxml = state->use_libxml;

  if (state->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    xmlNode *child;
    if (!state->libxml_child)
      return 0;
    child = state->libxml_child->next;
    for (; child; child = child->next)
      if (child->type == XML_ELEMENT_NODE) {
	state->libxml_child = childstate->libxml_node = child;
	childstate->libxml_child = child->children;
	childstate->libxml_attr = NULL;
	*tagp = (char*) child->name;
	return 1;
      } else if (child->type == XML_TEXT_NODE) {
	if (child->content && child->content[0] != '\0' && child->content[0] != '\n')
	  if (hwloc__xml_verbose())
	    fprintf(stderr, "ignoring object text content %s\n", (const char*) child->content);
      } else {
	if (hwloc__xml_verbose())
	  fprintf(stderr, "ignoring unexpected xml node type %u\n", child->type);
      }
#else
    assert(0);
#endif
    return 0;
  } else {
    char *buffer = state->tagbuffer;
    char *end;
    int namelen;

    /* auto-closed tags have no children */
    if (state->closed)
      return 0;

    /* find the beginning of the tag */
    buffer = hwloc__nolibxml_import_ignore_spaces(buffer);
    if (buffer[0] != '<')
      return -1;
    buffer++;

    /* if closing tag, return nothing and do not advance */
    if (buffer[0] == '/')
      return 0;

    /* normal tag */
    *tagp = childstate->tagname = buffer;

    /* find the end, mark it and return it */
    end = strchr(buffer, '>');
    if (!end)
      return -1;
    end[0] = '\0';
    childstate->tagbuffer = end+1;

    /* handle auto-closing tags */
    if (end[-1] == '/') {
      childstate->closed = 1;
      end[-1] = '\0';
    } else
      childstate->closed = 0;

    /* find attributes */
    namelen = strspn(buffer, "abcdefghijklmnopqrstuvwxyz_");
    /* cannot be without attributes */
    assert(buffer[namelen] != '\0');

    if (buffer[namelen] != ' ')
      return -1;

    /* found a space, likely starting attributes */
    buffer[namelen] = '\0';
    childstate->attrbuffer = buffer+namelen+1;
    return 1;
  }
}

/* look for an explicit closing tag </name> */
static int
hwloc__xml_import_close_tag(hwloc__xml_import_state_t state)
{
  if (state->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    /* nothing */
#else
    assert(0);
#endif
    return 0;
  } else {
    char *buffer = state->tagbuffer;
    char *end;

    /* auto-closed tags need nothing */
    if (state->closed)
      return 0;

    /* find the beginning of the tag */
    buffer = hwloc__nolibxml_import_ignore_spaces(buffer);
    if (buffer[0] != '<')
      return -1;
    buffer++;

    /* find the end, mark it and return it to the parent */
    end = strchr(buffer, '>');
    if (!end)
      return -1;
    end[0] = '\0';
    state->tagbuffer = end+1;

    /* if closing tag, return nothing */
    if (buffer[0] != '/' || strcmp(buffer+1, state->tagname) )
      return -1;
    return 0;
  }
}

static void
hwloc__xml_import_close_child(hwloc__xml_import_state_t state)
{
  if (state->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    /* nothing */
#else
    assert(0);
#endif
  } else {
    state->parent->tagbuffer = state->tagbuffer;
  }
}

static int
hwloc__xml_import_info(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
		       hwloc__xml_import_state_t state)
{
  char *infoname = NULL;
  char *infovalue = NULL;

  while (1) {
    char *attrname, *attrvalue;
    if (hwloc__xml_import_next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "name"))
      infoname = attrvalue;
    else if (!strcmp(attrname, "value"))
      infovalue = attrvalue;
    else
      return -1;
  }

  if (infoname)
    /* empty strings are ignored by libxml */
    hwloc_obj_add_info(obj, infoname, infovalue ? infovalue : "");

  return hwloc__xml_import_close_tag(state);
}

static int
hwloc__xml_import_pagetype(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
			   hwloc__xml_import_state_t state)
{
  uint64_t size = 0, count = 0;

  while (1) {
    char *attrname, *attrvalue;
    if (hwloc__xml_import_next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "size"))
      size = strtoull(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "count"))
      count = strtoull(attrvalue, NULL, 10);
    else
      return -1;
  }

  if (size) {
    int idx = obj->memory.page_types_len;
    obj->memory.page_types = realloc(obj->memory.page_types, (idx+1)*sizeof(*obj->memory.page_types));
    obj->memory.page_types_len = idx+1;
    obj->memory.page_types[idx].size = size;
    obj->memory.page_types[idx].count = count;
  }

  return hwloc__xml_import_close_tag(state);
}

static int
hwloc__xml_import_distances(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_obj_t obj,
			    hwloc__xml_import_state_t state)
{
  unsigned long reldepth = 0, nbobjs = 0;
  float latbase = 0;
  char *tag;
  int ret;

  while (1) {
    char *attrname, *attrvalue;
    if (hwloc__xml_import_next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "nbobjs"))
      nbobjs = strtoul(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "relative_depth"))
      reldepth = strtoul(attrvalue, NULL, 10);
    else if (!strcmp(attrname, "latency_base"))
      latbase = (float) atof(attrvalue);
    else
      return -1;
  }

  if (nbobjs && reldepth && latbase) {
    int idx = obj->distances_count;
    unsigned i;
    float *matrix, latmax = 0;

    obj->distances = realloc(obj->distances, (idx+1)*sizeof(*obj->distances));
    obj->distances_count = idx+1;
    obj->distances[idx] = malloc(sizeof(**obj->distances));
    obj->distances[idx]->relative_depth = reldepth;
    obj->distances[idx]->nbobjs = nbobjs;
    obj->distances[idx]->latency = matrix = malloc(nbobjs*nbobjs*sizeof(float));
    obj->distances[idx]->latency_base = latbase;

    for(i=0; i<nbobjs*nbobjs; i++) {
      struct hwloc__xml_import_state_s childstate;
      char *attrname, *attrvalue;
      float val;

      ret = hwloc__xml_import_find_child(state, &childstate, &tag);
      if (ret <= 0 || strcmp(tag, "latency")) {
	/* a latency child is needed */
	free(obj->distances[idx]->latency);
	free(obj->distances[idx]);
	obj->distances_count--;
	return -1;
      }

      ret = hwloc__xml_import_next_attr(&childstate, &attrname, &attrvalue);
      if (ret < 0 || strcmp(attrname, "value")) {
	free(obj->distances[idx]->latency);
	free(obj->distances[idx]);
	obj->distances_count--;
	return -1;
      }

      val = (float) atof((char *) attrvalue);
      matrix[i] = val;
      if (val > latmax)
	latmax = val;

      ret = hwloc__xml_import_close_tag(&childstate);
      if (ret < 0)
	return -1;

      hwloc__xml_import_close_child(&childstate);
    }

    obj->distances[idx]->latency_max = latmax;
  }

  return hwloc__xml_import_close_tag(state);
}

static int
hwloc__xml_import_object(hwloc_topology_t topology, hwloc_obj_t obj,
			 hwloc__xml_import_state_t state)
{
  /* process attributes */
  while (1) {
    char *attrname, *attrvalue;
    if (hwloc__xml_import_next_attr(state, &attrname, &attrvalue) < 0)
      break;
    if (!strcmp(attrname, "type")) {
      obj->type = hwloc_obj_type_of_string(attrvalue);
      if (obj->type == (hwloc_obj_type_t)-1)
        return -1;
    } else {
      /* type needed first */
      if (obj->type == (hwloc_obj_type_t)-1)
        return -1;
      hwloc__xml_import_object_attr(topology, obj, attrname, attrvalue);
    }
  }

  /* process subnodes */
  while (1) {
    struct hwloc__xml_import_state_s childstate;
    char *tag;
    int ret;

    ret = hwloc__xml_import_find_child(state, &childstate, &tag);
    if (ret < 0)
      return -1;
    if (!ret)
      break;

    if (!strcmp(tag, "object")) {
      hwloc_obj_t childobj = hwloc_alloc_setup_object(HWLOC_OBJ_TYPE_MAX, -1);
      hwloc_insert_object_by_parent(topology, obj, childobj);
      ret = hwloc__xml_import_object(topology, childobj, &childstate);
    } else if (!strcmp(tag, "page_type")) {
      ret = hwloc__xml_import_pagetype(topology, obj, &childstate);
    } else if (!strcmp(tag, "info")) {
      ret = hwloc__xml_import_info(topology, obj, &childstate);
    } else if (!strcmp(tag, "distances")) {
      ret = hwloc__xml_import_distances(topology, obj, &childstate);
    } else
      ret = -1;

    if (ret < 0)
      return ret;

    hwloc__xml_import_close_child(&childstate);
  }

  return hwloc__xml_import_close_tag(state);
}

/***********************************
 ********* main XML import *********
 ***********************************/

/* this canNOT be the first XML call */
int
hwloc_look_xml(struct hwloc_topology *topology)
{
  struct hwloc__xml_import_state_s state, childstate;
  char *tag;
#ifdef HWLOC_HAVE_LIBXML2
  char *env;
#endif
  int ret;

  state.use_libxml = 0;
  state.parent = NULL;

#ifdef HWLOC_HAVE_LIBXML2
  env = getenv("HWLOC_NO_LIBXML_IMPORT");
  if (!env || !atoi(env)) {
    xmlNode* root_node;
    xmlDtd *dtd;

    state.use_libxml = 1;

    dtd = xmlGetIntSubset((xmlDoc*) topology->backend_params.xml.doc);
    if (!dtd) {
      if (hwloc__xml_verbose())
	fprintf(stderr, "Loading XML topology without DTD\n");
    } else if (strcmp((char *) dtd->SystemID, "hwloc.dtd")) {
      if (hwloc__xml_verbose())
	fprintf(stderr, "Loading XML topology with wrong DTD SystemID (%s instead of %s)\n",
		(char *) dtd->SystemID, "hwloc.dtd");
    }

    root_node = xmlDocGetRootElement((xmlDoc*) topology->backend_params.xml.doc);

    if (strcmp((const char *) root_node->name, "topology") && strcmp((const char *) root_node->name, "root")) {
      /* root node should be in "topology" class (or "root" if importing from < 1.0) */
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring object of class `%s' not at the top the xml hierarchy\n", (const char *) root_node->name);
      goto failed;
    }

    state.libxml_node = root_node;
    state.libxml_child = root_node->children;
    state.libxml_attr = NULL;
  } else
#endif /* HWLOC_HAVE_LIBXML2 */
  {
    char *buffer = topology->backend_params.xml.buffer;

    /* skip headers */
    while (!strncmp(buffer, "<?xml ", 6) || !strncmp(buffer, "<!DOCTYPE ", 10)) {
      buffer = strchr(buffer, '\n');
      if (!buffer)
	goto failed;
      buffer++;
    }

    /* find topology tag */
    if (strncmp(buffer, "<topology>", 10))
	goto failed;

    /* prepare parsing state */
    state.tagbuffer = buffer+10;
    state.tagname = "topology";
    state.attrbuffer = NULL;
  }

  state.closed = 0;

  /* find root object tag and import it */
  ret = hwloc__xml_import_find_child(&state, &childstate, &tag);
  if (ret < 0 || !ret || strcmp(tag, "object"))
    goto failed;
  ret = hwloc__xml_import_object(topology, topology->levels[0][0], &childstate);
  if (ret < 0)
    goto failed;
  hwloc__xml_import_close_child(&childstate);

  /* find end of topology tag */
  hwloc__xml_import_close_tag(&state);

  /* keep the "Backend" information intact */
  /* we could add "BackendSource=XML" to notify that XML was used between the actual backend and here */

  topology->support.discovery->pu = 1;

  return 0;

 failed:
  if (state.use_libxml)
    /* not only when verbose */
    fprintf(stderr, "Failed to parse XML input with the minimalistic parser. If it was not\n"
	    "generated by hwloc, try enabling full XML support with libxml2.\n");
  return -1;
}

static void
hwloc_xml__check_distances(struct hwloc_topology *topology, hwloc_obj_t obj)
{
  hwloc_obj_t child;
  unsigned i=0;
  while (i<obj->distances_count) {
    unsigned depth = obj->depth + obj->distances[i]->relative_depth;
    unsigned nbobjs = hwloc_get_nbobjs_inside_cpuset_by_depth(topology, obj->cpuset, depth);
    if (nbobjs != obj->distances[i]->nbobjs) {
      if (hwloc__xml_verbose())
	fprintf(stderr, "ignoring invalid distance matrix with %u objs instead of %u\n",
		obj->distances[i]->nbobjs, nbobjs);
      hwloc_free_logical_distances(obj->distances[i]);
      memmove(&obj->distances[i], &obj->distances[i+1], (obj->distances_count-i-1)*sizeof(*obj->distances));
      obj->distances_count--;
    } else
      i++;
  }

  child = obj->first_child;
  while (child != NULL) {
    hwloc_xml__check_distances(topology, child);
    child = child->next_sibling;
  }
}

/* this canNOT be the first XML call */
void
hwloc_xml_check_distances(struct hwloc_topology *topology)
{
  /* now that the topology tree has been properly setup,
   * check that our distance matrice sizes make sense */
  hwloc_xml__check_distances(topology, topology->levels[0][0]);
}

/************************************************
 ********* XML export (common routines) *********
 ************************************************/

typedef struct hwloc__xml_export_output_s {
  int use_libxml;

  /* only useful if not using libxml */
  char *buffer; /* (moving) buffer where to write */
  size_t written; /* how many bytes were written (or would have be written if not truncated) */
  size_t remaining; /* how many bytes are still available in the buffer */
  unsigned indent; /* indentation level for the next line */

  /* only useful if not using libxml */
#ifdef HWLOC_HAVE_LIBXML2
  xmlNodePtr current_node; /* current node to output */
#endif
} * hwloc__xml_export_output_t;

/* NO-libxml helper: update output buffer */
static void
hwloc__xml_export_update_buffer(hwloc__xml_export_output_t output, int res)
{
  if (res >= 0) {
    output->written += res;
    if (res >= (int) output->remaining)
      res = output->remaining>0 ? output->remaining-1 : 0;
    output->buffer += res;
    output->remaining -= res;
  }
}

static void
hwloc__xml_export_new_child(hwloc__xml_export_output_t output, const char *name)
{
  if (output->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    output->current_node = xmlNewChild(output->current_node, NULL, BAD_CAST name, NULL);
#else
    assert(0);
#endif
  } else {
    int res = hwloc_snprintf(output->buffer, output->remaining, "%*s<%s", output->indent, "", name);
    hwloc__xml_export_update_buffer(output, res);
    output->indent += 2;
  }
}

/* NO-libxml helper: escape string */
static char *
hwloc__xml_export_escape_string(const char *src)
{
  int fulllen, sublen;
  char *escaped, *dst;

  fulllen = strlen(src);

  sublen = strcspn(src, "\n\r\t\"<>&");
  if (sublen == fulllen)
    return NULL; /* nothing to escape */

  escaped = malloc(fulllen*6+1); /* escaped chars are replaced by at most 6 char */
  dst = escaped;

  memcpy(dst, src, sublen);
  src += sublen;
  dst += sublen;

  while (*src) {
    int replen;
    switch (*src) {
    case '\n': strcpy(dst, "&#10;");  replen=5; break;
    case '\r': strcpy(dst, "&#13;");  replen=5; break;
    case '\t': strcpy(dst, "&#9;");   replen=4; break;
    case '\"': strcpy(dst, "&quot;"); replen=6; break;
    case '<':  strcpy(dst, "&lt;");   replen=4; break;
    case '>':  strcpy(dst, "&gt;");   replen=4; break;
    case '&':  strcpy(dst, "&amp;");  replen=5; break;
    default: replen=0; break;
    }
    dst+=replen; src++;

    sublen = strcspn(src, "\n\r\t\"<>&");
    memcpy(dst, src, sublen);
    src += sublen;
    dst += sublen;
  }

  *dst = 0;
  return escaped;
}

static void
hwloc__xml_export_new_prop(hwloc__xml_export_output_t output, const char *name, const char *value)
{
  if (output->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    xmlNewProp(output->current_node, BAD_CAST name, BAD_CAST value);
#else
    assert(0);
#endif
  } else {
    char *escaped = hwloc__xml_export_escape_string(value);
    int res = hwloc_snprintf(output->buffer, output->remaining, " %s=\"%s\"", name, escaped ? escaped : value);
    hwloc__xml_export_update_buffer(output, res);
    free(escaped);
  }
}

static void
hwloc__xml_export_end_props(hwloc__xml_export_output_t output, unsigned nr_children)
{
  if (output->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    /* nothing to do */
#else
    assert(0);
#endif
  } else {
    int res = hwloc_snprintf(output->buffer, output->remaining, nr_children ? ">\n" : "/>\n");
    hwloc__xml_export_update_buffer(output, res);
  }
}

static void
hwloc__xml_export_end_child(hwloc__xml_export_output_t output, const char *name, unsigned nr_children)
{
  if (output->use_libxml) {
#ifdef HWLOC_HAVE_LIBXML2
    output->current_node = output->current_node->parent;
#else
    assert(0);
#endif
  } else {
    int res;
    output->indent -= 2;
    if (nr_children) {
      res = hwloc_snprintf(output->buffer, output->remaining, "%*s</%s>\n", output->indent, "", name);
      hwloc__xml_export_update_buffer(output, res);
    }
  }
}

static void
hwloc__xml_export_object (hwloc__xml_export_output_t output, hwloc_topology_t topology, hwloc_obj_t obj)
{
  char *cpuset = NULL;
  char tmp[255];
  unsigned nr_children = obj->memory.page_types_len + obj->infos_count + obj->distances_count + obj->arity;
  unsigned i;

  hwloc__xml_export_new_child(output, "object");
  hwloc__xml_export_new_prop(output, "type", hwloc_obj_type_string(obj->type));
  sprintf(tmp, "%d", obj->os_level);
  hwloc__xml_export_new_prop(output, "os_level", tmp);
  if (obj->os_index != (unsigned) -1) {
    sprintf(tmp, "%u", obj->os_index);
    hwloc__xml_export_new_prop(output, "os_index", tmp);
  }
  if (obj->cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->cpuset);
    hwloc__xml_export_new_prop(output, "cpuset", cpuset);
    free(cpuset);
  }
  if (obj->complete_cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->complete_cpuset);
    hwloc__xml_export_new_prop(output, "complete_cpuset", cpuset);
    free(cpuset);
  }
  if (obj->online_cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->online_cpuset);
    hwloc__xml_export_new_prop(output, "online_cpuset", cpuset);
    free(cpuset);
  }
  if (obj->allowed_cpuset) {
    hwloc_bitmap_asprintf(&cpuset, obj->allowed_cpuset);
    hwloc__xml_export_new_prop(output, "allowed_cpuset", cpuset);
    free(cpuset);
  }
  if (obj->nodeset && !hwloc_bitmap_isfull(obj->nodeset)) {
    hwloc_bitmap_asprintf(&cpuset, obj->nodeset);
    hwloc__xml_export_new_prop(output, "nodeset", cpuset);
    free(cpuset);
  }
  if (obj->complete_nodeset && !hwloc_bitmap_isfull(obj->complete_nodeset)) {
    hwloc_bitmap_asprintf(&cpuset, obj->complete_nodeset);
    hwloc__xml_export_new_prop(output, "complete_nodeset", cpuset);
    free(cpuset);
  }
  if (obj->allowed_nodeset && !hwloc_bitmap_isfull(obj->allowed_nodeset)) {
    hwloc_bitmap_asprintf(&cpuset, obj->allowed_nodeset);
    hwloc__xml_export_new_prop(output, "allowed_nodeset", cpuset);
    free(cpuset);
  }

  if (obj->name)
    hwloc__xml_export_new_prop(output, "name", obj->name);

  switch (obj->type) {
  case HWLOC_OBJ_CACHE:
    sprintf(tmp, "%llu", (unsigned long long) obj->attr->cache.size);
    hwloc__xml_export_new_prop(output, "cache_size", tmp);
    sprintf(tmp, "%u", obj->attr->cache.depth);
    hwloc__xml_export_new_prop(output, "depth", tmp);
    sprintf(tmp, "%u", (unsigned) obj->attr->cache.linesize);
    hwloc__xml_export_new_prop(output, "cache_linesize", tmp);
    sprintf(tmp, "%d", (unsigned) obj->attr->cache.associativity);
    hwloc__xml_export_new_prop(output, "cache_associativity", tmp);
    break;
  case HWLOC_OBJ_GROUP:
    sprintf(tmp, "%u", obj->attr->group.depth);
    hwloc__xml_export_new_prop(output, "depth", tmp);
    break;
  case HWLOC_OBJ_BRIDGE:
    sprintf(tmp, "%u-%u", obj->attr->bridge.upstream_type, obj->attr->bridge.downstream_type);
    hwloc__xml_export_new_prop(output, "bridge_type", tmp);
    sprintf(tmp, "%u", obj->attr->bridge.depth);
    hwloc__xml_export_new_prop(output, "depth", tmp);
    if (obj->attr->bridge.downstream_type == HWLOC_OBJ_BRIDGE_PCI) {
      sprintf(tmp, "%04x:[%02x-%02x]",
	      (unsigned) obj->attr->bridge.downstream.pci.domain,
	      (unsigned) obj->attr->bridge.downstream.pci.secondary_bus,
	      (unsigned) obj->attr->bridge.downstream.pci.subordinate_bus);
      hwloc__xml_export_new_prop(output, "bridge_pci", tmp);
    }
    if (obj->attr->bridge.upstream_type != HWLOC_OBJ_BRIDGE_PCI)
      break;
    /* fallthrough */
  case HWLOC_OBJ_PCI_DEVICE:
    sprintf(tmp, "%04x:%02x:%02x.%01x",
	    (unsigned) obj->attr->pcidev.domain,
	    (unsigned) obj->attr->pcidev.bus,
	    (unsigned) obj->attr->pcidev.dev,
	    (unsigned) obj->attr->pcidev.func);
    hwloc__xml_export_new_prop(output, "pci_busid", tmp);
    sprintf(tmp, "%04x [%04x:%04x] [%04x:%04x] %02x",
	    (unsigned) obj->attr->pcidev.class_id,
	    (unsigned) obj->attr->pcidev.vendor_id, (unsigned) obj->attr->pcidev.device_id,
	    (unsigned) obj->attr->pcidev.subvendor_id, (unsigned) obj->attr->pcidev.subdevice_id,
	    (unsigned) obj->attr->pcidev.revision);
    hwloc__xml_export_new_prop(output, "pci_type", tmp);
    sprintf(tmp, "%f", obj->attr->pcidev.linkspeed);
    hwloc__xml_export_new_prop(output, "pci_link_speed", tmp);
    break;
  case HWLOC_OBJ_OS_DEVICE:
    sprintf(tmp, "%u", obj->attr->osdev.type);
    hwloc__xml_export_new_prop(output, "osdev_type", tmp);
    break;
  default:
    break;
  }

  if (obj->memory.local_memory) {
    sprintf(tmp, "%llu", (unsigned long long) obj->memory.local_memory);
    hwloc__xml_export_new_prop(output, "local_memory", tmp);
  }

  hwloc__xml_export_end_props(output, nr_children);

  for(i=0; i<obj->memory.page_types_len; i++) {
    hwloc__xml_export_new_child(output, "page_type");
    sprintf(tmp, "%llu", (unsigned long long) obj->memory.page_types[i].size);
    hwloc__xml_export_new_prop(output, "size", tmp);
    sprintf(tmp, "%llu", (unsigned long long) obj->memory.page_types[i].count);
    hwloc__xml_export_new_prop(output, "count", tmp);
    hwloc__xml_export_end_props(output, 0);
    hwloc__xml_export_end_child(output, "page_type", 0);
  }

  for(i=0; i<obj->infos_count; i++) {
    hwloc__xml_export_new_child(output, "info");
    hwloc__xml_export_new_prop(output, "name", obj->infos[i].name);
    hwloc__xml_export_new_prop(output, "value", obj->infos[i].value);
    hwloc__xml_export_end_props(output, 0);
    hwloc__xml_export_end_child(output, "info", 0);
  }

  for(i=0; i<obj->distances_count; i++) {
    unsigned nbobjs = obj->distances[i]->nbobjs;
    unsigned j;
    hwloc__xml_export_new_child(output, "distances");
    sprintf(tmp, "%u", nbobjs);
    hwloc__xml_export_new_prop(output, "nbobjs", tmp);
    sprintf(tmp, "%u", obj->distances[i]->relative_depth);
    hwloc__xml_export_new_prop(output, "relative_depth", tmp);
    sprintf(tmp, "%f", obj->distances[i]->latency_base);
    hwloc__xml_export_new_prop(output, "latency_base", tmp);
    hwloc__xml_export_end_props(output, nbobjs*nbobjs);
    for(j=0; j<nbobjs*nbobjs; j++) {
      hwloc__xml_export_new_child(output, "latency");
      sprintf(tmp, "%f", obj->distances[i]->latency[j]);
      hwloc__xml_export_new_prop(output, "value", tmp);
      hwloc__xml_export_end_props(output, 0);
      hwloc__xml_export_end_child(output, "latency", 0);
    }
    hwloc__xml_export_end_child(output, "distances", nbobjs*nbobjs);
  }

  if (obj->arity) {
    unsigned x;
    for (x=0; x<obj->arity; x++)
      hwloc__xml_export_object (output, topology, obj->children[x]);
  }

  hwloc__xml_export_end_child(output, "object", nr_children);
}

/************************************************
 ********* XML export (libxml2 routines) ********
 ************************************************/

#ifdef HWLOC_HAVE_LIBXML2
/* libxml2 specific export preparation */
static xmlDocPtr
hwloc__libxml2_prepare_export(hwloc_topology_t topology)
{
  struct hwloc__xml_export_output_s output;
  xmlDocPtr doc = NULL;       /* document pointer */
  xmlNodePtr root_node = NULL; /* root pointer */
  xmlDtdPtr dtd = NULL;       /* DTD pointer */

  LIBXML_TEST_VERSION;
  hwloc_libxml2_disable_stderrwarnings();

  /* Creates a new document, a node and set it as a root node. */
  doc = xmlNewDoc(BAD_CAST "1.0");
  root_node = xmlNewNode(NULL, BAD_CAST "topology");
  xmlDocSetRootElement(doc, root_node);

  /* Creates a DTD declaration. Isn't mandatory. */
  dtd = xmlCreateIntSubset(doc, BAD_CAST "topology", NULL, BAD_CAST "hwloc.dtd");

  output.use_libxml = 1;
  output.current_node = root_node;
  hwloc__xml_export_object (&output, topology, hwloc_get_root_obj(topology));

  return doc;
}
#endif /* HWLOC_HAVE_LIBXML2 */

/***************************************************
 ********* XML export (NO-libxml2 routines) ********
 ***************************************************/

static size_t
hwloc___nolibxml_prepare_export(hwloc_topology_t topology, char *xmlbuffer, int buflen)
{
  struct hwloc__xml_export_output_s output;
  int res;

  output.use_libxml = 0;
  output.indent = 0;
  output.written = 0;
  output.buffer = xmlbuffer;
  output.remaining = buflen;

  res = hwloc_snprintf(output.buffer, output.remaining,
		 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		 "<!DOCTYPE topology SYSTEM \"hwloc.dtd\">\n");
  hwloc__xml_export_update_buffer(&output, res);
  hwloc__xml_export_new_child(&output, "topology");
  hwloc__xml_export_end_props(&output, 1);
  hwloc__xml_export_object (&output, topology, hwloc_get_root_obj(topology));
  hwloc__xml_export_end_child(&output, "topology", 1);

  return output.written+1;
}

static void
hwloc__nolibxml_prepare_export(hwloc_topology_t topology, char **bufferp, int *buflenp)
{
  char *buffer;
  size_t bufferlen, res;

  bufferlen = 16384; /* random guess for large enough default */
  buffer = malloc(bufferlen);
  res = hwloc___nolibxml_prepare_export(topology, buffer, bufferlen);

  if (res > bufferlen) {
    buffer = realloc(buffer, res);
    hwloc___nolibxml_prepare_export(topology, buffer, res);
  }

  *bufferp = buffer;
  *buflenp = res;
}

/**********************************
 ********* main XML export ********
 **********************************/

/* this can be the first XML call */
int hwloc_topology_export_xml(hwloc_topology_t topology, const char *filename)
{
#ifdef HWLOC_HAVE_LIBXML2
  char *env = getenv("HWLOC_NO_LIBXML_EXPORT");
  if (!env || !atoi(env)) {
    xmlDocPtr doc;
    int ret;

    errno = 0; /* set to 0 so that we know if libxml2 changed it */

    doc = hwloc__libxml2_prepare_export(topology);
    ret = xmlSaveFormatFileEnc(filename, doc, "UTF-8", 1);
    xmlFreeDoc(doc);

    if (ret < 0) {
      if (!errno)
	/* libxml2 likely got an error before doing I/O */
	errno = EINVAL;
      return -1;
    }
  } else
#endif
  {
    FILE *file;
    char *buffer;
    int bufferlen;

    if (!strcmp(filename, "-")) {
      file = stdout;
    } else {
      file = fopen(filename, "w");
      if (!file)
        return -1;
    }

    hwloc__nolibxml_prepare_export(topology, &buffer, &bufferlen);
    fwrite(buffer, bufferlen-1 /* don't write the ending \0 */, 1, file);
    free(buffer);

    if (file != stdout)
      fclose(file);
  }

  return 0;
}

/* this can be the first XML call */
int hwloc_topology_export_xmlbuffer(hwloc_topology_t topology, char **xmlbuffer, int *buflen)
{
#ifdef HWLOC_HAVE_LIBXML2
  char *env = getenv("HWLOC_NO_LIBXML_EXPORT");
  if (!env || !atoi(env)) {
    xmlDocPtr doc = hwloc__libxml2_prepare_export(topology);
    xmlDocDumpFormatMemoryEnc(doc, (xmlChar **)xmlbuffer, buflen, "UTF-8", 1);
    xmlFreeDoc(doc);
  } else
#endif
  {
    hwloc__nolibxml_prepare_export(topology, xmlbuffer, buflen);
  }
  return 0;
}

void hwloc_free_xmlbuffer(hwloc_topology_t topology __hwloc_attribute_unused, char *xmlbuffer)
{
#ifdef HWLOC_HAVE_LIBXML2
  char *env = getenv("HWLOC_NO_LIBXML_EXPORT");
  if (!env || !atoi(env)) {
    xmlFree(BAD_CAST xmlbuffer);
  } else
#endif
  {
    free(xmlbuffer);
  }
}
