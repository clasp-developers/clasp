/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2011 inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux 1
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

int
hwloc_get_type_depth (struct hwloc_topology *topology, hwloc_obj_type_t type)
{
  return topology->type_depth[type];
}

hwloc_obj_type_t
hwloc_get_depth_type (hwloc_topology_t topology, unsigned depth)
{
  if (depth >= topology->nb_levels)
    switch (depth) {
    case HWLOC_TYPE_DEPTH_BRIDGE:
      return HWLOC_OBJ_BRIDGE;
    case HWLOC_TYPE_DEPTH_PCI_DEVICE:
      return HWLOC_OBJ_PCI_DEVICE;
    case HWLOC_TYPE_DEPTH_OS_DEVICE:
      return HWLOC_OBJ_OS_DEVICE;
    default:
      return (hwloc_obj_type_t) -1;
    }
  return topology->levels[depth][0]->type;
}

unsigned
hwloc_get_nbobjs_by_depth (struct hwloc_topology *topology, unsigned depth)
{
  if (depth >= topology->nb_levels)
    switch (depth) {
    case HWLOC_TYPE_DEPTH_BRIDGE:
      return topology->bridge_nbobjects;
    case HWLOC_TYPE_DEPTH_PCI_DEVICE:
      return topology->pcidev_nbobjects;
    case HWLOC_TYPE_DEPTH_OS_DEVICE:
      return topology->osdev_nbobjects;
    default:
      return 0;
    }
  return topology->level_nbobjects[depth];
}

struct hwloc_obj *
hwloc_get_obj_by_depth (struct hwloc_topology *topology, unsigned depth, unsigned idx)
{
  if (depth >= topology->nb_levels)
    switch (depth) {
    case HWLOC_TYPE_DEPTH_BRIDGE:
      return idx < topology->bridge_nbobjects ? topology->bridge_level[idx] : NULL;
    case HWLOC_TYPE_DEPTH_PCI_DEVICE:
      return idx < topology->pcidev_nbobjects ? topology->pcidev_level[idx] : NULL;
    case HWLOC_TYPE_DEPTH_OS_DEVICE:
      return idx < topology->osdev_nbobjects ? topology->osdev_level[idx] : NULL;
    default:
      return NULL;
    }
  if (idx >= topology->level_nbobjects[depth])
    return NULL;
  return topology->levels[depth][idx];
}

unsigned hwloc_get_closest_objs (struct hwloc_topology *topology, struct hwloc_obj *src, struct hwloc_obj **objs, unsigned max)
{
  struct hwloc_obj *parent, *nextparent, **src_objs;
  int i,src_nbobjects;
  unsigned stored = 0;

  if (!src->cpuset)
    return 0;

  src_nbobjects = topology->level_nbobjects[src->depth];
  src_objs = topology->levels[src->depth];

  parent = src;
  while (stored < max) {
    while (1) {
      nextparent = parent->parent;
      if (!nextparent)
	goto out;
      if (!nextparent->cpuset || !hwloc_bitmap_isequal(parent->cpuset, nextparent->cpuset))
	break;
      parent = nextparent;
    }

    if (!nextparent->cpuset)
      break;

    /* traverse src's objects and find those that are in nextparent and were not in parent */
    for(i=0; i<src_nbobjects; i++) {
      if (hwloc_bitmap_isincluded(src_objs[i]->cpuset, nextparent->cpuset)
	  && !hwloc_bitmap_isincluded(src_objs[i]->cpuset, parent->cpuset)) {
	objs[stored++] = src_objs[i];
	if (stored == max)
	  goto out;
      }
    }
    parent = nextparent;
  }

 out:
  return stored;
}

static int
hwloc__get_largest_objs_inside_cpuset (struct hwloc_obj *current, hwloc_const_bitmap_t set,
				       struct hwloc_obj ***res, int *max)
{
  int gotten = 0;
  unsigned i;

  /* the caller must ensure this */
  if (*max <= 0)
    return 0;

  if (hwloc_bitmap_isequal(current->cpuset, set)) {
    **res = current;
    (*res)++;
    (*max)--;
    return 1;
  }

  for (i=0; i<current->arity; i++) {
    hwloc_bitmap_t subset = hwloc_bitmap_dup(set);
    int ret;

    /* split out the cpuset part corresponding to this child and see if there's anything to do */
    if (current->children[i]->cpuset) {
      hwloc_bitmap_and(subset, subset, current->children[i]->cpuset);
      if (hwloc_bitmap_iszero(subset)) {
        hwloc_bitmap_free(subset);
        continue;
      }
    }

    ret = hwloc__get_largest_objs_inside_cpuset (current->children[i], subset, res, max);
    gotten += ret;
    hwloc_bitmap_free(subset);

    /* if no more room to store remaining objects, return what we got so far */
    if (!*max)
      break;
  }

  return gotten;
}

int
hwloc_get_largest_objs_inside_cpuset (struct hwloc_topology *topology, hwloc_const_bitmap_t set,
				      struct hwloc_obj **objs, int max)
{
  struct hwloc_obj *current = topology->levels[0][0];

  if (!current->cpuset || !hwloc_bitmap_isincluded(set, current->cpuset))
    return -1;

  if (max <= 0)
    return 0;

  return hwloc__get_largest_objs_inside_cpuset (current, set, &objs, &max);
}

const char *
hwloc_obj_type_string (hwloc_obj_type_t obj)
{
  switch (obj)
    {
    case HWLOC_OBJ_SYSTEM: return "System";
    case HWLOC_OBJ_MACHINE: return "Machine";
    case HWLOC_OBJ_MISC: return "Misc";
    case HWLOC_OBJ_GROUP: return "Group";
    case HWLOC_OBJ_NODE: return "NUMANode";
    case HWLOC_OBJ_SOCKET: return "Socket";
    case HWLOC_OBJ_CACHE: return "Cache";
    case HWLOC_OBJ_CORE: return "Core";
    case HWLOC_OBJ_BRIDGE: return "Bridge";
    case HWLOC_OBJ_PCI_DEVICE: return "PCIDev";
    case HWLOC_OBJ_OS_DEVICE: return "OSDev";
    case HWLOC_OBJ_PU: return "PU";
    default: return "Unknown";
    }
}

hwloc_obj_type_t
hwloc_obj_type_of_string (const char * string)
{
  if (!strcasecmp(string, "System")) return HWLOC_OBJ_SYSTEM;
  if (!strcasecmp(string, "Machine")) return HWLOC_OBJ_MACHINE;
  if (!strcasecmp(string, "Misc")) return HWLOC_OBJ_MISC;
  if (!strcasecmp(string, "Group")) return HWLOC_OBJ_GROUP;
  if (!strcasecmp(string, "NUMANode") || !strcasecmp(string, "Node")) return HWLOC_OBJ_NODE;
  if (!strcasecmp(string, "Socket")) return HWLOC_OBJ_SOCKET;
  if (!strcasecmp(string, "Cache")) return HWLOC_OBJ_CACHE;
  if (!strcasecmp(string, "Core")) return HWLOC_OBJ_CORE;
  if (!strcasecmp(string, "PU") || !strcasecmp(string, "proc") /* backward compatiliby with 0.9 */) return HWLOC_OBJ_PU;
  if (!strcasecmp(string, "Bridge")) return HWLOC_OBJ_BRIDGE;
  if (!strcasecmp(string, "PCIDev")) return HWLOC_OBJ_PCI_DEVICE;
  if (!strcasecmp(string, "OSDev")) return HWLOC_OBJ_OS_DEVICE;
  return (hwloc_obj_type_t) -1;
}

static const char *
hwloc_pci_class_string(unsigned short class_id)
{
  switch ((class_id & 0xff00) >> 8) {
    case 0x00:
      switch (class_id) {
	case 0x0001: return "VGA";
      }
      return "PCI";
    case 0x01:
      switch (class_id) {
	case 0x0100: return "SCSI";
	case 0x0101: return "IDE";
	case 0x0102: return "Flop";
	case 0x0103: return "IPI";
	case 0x0104: return "RAID";
	case 0x0105: return "ATA";
	case 0x0106: return "SATA";
	case 0x0107: return "SAS";
      }
      return "Stor";
    case 0x02:
      switch (class_id) {
	case 0x0200: return "Ether";
	case 0x0201: return "TokRn";
	case 0x0202: return "FDDI";
	case 0x0203: return "ATM";
	case 0x0204: return "ISDN";
	case 0x0205: return "WrdFip";
	case 0x0206: return "PICMG";
      }
      return "Net";
    case 0x03:
      switch (class_id) {
	case 0x0300: return "VGA";
	case 0x0301: return "XGA";
	case 0x0302: return "3D";
      }
      return "Disp";
    case 0x04:
      switch (class_id) {
	case 0x0400: return "Video";
	case 0x0401: return "Audio";
	case 0x0402: return "Phone";
	case 0x0403: return "Auddv";
      }
      return "MM";
    case 0x05:
      switch (class_id) {
	case 0x0500: return "RAM";
	case 0x0501: return "Flash";
      }
      return "Mem";
    case 0x06:
      switch (class_id) {
	case 0x0600: return "Host";
	case 0x0601: return "ISA";
	case 0x0602: return "EISA";
	case 0x0603: return "MC";
	case 0x0604: return "PCI_B";
	case 0x0605: return "PCMCIA";
	case 0x0606: return "Nubus";
	case 0x0607: return "CardBus";
	case 0x0608: return "RACEway";
	case 0x0609: return "PCI_SB";
	case 0x060a: return "IB_B";
      }
      return "Bridg";
    case 0x07:
      switch (class_id) {
	case 0x0700: return "Ser";
	case 0x0701: return "Para";
	case 0x0702: return "MSer";
	case 0x0703: return "Modm";
	case 0x0704: return "GPIB";
	case 0x0705: return "SmrtCrd";
      }
      return "Comm";
    case 0x08:
      switch (class_id) {
	case 0x0800: return "PIC";
	case 0x0801: return "DMA";
	case 0x0802: return "Time";
	case 0x0803: return "RTC";
	case 0x0804: return "HtPl";
	case 0x0805: return "SD-HtPl";
      }
      return "Syst";
    case 0x09:
      switch (class_id) {
	case 0x0900: return "Kbd";
	case 0x0901: return "Pen";
	case 0x0902: return "Mouse";
	case 0x0903: return "Scan";
	case 0x0904: return "Game";
      }
      return "In";
    case 0x0a:
      return "Dock";
    case 0x0b:
      switch (class_id) {
	case 0x0b00: return "386";
	case 0x0b01: return "486";
	case 0x0b02: return "Pent";
	case 0x0b10: return "Alpha";
	case 0x0b20: return "PPC";
	case 0x0b30: return "MIPS";
	case 0x0b40: return "CoProc";
      }
      return "Proc";
    case 0x0c:
      switch (class_id) {
	case 0x0c00: return "Firw";
	case 0x0c01: return "ACCES";
	case 0x0c02: return "SSA";
	case 0x0c03: return "USB";
	case 0x0c04: return "Fiber";
	case 0x0c05: return "SMBus";
	case 0x0c06: return "IB";
	case 0x0c07: return "IPMI";
	case 0x0c08: return "SERCOS";
	case 0x0c09: return "CANBUS";
      }
      return "Ser";
    case 0x0d:
      switch (class_id) {
	case 0x0d00: return "IRDA";
	case 0x0d01: return "IR";
	case 0x0d10: return "RF";
	case 0x0d11: return "Blueth";
	case 0x0d12: return "BroadB";
	case 0x0d20: return "802.1a";
	case 0x0d21: return "802.1b";
      }
      return "Wifi";
    case 0x0e:
      switch (class_id) {
	case 0x0e00: return "I2O";
      }
      return "Intll";
    case 0x0f:
      switch (class_id) {
	case 0x0f00: return "S-TV";
	case 0x0f01: return "S-Aud";
	case 0x0f02: return "S-Voice";
	case 0x0f03: return "S-Data";
      }
      return "Satel";
    case 0x10:
      return "Crypt";
    case 0x11:
      return "Signl";
    case 0xff:
      return "Oth";
  }
  return "PCI";
}

#define hwloc_memory_size_printf_value(_size, _verbose) \
  ((_size) < (10ULL<<20) || _verbose ? (((_size)>>9)+1)>>1 : (_size) < (10ULL<<30) ? (((_size)>>19)+1)>>1 : (((_size)>>29)+1)>>1)
#define hwloc_memory_size_printf_unit(_size, _verbose) \
  ((_size) < (10ULL<<20) || _verbose ? "KB" : (_size) < (10ULL<<30) ? "MB" : "GB")

int
hwloc_obj_type_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj, int verbose)
{
  hwloc_obj_type_t type = obj->type;
  switch (type) {
  case HWLOC_OBJ_MISC:
  case HWLOC_OBJ_SYSTEM:
  case HWLOC_OBJ_MACHINE:
  case HWLOC_OBJ_NODE:
  case HWLOC_OBJ_SOCKET:
  case HWLOC_OBJ_CORE:
  case HWLOC_OBJ_PU:
    return hwloc_snprintf(string, size, "%s", hwloc_obj_type_string(type));
  case HWLOC_OBJ_CACHE:
    return hwloc_snprintf(string, size, "L%u%s", obj->attr->cache.depth, verbose ? hwloc_obj_type_string(type): "");
  case HWLOC_OBJ_GROUP:
	  /* TODO: more pretty presentation? */
    return hwloc_snprintf(string, size, "%s%u", hwloc_obj_type_string(type), obj->attr->group.depth);
  case HWLOC_OBJ_BRIDGE:
    if (verbose)
      return snprintf(string, size, "Bridge %s->%s",
		      obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI ? "PCI" : "Host",
		      "PCI");
    else
      return snprintf(string, size, obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI ? "PCIBridge" : "HostBridge");
  case HWLOC_OBJ_PCI_DEVICE:
    return snprintf(string, size, "PCI %04x:%04x",
		    obj->attr->pcidev.vendor_id, obj->attr->pcidev.device_id);
  case HWLOC_OBJ_OS_DEVICE:
    switch (obj->attr->osdev.type) {
    case HWLOC_OBJ_OSDEV_BLOCK: return hwloc_snprintf(string, size, "Block");
    case HWLOC_OBJ_OSDEV_NETWORK: return hwloc_snprintf(string, size, "Net");
    case HWLOC_OBJ_OSDEV_OPENFABRICS: return hwloc_snprintf(string, size, "OpenFabrics");
    case HWLOC_OBJ_OSDEV_DMA: return hwloc_snprintf(string, size, "DMA");
    case HWLOC_OBJ_OSDEV_GPU: return hwloc_snprintf(string, size, "GPU");
    default:
      *string = '\0';
      return 0;
    }
    break;
  default:
    if (size > 0)
      *string = '\0';
    return 0;
  }
}

int
hwloc_obj_attr_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj, const char * separator, int verbose)
{
  const char *prefix = "";
  char *tmp = string;
  ssize_t tmplen = size;
  int ret = 0;
  int res;

  /* make sure we output at least an empty string */
  if (size)
    *string = '\0';

  /* print memory attributes */
  res = 0;
  if (verbose) {
    if (obj->memory.local_memory)
      res = hwloc_snprintf(tmp, tmplen, "%slocal=%lu%s%stotal=%lu%s",
			   prefix,
			   (unsigned long) hwloc_memory_size_printf_value(obj->memory.total_memory, verbose),
			   hwloc_memory_size_printf_unit(obj->memory.total_memory, verbose),
			   separator,
			   (unsigned long) hwloc_memory_size_printf_value(obj->memory.local_memory, verbose),
			   hwloc_memory_size_printf_unit(obj->memory.local_memory, verbose));
    else if (obj->memory.total_memory)
      res = hwloc_snprintf(tmp, tmplen, "%stotal=%lu%s",
			   prefix,
			   (unsigned long) hwloc_memory_size_printf_value(obj->memory.total_memory, verbose),
			   hwloc_memory_size_printf_unit(obj->memory.total_memory, verbose));
  } else {
    if (obj->memory.total_memory)
      res = hwloc_snprintf(tmp, tmplen, "%s%lu%s",
			   prefix,
			   (unsigned long) hwloc_memory_size_printf_value(obj->memory.total_memory, verbose),
			   hwloc_memory_size_printf_unit(obj->memory.total_memory, verbose));
  }
  if (res < 0)
    return -1;
  ret += res;
  if (ret > 0)
    prefix = separator;
  if (res >= tmplen)
    res = tmplen>0 ? tmplen - 1 : 0;
  tmp += res;
  tmplen -= res;

  /* printf type-specific attributes */
  res = 0;
  switch (obj->type) {
  case HWLOC_OBJ_CACHE:
    if (verbose) {
      char assoc[32];
      if (obj->attr->cache.associativity == -1)
	snprintf(assoc, sizeof(assoc), "%sfully-associative", separator);
      else if (obj->attr->cache.associativity == 0)
	*assoc = '\0';
      else
	snprintf(assoc, sizeof(assoc), "%sways=%d", separator, obj->attr->cache.associativity);
      res = hwloc_snprintf(tmp, tmplen, "%ssize=%lu%s%slinesize=%u%s",
			   prefix,
			   (unsigned long) hwloc_memory_size_printf_value(obj->attr->cache.size, verbose),
			   hwloc_memory_size_printf_unit(obj->attr->cache.size, verbose),
			   separator, obj->attr->cache.linesize,
			   assoc);
    } else
      res = hwloc_snprintf(tmp, tmplen, "%s%lu%s",
			   prefix,
			   (unsigned long) hwloc_memory_size_printf_value(obj->attr->cache.size, verbose),
			   hwloc_memory_size_printf_unit(obj->attr->cache.size, verbose));
    break;
  case HWLOC_OBJ_BRIDGE:
    if (verbose) {
      char up[128], down[64];
      /* upstream is PCI or HOST */
      if (obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI) {
        char linkspeed[64]= "";
        if (obj->attr->pcidev.linkspeed)
          snprintf(linkspeed, sizeof(linkspeed), "%slink=%.2fGB/s", separator, obj->attr->pcidev.linkspeed);
	snprintf(up, sizeof(up), "busid=%04x:%02x:%02x.%01x%sid=%04x:%04x%sclass=%04x(%s)%s",
		 obj->attr->pcidev.domain, obj->attr->pcidev.bus, obj->attr->pcidev.dev, obj->attr->pcidev.func, separator,
		 obj->attr->pcidev.vendor_id, obj->attr->pcidev.device_id, separator,
		 obj->attr->pcidev.class_id, hwloc_pci_class_string(obj->attr->pcidev.class_id), linkspeed);
      } else
        *up = '\0';
      /* downstream is_PCI */
      snprintf(down, sizeof(down), "buses=%04x:[%02x-%02x]",
	       obj->attr->bridge.downstream.pci.domain, obj->attr->bridge.downstream.pci.secondary_bus, obj->attr->bridge.downstream.pci.subordinate_bus);
      if (*up)
	res = snprintf(string, size, "%s%s%s", up, separator, down);
      else
	res = snprintf(string, size, "%s", down);
    }
    break;
  case HWLOC_OBJ_PCI_DEVICE:
    if (verbose) {
      char linkspeed[64]= "";
      if (obj->attr->pcidev.linkspeed)
        snprintf(linkspeed, sizeof(linkspeed), "%slink=%.2fGB/s", separator, obj->attr->pcidev.linkspeed);
      res = snprintf(string, size, "busid=%04x:%02x:%02x.%01x%sclass=%04x(%s)%s",
		     obj->attr->pcidev.domain, obj->attr->pcidev.bus, obj->attr->pcidev.dev, obj->attr->pcidev.func, separator,
		     obj->attr->pcidev.class_id, hwloc_pci_class_string(obj->attr->pcidev.class_id), linkspeed);
    }
    break;
  default:
    break;
  }
  if (res < 0)
    return -1;
  ret += res;
  if (ret > 0)
    prefix = separator;
  if (res >= tmplen)
    res = tmplen>0 ? tmplen - 1 : 0;
  tmp += res;
  tmplen -= res;

  /* printf infos */
  if (verbose) {
    unsigned i;
    for(i=0; i<obj->infos_count; i++) {
      if (strchr(obj->infos[i].value, ' '))
	res = hwloc_snprintf(tmp, tmplen, "%s%s=\"%s\"",
			     prefix,
			     obj->infos[i].name, obj->infos[i].value);
      else
	res = hwloc_snprintf(tmp, tmplen, "%s%s=%s",
			     prefix,
			     obj->infos[i].name, obj->infos[i].value);
      if (res < 0)
        return -1;
      ret += res;
      if (res >= tmplen)
        res = tmplen>0 ? tmplen - 1 : 0;
      tmp += res;
      tmplen -= res;
      if (ret > 0)
        prefix = separator;
    }
  }

  return ret;
}


int
hwloc_obj_snprintf(char *string, size_t size,
    struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *l, const char *_indexprefix, int verbose)
{
  const char *indexprefix = _indexprefix ? _indexprefix : "#";
  char os_index[12] = "";
  char type[64];
  char attr[128];
  int attrlen;

  if (l->os_index != (unsigned) -1) {
    hwloc_snprintf(os_index, 12, "%s%u", indexprefix, l->os_index);
  }

  hwloc_obj_type_snprintf(type, sizeof(type), l, verbose);
  attrlen = hwloc_obj_attr_snprintf(attr, sizeof(attr), l, " ", verbose);

  if (attrlen > 0)
    return hwloc_snprintf(string, size, "%s%s(%s)", type, os_index, attr);
  else
    return hwloc_snprintf(string, size, "%s%s", type, os_index);
}

int hwloc_obj_cpuset_snprintf(char *str, size_t size, size_t nobj, struct hwloc_obj * const *objs)
{
  hwloc_bitmap_t set = hwloc_bitmap_alloc();
  int res;
  unsigned i;

  hwloc_bitmap_zero(set);
  for(i=0; i<nobj; i++)
    if (objs[i]->cpuset)
      hwloc_bitmap_or(set, set, objs[i]->cpuset);

  res = hwloc_bitmap_snprintf(str, size, set);
  hwloc_bitmap_free(set);
  return res;
}
