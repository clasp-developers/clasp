/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2011 inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/helper.h>
#include <private/private.h>
#include <private/debug.h>
#include <private/misc.h>

#ifdef HWLOC_HAVE_LIBPCI

#include <pci/pci.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <setjmp.h>
#ifdef HWLOC_LINUX_SYS
#include <hwloc/linux.h>
#include <dirent.h>
#include <sys/types.h>
#endif

#define CONFIG_SPACE_CACHESIZE 256

static void
hwloc_pci_traverse_print_cb(struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *pcidev, int depth __hwloc_attribute_unused)
{
  char busid[14];
  snprintf(busid, sizeof(busid), "%04x:%02x:%02x.%01x",
           pcidev->attr->pcidev.domain, pcidev->attr->pcidev.bus, pcidev->attr->pcidev.dev, pcidev->attr->pcidev.func);

  if (pcidev->type == HWLOC_OBJ_BRIDGE) {
    if (pcidev->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_HOST)
      hwloc_debug("%*s HostBridge", depth, "");
    else
      hwloc_debug("%*s %s Bridge [%04x:%04x]", depth, "", busid,
		  pcidev->attr->pcidev.vendor_id, pcidev->attr->pcidev.device_id);
    hwloc_debug(" to %04x:[%02x:%02x]\n",
		pcidev->attr->bridge.downstream.pci.domain, pcidev->attr->bridge.downstream.pci.secondary_bus, pcidev->attr->bridge.downstream.pci.subordinate_bus);
  } else
    hwloc_debug("%*s %s Device [%04x:%04x (%04x:%04x) rev=%02x class=%04x]\n", depth, "", busid,
		pcidev->attr->pcidev.vendor_id, pcidev->attr->pcidev.device_id,
		pcidev->attr->pcidev.subvendor_id, pcidev->attr->pcidev.subdevice_id,
		pcidev->attr->pcidev.revision, pcidev->attr->pcidev.class_id);
}

static void
hwloc_pci_traverse_setbridgedepth_cb(struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *pcidev, int depth)
{
  if (pcidev->type == HWLOC_OBJ_BRIDGE)
    pcidev->attr->bridge.depth = depth;
}

#ifdef HWLOC_LINUX_SYS
static hwloc_obj_t
hwloc_linux_add_os_device(struct hwloc_topology *topology, struct hwloc_obj *pcidev, hwloc_obj_osdev_type_t type, const char *name)
{
  struct hwloc_obj *obj = hwloc_alloc_setup_object(HWLOC_OBJ_OS_DEVICE, -1);
  obj->name = strdup(name);
  obj->logical_index = -1;
  obj->attr->osdev.type = type;

  hwloc_insert_object_by_parent(topology, pcidev, obj);

  return obj;
}

typedef void (*hwloc_linux_class_fillinfos_t)(struct hwloc_topology *topology, struct hwloc_obj *osdev, const char *osdevpath);

/* look for objects of the given class below a sysfs directory */
static void
hwloc_linux_class_readdir(struct hwloc_topology *topology, struct hwloc_obj *pcidev, const char *devicepath,
			  hwloc_obj_osdev_type_t type, const char *classname,
			  hwloc_linux_class_fillinfos_t fillinfo)
{
  size_t classnamelen = strlen(classname);
  char path[256];
  DIR *dir;
  struct dirent *dirent;
  hwloc_obj_t obj;

  snprintf(path, sizeof(path), "%s/%s", devicepath, classname);
  dir = opendir(path);
  if (dir) {
    /* modern sysfs: <device>/<class>/<name> */
    while ((dirent = readdir(dir)) != NULL) {
      if (!strcmp(dirent->d_name, ".") || !strcmp(dirent->d_name, ".."))
	continue;
      obj = hwloc_linux_add_os_device(topology, pcidev, type, dirent->d_name);
      if (fillinfo) {
	snprintf(path, sizeof(path), "%s/%s/%s", devicepath, classname, dirent->d_name);
	fillinfo(topology, obj, path);
      }
    }
    closedir(dir);
  } else {
    /* deprecated sysfs: <device>/<class>:<name> */
    dir = opendir(devicepath);
    if (dir) {
      while ((dirent = readdir(dir)) != NULL) {
	if (strncmp(dirent->d_name, classname, classnamelen) || dirent->d_name[classnamelen] != ':')
	  continue;
	obj = hwloc_linux_add_os_device(topology, pcidev, type, dirent->d_name + classnamelen+1);
	if (fillinfo) {
	  snprintf(path, sizeof(path), "%s/%s", devicepath, dirent->d_name);
	  fillinfo(topology, obj, path);
	}
      }
      closedir(dir);
    }
  }
}

/* class objects that are immediately below pci devices */
static void
hwloc_linux_net_class_fillinfos(struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *obj, const char *osdevpath)
{
  FILE *fd;
  char path[256];
  snprintf(path, sizeof(path), "%s/address", osdevpath);
  fd = fopen(path, "r");
  if (fd) {
    char address[128];
    if (fgets(address, sizeof(address), fd)) {
      char *eol = strchr(address, '\n');
      if (eol)
        *eol = 0;
      hwloc_obj_add_info(obj, "Address", address);
    }
    fclose(fd);
  }
}
static void
hwloc_linux_lookup_net_class(struct hwloc_topology *topology, struct hwloc_obj *pcidev, const char *pcidevpath)
{
  hwloc_linux_class_readdir(topology, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_NETWORK, "net", hwloc_linux_net_class_fillinfos);
}
static void
hwloc_linux_lookup_openfabrics_class(struct hwloc_topology *topology, struct hwloc_obj *pcidev, const char *pcidevpath)
{
  hwloc_linux_class_readdir(topology, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_OPENFABRICS, "infiniband", NULL);
}
static void
hwloc_linux_lookup_dma_class(struct hwloc_topology *topology, struct hwloc_obj *pcidev, const char *pcidevpath)
{
  hwloc_linux_class_readdir(topology, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_DMA, "dma", NULL);
}
static void
hwloc_linux_lookup_drm_class(struct hwloc_topology *topology, struct hwloc_obj *pcidev, const char *pcidevpath)
{
  hwloc_linux_class_readdir(topology, pcidev, pcidevpath, HWLOC_OBJ_OSDEV_GPU, "drm", NULL);

  /* we could look at the "graphics" class too, but it doesn't help for proprietary drivers either */

  /* GPU devices (even with a proprietary driver) seem to have a boot_vga field in their PCI device directory (since 2.6.30),
   * so we could create a OS device for each PCI devices with such a field.
   * boot_vga is actually created when class >> 8 == VGA (it contains 1 for boot vga device), so it's trivial anyway.
   */
}

/* block class objects are in
 * host%d/target%d:%d:%d/%d:%d:%d:%d/
 * or
 * host%d/port-%d:%d/end_device-%d:%d/target%d:%d:%d/%d:%d:%d:%d/
 * or
 * ide%d/%d.%d/
 * below pci devices */
static void
hwloc_linux_lookup_host_block_class(struct hwloc_topology *topology, struct hwloc_obj *pcidev, char *path, size_t pathlen)
{
  DIR *hostdir, *portdir, *targetdir;
  struct dirent *hostdirent, *portdirent, *targetdirent;
  int dummy;
  hostdir = opendir(path);
  if (!hostdir)
    return;
  while ((hostdirent = readdir(hostdir)) != NULL) {
    if (sscanf(hostdirent->d_name, "port-%d:%d", &dummy, &dummy) == 2)
    {
      /* found host%d/port-%d:%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], hostdirent->d_name);
      pathlen += 1+strlen(hostdirent->d_name);
      portdir = opendir(path);
      if (!portdir)
	continue;
      while ((portdirent = readdir(portdir)) != NULL) {
	if (sscanf(portdirent->d_name, "end_device-%d:%d", &dummy, &dummy) == 2) {
	  /* found host%d/port-%d:%d/end_device-%d:%d */
	  path[pathlen] = '/';
	  strcpy(&path[pathlen+1], portdirent->d_name);
	  pathlen += 1+strlen(portdirent->d_name);
	  hwloc_linux_lookup_host_block_class(topology, pcidev, path, pathlen);
	  pathlen -= 1+strlen(portdirent->d_name);
	  path[pathlen] = '\0';
	}
      }
      closedir(portdir);
      /* restore parent path */
      pathlen -= 1+strlen(hostdirent->d_name);
      path[pathlen] = '\0';
      continue;
    } else if (sscanf(hostdirent->d_name, "target%d:%d:%d", &dummy, &dummy, &dummy) == 3) {
      /* found host%d/target%d:%d:%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], hostdirent->d_name);
      pathlen += 1+strlen(hostdirent->d_name);
      targetdir = opendir(path);
      if (!targetdir)
	continue;
      while ((targetdirent = readdir(targetdir)) != NULL) {
	if (sscanf(targetdirent->d_name, "%d:%d:%d:%d", &dummy, &dummy, &dummy, &dummy) != 4)
	  continue;
	/* found host%d/target%d:%d:%d/%d:%d:%d:%d */
	path[pathlen] = '/';
	strcpy(&path[pathlen+1], targetdirent->d_name);
	pathlen += 1+strlen(targetdirent->d_name);
	/* lookup block class for real */
	hwloc_linux_class_readdir(topology, pcidev, path, HWLOC_OBJ_OSDEV_BLOCK, "block", NULL);
	/* restore parent path */
	pathlen -= 1+strlen(targetdirent->d_name);
	path[pathlen] = '\0';
      }
      closedir(targetdir);
      /* restore parent path */
      pathlen -= 1+strlen(hostdirent->d_name);
      path[pathlen] = '\0';
    }
  }
  closedir(hostdir);
}

static void
hwloc_linux_lookup_block_class(struct hwloc_topology *topology, struct hwloc_obj *pcidev, const char *pcidevpath)
{
  size_t pathlen;
  DIR *devicedir, *hostdir;
  struct dirent *devicedirent, *hostdirent;
  char path[256];
  int dummy;

  strcpy(path, pcidevpath);
  pathlen = strlen(path);

  devicedir = opendir(pcidevpath);
  if (!devicedir)
    return;
  while ((devicedirent = readdir(devicedir)) != NULL) {
    if (sscanf(devicedirent->d_name, "ide%d", &dummy) == 1) {
      /* found ide%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], devicedirent->d_name);
      pathlen += 1+strlen(devicedirent->d_name);
      hostdir = opendir(path);
      if (!hostdir)
	continue;
      while ((hostdirent = readdir(hostdir)) != NULL) {
	if (sscanf(hostdirent->d_name, "%d.%d", &dummy, &dummy) == 2) {
	  /* found ide%d/%d.%d */
	  path[pathlen] = '/';
	  strcpy(&path[pathlen+1], hostdirent->d_name);
	  pathlen += 1+strlen(hostdirent->d_name);
	  /* lookup block class for real */
	  hwloc_linux_class_readdir(topology, pcidev, path, HWLOC_OBJ_OSDEV_BLOCK, "block", NULL);
	  /* restore parent path */
	  pathlen -= 1+strlen(hostdirent->d_name);
	  path[pathlen] = '\0';
	}
      }
    } else if (sscanf(devicedirent->d_name, "host%d", &dummy) == 1) {
      /* found host%d */
      path[pathlen] = '/';
      strcpy(&path[pathlen+1], devicedirent->d_name);
      pathlen += 1+strlen(devicedirent->d_name);
      hwloc_linux_lookup_host_block_class(topology, pcidev, path, pathlen);
      /* restore parent path */
      pathlen -= 1+strlen(devicedirent->d_name);
      path[pathlen] = '\0';
    }
  }
  closedir(devicedir);
}
#endif /* HWLOC_LINUX_SYS */

static void
hwloc_pci_traverse_lookuposdevices_cb(struct hwloc_topology *topology, struct hwloc_obj *pcidev, int depth __hwloc_attribute_unused)
{
  if (pcidev->type == HWLOC_OBJ_BRIDGE)
    return;

#ifdef HWLOC_LINUX_SYS
  {
  char pcidevpath[256];
  snprintf(pcidevpath, sizeof(pcidevpath), "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/",
	   pcidev->attr->pcidev.domain, pcidev->attr->pcidev.bus,
	   pcidev->attr->pcidev.dev, pcidev->attr->pcidev.func);

  hwloc_linux_lookup_net_class(topology, pcidev, pcidevpath);
  hwloc_linux_lookup_openfabrics_class(topology, pcidev, pcidevpath);
  hwloc_linux_lookup_dma_class(topology, pcidev, pcidevpath);
  hwloc_linux_lookup_drm_class(topology, pcidev, pcidevpath);
  hwloc_linux_lookup_block_class(topology, pcidev, pcidevpath);
  }
#endif /* HWLOC_LINUX_SYS */
}

static void
hwloc_pci__traverse(struct hwloc_topology *topology, struct hwloc_obj *root,
		    void (*cb)(struct hwloc_topology *, struct hwloc_obj *, int depth),
		    int depth)
{
  struct hwloc_obj *child = root->first_child;
  while (child) {
    cb(topology, child, depth);
    if (child->type == HWLOC_OBJ_BRIDGE)
      hwloc_pci__traverse(topology, child, cb, depth+1);
    child = child->next_sibling;
  }
}

static void
hwloc_pci_traverse(struct hwloc_topology *topology, struct hwloc_obj *root,
		   void (*cb)(struct hwloc_topology *, struct hwloc_obj *, int depth))
{
  hwloc_pci__traverse(topology, root, cb, 0);
}

enum hwloc_pci_busid_comparison_e {
  HWLOC_PCI_BUSID_LOWER,
  HWLOC_PCI_BUSID_HIGHER,
  HWLOC_PCI_BUSID_INCLUDED,
  HWLOC_PCI_BUSID_SUPERSET
};

static int
hwloc_pci_compare_busids(struct hwloc_obj *a, struct hwloc_obj *b)
{
  if (a->type == HWLOC_OBJ_BRIDGE)
    assert(a->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI);
  if (b->type == HWLOC_OBJ_BRIDGE)
    assert(b->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI);

  if (a->attr->pcidev.domain < b->attr->pcidev.domain)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.domain > b->attr->pcidev.domain)
    return HWLOC_PCI_BUSID_HIGHER;

  if (a->type == HWLOC_OBJ_BRIDGE
      && b->attr->pcidev.bus >= a->attr->bridge.downstream.pci.secondary_bus
      && b->attr->pcidev.bus <= a->attr->bridge.downstream.pci.subordinate_bus)
    return HWLOC_PCI_BUSID_SUPERSET;
  if (b->type == HWLOC_OBJ_BRIDGE
      && a->attr->pcidev.bus >= b->attr->bridge.downstream.pci.secondary_bus
      && a->attr->pcidev.bus <= b->attr->bridge.downstream.pci.subordinate_bus)
    return HWLOC_PCI_BUSID_INCLUDED;

  if (a->attr->pcidev.bus < b->attr->pcidev.bus)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.bus > b->attr->pcidev.bus)
    return HWLOC_PCI_BUSID_HIGHER;

  if (a->attr->pcidev.dev < b->attr->pcidev.dev)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.dev > b->attr->pcidev.dev)
    return HWLOC_PCI_BUSID_HIGHER;

  if (a->attr->pcidev.func < b->attr->pcidev.func)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.func > b->attr->pcidev.func)
    return HWLOC_PCI_BUSID_HIGHER;

  /* Should never reach here.  Abort on both debug builds and
     non-debug builds */
  assert(0);
  fprintf(stderr, "Bad assertion in hwloc %s:%d (aborting)\n", __FILE__, __LINE__);
  exit(1);
}

static void
hwloc_pci_add_child_before(struct hwloc_obj *root, struct hwloc_obj *child, struct hwloc_obj *new)
{
  if (child) {
    new->prev_sibling = child->prev_sibling;
    child->prev_sibling = new;
  } else {
    new->prev_sibling = root->last_child;
    root->last_child = new;
  }

  if (new->prev_sibling)
    new->prev_sibling->next_sibling = new;
  else
    root->first_child = new;
  new->next_sibling = child;
}

static void
hwloc_pci_remove_child(struct hwloc_obj *root, struct hwloc_obj *child)
{
  if (child->next_sibling)
    child->next_sibling->prev_sibling = child->prev_sibling;
  else
    root->last_child = child->prev_sibling;
  if (child->prev_sibling)
    child->prev_sibling->next_sibling = child->next_sibling;
  else
    root->first_child = child->next_sibling;
  child->prev_sibling = NULL;
  child->next_sibling = NULL;
}

static void hwloc_pci_add_object(struct hwloc_obj *root, struct hwloc_obj *new);

static void
hwloc_pci_try_insert_siblings_below_new_bridge(struct hwloc_obj *root, struct hwloc_obj *new)
{
  enum hwloc_pci_busid_comparison_e comp;
  struct hwloc_obj *current, *next;

  next = new->next_sibling;
  while (next) {
    current = next;
    next = current->next_sibling;

    comp = hwloc_pci_compare_busids(current, new);
    assert(comp != HWLOC_PCI_BUSID_SUPERSET);
    if (comp == HWLOC_PCI_BUSID_HIGHER)
      continue;
    assert(comp == HWLOC_PCI_BUSID_INCLUDED);

    /* move this object below the new bridge */
    hwloc_pci_remove_child(root, current);
    hwloc_pci_add_object(new, current);
  }
}

static void
hwloc_pci_add_object(struct hwloc_obj *root, struct hwloc_obj *new)
{
  struct hwloc_obj *current;

  current = root->first_child;
  while (current) {
    enum hwloc_pci_busid_comparison_e comp = hwloc_pci_compare_busids(new, current);
    switch (comp) {
    case HWLOC_PCI_BUSID_HIGHER:
      /* go further */
      current = current->next_sibling;
      continue;
    case HWLOC_PCI_BUSID_INCLUDED:
      /* insert below current bridge */
      hwloc_pci_add_object(current, new);
      return;
    case HWLOC_PCI_BUSID_LOWER:
    case HWLOC_PCI_BUSID_SUPERSET:
      /* insert before current object */
      hwloc_pci_add_child_before(root, current, new);
      /* walk next siblings and move them below new bridge if needed */
      hwloc_pci_try_insert_siblings_below_new_bridge(root, new);
      return;
    }
  }
  /* add to the end of the list if higher than everybody */
  hwloc_pci_add_child_before(root, NULL, new);
}

static struct hwloc_obj *
hwloc_pci_find_hostbridge_parent(struct hwloc_topology *topology, struct hwloc_obj *hostbridge, int *created)
{
  hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
  struct hwloc_obj *parent;
  char *env;
  int err;

  /* override the cpuset with the environment if given */
  char envname[256];
  snprintf(envname, sizeof(envname), "HWLOC_PCI_%04x_%02x_LOCALCPUS",
	   hostbridge->first_child->attr->pcidev.domain, hostbridge->first_child->attr->pcidev.bus);
  env = getenv(envname);
  if (env) {
    hwloc_debug("Overriding localcpus using %s in the environment\n", envname);
    hwloc_bitmap_sscanf(cpuset, env);
    goto found;
  }

  /* get the hostbridge cpuset. it's not a PCI device, so we use its first child locality info */
#ifdef HWLOC_LINUX_SYS
  {
  char path[256];
  FILE *file;
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/local_cpus",
	   hostbridge->first_child->attr->pcidev.domain, hostbridge->first_child->attr->pcidev.bus,
	   hostbridge->first_child->attr->pcidev.dev, hostbridge->first_child->attr->pcidev.func);
  file = fopen(path, "r"); /* the libpci backend doesn't use sysfs.fsroot */
  if (file) {
    err = hwloc_linux_parse_cpumap_file(file, cpuset);
    fclose(file);
    if (!err && !hwloc_bitmap_iszero(cpuset))
      goto found;
  }
  }
#endif

  /* if we got nothing, assume the hostbridge is attached to the top of hierarchy */
  hwloc_bitmap_copy(cpuset, hwloc_topology_get_topology_cpuset(topology));

 found:
  hwloc_debug_bitmap("Attaching hostbridge to cpuset %s\n", cpuset);

  /* restrict to the existing topology cpuset to avoid errors later */
  hwloc_bitmap_and(cpuset, cpuset, topology->levels[0][0]->cpuset);

  /* if the remaining cpuset is empty, take the root */
  if (hwloc_bitmap_iszero(cpuset))
    hwloc_bitmap_copy(cpuset, hwloc_topology_get_topology_cpuset(topology));

  /* attach the hostbridge now that it contains the right objects */
  parent = hwloc_get_obj_covering_cpuset(topology, cpuset);
  /* in the worst case, we got the root object */

  if (hwloc_bitmap_isequal(cpuset, parent->cpuset)) {
    /* this object has the right cpuset, but it could be a cache or so,
     * go up as long as the cpuset is the same
     */
    while (parent->parent && hwloc_bitmap_isequal(parent->cpuset, parent->parent->cpuset))
      parent = parent->parent;
  } else {
    /* the object we found is too large, insert an intermediate group */
    hwloc_obj_t group_obj = group_obj = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, -1);
    if (group_obj) {
      group_obj->cpuset = hwloc_bitmap_dup(cpuset);
      group_obj->attr->group.depth = topology->next_group_depth;
      hwloc__insert_object_by_cpuset(topology, group_obj, hwloc_report_os_error);
      parent = group_obj;
      *created = 1;
    }
  }

  hwloc_bitmap_free(cpuset);

  return parent;
}

/* Avoid letting libpci call exit(1) when no PCI bus is available. */
static jmp_buf err_buf;
static void
hwloc_pci_error(char *msg, ...)
{
  va_list args;

  va_start(args, msg);
  fprintf(stderr, "pcilib: ");
  vfprintf(stderr, msg, args);
  fprintf(stderr, "\n");
  longjmp(err_buf, 1);
}

static void
hwloc_pci_warning(char *msg __hwloc_attribute_unused, ...)
{
}

void
hwloc_look_libpci(struct hwloc_topology *topology)
{
  struct pci_access *pciaccess;
  struct pci_dev *pcidev;
  struct hwloc_obj fakehostbridge; /* temporary object covering the whole PCI hierarchy until its complete */
  unsigned current_hostbridge;
  int createdgroups = 0;

  fakehostbridge.first_child = NULL;
  fakehostbridge.last_child = NULL;

  hwloc_debug("%s", "\nScanning PCI buses...\n");

  pciaccess = pci_alloc();
  pciaccess->error = hwloc_pci_error;
  pciaccess->warning = hwloc_pci_warning;

  if (setjmp(err_buf)) {
    pci_cleanup(pciaccess);
    return;
  }

  pci_init(pciaccess);
  pci_scan_bus(pciaccess);

  pcidev = pciaccess->devices;
  while (pcidev) {
    char name[128], *resname;
    u8 config_space_cache[CONFIG_SPACE_CACHESIZE];
    struct hwloc_obj *obj;
    unsigned char headertype;
    unsigned os_index;
    unsigned isbridge;
    unsigned domain;
    unsigned device_class;

    /* cache what we need of the config space */
    pci_read_block(pcidev, 0, config_space_cache, CONFIG_SPACE_CACHESIZE);

    /* read some fields that may not always be available */
#ifdef HWLOC_HAVE_PCIDEV_DOMAIN
    domain = pcidev->domain;
#else
    domain = 0; /* default domain number */
#endif
#ifdef HWLOC_HAVE_PCIDEV_DEVICE_CLASS
    device_class = pcidev->device_class;
#else
    HWLOC_BUILD_ASSERT(PCI_CLASS_DEVICE < CONFIG_SPACE_CACHESIZE);
    device_class = config_space_cache[PCI_CLASS_DEVICE] | (config_space_cache[PCI_CLASS_DEVICE+1] << 8);
#endif

    /* is this a bridge? */
    HWLOC_BUILD_ASSERT(PCI_HEADER_TYPE < CONFIG_SPACE_CACHESIZE);
    headertype = config_space_cache[PCI_HEADER_TYPE] & 0x7f;
    isbridge = (device_class == PCI_CLASS_BRIDGE_PCI
		&& headertype == PCI_HEADER_TYPE_BRIDGE);

    /* might be useful for debugging (note that domain might be truncated) */
    os_index = (domain << 20) + (pcidev->bus << 12) + (pcidev->dev << 4) + pcidev->func;

    obj = hwloc_alloc_setup_object(isbridge ? HWLOC_OBJ_BRIDGE : HWLOC_OBJ_PCI_DEVICE, os_index);
    obj->attr->pcidev.domain = domain;
    obj->attr->pcidev.bus = pcidev->bus;
    obj->attr->pcidev.dev = pcidev->dev;
    obj->attr->pcidev.func = pcidev->func;
    obj->attr->pcidev.vendor_id = pcidev->vendor_id;
    obj->attr->pcidev.device_id = pcidev->device_id;
    obj->attr->pcidev.class_id = device_class;
    HWLOC_BUILD_ASSERT(PCI_REVISION_ID < CONFIG_SPACE_CACHESIZE);
    obj->attr->pcidev.revision = config_space_cache[PCI_REVISION_ID];
    HWLOC_BUILD_ASSERT(PCI_SUBSYSTEM_VENDOR_ID < CONFIG_SPACE_CACHESIZE);
    obj->attr->pcidev.subvendor_id = config_space_cache[PCI_SUBSYSTEM_VENDOR_ID];
    HWLOC_BUILD_ASSERT(PCI_SUBSYSTEM_ID < CONFIG_SPACE_CACHESIZE);
    obj->attr->pcidev.subdevice_id = config_space_cache[PCI_SUBSYSTEM_ID];

    obj->attr->pcidev.linkspeed = 0; /* unknown */
#ifdef HWLOC_HAVE_PCI_FIND_CAP
    {
    struct pci_cap *cap = pci_find_cap(pcidev, PCI_CAP_ID_EXP, PCI_CAP_NORMAL);
    if (cap) {
      if (cap->addr + PCI_EXP_LNKSTA >= CONFIG_SPACE_CACHESIZE) {
        fprintf(stderr, "cannot read PCI_EXP_LNKSTA cap at %d (only %d cached)\n", cap->addr + PCI_EXP_LNKSTA, CONFIG_SPACE_CACHESIZE);
      } else {
        unsigned linksta = * (unsigned*) &config_space_cache[cap->addr + PCI_EXP_LNKSTA];
        unsigned speed = linksta & PCI_EXP_LNKSTA_SPEED; /* PCIe generation */
        unsigned width = (linksta & PCI_EXP_LNKSTA_WIDTH) >> 4; /* how many lanes */
	/* PCIe Gen1 = 2.5GT/s signal-rate per lane with 8/10 encoding    = 0.25GB/s data-rate per lane
	 * PCIe Gen2 = 5  GT/s signal-rate per lane with 8/10 encoding    = 0.5 GB/s data-rate per lane
	 * PCIe Gen3 = 8  GT/s signal-rate per lane with 128/130 encoding = 1   GB/s data-rate per lane
	 */
        float lanespeed = speed <= 2 ? 2.5 * speed * 0.8 : 8 * 128/130; /* Gbit/s per lane */
        obj->attr->pcidev.linkspeed = lanespeed * width / 8; /* GB/s */
      }
    }
    }
#endif /* HWLOC_HAVE_PCI_FIND_CAP */

    if (isbridge) {
      HWLOC_BUILD_ASSERT(PCI_PRIMARY_BUS < CONFIG_SPACE_CACHESIZE);
      HWLOC_BUILD_ASSERT(PCI_SECONDARY_BUS < CONFIG_SPACE_CACHESIZE);
      HWLOC_BUILD_ASSERT(PCI_SUBORDINATE_BUS < CONFIG_SPACE_CACHESIZE);
      if (config_space_cache[PCI_PRIMARY_BUS] != pcidev->bus)
	hwloc_debug("  %04x:%02x:%02x.%01x bridge with (ignored) invalid PCI_PRIMARY_BUS %02x\n",
		    domain, pcidev->bus, pcidev->dev, pcidev->func, config_space_cache[PCI_PRIMARY_BUS]);
      obj->attr->bridge.upstream_type = HWLOC_OBJ_BRIDGE_PCI;
      obj->attr->bridge.downstream_type = HWLOC_OBJ_BRIDGE_PCI;
      obj->attr->bridge.downstream.pci.domain = domain;
      obj->attr->bridge.downstream.pci.secondary_bus = config_space_cache[PCI_SECONDARY_BUS];
      obj->attr->bridge.downstream.pci.subordinate_bus = config_space_cache[PCI_SUBORDINATE_BUS];
    }

/* starting from pciutils 2.2, pci_lookup_name() takes a variable number
 * of arguments, and supports the PCI_LOOKUP_NO_NUMBERS flag.
 */

    resname = pci_lookup_name(pciaccess, name, sizeof(name),
#if HAVE_DECL_PCI_LOOKUP_NO_NUMBERS
			      PCI_LOOKUP_VENDOR|PCI_LOOKUP_NO_NUMBERS,
			      pcidev->vendor_id
#else
			      PCI_LOOKUP_VENDOR,
			      pcidev->vendor_id, 0, 0, 0
#endif
			      );
    if (resname)
      hwloc_obj_add_info(obj, "PCIVendor", resname);

    resname = pci_lookup_name(pciaccess, name, sizeof(name),
#if HAVE_DECL_PCI_LOOKUP_NO_NUMBERS
			      PCI_LOOKUP_DEVICE|PCI_LOOKUP_NO_NUMBERS,
			      pcidev->vendor_id, pcidev->device_id
#else
			      PCI_LOOKUP_DEVICE,
			      pcidev->vendor_id, pcidev->device_id, 0, 0
#endif
			      );
    if (resname)
      hwloc_obj_add_info(obj, "PCIDevice", resname);

    resname = pci_lookup_name(pciaccess, name, sizeof(name),
#if HAVE_DECL_PCI_LOOKUP_NO_NUMBERS
			      PCI_LOOKUP_VENDOR|PCI_LOOKUP_DEVICE|PCI_LOOKUP_NO_NUMBERS,
			      pcidev->vendor_id, pcidev->device_id
#else
			      PCI_LOOKUP_VENDOR|PCI_LOOKUP_DEVICE,
			      pcidev->vendor_id, pcidev->device_id, 0, 0
#endif
			      );
    if (resname)
      obj->name = strdup(resname);
    else
      resname = "??";

    hwloc_debug("  %04x:%02x:%02x.%01x %04x %04x:%04x %s\n",
		domain, pcidev->bus, pcidev->dev, pcidev->func,
		device_class, pcidev->vendor_id, pcidev->device_id,
		resname);

    hwloc_pci_add_object(&fakehostbridge, obj);
    pcidev = pcidev->next;
  }

  pci_cleanup(pciaccess);

  hwloc_debug("%s", "\nPCI hierarchy after basic scan:\n");
  hwloc_pci_traverse(topology, &fakehostbridge, hwloc_pci_traverse_print_cb);

  if (!fakehostbridge.first_child)
    /* found nothing, exit */
    return;

  /* walk the hierarchy, set bridge depth and lookup OS devices */
  hwloc_pci_traverse(topology, &fakehostbridge, hwloc_pci_traverse_setbridgedepth_cb);
  hwloc_pci_traverse(topology, &fakehostbridge, hwloc_pci_traverse_lookuposdevices_cb);

  /*
   * fakehostbridge lists all objects connected to any upstream bus in the machine.
   * We now create one real hostbridge object per upstream bus.
   * It's not actually a PCI device so we have to create it.
   */
  current_hostbridge = 0;
  while (fakehostbridge.first_child) {
    /* start a new host bridge */
    struct hwloc_obj *hostbridge = hwloc_alloc_setup_object(HWLOC_OBJ_BRIDGE, current_hostbridge++);
    struct hwloc_obj *child = fakehostbridge.first_child;
    struct hwloc_obj *next_child;
    struct hwloc_obj *parent;
    unsigned short current_domain = child->attr->pcidev.domain;
    unsigned char current_bus = child->attr->pcidev.bus;
    unsigned char current_subordinate = current_bus;

    hwloc_debug("Starting new PCI hostbridge %04x:%02x\n", current_domain, current_bus);

    /*
     * attach all objects from the same upstream domain/bus
     */
  next_child:
    next_child = child->next_sibling;
    hwloc_pci_remove_child(&fakehostbridge, child);
    hwloc_pci_add_child_before(hostbridge, NULL, child);

    /* compute hostbridge secondary/subordinate buses */
    if (child->type == HWLOC_OBJ_BRIDGE
	&& child->attr->bridge.downstream.pci.subordinate_bus > current_subordinate)
      current_subordinate = child->attr->bridge.downstream.pci.subordinate_bus;

    /* use next child if it has the same domains/bus */
    child = next_child;
    if (child
	&& child->attr->pcidev.domain == current_domain
	&& child->attr->pcidev.bus == current_bus)
      goto next_child;

    /* finish setting up this hostbridge */
    hostbridge->attr->bridge.upstream_type = HWLOC_OBJ_BRIDGE_HOST;
    hostbridge->attr->bridge.downstream_type = HWLOC_OBJ_BRIDGE_PCI;
    hostbridge->attr->bridge.downstream.pci.domain = current_domain;
    hostbridge->attr->bridge.downstream.pci.secondary_bus = current_bus;
    hostbridge->attr->bridge.downstream.pci.subordinate_bus = current_subordinate;
    hwloc_debug("New PCI hostbridge %04x:[%02x-%02x]\n",
		current_domain, current_bus, current_subordinate);

    /* attach the hostbridge where it belongs */
    parent = hwloc_pci_find_hostbridge_parent(topology, hostbridge, &createdgroups);
    hwloc_insert_object_by_parent(topology, parent, hostbridge);
  }

  if (createdgroups)
    topology->next_group_depth++;
}

#endif /* HWLOC_HAVE_LIBPCI */
