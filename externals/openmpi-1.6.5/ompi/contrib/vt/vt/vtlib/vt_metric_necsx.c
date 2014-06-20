/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2007-2008, High Performance Computing Center Stuttgart,
 *                          Federal Republic of Germany
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_metric.h"

/*
 * This is the main routine reading all metrics (first _open'd and _create'd
 */
extern void hpm_rf_(uint64_t* values);

/*
 * Local Function definitions
 */
static uint64_t sx_fpec(void);
static uint64_t sx_vx_per_ex(void);
static uint64_t sx_ve_per_vx(void);
static uint64_t sx_ex_plus_vx(void);
static uint64_t sx_vldec_per_vecc(void);
static uint64_t sx_bpfc_per_brec(void);
static uint64_t sx_all_clocks_missed_per_usr_clocks(void);
static uint64_t sx_iphcc(void);
static uint64_t sx_bccc(void);
static uint64_t sx_icmcc(void);
static uint64_t sx_ocmcc(void);
static uint64_t sx_binop(void); /* XXX Delete again */

static void metricv_add(int number);

/*
 * Global Variables
 */
struct metric
{
  char * name;
  char * descr;
  char * unit;
  uint32_t props;
  /* per metric function to read&calculate the value (FLOP) */
  uint64_t (*read_and_compute)(void);
  /* number from within vt_sx_metrics for setting the bitmask */
  int number;
};

static struct metric vt_sx_metrics[] = {
/* First the more important ones, instruction/execution related */
{
  "sx_fpec",
  "Floating Point Data Execution Counter (Avg. MFLOPS)",
  "MFLOPS",
  VT_CNTR_ACC,
  &sx_fpec
},
{
  "sx_vx_per_ex",
  "Percentage of vector instructions (VX) of all instructions executed (EX) (Avg. Vector Operation Ratio)",
  "%",
  VT_CNTR_ABS | VT_CNTR_NEXT,
  &sx_vx_per_ex
},
{
  "sx_ve_per_vx",
  "Vector elements (VE) per vector instruction (VX) (Avg. Vector Length)",
  "#",
  VT_CNTR_ABS | VT_CNTR_NEXT, 
  &sx_ve_per_vx
},
{
  "sx_ex_plus_vx",
  "Vector instructions (VX) plus scalar instructions (EX) executed",
  "#",
  VT_CNTR_ACC,
  &sx_ex_plus_vx
},
{
  "sx_vldec_per_vecc",
  "Vector load execution clocks (VLDEC) per vector execution clocks (VECC)",
  "#",
  VT_CNTR_ABS | VT_CNTR_NEXT, 
  &sx_vldec_per_vecc
},
{
  "sx_bpfc_per_brec",
  "Percentage of mispredicted branches (BPFC) per branches executed (BREC)",
  "%",
  VT_CNTR_ABS | VT_CNTR_NEXT,
  &sx_bpfc_per_brec
},
{
  "sx_all_clocks_missed_per_usr_clocks",
  "All clocks missed for execution per overall clocks ((IPHCC+ICMCC+OCMCC+MNCCC+BCCC+SRACC)/USRCC)",
  "#",
  VT_CNTR_ABS | VT_CNTR_NEXT,
  &sx_all_clocks_missed_per_usr_clocks
},
{
  "sx_iphcc",
  "Instruction Pipeline Hold Clock Counter (IPHCC)",
  "#",
  VT_CNTR_ACC,
  &sx_iphcc
},

/* Now the memory/network related */
{
  "sx_bccc",
  "Bank Conflict Clock Counter (BCCC)",
  "#",
  VT_CNTR_ACC,
  &sx_bccc
},
{
  "sx_icmcc",
  "Instruction Cache Miss Clock Counter (ICMCC)",
  "#",
  VT_CNTR_ACC,
  &sx_icmcc
},
{
  "sx_ocmcc",
  "Operand Cache Miss Clock Counter (OCMCC)",
  "#",
  VT_CNTR_ACC,
  &sx_ocmcc
},
{
  "sx_binop",
  "Binary Flip-Flop every other event",
  "#",
  VT_CNTR_ABS | VT_CNTR_NEXT,
  &sx_binop
}
};

enum {
  SX_CTR_STM = 0,    /*  0  stm    system timer reg */
  SX_CTR_USRCC,      /*  1  usrcc  user clock counter */
  SX_CTR_EX,         /*  2  ex     execution counter */
  SX_CTR_VX,         /*  3  vx     vector execution counter */
  SX_CTR_VE,         /*  4  ve     vector element counter */
  SX_CTR_VECC,       /*  5  vecc   vector execution clock counter */
  SX_CTR_VAREC,      /*  6  varec  vector arithmetic execution clock counter */
  SX_CTR_VLDEC,      /*  7  vldec  vector load execution clock counter */
  SX_CTR_FPEC,       /*  8  fpec   floating point data execution counter */
  SX_CTR_BCCC,       /*  9  bccc   bank conflict clock counter */
  SX_CTR_ICMCC,      /* 10  icmcc  instruction cache miss clock counter */
  SX_CTR_OCMCC,      /* 11  ocmcc  operand cache miss clock counter */
  SX_CTR_IPHCC,      /* 12  iphcc  instruction pipeline hold clock counter */
  SX_CTR_MNCCC,      /* 13  mnccc  memory network conflict clock counter */
  SX_CTR_SRACC,      /* 14  sracc  shared resource access clock counter */
  SX_CTR_BREC,       /* 16  brec   branch execution counter */
  SX_CTR_BPFC,       /* 17  bpfc   branch prediction failure counter */
  SX_CTR_MAX         /* Last entry */
} vt_sx_counter;

enum {
  SX4_ARCH = 0,
  SX5_ARCH,
  SX6_ARCH,
  SX7_ARCH,
  SX8_ARCH,
  SX_ARCH_MAX
} vt_sx_arch;

uint64_t values_overflow[SX_ARCH_MAX][SX_CTR_MAX] = {
{
  /* NEC SX-4 Counter widths */
  0x0,
},
{
  /* NEC SX-5 Counter widths */
  0x0,
},
{
  /* NEC SX-6 Counter widths */
  0x0,
},
{
  /* NEC SX-7 Counter widths, are equal to SX8 */
  0x0000000000000000,       /* stm,   SX8: 56-bit NO overflow detection */
  0x0000000000000000,       /* usrcc, SX8: 56-bit NO overflow detection */
  0x0010000000000000,       /* ex,    SX8: 52-bit overflow */
  0x0001000000000000,       /* vx,    SX8: 48-bit overflow */
  0x0100000000000000,       /* ve,    SX8: 56-bit overflow */
  0x0100000000000000,       /* vecc,  SX8: 56-bit overflow */
  0x0001000000000000,       /* varec, SX8: 48-bit overflow */
  0x0001000000000000,       /* vldec, SX8: 48-bit overflow */
  0x0100000000000000,       /* fpec,  SX8: 56-bit overflow */
  0x0001000000000000,       /* bccc,  SX8: 48-bit overflow */
  0x0001000000000000,       /* icmcc, SX8: 48-bit overflow */
  0x0001000000000000,       /* ocmcc, SX8: 48-bit overflow */
  0x0001000000000000,       /* iphcc, SX8: 48-bit overflow */
  0x0001000000000000,       /* mnccc, SX8: 48-bit overflow */
  0x0001000000000000,       /* sracc, SX8: 48-bit overflow */
  0x0001000000000000,       /* brec,  SX8: 48-bit overflow */
  0x0001000000000000        /* bpfc,  SX8: 48-bit overflow */
},
{
  /* NEC SX-8 Counter widths */
  0x0000000000000000,       /* stm,   SX8: 56-bit NO overflow detection */
  0x0000000000000000,       /* usrcc, SX8: 56-bit NO overflow detection */
  0x0010000000000000,       /* ex,    SX8: 52-bit overflow */
  0x0001000000000000,       /* vx,    SX8: 48-bit overflow */
  0x0100000000000000,       /* ve,    SX8: 56-bit overflow */
  0x0100000000000000,       /* vecc,  SX8: 56-bit overflow */
  0x0001000000000000,       /* varec, SX8: 48-bit overflow */
  0x0001000000000000,       /* vldec, SX8: 48-bit overflow */
  0x0100000000000000,       /* fpec,  SX8: 56-bit overflow */
  0x0001000000000000,       /* bccc,  SX8: 48-bit overflow */
  0x0001000000000000,       /* icmcc, SX8: 48-bit overflow */
  0x0001000000000000,       /* ocmcc, SX8: 48-bit overflow */
  0x0001000000000000,       /* iphcc, SX8: 48-bit overflow */
  0x0001000000000000,       /* mnccc, SX8: 48-bit overflow */
  0x0001000000000000,       /* sracc, SX8: 48-bit overflow */
  0x0001000000000000,       /* brec,  SX8: 48-bit overflow */
  0x0001000000000000        /* bpfc,  SX8: 48-bit overflow */
}
};

struct vt_metv
{
  /* bitset for each defined metric-set vt_sx_metrics from above */
  uint64_t metrics_bitset;
};

static struct vt_metv vt_metv_used;

static struct metric* metricv[VT_METRIC_MAXNUM];
static int nmetrics = 0;
static uint64_t* sx_ctr_array;

/*
 * Local Metric functions
 *
 * XXX Have to adapt the counters and check for possible overflow
 * Copy all the functions to the above declaration.
 */
static uint64_t sx_fpec(void)
{
  return sx_ctr_array[SX_CTR_FPEC];
}

static uint64_t sx_vx_per_ex(void)
{
  return (100.0 * sx_ctr_array[SX_CTR_VX]) / sx_ctr_array[SX_CTR_EX];
}

static uint64_t sx_ve_per_vx(void)
{
  return (100.0 * sx_ctr_array[SX_CTR_VE]) / sx_ctr_array[SX_CTR_VX];
}

static uint64_t sx_ex_plus_vx(void)
{
  return sx_ctr_array[SX_CTR_EX] + sx_ctr_array[SX_CTR_VX];
}

static uint64_t sx_vldec_per_vecc(void)
{
  return (100.0 * sx_ctr_array[SX_CTR_VLDEC]) / sx_ctr_array[SX_CTR_VECC];
}

static uint64_t sx_bpfc_per_brec(void)
{
  return (100.0 * sx_ctr_array[SX_CTR_BPFC]) / sx_ctr_array[SX_CTR_BREC];
}

static uint64_t sx_all_clocks_missed_per_usr_clocks(void)
{
  /* IPHCC+ICMCC+OCMCC+MNCCC+BCCC+SRACC)/USRCC */
  return (100.0 * (sx_ctr_array[SX_CTR_IPHCC] +
          sx_ctr_array[SX_CTR_ICMCC] +
          sx_ctr_array[SX_CTR_OCMCC] +
          sx_ctr_array[SX_CTR_MNCCC] +
          sx_ctr_array[SX_CTR_BCCC] +
          sx_ctr_array[SX_CTR_SRACC])) / sx_ctr_array[SX_CTR_USRCC];
}


static uint64_t sx_iphcc(void)
{
  return sx_ctr_array[SX_CTR_IPHCC];
}


static uint64_t sx_bccc(void)
{
  return sx_ctr_array[SX_CTR_BCCC];
}

static uint64_t sx_icmcc(void)
{
  return sx_ctr_array[SX_CTR_ICMCC];
}

static uint64_t sx_ocmcc(void)
{
  return sx_ctr_array[SX_CTR_OCMCC];
}

/*
 XXX Delete again
 */
static uint64_t sx_binop(void)
{
  static uint64_t sx_binop_state = 0;
  sx_binop_state = sx_binop_state ? 0 : 1;
  return sx_binop_state;
}


static void metricv_add(int number)
{
  if (nmetrics >= VT_METRIC_MAXNUM) {
    vt_error_msg("Number of counters exceeds VampirTrace allowed maximum "
		 "of %d", VT_METRIC_MAXNUM);
  } else {
    metricv[nmetrics] = (struct metric*)malloc(sizeof(struct metric));
    metricv[nmetrics]->name = strdup(vt_sx_metrics[number].name);
    metricv[nmetrics]->descr = strdup(vt_sx_metrics[number].descr);
    metricv[nmetrics]->props = vt_sx_metrics[number].props;
    metricv[nmetrics]->read_and_compute =
      vt_sx_metrics[number].read_and_compute;
    metricv[nmetrics]->number = number;
    nmetrics++;
  }
}

int vt_metric_open()
{
  int i;
  char* env;
  char* env_sep;
  char* var;
  char* token;
  const int max_metrics = sizeof (vt_sx_metrics) / sizeof (vt_sx_metrics[0]);

  /* read environment variable "VT_METRICS"; return if unset. */
  env = vt_env_metrics();
  if ( env == NULL )
    return nmetrics;

  env_sep = vt_env_metrics_sep();

  var = strdup(env);
  vt_cntl_msg(2, "VT_METRICS=%s", var);

  /* convert VT_METRICS's letters to lower case */
  token = var;
  while ( *token ) { *token = tolower(*token); token++; }

  /* read metrics from specification string */
  token = strtok(var, env_sep);
  if (token && (0 == strcmp (token, "all"))) {
    vt_cntl_msg(2, "token:%s Adding all metrics", token);
    for (i = 0; i < max_metrics; i++) {
      metricv_add(i);
      vt_cntl_msg(2, "metric i:%d name:%s", i, vt_sx_metrics[i].name);
    }
  } else {
    while ( token && (nmetrics < VT_METRIC_MAXNUM) ) {
      /* search metricmap for a suitable definition */
      /* printf("Token%d: <%s>\n", nmetrics, token); */
      for (i = 0; i < max_metrics; i++) {
	if (0 == strcmp (token, vt_sx_metrics[i].name)) {
	  metricv_add(i);
	  vt_cntl_msg(2, "metric i:%d token:%s", i, token);
	  break;
	}
      }
      if (i == max_metrics) {
	vt_error_msg ("Metric <%s> not supported", token);
      }
      token = strtok(NULL, env_sep);
    }
  }

  sx_ctr_array = calloc(SX_CTR_MAX, sizeof (uint64_t));

  /* clean up */
  free(var);

  return nmetrics;
}

void vt_metric_close()
{
  int i;

  for ( i = 0; i < nmetrics; i++ ) {
    free(metricv[i]->name);
    free (metricv[i]);
  }
}

struct vt_metv* vt_metric_create(void)
{
  int i;
  for (i = 0; i < nmetrics; i++)
    vt_metv_used.metrics_bitset |= (1 << metricv[i]->number);
  return &vt_metv_used;
}

void vt_metric_free(struct vt_metv* metv, uint32_t tid)
{
  (void)tid;
  (void)metv;
}

void vt_metric_thread_init(long (*id_fn)(void))
{
  (void)id_fn;

  if ( nmetrics == 0 )
    return;

  /* we don't support threads for the moment */
#if (defined(VT_MT) || defined(VT_HYB) || defined(VT_JAVA))
  vt_error_msg("NEC SX Performance Counters for threaded application "
               "not yet supported");
#endif /* VT_MT || VT_HYB || VT_JAVA */
}

void vt_metric_thread_fini()
{
}

void vt_metric_read(struct vt_metv* metv, uint64_t offsets[],
                    uint64_t values[])
{
  int i;

  (void)offsets;

  if ( metv == NULL )
    return;

  /* we once read all the counters, all the other functions work on them */
  hpm_rf_ (sx_ctr_array);

  for (i = 0; i < nmetrics; i++)
    values[i] = (metricv[i])->read_and_compute();
}

int vt_metric_num()
{
  return nmetrics;
}

const char* vt_metric_name(int i)
{
  return metricv[i]->name;
}

const char* vt_metric_descr(int i)
{
  return metricv[i]->descr;
}

const char* vt_metric_unit(int i)
{
  return metricv[i]->unit;
}

uint32_t vt_metric_props(int i)
{
  return metricv[i]->props;
}
