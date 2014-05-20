#! /bin/env python

import sys
from hotshot import stats

s = stats.load(sys.argv[1])
s.sort_stats("time").print_stats()
