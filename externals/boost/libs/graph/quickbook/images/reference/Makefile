# Copyright (C) 2007-2009 Andrew Sutton
#
# Distributed under the Boost Software License, Version 1.0. (See accompanying
# file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

GVFLAGS = -Tpng

dot_src = \
	connected_components_before.dot \
	connected_components_after.dot \
	geodesic.dot \
	info_network.dot
circo_src = \
	distribution_undirected.circo \
	distribution_directed.circo \
	triangle.circo \
	prism_3_2.circo
fdp_src = \
	social_network.fdp
neato_src = \
	comm_network.neato

dot_png = $(dot_src:%.dot=%.png)
circo_png = $(circo_src:%.circo=%.png)
fdp_png = $(fdp_src:%.fdp=%.png)
neato_png = $(neato_src:%.neato=%.png)

%.png: %.dot
	dot $(GVFLAGS) -o $@ $<
	convert $@ -resize 80% $@

%.png: %.circo
	circo $(GVFLAGS) -o $@ $<
	convert $@ -resize 80% $@

%.png: %.fdp
	fdp $(GVFLAGS) -o $@ $<
	convert $@ -resize 80% $@

%.png: %.neato
	neato $(GVFLAGS) -o $@ $<
	convert $@ -resize 80% $@

all: $(dot_png) $(circo_png) $(fdp_png) $(neato_png)

clean:
	rm -rf $(dot_png)
	rm -rf $(circo_png)
	rm -rf $(fdp_png)
	rm -rf $(neato_png)
