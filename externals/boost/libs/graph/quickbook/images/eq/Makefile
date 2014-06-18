# Copyright (C) 2007-2009 Andrew Sutton
#
# Distributed under the Boost Software License, Version 1.0. (See accompanying
# file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

src = \
	mean_geodesic.tex \
	small_world.tex \
	closeness.tex \
	scaled_closeness.tex \
	betweenness.tex \
	relative_betweenness.tex \
	central_point_dominance.tex \
	num_paths_directed.tex \
	num_paths_undirected.tex \
	num_triangles.tex \
	clustering_coef.tex
dvi = $(src:%.tex=%.dvi)
ps = $(src:%.tex=%.ps)
png = $(src:%.tex=%.png)
aux = $(src:%.tex=%.aux)
log = $(src:%.tex=%.log)

%.png: %.ps
	convert -density 120 $< $@

%.ps: %.dvi
	dvips -E $< -o $@

%.dvi: %.tex
	latex --interaction=nonstopmode $<

all: $(png)
	make clean-aux

clean:
	rm -f $(png) $(ps) $(dvi) $(aux) $(log)

clean-aux:
	rm -rf $(aux) $(log) $(dvi)