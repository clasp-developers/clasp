### Clasp ebuilds for Gentoo
#### Tested against profile 13.0, probably compatible with 17.0

The ebuild(s) in this directory nominally go(es) in `dev-lisp/clasp` (they
could go in `dev-lang/clasp`, but the vast majority of the CLISP
implementations in the Gentoo tree seem to live in `dev-lisp`, so I'm following
suit).

There are doubtless other GCC-specific processor flags (and, perhaps,
GCC-specific arch-independent flags) that I missed in my filter-flags call;
*please* let me know if you catch one, either by
[e-mail](mailto:robink@creosotehill.org) or by submitting a pull request to
[this repo](https://github.com/drmeister/clasp) or
[mine](https://github.com/Haifen/clasp).

This initial rough-draft ebuild was written by [Robin
Kauffman](https://github.com/Haifen).  It is covered by the [GNU General Public
License version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.txt) in
accordance with the Gentoo ebuild licensing scheme.  Copyright is assigned to
(and is retained by) the Gentoo Foundation in hopes of getting it upstreamed
sooner.

