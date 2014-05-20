# GMP random numbers module.

# Copyright 2001, 2003 Free Software Foundation, Inc.
#
# This file is part of the GNU MP Library.
#
# The GNU MP Library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or (at
# your option) any later version.
#
# The GNU MP Library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.


package GMP::Rand;

require GMP;
require Exporter;
@ISA = qw(GMP Exporter);
@EXPORT = qw();
%EXPORT_TAGS = ('all' => [qw(
			     randstate mpf_urandomb mpz_rrandomb
			     mpz_urandomb mpz_urandomm gmp_urandomb_ui
			     gmp_urandomm_ui)]);
Exporter::export_ok_tags('all');
1;
__END__
