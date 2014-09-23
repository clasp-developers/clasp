/*
    File: improperRestraintTerms.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
double CosPhi, CosPhiMinusPhase, DePhi, DphiDx1, DphiDx2, DphiDx3, DphiDx4, DphiDy1, DphiDy2, DphiDy3, DphiDy4, DphiDz1, DphiDz2, DphiDz3, DphiDz4, dx1, dx2, dx3, dx4, dy1, dy2, dy3, dy4, dz1, dz2, dz3, dz4, EImproperRestraint, fx1, fx2, fx3, fx4, fy1, fy2, fy3, fy4, fz1, fz2, fz3, fz4, LenA, LenB, ReciprocalLenA, ReciprocalLenB, RecLenARecLenB, SinPhi, SinPhiMinusPhase, SinPhiMinusPhaseMax, SinPhiMinusPhaseMin, tx1, tx10, tx100, tx101, tx102, tx103, tx104, tx105, tx106, tx107, tx108, tx109, tx11, tx110, tx12, tx13, tx14, tx15, tx16, tx17, tx18, tx19, tx2, tx20, tx21, tx22, tx23, tx24, tx25, tx26, tx27, tx28, tx29, tx3, tx30, tx31, tx32, tx33, tx34, tx35, tx36, tx37, tx38, tx39, tx4, tx40, tx41, tx42, tx43, tx44, tx45, tx46, tx47, tx48, tx49, tx5, tx50, tx51, tx52, tx53, tx54, tx55, tx56, tx57, tx58, tx59, tx6, tx60, tx61, tx62, tx63, tx64, tx65, tx66, tx67, tx68, tx69, tx7, tx70, tx71, tx72, tx73, tx74, tx75, tx76, tx77, tx78, tx79, tx8, tx80, tx81, tx82, tx83, tx84, tx85, tx86, tx87, tx88, tx89, tx9, tx90, tx91, tx92, tx93, tx94, tx95, tx96, tx97, tx98, tx99, tzz111, tzz112, tzz113, tzz114, tzz115, tzz116, tzz120, tzz121, tzz122, tzz123, tzz124, tzz128, tzz129, tzz130, xxxxDummy;
	tx77 = -x2; 		/* rule 1 */
	tx1 = tx77*y1; 		/* rule 2 */
	tx2 = x3*y1; 		/* rule 3 */
	tx3 = x1*y2; 		/* rule 4 */
	tx78 = -y2; 		/* rule 5 */
	tx4 = tx78*x3; 		/* rule 6 */
	tx50 = -y3; 		/* rule 7 */
	tx5 = tx50*x1; 		/* rule 8 */
	tx6 = x2*y3; 		/* rule 9 */
	tx7 = x2*z1; 		/* rule 10 */
	tx49 = -x3; 		/* rule 11 */
	tx8 = tx49*z1; 		/* rule 12 */
	tx9 = tx78*z1; 		/* rule 13 */
	tx10 = y3*z1; 		/* rule 14 */
	tx79 = -z2; 		/* rule 15 */
	tx11 = tx79*x1; 		/* rule 16 */
	tx12 = x3*z2; 		/* rule 17 */
	tx13 = y1*z2; 		/* rule 18 */
	tx14 = tx79*y3; 		/* rule 19 */
	tx15 = x1*z3; 		/* rule 20 */
	tx51 = -z3; 		/* rule 21 */
	tx16 = tx51*x2; 		/* rule 22 */
	tx17 = tx51*y1; 		/* rule 23 */
	tx18 = y2*z3; 		/* rule 24 */
	tzz128 = tx4 + tx6; 		/* rule 25 */
	tx19 = tx1 + tx2 + tx3 + tx5 + tzz128; 		/* rule 26 */
	tzz130 = tx12 + tx16; 		/* rule 27 */
	tx20 = tx11 + tx15 + tx7 + tx8 + tzz130; 		/* rule 28 */
	tzz129 = tx14 + tx18; 		/* rule 29 */
	tx21 = tx10 + tx13 + tx17 + tx9 + tzz129; 		/* rule 30 */
	tx22 = power2(tx19); 		/* rule 31 */
	tx23 = power2(tx20); 		/* rule 32 */
	tx24 = power2(tx21); 		/* rule 33 */
	tx25 = tx22 + tx23 + tx24; 		/* rule 34 */
	LenA = mysqrt(tx25); 		/* rule 35 */
	tx26 = x4*y2; 		/* rule 36 */
	tx27 = tx50*x4; 		/* rule 37 */
	tx28 = tx77*y4; 		/* rule 38 */
	tx29 = x3*y4; 		/* rule 39 */
	tx30 = tx79*x4; 		/* rule 40 */
	tx31 = y4*z2; 		/* rule 41 */
	tx32 = x4*z3; 		/* rule 42 */
	tx33 = tx51*y4; 		/* rule 43 */
	tx34 = x2*z4; 		/* rule 44 */
	tx35 = tx49*z4; 		/* rule 45 */
	tx36 = tx78*z4; 		/* rule 46 */
	tx37 = y3*z4; 		/* rule 47 */
	tx38 = tx30 + tx32 + tx34 + tx35 + tzz130; 		/* rule 48 */
	tx39 = tx31 + tx33 + tx36 + tx37 + tzz129; 		/* rule 49 */
	tx40 = tx26 + tx27 + tx28 + tx29 + tzz128; 		/* rule 50 */
	tx41 = power2(tx38); 		/* rule 51 */
	tx42 = power2(tx39); 		/* rule 52 */
	tx43 = power2(tx40); 		/* rule 53 */
	tx44 = tx41 + tx42 + tx43; 		/* rule 54 */
	LenB = mysqrt(tx44); 		/* rule 55 */
	ReciprocalLenA = reciprocal(LenA); 		/* rule 56 */
	ReciprocalLenB = reciprocal(LenB); 		/* rule 57 */
	if (fabs(LenA)<TENM3) ReciprocalLenA = 0.0;
	if (fabs(LenB)<TENM3) ReciprocalLenB = 0.0;
	RecLenARecLenB = ReciprocalLenA*ReciprocalLenB; 		/* rule 60 */
	EraseLinearDihedral = 1.0;
	if (RecLenARecLenB==0.0) EraseLinearDihedral = 0.0;
	tx45 = tx20*tx38; 		/* rule 63 */
	tx46 = tx21*tx39; 		/* rule 64 */
	tx47 = tx19*tx40; 		/* rule 65 */
	tx48 = tx45 + tx46 + tx47; 		/* rule 66 */
	CosPhi = RecLenARecLenB*tx48; 		/* rule 67 */
	tx52 = tx49 + x2; 		/* rule 68 */
	tx53 = tx50 + y2; 		/* rule 69 */
	tx54 = tx51 + z2; 		/* rule 70 */
	tx55 = power2(tx52); 		/* rule 71 */
	tx56 = power2(tx53); 		/* rule 72 */
	tx57 = power2(tx54); 		/* rule 73 */
	tx58 = tx49 + x4; 		/* rule 74 */
	tx59 = tx50 + y4; 		/* rule 75 */
	tx60 = tx51 + z4; 		/* rule 76 */
	tx61 = tx55 + tx56 + tx57; 		/* rule 77 */
	tx62 = tx21*tx58; 		/* rule 78 */
	tx63 = tx20*tx59; 		/* rule 79 */
	tx64 = tx19*tx60; 		/* rule 80 */
	tx65 = mysqrt(tx61); 		/* rule 81 */
	tx66 = tx62 + tx63 + tx64; 		/* rule 82 */
	SinPhi = RecLenARecLenB*tx65*tx66; 		/* rule 83 */
	CosPhi=MAX(-1.0,MIN(1.0,CosPhi));
/*RestraintActive=False;*/
	RestraintActive = false;
	tzz123 = -CosPhi; 		/* rule 87 */
	tx67 = sinLower*tzz123; 		/* rule 88 */
	tx68 = cosLower*SinPhi; 		/* rule 89 */
	SinPhiMinusPhaseMin = tx67 + tx68; 		/* rule 90 */
/*If[SinPhiMinusPhaseMin<0.0,RestraintActive=True;sinPhase=sinLower;cosPhase=cosLower;];*/
	if(SinPhiMinusPhaseMin<0.0) { RestraintActive=true;sinPhase=sinLower;cosPhase=cosLower;}
	tx69 = cosUpper*SinPhi; 		/* rule 93 */
	tx70 = sinUpper*tzz123; 		/* rule 94 */
	SinPhiMinusPhaseMax = tx69 + tx70; 		/* rule 95 */
/*If[SinPhiMinusPhaseMax>0.0,RestraintActive=True;sinPhase=sinUpper;cosPhase=cosUpper;];*/
	if(SinPhiMinusPhaseMax<0.0) { RestraintActive=true;sinPhase=sinUpper;cosPhase=cosUpper; }
/*If[RestraintActive,*/
	if(RestraintActive) {
	tx71 = cosPhase*CosPhi; 		/* rule 100 */
	tx72 = sinPhase*SinPhi; 		/* rule 101 */
	CosPhiMinusPhase = tx71 + tx72; 		/* rule 102 */
	tx73 = sinPhase*tzz123; 		/* rule 103 */
	tx74 = cosPhase*SinPhi; 		/* rule 104 */
	SinPhiMinusPhase = tx73 + tx74; 		/* rule 105 */
/*If[CosPhiMinusPhase>0.1,PhiMinusPhase=ArcSin[SinPhiMinusPhase],
					PhiMinusPhase=ArcCos[CosPhiMinusPhase]*Sign[SinPhiMinusPhase]];*/
	if(CosPhiMinusPhase>0.1){PhiMinusPhase=asin(SinPhiMinusPhase);}else
					{PhiMinusPhase=acos(CosPhiMinusPhase)*SIGN(SinPhiMinusPhase);}
	tx75 = power2(PhiMinusPhase); 		/* rule 108 */
	tzz124 = EraseLinearDihedral*K; 		/* rule 109 */
	EImproperRestraint = tx75*tzz124; 		/* rule 110 */
	if ( !calcForce ) goto NO_GRADIENT;
	DePhi = -2.*PhiMinusPhase*tzz124; 		/* rule 112 */
	tx76 = reciprocal(tx25); 		/* rule 113 */
	tzz114 = tx65*tx76; 		/* rule 114 */
	tzz120 = -tzz114; 		/* rule 115 */
	DphiDx1 = tx21*tzz120; 		/* rule 116 */
	dx1 = DePhi*DphiDx1; 		/* rule 117 */
	DphiDy1 = tx20*tzz120; 		/* rule 118 */
	dy1 = DePhi*DphiDy1; 		/* rule 119 */
	DphiDz1 = tx19*tzz120; 		/* rule 120 */
	dz1 = DePhi*DphiDz1; 		/* rule 121 */
	tx80 = tx77 + x1; 		/* rule 122 */
	tx81 = tx78 + y1; 		/* rule 123 */
	tx82 = tx79 + z1; 		/* rule 124 */
	tx83 = tx52*tx58; 		/* rule 125 */
	tx84 = tx53*tx59; 		/* rule 126 */
	tx85 = tx54*tx60; 		/* rule 127 */
	tx86 = tx52*tx80; 		/* rule 128 */
	tx87 = tx53*tx81; 		/* rule 129 */
	tx88 = tx54*tx82; 		/* rule 130 */
	tx89 = reciprocal(tx44); 		/* rule 131 */
	tx90 = reciprocal(tx65); 		/* rule 132 */
	tx91 = tx83 + tx84 + tx85; 		/* rule 133 */
	tx92 = tx86 + tx87 + tx88; 		/* rule 134 */
	tx93 = tx21*tzz114; 		/* rule 135 */
	tzz111 = tx90*tx91; 		/* rule 136 */
	tzz112 = tx89*tzz111; 		/* rule 137 */
	tzz122 = -tzz112; 		/* rule 138 */
	tx94 = tx39*tzz122; 		/* rule 139 */
	tzz115 = tx90*tx92; 		/* rule 140 */
	tzz116 = tx76*tzz115; 		/* rule 141 */
	tx95 = tx21*tzz116; 		/* rule 142 */
	DphiDx2 = tx93 + tx94 + tx95; 		/* rule 143 */
	dx2 = DePhi*DphiDx2; 		/* rule 144 */
	tx96 = tx20*tzz114; 		/* rule 145 */
	tx97 = tx38*tzz122; 		/* rule 146 */
	tx98 = tx20*tzz116; 		/* rule 147 */
	DphiDy2 = tx96 + tx97 + tx98; 		/* rule 148 */
	dy2 = DePhi*DphiDy2; 		/* rule 149 */
	tx99 = tx19*tzz114; 		/* rule 150 */
	tx100 = tx40*tzz122; 		/* rule 151 */
	tx101 = tx19*tzz116; 		/* rule 152 */
	DphiDz2 = tx100 + tx101 + tx99; 		/* rule 153 */
	dz2 = DePhi*DphiDz2; 		/* rule 154 */
	tzz113 = tx65*tx89; 		/* rule 155 */
	tzz121 = -tzz113; 		/* rule 156 */
	tx102 = tx39*tzz121; 		/* rule 157 */
	tx103 = tx39*tzz112; 		/* rule 158 */
	tx104 = -tx95; 		/* rule 159 */
	DphiDx3 = tx102 + tx103 + tx104; 		/* rule 160 */
	dx3 = DePhi*DphiDx3; 		/* rule 161 */
	tx105 = tx38*tzz121; 		/* rule 162 */
	tx106 = tx38*tzz112; 		/* rule 163 */
	tx107 = -tx98; 		/* rule 164 */
	DphiDy3 = tx105 + tx106 + tx107; 		/* rule 165 */
	dy3 = DePhi*DphiDy3; 		/* rule 166 */
	tx108 = -tx101; 		/* rule 167 */
	tx109 = tx40*tzz121; 		/* rule 168 */
	tx110 = tx40*tzz112; 		/* rule 169 */
	DphiDz3 = tx108 + tx109 + tx110; 		/* rule 170 */
	dz3 = DePhi*DphiDz3; 		/* rule 171 */
	DphiDx4 = tx39*tzz113; 		/* rule 172 */
	dx4 = DePhi*DphiDx4; 		/* rule 173 */
	DphiDy4 = tx38*tzz113; 		/* rule 174 */
	dy4 = DePhi*DphiDy4; 		/* rule 175 */
	DphiDz4 = tx40*tzz113; 		/* rule 176 */
	dz4 = DePhi*DphiDz4; 		/* rule 177 */
	fx1 = -dx1; 		/* rule 178 */
	fy1 = -dy1; 		/* rule 179 */
	fz1 = -dz1; 		/* rule 180 */
	fx2 = -dx2; 		/* rule 181 */
	fy2 = -dy2; 		/* rule 182 */
	fz2 = -dz2; 		/* rule 183 */
	fx3 = -dx3; 		/* rule 184 */
	fy3 = -dy3; 		/* rule 185 */
	fz3 = -dz3; 		/* rule 186 */
	fx4 = -dx4; 		/* rule 187 */
	fy4 = -dy4; 		/* rule 188 */
	fz4 = -dz4; 		/* rule 189 */
	}
/*]*/
