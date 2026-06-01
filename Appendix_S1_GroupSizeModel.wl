(* ::Package:: *)

(* ============================================================ *)
(* Appendix S1. Mathematica code for the group-size model       *)
(* ============================================================ *)
(* ----- FORMULATION NOTE ---------------------------------------
Both the natal-dispersal (d_n) and floater-joining (j_n) best
responses use the collective inclusive-fitness aggregator described
in the manuscript:
        Delta(n) = a*DI_out(n) + (1-a)*n*DI_in(n),
i.e. the insider term is summed over the n existing members. The
two cases differ only in the relatedness of the focal outsider to
the insiders (r_out = (1+(n-1) r_n)/n for a natal offspring, 0 for
a floater). j0 (n=0, no insiders) and j1 (n=1, where n*x = x) carry
no extra n factor.
-------------------------------------------------------------- *)
(*
This script implements the numerical procedure used in the manuscript.
It is a cleaned-up version of the original analysis code.

Model overview:
  - Maximum group size: nmax = 9
  - Groups of size n have frequency f[n], floaters have density fd
  - Mortality rates m[n], fecundity p[n], floater mortality mf
  - Dispersal probability d[n], joining probability j[n]
  - k = encounter rate between floaters and groups
  - a = "extent of out-group control" parameter
  - err = selection strength parameter (smaller = stronger selection)

Main function: run[{m, p, mf, k, err, a, d, j}, nIterations]
Returns: {final d, final j, {f with fd, r}, {f with fd, r, v with vf}}
*)

(* ============================================================ *)
(* PARAMETER SETTINGS *)
(* ============================================================ *)

nmax = 9;

(* Test/baseline parameters *)
mtest = Table[1, {n, 1, 9}];
ptest = Table[0.2 (n^1.5) ((9 - n)^0.75), {n, 1, 9}];
dtest = Table[If[n < 9, 0.5, 1], {n, 1, 9}];
jtest = Table[If[n < 9, 1, 0], {n, 0, 9}];

(* Suppress FindRoot warnings - these are normal for this model *)
Off[FindRoot::lstol];

(* ============================================================ *)
(* SOLUTION FUNCTIONS *)
(* ============================================================ *)

(* sol: Solve for demographic equilibrium and relatedness *)
sol[{m1_, m2_, m3_, m4_, m5_, m6_, m7_, m8_, m9_},
    {p1_, p2_, p3_, p4_, p5_, p6_, p7_, p8_, p9_},
    mf_, k_,
    {d1_, d2_, d3_, d4_, d5_, d6_, d7_, d8_, 1},
    {j0_, j1_, j2_, j3_, j4_, j5_, j6_, j7_, j8_, 0}] := (

  (* Solve demographic equilibrium *)
  feq = FindRoot[{
    (* f0 equation: flow in from f1 mortality, flow out from joining *)
    -f0 fd j0 k + f1 m1 == 0,
    (* f1 equation *)
    f0 fd j0 k - f1 fd j1 k - f1 m1 + 2 f2 m2 - (1 - d1) f1 p1 == 0,
    (* f2 equation *)
    f1 fd j1 k - f2 fd j2 k - 2 f2 m2 + 3 f3 m3 + (1 - d1) f1 p1 - (1 - d2) f2 p2 == 0,
    (* f3 equation *)
    f2 fd j2 k - f3 fd j3 k - 3 f3 m3 + 4 f4 m4 + (1 - d2) f2 p2 - (1 - d3) f3 p3 == 0,
    (* f4 equation *)
    f3 fd j3 k - f4 fd j4 k - 4 f4 m4 + 5 f5 m5 + (1 - d3) f3 p3 - (1 - d4) f4 p4 == 0,
    (* f5 equation *)
    f4 fd j4 k - f5 fd j5 k - 5 f5 m5 + 6 f6 m6 + (1 - d4) f4 p4 - (1 - d5) f5 p5 == 0,
    (* f6 equation *)
    f5 fd j5 k - f6 fd j6 k - 6 f6 m6 + 7 f7 m7 + (1 - d5) f5 p5 - (1 - d6) f6 p6 == 0,
    (* f7 equation *)
    f6 fd j6 k - f7 fd j7 k - 7 f7 m7 + 8 f8 m8 + (1 - d6) f6 p6 - (1 - d7) f7 p7 == 0,
    (* f8 equation *)
    f7 fd j7 k - f8 fd j8 k - 8 f8 m8 + 9 f9 m9 + (1 - d7) f7 p7 - (1 - d8) f8 p8 == 0,
    (* Normalization *)
    f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 == 1,
    (* Floater balance *)
    -fd (f0 j0 + f1 j1 + f2 j2 + f3 j3 + f4 j4 + f5 j5 + f6 j6 + f7 j7 + f8 j8) k -
     fd mf + d1 f1 p1 + d2 f2 p2 + d3 f3 p3 + d4 f4 p4 + d5 f5 p5 + d6 f6 p6 +
     d7 f7 p7 + d8 f8 p8 + f9 p9 == 0
  },
  {f0, 0}, {f1, 0.1}, {f2, 0.1}, {f3, 0.1}, {f4, 0.1},
  {f5, 0.1}, {f6, 0.1}, {f7, 0.1}, {f8, 0.1}, {f9, 0.2}, {fd, 1}];

  (* Solve relatedness equilibrium *)
  req = FindRoot[{
    r2 == (f1 (p1 - d1 p1) + 3 f3 m3 r3) / (3 f3 m3 + f1 (fd j1 k + p1 - d1 p1)),
    r3 == (f2 (fd j2 k r2 - (-1 + d2) p2 (1 + 2 r2)) + 12 f4 m4 r4) / (3 (4 f4 m4 + f2 (fd j2 k + p2 - d2 p2))),
    r4 == (f3 (3 fd j3 k r3 - (-1 + d3) p3 (1 + 5 r3)) + 30 f5 m5 r5) / (6 (5 f5 m5 + f3 (fd j3 k + p3 - d3 p3))),
    r5 == (f4 (6 fd j4 k r4 - (-1 + d4) p4 (1 + 9 r4)) + 60 f6 m6 r6) / (10 (6 f6 m6 + f4 (fd j4 k + p4 - d4 p4))),
    r6 == (f5 (10 fd j5 k r5 - (-1 + d5) p5 (1 + 14 r5)) + 105 f7 m7 r7) / (15 (7 f7 m7 + f5 (fd j5 k + p5 - d5 p5))),
    r7 == (f6 (15 fd j6 k r6 - (-1 + d6) p6 (1 + 20 r6)) + 168 f8 m8 r8) / (21 (8 f8 m8 + f6 (fd j6 k + p6 - d6 p6))),
    r8 == (f7 (21 fd j7 k r7 - (-1 + d7) p7 (1 + 27 r7)) + 252 f9 m9 r9) / (28 (9 f9 m9 + f7 (fd j7 k + p7 - d7 p7))),
    r9 == -((-28 fd j8 k r8 + (-1 + d8) p8 (1 + 35 r8)) / (36 (fd j8 k + p8 - d8 p8)))
  } /. feq,
  {r2, 0}, {r3, 0}, {r4, 0}, {r5, 0}, {r6, 0}, {r7, 0}, {r8, 0}, {r9, 0}];

  {{f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, fd} /. feq,
   {r2, r3, r4, r5, r6, r7, r8, r9} /. req}
);

(* fullsol: Solve for demographic equilibrium, relatedness, and reproductive values *)
fullsol[{m1_, m2_, m3_, m4_, m5_, m6_, m7_, m8_, m9_},
        {p1_, p2_, p3_, p4_, p5_, p6_, p7_, p8_, p9_},
        mf_, k_,
        {d1_, d2_, d3_, d4_, d5_, d6_, d7_, d8_, 1},
        {j0_, j1_, j2_, j3_, j4_, j5_, j6_, j7_, j8_, 0}] := (

  (* Solve demographic equilibrium *)
  feq = FindRoot[{
    -f0 fd j0 k + f1 m1 == 0,
    f0 fd j0 k - f1 fd j1 k - f1 m1 + 2 f2 m2 - (1 - d1) f1 p1 == 0,
    f1 fd j1 k - f2 fd j2 k - 2 f2 m2 + 3 f3 m3 + (1 - d1) f1 p1 - (1 - d2) f2 p2 == 0,
    f2 fd j2 k - f3 fd j3 k - 3 f3 m3 + 4 f4 m4 + (1 - d2) f2 p2 - (1 - d3) f3 p3 == 0,
    f3 fd j3 k - f4 fd j4 k - 4 f4 m4 + 5 f5 m5 + (1 - d3) f3 p3 - (1 - d4) f4 p4 == 0,
    f4 fd j4 k - f5 fd j5 k - 5 f5 m5 + 6 f6 m6 + (1 - d4) f4 p4 - (1 - d5) f5 p5 == 0,
    f5 fd j5 k - f6 fd j6 k - 6 f6 m6 + 7 f7 m7 + (1 - d5) f5 p5 - (1 - d6) f6 p6 == 0,
    f6 fd j6 k - f7 fd j7 k - 7 f7 m7 + 8 f8 m8 + (1 - d6) f6 p6 - (1 - d7) f7 p7 == 0,
    f7 fd j7 k - f8 fd j8 k - 8 f8 m8 + 9 f9 m9 + (1 - d7) f7 p7 - (1 - d8) f8 p8 == 0,
    f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 == 1,
    -fd (f0 j0 + f1 j1 + f2 j2 + f3 j3 + f4 j4 + f5 j5 + f6 j6 + f7 j7 + f8 j8) k -
     fd mf + d1 f1 p1 + d2 f2 p2 + d3 f3 p3 + d4 f4 p4 + d5 f5 p5 + d6 f6 p6 +
     d7 f7 p7 + d8 f8 p8 + f9 p9 == 0
  },
  {f0, 0}, {f1, 0.1}, {f2, 0.1}, {f3, 0.1}, {f4, 0.1},
  {f5, 0.1}, {f6, 0.1}, {f7, 0.1}, {f8, 0.1}, {f9, 0.2}, {fd, 1}];

  (* Solve relatedness equilibrium *)
  req = FindRoot[{
    r2 == (f1 (p1 - d1 p1) + 3 f3 m3 r3) / (3 f3 m3 + f1 (fd j1 k + p1 - d1 p1)),
    r3 == (f2 (fd j2 k r2 - (-1 + d2) p2 (1 + 2 r2)) + 12 f4 m4 r4) / (3 (4 f4 m4 + f2 (fd j2 k + p2 - d2 p2))),
    r4 == (f3 (3 fd j3 k r3 - (-1 + d3) p3 (1 + 5 r3)) + 30 f5 m5 r5) / (6 (5 f5 m5 + f3 (fd j3 k + p3 - d3 p3))),
    r5 == (f4 (6 fd j4 k r4 - (-1 + d4) p4 (1 + 9 r4)) + 60 f6 m6 r6) / (10 (6 f6 m6 + f4 (fd j4 k + p4 - d4 p4))),
    r6 == (f5 (10 fd j5 k r5 - (-1 + d5) p5 (1 + 14 r5)) + 105 f7 m7 r7) / (15 (7 f7 m7 + f5 (fd j5 k + p5 - d5 p5))),
    r7 == (f6 (15 fd j6 k r6 - (-1 + d6) p6 (1 + 20 r6)) + 168 f8 m8 r8) / (21 (8 f8 m8 + f6 (fd j6 k + p6 - d6 p6))),
    r8 == (f7 (21 fd j7 k r7 - (-1 + d7) p7 (1 + 27 r7)) + 252 f9 m9 r9) / (28 (9 f9 m9 + f7 (fd j7 k + p7 - d7 p7))),
    r9 == -((-28 fd j8 k r8 + (-1 + d8) p8 (1 + 35 r8)) / (36 (fd j8 k + p8 - d8 p8)))
  } /. feq,
  {r2, 0.5}, {r3, 0.5}, {r4, 0.5}, {r5, 0.5}, {r6, 0.5}, {r7, 0.5}, {r8, 0.5}, {r9, 0.5}];

  (* Solve reproductive value equilibrium *)
  veq = FindRoot[{
    -m1 v1 + fd j1 k (-v1 + v2) + (1 - d1) p1 (-v1 + 2 v2) + d1 p1 vf == 0,
    m2 (v1 - v2) - m2 v2 + fd j2 k (-v2 + v3) + (1 - d2) p2 (-v2 + (3 v3)/2) + (d2 p2 vf)/2 == 0,
    2 m3 (v2 - v3) - m3 v3 + fd j3 k (-v3 + v4) + (1 - d3) p3 (-v3 + (4 v4)/3) + (d3 p3 vf)/3 == 0,
    3 m4 (v3 - v4) - m4 v4 + fd j4 k (-v4 + v5) + (1 - d4) p4 (-v4 + (5 v5)/4) + (d4 p4 vf)/4 == 0,
    4 m5 (v4 - v5) - m5 v5 + fd j5 k (-v5 + v6) + (1 - d5) p5 (-v5 + (6 v6)/5) + (d5 p5 vf)/5 == 0,
    5 m6 (v5 - v6) - m6 v6 + fd j6 k (-v6 + v7) + (1 - d6) p6 (-v6 + (7 v7)/6) + (d6 p6 vf)/6 == 0,
    6 m7 (v6 - v7) - m7 v7 + fd j7 k (-v7 + v8) + (1 - d7) p7 (-v7 + (8 v8)/7) + (d7 p7 vf)/7 == 0,
    7 m8 (v7 - v8) - m8 v8 + fd j8 k (-v8 + v9) + (1 - d8) p8 (-v8 + (9 v9)/8) + (d8 p8 vf)/8 == 0,
    (* Normalization: average reproductive value = 1 *)
    (f1 v1 + 2 f2 v2 + 3 f3 v3 + 4 f4 v4 + 5 f5 v5 + 6 f6 v6 + 7 f7 v7 + 8 f8 v8 + 9 f9 v9 + fd vf) /
    (f1 + 2 f2 + 3 f3 + 4 f4 + 5 f5 + 6 f6 + 7 f7 + 8 f8 + 9 f9 + fd) == 1,
    (* Floater reproductive value equation *)
    f0 j0 k (v1 - vf) + f1 j1 k (v2 - vf) + f2 j2 k (v3 - vf) + f3 j3 k (v4 - vf) +
    f4 j4 k (v5 - vf) + f5 j5 k (v6 - vf) + f6 j6 k (v7 - vf) + f7 j7 k (v8 - vf) +
    f8 j8 k (v9 - vf) - mf vf == 0
  } /. feq /. req,
  {v1, 1}, {v2, 1}, {v3, 1}, {v4, 1}, {v5, 1}, {v6, 1}, {v7, 1}, {v8, 1}, {v9, 1}, {vf, 1}];

  {{f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, fd} /. feq,
   {r2, r3, r4, r5, r6, r7, r8, r9} /. req,
   {v1, v2, v3, v4, v5, v6, v7, v8, v9, vf} /. veq}
);

(* ============================================================ *)
(* UPDATE FUNCTION (MAIN ITERATION STEP) *)
(* ============================================================ *)

upd[{{m1_, m2_, m3_, m4_, m5_, m6_, m7_, m8_, m9_},
     {p1_, p2_, p3_, p4_, p5_, p6_, p7_, p8_, p9_},
     mf_, k_, err_, a_,
     {d1_, d2_, d3_, d4_, d5_, d6_, d7_, d8_, 1},
     {j0_, j1_, j2_, j3_, j4_, j5_, j6_, j7_, j8_, 0}}] := (

  (* Step 1: Solve demographic equilibrium *)
  feq = FindRoot[{
    -f0 fd j0 k + f1 m1 == 0,
    f0 fd j0 k - f1 fd j1 k - f1 m1 + 2 f2 m2 - (1 - d1) f1 p1 == 0,
    f1 fd j1 k - f2 fd j2 k - 2 f2 m2 + 3 f3 m3 + (1 - d1) f1 p1 - (1 - d2) f2 p2 == 0,
    f2 fd j2 k - f3 fd j3 k - 3 f3 m3 + 4 f4 m4 + (1 - d2) f2 p2 - (1 - d3) f3 p3 == 0,
    f3 fd j3 k - f4 fd j4 k - 4 f4 m4 + 5 f5 m5 + (1 - d3) f3 p3 - (1 - d4) f4 p4 == 0,
    f4 fd j4 k - f5 fd j5 k - 5 f5 m5 + 6 f6 m6 + (1 - d4) f4 p4 - (1 - d5) f5 p5 == 0,
    f5 fd j5 k - f6 fd j6 k - 6 f6 m6 + 7 f7 m7 + (1 - d5) f5 p5 - (1 - d6) f6 p6 == 0,
    f6 fd j6 k - f7 fd j7 k - 7 f7 m7 + 8 f8 m8 + (1 - d6) f6 p6 - (1 - d7) f7 p7 == 0,
    f7 fd j7 k - f8 fd j8 k - 8 f8 m8 + 9 f9 m9 + (1 - d7) f7 p7 - (1 - d8) f8 p8 == 0,
    f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 == 1,
    -fd (f0 j0 + f1 j1 + f2 j2 + f3 j3 + f4 j4 + f5 j5 + f6 j6 + f7 j7 + f8 j8) k -
     fd mf + d1 f1 p1 + d2 f2 p2 + d3 f3 p3 + d4 f4 p4 + d5 f5 p5 + d6 f6 p6 +
     d7 f7 p7 + d8 f8 p8 + f9 p9 == 0
  },
  {f0, 0}, {f1, 0.1}, {f2, 0.1}, {f3, 0.1}, {f4, 0.1},
  {f5, 0.1}, {f6, 0.1}, {f7, 0.1}, {f8, 0.1}, {f9, 0.2}, {fd, 1}];

  (* Step 2: Solve relatedness equilibrium *)
  req = FindRoot[{
    r2 == (f1 (p1 - d1 p1) + 3 f3 m3 r3) / (3 f3 m3 + f1 (fd j1 k + p1 - d1 p1)),
    r3 == (f2 (fd j2 k r2 - (-1 + d2) p2 (1 + 2 r2)) + 12 f4 m4 r4) / (3 (4 f4 m4 + f2 (fd j2 k + p2 - d2 p2))),
    r4 == (f3 (3 fd j3 k r3 - (-1 + d3) p3 (1 + 5 r3)) + 30 f5 m5 r5) / (6 (5 f5 m5 + f3 (fd j3 k + p3 - d3 p3))),
    r5 == (f4 (6 fd j4 k r4 - (-1 + d4) p4 (1 + 9 r4)) + 60 f6 m6 r6) / (10 (6 f6 m6 + f4 (fd j4 k + p4 - d4 p4))),
    r6 == (f5 (10 fd j5 k r5 - (-1 + d5) p5 (1 + 14 r5)) + 105 f7 m7 r7) / (15 (7 f7 m7 + f5 (fd j5 k + p5 - d5 p5))),
    r7 == (f6 (15 fd j6 k r6 - (-1 + d6) p6 (1 + 20 r6)) + 168 f8 m8 r8) / (21 (8 f8 m8 + f6 (fd j6 k + p6 - d6 p6))),
    r8 == (f7 (21 fd j7 k r7 - (-1 + d7) p7 (1 + 27 r7)) + 252 f9 m9 r9) / (28 (9 f9 m9 + f7 (fd j7 k + p7 - d7 p7))),
    r9 == -((-28 fd j8 k r8 + (-1 + d8) p8 (1 + 35 r8)) / (36 (fd j8 k + p8 - d8 p8)))
  } /. feq,
  {r2, 0}, {r3, 0}, {r4, 0}, {r5, 0}, {r6, 0}, {r7, 0}, {r8, 0}, {r9, 0}];

  (* Step 3: Solve reproductive value equilibrium *)
  veq = FindRoot[{
    -m1 v1 + fd j1 k (-v1 + v2) + (1 - d1) p1 (-v1 + 2 v2) + d1 p1 vf == 0,
    m2 (v1 - v2) - m2 v2 + fd j2 k (-v2 + v3) + (1 - d2) p2 (-v2 + (3 v3)/2) + (d2 p2 vf)/2 == 0,
    2 m3 (v2 - v3) - m3 v3 + fd j3 k (-v3 + v4) + (1 - d3) p3 (-v3 + (4 v4)/3) + (d3 p3 vf)/3 == 0,
    3 m4 (v3 - v4) - m4 v4 + fd j4 k (-v4 + v5) + (1 - d4) p4 (-v4 + (5 v5)/4) + (d4 p4 vf)/4 == 0,
    4 m5 (v4 - v5) - m5 v5 + fd j5 k (-v5 + v6) + (1 - d5) p5 (-v5 + (6 v6)/5) + (d5 p5 vf)/5 == 0,
    5 m6 (v5 - v6) - m6 v6 + fd j6 k (-v6 + v7) + (1 - d6) p6 (-v6 + (7 v7)/6) + (d6 p6 vf)/6 == 0,
    6 m7 (v6 - v7) - m7 v7 + fd j7 k (-v7 + v8) + (1 - d7) p7 (-v7 + (8 v8)/7) + (d7 p7 vf)/7 == 0,
    7 m8 (v7 - v8) - m8 v8 + fd j8 k (-v8 + v9) + (1 - d8) p8 (-v8 + (9 v9)/8) + (d8 p8 vf)/8 == 0,
    (f1 v1 + 2 f2 v2 + 3 f3 v3 + 4 f4 v4 + 5 f5 v5 + 6 f6 v6 + 7 f7 v7 + 8 f8 v8 + 9 f9 v9 + fd vf) /
    (f1 + 2 f2 + 3 f3 + 4 f4 + 5 f5 + 6 f6 + 7 f7 + 8 f8 + 9 f9 + fd) == 1,
    f0 j0 k (v1 - vf) + f1 j1 k (v2 - vf) + f2 j2 k (v3 - vf) + f3 j3 k (v4 - vf) +
    f4 j4 k (v5 - vf) + f5 j5 k (v6 - vf) + f6 j6 k (v7 - vf) + f7 j7 k (v8 - vf) +
    f8 j8 k (v9 - vf) - mf vf == 0
  } /. feq,
  {v1, 1}, {v2, 1}, {v3, 1}, {v4, 1}, {v5, 1}, {v6, 1}, {v7, 1}, {v8, 1}, {v9, 1}, {vf, 1}];

  (* Step 4: Calculate best-response strategies using Tanh function *)
  (* br[[1]] = dispersal probabilities, br[[2]] = joining probabilities *)
  br = {{
    (* d1: dispersal from groups of size 1 *)
    (1 + Tanh[(v1 - 2 v2 + vf)/err])/2,
    (* d2 *)
    (1 + Tanh[(-((-2 + a) (1 + r2) v2) + (-3 + a - 3 r2 + 2 a r2) v3 + (1 + r2 - a r2) vf)/err])/2,
    (* d3 *)
    (1 + Tanh[(-((-1 + a) (1 + 2 r3) (3 v3 - 4 v4 + vf)) + a (v3 + 2 r3 v3 - 2 (1 + r3) v4 + vf))/err])/2,
    (* d4 *)
    (1 + Tanh[(-((-1 + a) (1 + 3 r4) (4 v4 - 5 v5 + vf)) + a (v4 + 3 r4 v4 - (2 + 3 r4) v5 + vf))/err])/2,
    (* d5 *)
    (1 + Tanh[(-((-1 + a) (1 + 4 r5) (5 v5 - 6 v6 + vf)) + a (v5 + 4 r5 v5 - 2 (1 + 2 r5) v6 + vf))/err])/2,
    (* d6 *)
    (1 + Tanh[(-((-1 + a) (1 + 5 r6) (6 v6 - 7 v7 + vf)) + a (v6 + 5 r6 v6 - (2 + 5 r6) v7 + vf))/err])/2,
    (* d7 *)
    (1 + Tanh[(-((-1 + a) (1 + 6 r7) (7 v7 - 8 v8 + vf)) + a (v7 + 6 r7 v7 - 2 (1 + 3 r7) v8 + vf))/err])/2,
    (* d8 *)
    (1 + Tanh[(-((-1 + a) (1 + 7 r8) (8 v8 - 9 v9 + vf)) + a (v8 + 7 r8 v8 - (2 + 7 r8) v9 + vf))/err])/2
  }, {
    (* j0: joining into empty territories (n=0, no insiders, unaffected) *)
    (1 + Tanh[(v1 - vf)/err])/2,
    (* j1 (n=1: 1*x = x, identical to per-capita) *)
    (1 + Tanh[((-1 + a) v1 + v2 - a vf)/err])/2,
    (* j2  -- collective: insider term x n=2 *)
    (1 + Tanh[(2 (-1 + a) (1 + r2) (v2 - v3) + a (v3 - vf))/err])/2,
    (* j3  -- collective: insider term x n=3 *)
    (1 + Tanh[(3 (-1 + a) (1 + 2 r3) (v3 - v4) + a (v4 - vf))/err])/2,
    (* j4  -- collective: insider term x n=4 *)
    (1 + Tanh[(4 (-1 + a) (1 + 3 r4) (v4 - v5) + a (v5 - vf))/err])/2,
    (* j5  -- collective: insider term x n=5 *)
    (1 + Tanh[(5 (-1 + a) (1 + 4 r5) (v5 - v6) + a (v6 - vf))/err])/2,
    (* j6  -- collective: insider term x n=6 *)
    (1 + Tanh[(6 (-1 + a) (1 + 5 r6) (v6 - v7) + a (v7 - vf))/err])/2,
    (* j7  -- collective: insider term x n=7 *)
    (1 + Tanh[(7 (-1 + a) (1 + 6 r7) (v7 - v8) + a (v8 - vf))/err])/2,
    (* j8  -- collective: insider term x n=8 *)
    (1 + Tanh[(8 (-1 + a) (1 + 7 r8) (v8 - v9) + a (v9 - vf))/err])/2
  }} /. feq /. req /. veq;

  (* Step 5: Update strategies (weighted average: 90% old + 10% new) *)
  {{m1, m2, m3, m4, m5, m6, m7, m8, m9},
   {p1, p2, p3, p4, p5, p6, p7, p8, p9},
   mf, k, err, a,
   Append[0.9 {d1, d2, d3, d4, d5, d6, d7, d8} + 0.1 br[[1]], 1],
   Append[0.9 {j0, j1, j2, j3, j4, j5, j6, j7, j8} + 0.1 br[[2]], 0]}
);

(* ============================================================ *)
(* MAIN RUN FUNCTION *)
(* ============================================================ *)

run[{{m1_, m2_, m3_, m4_, m5_, m6_, m7_, m8_, m9_},
     {p1_, p2_, p3_, p4_, p5_, p6_, p7_, p8_, p9_},
     mf_, k_, err_, a_,
     {d1_, d2_, d3_, d4_, d5_, d6_, d7_, d8_, 1},
     {j0_, j1_, j2_, j3_, j4_, j5_, j6_, j7_, j8_, 0}},
    nit_] := (

  (* Iterate upd function nit times *)
  dum = Nest[upd,
    {{m1, m2, m3, m4, m5, m6, m7, m8, m9},
     {p1, p2, p3, p4, p5, p6, p7, p8, p9},
     mf, k, err, a,
     {d1, d2, d3, d4, d5, d6, d7, d8, 1},
     {j0, j1, j2, j3, j4, j5, j6, j7, j8, 0}},
    nit];

  (* Return final results *)
  {dum[[-2]],   (* final d *)
   dum[[-1]],   (* final j *)
   sol[{m1, m2, m3, m4, m5, m6, m7, m8, m9},
       {p1, p2, p3, p4, p5, p6, p7, p8, p9},
       mf, k, dum[[-2]], dum[[-1]]],
   fullsol[{m1, m2, m3, m4, m5, m6, m7, m8, m9},
           {p1, p2, p3, p4, p5, p6, p7, p8, p9},
           mf, k, dum[[-2]], dum[[-1]]]}
);

(* ============================================================ *)
(* EXAMPLE USAGE *)
(* ============================================================ *)
(*
To run with default parameters:

res = run[{mtest, ptest, 1, 7, 0.2, 0.1, dtest, jtest}, 1000];

res[[1]] gives final dispersal probabilities d
res[[2]] gives final joining probabilities j
res[[3]] gives {group frequencies with floater density, relatedness values}
res[[4]] gives {group frequencies with floater density, relatedness values, reproductive values with floater RV}

Parameters:
  mtest = mortality rates (all 1)
  ptest = fecundity rates
  1 = floater mortality (mf)
  7 = encounter rate (k)
  0.2 = selection strength (err); smaller = stronger selection
  0.1 = out-group control parameter (a); 0.1 = insider control, 0.9 = outsider control
  dtest = initial dispersal probabilities
  jtest = initial joining probabilities
  1000 = number of iterations
*)
