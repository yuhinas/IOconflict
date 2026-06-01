(* ::Package:: *)

(* ::Title:: *)
(*Appendix S2. Figure-generation script (manuscript style)*)


(*
This script produces the figures used in the manuscript, styled to
match the appearance of the published figures (orange bars, blue
secondary line, four-marker palette for sweeps, italic axis labels,
panel labels in upper-left, "floater" tick at x=nmax+1).
*)

If[$FrontEnd === Null,
  If[StringQ@$InputFileName, SetDirectory@DirectoryName@$InputFileName],
  SetDirectory[NotebookDirectory[]]
];

(* Load the group-size model (Appendix S1) *)
Get["Appendix_S1_GroupSizeModel.wl"];

(* ============================================================ *)
(* SETTINGS *)
(* ============================================================ *)

QuickMode = False;
nIterations = If[QuickMode, 200, 1000];

Print["QuickMode = ", QuickMode, ", nIterations = ", nIterations];

(* ============================================================ *)
(* STYLE CONSTANTS                                              *)
(* ============================================================ *)

(* Sweep palette: smallest -> orange filled, ..., largest -> black open *)
sweepColors  = {RGBColor[1.0, 0.55, 0.05], GrayLevel[0.5],
                RGBColor[0.55, 0.25, 0.55], Black};
sweepMarkers = {{"\[FilledCircle]",       11},
                {"\[FilledSquare]",       11},
                {"\[FilledUpTriangle]",   12},
                {"\[EmptyCircle]",        11}};

(* Bar fill (Fig 2, Fig 5) and Fig 2 secondary line color *)
barColor = RGBColor[1.0, 0.72, 0.30];
lineColor = RGBColor[0.30, 0.45, 0.75];

(* Italic math helper *)
mi[s_] := Style[s, Italic, FontFamily -> "Times"];

(* Sweep variable symbols (italic, with subscript for m_f) *)
mfSym = Subscript[Style["m", Italic, FontFamily -> "Times"],
                  Style["f", Italic, FontFamily -> "Times"]];
kSym  = Style["k", Italic, FontFamily -> "Times"];
gSym  = Style["g", Italic, FontFamily -> "Times"];

(* Number format: integers as "5", reals as "0.8"/"1.0"/"1.5" *)
numFormat[v_] := Switch[Head[v],
  Integer, ToString[v],
  Real,    ToString[NumberForm[v, {3, 1}]],
  _,       ToString[v]
];

(* Standard frame options *)
frameOpts = Sequence[
  Frame -> True,
  FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
  LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 11],
  ImageSize -> 320
];

(* Panel label inside plot area at upper-left (very top, so it sits
   above an upper-left legend when stacked). *)
panelLabel[ch_String] := Epilog -> {
  Inset[Style[ch, FontFamily -> "Times", FontSize -> 13],
        Scaled[{0.06, 0.97}], {Left, Top}]
};

(* Italic title for baseline panels *)
titleStr[label_String, aVal_] :=
  Row[{label, " (", mi["a"], " = ", aVal, ")"}];

(* Common x-axis labels *)
xLabelGroupSize = Row[{"Group size (", mi["n"], ")"}];

(* X-tick spec ending with "floater" at x=nmax+1 *)
floaterXTicks[] := Join[
  Table[{n, ToString[n]}, {n, 0, nmax, 2}],
  {{nmax + 1, "floater"}}
];

(* Plain x-ticks 0..nmax *)
plainXTicks[] := Table[{n, ToString[n]}, {n, 0, nmax}];

(* ============================================================ *)
(* HELPER FUNCTIONS                                              *)
(* ============================================================ *)

GroupSizeFrequencies[fWithFloater_List] := Drop[Drop[fWithFloater, 1], -1];

(* Export both PDF and SVG. SVG keeps text as standard Unicode for
   reliable editing in Illustrator / Inkscape / Affinity. *)
ExportPDF[file_String, graphic_] := (
  Export[file, graphic, "PDF"];
  Export[StringReplace[file, ".pdf" -> ".svg"], graphic, "SVG"];
);

WeightedCorrelation[x_List, y_List, w_List] :=
  Module[{w0 = Total[w], mx, my, cov, sx, sy},
    mx = Total[w x]/w0;
    my = Total[w y]/w0;
    cov = Total[w (x - mx) (y - my)]/w0;
    sx = Sqrt[Total[w (x - mx)^2]/w0];
    sy = Sqrt[Total[w (y - my)^2]/w0];
    If[sx == 0 || sy == 0, 0, cov/(sx sy)]
  ];

(* ============================================================ *)
(* CACHE SYSTEM                                                  *)
(* ============================================================ *)

ResultCache = <||>;

RunModelCached[mf_, k_, err_, a_, pVec_: None] := Module[{key, pUsed, res},
  pUsed = If[pVec === None, ptest, pVec];
  key = Hash[{mtest, pUsed, mf, k, err, a, dtest, jtest, nIterations}];
  If[KeyExistsQ[ResultCache, key],
    ResultCache[key],
    res = run[{mtest, pUsed, mf, k, err, a, dtest, jtest}, nIterations];
    ResultCache[key] = res;
    res
  ]
];

(* ============================================================ *)
(* PARAMETERS                                                   *)
(* ============================================================ *)

aInsider = 0.2;
aOutsider = 0.8;
mfVals = {0.8, 1.0, 1.2, 1.5};
kVals = {5, 7, 10, 15};
pScaleVals = {0.9, 1.0, 1.2, 1.5};  (* labelled "g" in manuscript *)
aVals = Range[0, 1, 0.1];

mfDefault = 1;
kDefault = 7;
errDefault = 0.05;

(* ============================================================ *)
(* PRE-COMPUTE ALL SCENARIOS                                     *)
(* ============================================================ *)

Print[""];
Print["Pre-computing all scenarios..."];

allScenarios = {};

AppendTo[allScenarios, {mfDefault, kDefault, errDefault, aInsider, None}];
AppendTo[allScenarios, {mfDefault, kDefault, errDefault, aOutsider, None}];

Do[
  AppendTo[allScenarios, {mf, kDefault, errDefault, aInsider, None}];
  AppendTo[allScenarios, {mf, kDefault, errDefault, aOutsider, None}];
, {mf, mfVals}];

Do[
  AppendTo[allScenarios, {mfDefault, kval, errDefault, aInsider, None}];
  AppendTo[allScenarios, {mfDefault, kval, errDefault, aOutsider, None}];
, {kval, kVals}];

Do[
  AppendTo[allScenarios, {mfDefault, kDefault, errDefault, aInsider, pScale * ptest}];
  AppendTo[allScenarios, {mfDefault, kDefault, errDefault, aOutsider, pScale * ptest}];
, {pScale, pScaleVals}];

Do[
  AppendTo[allScenarios, {mfDefault, kDefault, errDefault, a, None}];
, {a, aVals}];

allScenarios = DeleteDuplicates[allScenarios];

Print["Total unique scenarios: ", Length[allScenarios]];

Do[
  Print["  Running scenario ", i, "/", Length[allScenarios], "..."];
  RunModelCached @@ allScenarios[[i]];
, {i, Length[allScenarios]}];

Print["Pre-computation complete!"];
Print[""];

(* ============================================================ *)
(* FIGURE 2  -- group-size distribution + per-capita productivity *)
(* ============================================================ *)

Fig2Panel[res_, aVal_, label_String, panelCh_String] :=
  Module[{freq, perCap, maxL, maxR, scale, rescaledLine, rightTicks},
    freq = GroupSizeFrequencies[res[[4, 1]]];
    perCap = Table[ptest[[n]]/n, {n, 1, nmax}];
    maxL = Max[freq] * 1.15;
    maxR = Max[perCap] * 1.15;
    scale = maxL / maxR;
    rescaledLine = Table[{n, perCap[[n]] * scale}, {n, 1, nmax}];
    rightTicks = Table[{v * scale, ToString[NumberForm[N[v], {2, 1}]]},
                       {v, 0, Floor[maxR * 10]/10, 0.2}];
    Show[
      BarChart[freq,
        ChartStyle -> Directive[EdgeForm[Directive[Black, AbsoluteThickness[0.4]]], barColor],
        ChartLayout -> "Grouped",
        PlotRange -> {{0.4, nmax + 0.6}, {0, maxL}},
        Frame -> True,
        FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
        LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 11],
        FrameTicks -> {{Automatic, rightTicks}, {plainXTicks[], None}},
        FrameLabel -> {{"Frequency", "per capita productivity"},
                       {xLabelGroupSize, titleStr[label, aVal]}},
        ImageSize -> 380
      ],
      ListLinePlot[rescaledLine,
        PlotStyle -> Directive[lineColor, AbsoluteThickness[1.6]],
        PlotMarkers -> {Graphics[{lineColor, AbsoluteThickness[1.6], Circle[{0,0},1]}], 9}
      ],
      panelLabel[panelCh]
    ]
  ];

MakeFig2[] := Module[{resIn, resOut, pIn, pOut},
  Print["Generating Figure 2..."];
  resIn = RunModelCached[mfDefault, kDefault, errDefault, aInsider];
  resOut = RunModelCached[mfDefault, kDefault, errDefault, aOutsider];
  pIn  = Fig2Panel[resIn,  aInsider,  "Insider-control",  "a"];
  pOut = Fig2Panel[resOut, aOutsider, "Outsider-control", "b"];
  ExportPDF["Fig2_groupSizeDistribution.pdf", GraphicsRow[{pIn, pOut}, Spacings -> 0.5, ImageSize -> 760]];
  Print["  Done."];
];

(* ============================================================ *)
(* SWEEP MULTI-LINE PLOTS (used by Fig 3, Fig 4, Fig 6)         *)
(* Plot a list of curves with shared styling.                    *)
(* curves: list of {{x1,y1}, {x2,y2}, ...} per series.           *)
(* ============================================================ *)

(* SweepPlot builds the data plot only. Legend is added by each Panel
   function via Epilog Inset (gives full control over position; the
   built-in PlotLegends Placed[..., {x,y}] is unreliable when the plot
   has a right-side dual y-axis). *)
SweepPlot[curves_, opts___] :=
  ListLinePlot[curves,
    PlotStyle -> Map[Directive[#, AbsoluteThickness[1.3]] &, sweepColors],
    PlotMarkers -> MapIndexed[{sweepMarkers[[#2[[1]], 1]], sweepMarkers[[#2[[1]], 2]]} &, sweepColors],
    Frame -> True,
    FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
    LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 11],
    opts
  ];

(* Build a reusable legend object for sweep plots *)
makeLegend[labels_List] := LineLegend[sweepColors, labels,
  LegendMarkers -> sweepMarkers,
  LabelStyle -> Directive[FontFamily -> "Times", FontSize -> 10],
  LegendMargins -> 0,
  Background -> Directive[White, Opacity[0.9]]
];

(* Build an Epilog that contains both the panel label (upper-left) and
   the legend (custom position). pos and anchor specify legend placement
   in plot-scaled coordinates. *)
panelAndLegendEpilog[ch_String, labels_List, pos_:Scaled[{0.05, 0.91}], anchor_:{Left, Top}] :=
  Epilog -> {
    Inset[Style[ch, FontFamily -> "Times", FontSize -> 13],
          Scaled[{0.06, 0.97}], {Left, Top}],
    Inset[makeLegend[labels], pos, anchor]
  };

(* Compose a series including the floater point at x=nmax+1 *)
WithFloater[yVec_List, floaterY_] :=
  Append[Table[{n, yVec[[n]]}, {n, 1, Length[yVec]}], {nmax + 1, floaterY}];

(* ============================================================ *)
(* FIGURE 3  -- f_n sweeps with floater density (right axis)    *)
(* ============================================================ *)

(* Reviewer request: overlay per-capita productivity p(n)/n as in Fig 2.
   Right axis is already taken by floater density, so we draw a single
   light-gray dashed background curve rescaled to the left axis (axis
   suppressed). Baseline p(n)/n shape is identical across all panels
   regardless of m_f, k, or g, so one curve per panel suffices.
   Drawn via Prolog on the SweepPlot so it sits visually BEHIND the
   colored sweep curves and the floater points. *)
Fig3Panel[results_, aVal_, sweepLabel_String, sweepValues_, sweepVar_,
          panelCh_String, controlLabel_String] :=
  Module[{freqCurves, floaterVals, allY, scale, leftMax, rightMax,
          rightTicks, scaledFloaterPoints, legendLabels, p1, p2,
          perCap, perCapMax, perCapDashedLine},
    freqCurves = Table[
      Module[{r, fWF, freq, fd},
        r = results[[i]];
        fWF = r[[4, 1]];
        freq = GroupSizeFrequencies[fWF];
        fd = Last[fWF];
        WithFloater[freq, fd]  (* show floater density at x=nmax+1, but rescaled below *)
      ], {i, Length[results]}];
    (* split freq portion vs floater point *)
    freqCurves = Table[
      Module[{r, fWF, freq},
        r = results[[i]];
        fWF = r[[4, 1]];
        freq = GroupSizeFrequencies[fWF];
        Table[{n, freq[[n]]}, {n, 1, nmax}]
      ], {i, Length[results]}];
    floaterVals = Table[Last[results[[i, 4, 1]]], {i, Length[results]}];
    (* 1.6x headroom so the upper-left legend doesn't sit on data *)
    leftMax = 1.6 * Max[Flatten[freqCurves[[All, All, 2]]]];
    rightMax = 1.6 * Max[floaterVals];
    If[rightMax <= 0, rightMax = 1];
    scale = leftMax / rightMax;
    rightTicks = Table[{v * scale, ToString[NumberForm[N[v], {2, 1}]]},
                       {v, 0, Floor[rightMax * 10]/10, Max[0.2, Round[rightMax/5, 0.1]]}];
    scaledFloaterPoints = Table[{nmax + 1, floaterVals[[i]] * scale}, {i, Length[floaterVals]}];
    legendLabels = Table[Row[{sweepVar, " = ", numFormat[sweepValues[[i]]]}], {i, Length[sweepValues]}];
    (* Baseline per-capita productivity, rescaled to left axis. The 0.85
       factor leaves headroom under leftMax so the line does not collide
       with the upper-left legend. *)
    perCap = Table[ptest[[n]]/n, {n, 1, nmax}];
    perCapMax = Max[perCap];
    perCapDashedLine = Table[{n, perCap[[n]] * (leftMax / perCapMax) * 0.85},
                             {n, 1, nmax}];
    Show[
      SweepPlot[freqCurves,
        PlotRange -> {{0, nmax + 1.5}, {0, leftMax}},
        FrameTicks -> {{Automatic, rightTicks}, {floaterXTicks[], None}},
        FrameLabel -> {{"Frequency of group sizes", "Floater density"},
                       {xLabelGroupSize, If[panelCh === "a" || panelCh === "d",
                          titleStr[controlLabel, aVal], None]}},
        Prolog -> {GrayLevel[0.55], AbsoluteThickness[1.0],
                   Dashing[{Small, Small}], Line[perCapDashedLine]},
        ImageSize -> 360
      ],
      Graphics[
        MapIndexed[
          {sweepColors[[#2[[1]]]], PointSize[0.025], Point[#1]} &,
          scaledFloaterPoints
        ]
      ],
      panelAndLegendEpilog[panelCh, legendLabels]
    ]
  ];

MakeFig3[] := Module[{rowMf, rowK, rowG, grid},
  Print["Generating Figure 3..."];

  rowMf = {
    Fig3Panel[
      Table[RunModelCached[mf, kDefault, errDefault, aInsider], {mf, mfVals}],
      aInsider, "m_f", mfVals, mfSym, "a", "Insider-control"],
    Fig3Panel[
      Table[RunModelCached[mf, kDefault, errDefault, aOutsider], {mf, mfVals}],
      aOutsider, "m_f", mfVals, mfSym, "d", "Outsider-control"]
  };
  rowK = {
    Fig3Panel[
      Table[RunModelCached[mfDefault, kval, errDefault, aInsider], {kval, kVals}],
      aInsider, "k", kVals, kSym, "b", "Insider-control"],
    Fig3Panel[
      Table[RunModelCached[mfDefault, kval, errDefault, aOutsider], {kval, kVals}],
      aOutsider, "k", kVals, kSym, "e", "Outsider-control"]
  };
  rowG = {
    Fig3Panel[
      Table[RunModelCached[mfDefault, kDefault, errDefault, aInsider, pScale * ptest], {pScale, pScaleVals}],
      aInsider, "g", pScaleVals, gSym, "c", "Insider-control"],
    Fig3Panel[
      Table[RunModelCached[mfDefault, kDefault, errDefault, aOutsider, pScale * ptest], {pScale, pScaleVals}],
      aOutsider, "g", pScaleVals, gSym, "f", "Outsider-control"]
  };

  grid = GraphicsGrid[{rowMf, rowK, rowG}, Spacings -> {0.1, 0.1}, ImageSize -> 760];
  ExportPDF["Fig3_groupSize_and_floaterDensity.pdf", grid];
  Print["  Done."];
];

(* ============================================================ *)
(* FIGURE 4  -- reproductive values                              *)
(* ============================================================ *)

Fig4Panel[results_, aVal_, sweepValues_, sweepVar_,
          panelCh_String, controlLabel_String] :=
  Module[{curves, floaterPoints, legendLabels, yMin, yMax, allY},
    (* Group-size curves: n=1..nmax only. Floater is plotted separately
       (no connecting line between n=nmax and the floater point, since
       it is a discrete category, not the next integer in the sweep). *)
    curves = Table[
      Module[{r, v},
        r = results[[i]];
        v = r[[4, 3, 1 ;; nmax]];
        Table[{n, v[[n]]}, {n, 1, nmax}]
      ], {i, Length[results]}];
    floaterPoints = Table[{nmax + 1, results[[i, 4, 3, -1]]}, {i, Length[results]}];
    allY = Join[Flatten[curves[[All, All, 2]]], floaterPoints[[All, 2]]];
    yMin = Min[allY] - 0.1;
    (* Add ~40% headroom above data so the upper-left legend doesn't
       sit on top of the curves *)
    yMax = Max[allY] + 0.45 * (Max[allY] - (yMin + 0.1));
    legendLabels = Table[Row[{sweepVar, " = ", numFormat[sweepValues[[i]]]}], {i, Length[sweepValues]}];
    Show[
      SweepPlot[curves,
        PlotRange -> {{0, nmax + 1.5}, {Max[0, yMin], yMax}},
        FrameTicks -> {{Automatic, None}, {floaterXTicks[], None}},
        FrameLabel -> {{"Reproductive values", None},
                       {xLabelGroupSize, If[panelCh === "a" || panelCh === "d",
                          titleStr[controlLabel, aVal], None]}},
        ImageSize -> 360
      ],
      (* Disconnected floater markers, same shape/color as curve markers *)
      Graphics[
        MapIndexed[
          With[{idx = #2[[1]], pt = #1},
            Inset[Style[sweepMarkers[[idx, 1]],
                        FontSize -> sweepMarkers[[idx, 2]],
                        sweepColors[[idx]],
                        FontFamily -> "Times"],
                  pt]
          ] &,
          floaterPoints
        ]
      ],
      (* Fig 4: legend at upper-right; data declines L->R so upper-right is empty *)
      panelAndLegendEpilog[panelCh, legendLabels,
                           Scaled[{0.95, 0.91}], {Right, Top}]
    ]
  ];

MakeFig4[] := Module[{rowMf, rowK, rowG, grid},
  Print["Generating Figure 4..."];

  rowMf = {
    Fig4Panel[Table[RunModelCached[mf, kDefault, errDefault, aInsider], {mf, mfVals}],
      aInsider, mfVals, mfSym, "a", "Insider-control"],
    Fig4Panel[Table[RunModelCached[mf, kDefault, errDefault, aOutsider], {mf, mfVals}],
      aOutsider, mfVals, mfSym, "d", "Outsider-control"]
  };
  rowK = {
    Fig4Panel[Table[RunModelCached[mfDefault, kval, errDefault, aInsider], {kval, kVals}],
      aInsider, kVals, kSym, "b", "Insider-control"],
    Fig4Panel[Table[RunModelCached[mfDefault, kval, errDefault, aOutsider], {kval, kVals}],
      aOutsider, kVals, kSym, "e", "Outsider-control"]
  };
  rowG = {
    Fig4Panel[Table[RunModelCached[mfDefault, kDefault, errDefault, aInsider, pScale * ptest], {pScale, pScaleVals}],
      aInsider, pScaleVals, gSym, "c", "Insider-control"],
    Fig4Panel[Table[RunModelCached[mfDefault, kDefault, errDefault, aOutsider, pScale * ptest], {pScale, pScaleVals}],
      aOutsider, pScaleVals, gSym, "f", "Outsider-control"]
  };

  grid = GraphicsGrid[{rowMf, rowK, rowG}, Spacings -> {0.1, 0.1}, ImageSize -> 760];
  ExportPDF["Fig4_reproductiveValues.pdf", grid];
  Print["  Done."];
];

(* ============================================================ *)
(* FIGURE 5  -- rate decomposition (orange bar charts, 2x4 grid) *)
(* ============================================================ *)

Fig5BarPanel[data_List, yLabel_, panelCh_String, yRange_:Automatic] :=
  Labeled[
    BarChart[data,
      ChartStyle -> Directive[EdgeForm[Directive[Black, AbsoluteThickness[0.4]]], barColor],
      ChartLayout -> "Grouped",
      PlotRange -> {{0.4, nmax + 0.6}, yRange},
      Frame -> True,
      FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
      LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 10],
      FrameTicks -> {{Automatic, None}, {plainXTicks[], None}},
      FrameLabel -> {{yLabel, None}, {Row[{"Group size"}], None}},
      ImageSize -> 250
    ],
    Style[panelCh, FontFamily -> "Times", FontSize -> 12],
    {Top, Left}
  ];

Fig5Row[res_, aVal_, controlLabel_String, panelLetters_List] :=
  Module[{d, j, fWF, fd, natal, floater, mortality, net},
    d = res[[1]];
    j = res[[2]];
    fWF = res[[4, 1]];
    fd = Last[fWF];

    natal     = Table[ptest[[n]] (1 - d[[n]]), {n, 1, nmax}];
    floater   = Table[fd * kDefault * j[[n + 1]], {n, 1, nmax}];
    mortality = Table[-n * mtest[[n]], {n, 1, nmax}];
    net       = natal + floater + mortality;

    {
      Fig5BarPanel[natal,     "Joining rate of natal offspring", panelLetters[[1]]],
      Fig5BarPanel[floater,   "Joining rate of floater",         panelLetters[[2]]],
      Fig5BarPanel[mortality, "Mortality of insider",            panelLetters[[3]]],
      Fig5BarPanel[net,       "Rate of change in group size",    panelLetters[[4]]]
    }
  ];

MakeFig5[] := Module[{resIn, resOut, rowIn, rowOut, header1, header2, grid},
  Print["Generating Figure 5..."];

  resIn  = RunModelCached[mfDefault, kDefault, errDefault, aInsider];
  resOut = RunModelCached[mfDefault, kDefault, errDefault, aOutsider];

  rowIn  = Fig5Row[resIn,  aInsider,  "Insider-control",  {"a", "b", "c", "d"}];
  rowOut = Fig5Row[resOut, aOutsider, "Outsider-control", {"e", "f", "g", "h"}];

  header1 = Style[titleStr["Insider-control", aInsider],
                  FontFamily -> "Times", FontSize -> 13];
  header2 = Style[titleStr["Outsider-control", aOutsider],
                  FontFamily -> "Times", FontSize -> 13];

  grid = Column[{
    header1,
    GraphicsRow[rowIn,  Spacings -> 0.2, ImageSize -> 1040],
    header2,
    GraphicsRow[rowOut, Spacings -> 0.2, ImageSize -> 1040]
  }, Alignment -> Center, Spacings -> 0.5];

  ExportPDF["Fig5_rateDecomposition.pdf", grid];
  Print["  Done."];
];

(* ============================================================ *)
(* FIGURE 6  -- relatedness across sweeps                        *)
(* ============================================================ *)

Fig6Panel[results_, aVal_, sweepValues_, sweepVar_,
          panelCh_String, controlLabel_String] :=
  Module[{curves, legendLabels},
    curves = Table[
      Module[{r, rVec, rFull},
        r = results[[i]];
        rVec = r[[4, 2]];          (* r2..r9 *)
        rFull = Join[{0}, rVec];   (* r1..r9, with r1=0 by convention *)
        Table[{n, rFull[[n]]}, {n, 2, nmax}]   (* skip n=1 (relatedness undefined for solitary); no floater point *)
      ], {i, Length[results]}];
    legendLabels = Table[Row[{sweepVar, " = ", numFormat[sweepValues[[i]]]}], {i, Length[sweepValues]}];
    Show[
      (* Fig 6: y is bounded to 1.0 — can't add headroom.
         Insider panels (a, b, c) have a high plateau ~0.7-0.8, so
         legend goes to lower-right (where r is near 0).
         Outsider panels (d, e, f) have low values ~0.2-0.3, so
         legend stays at upper-right. *)
      SweepPlot[curves,
        PlotRange -> {{0, nmax + 1.5}, {0, 1.0}},
        FrameTicks -> {{Automatic, None}, {floaterXTicks[], None}},
        FrameLabel -> {{"Relatedness", None},
                       {xLabelGroupSize, If[panelCh === "a" || panelCh === "d",
                          titleStr[controlLabel, aVal], None]}},
        ImageSize -> 360
      ],
      (* Insider (a,b,c) lower-right; outsider (d,e,f) upper-right *)
      If[MemberQ[{"a", "b", "c"}, panelCh],
        panelAndLegendEpilog[panelCh, legendLabels,
                             Scaled[{0.95, 0.05}], {Right, Bottom}],
        panelAndLegendEpilog[panelCh, legendLabels,
                             Scaled[{0.95, 0.92}], {Right, Top}]
      ]
    ]
  ];

MakeFig6[] := Module[{rowMf, rowK, rowG, grid},
  Print["Generating Figure 6..."];

  rowMf = {
    Fig6Panel[Table[RunModelCached[mf, kDefault, errDefault, aInsider], {mf, mfVals}],
      aInsider, mfVals, mfSym, "a", "Insider-control"],
    Fig6Panel[Table[RunModelCached[mf, kDefault, errDefault, aOutsider], {mf, mfVals}],
      aOutsider, mfVals, mfSym, "d", "Outsider-control"]
  };
  rowK = {
    Fig6Panel[Table[RunModelCached[mfDefault, kval, errDefault, aInsider], {kval, kVals}],
      aInsider, kVals, kSym, "b", "Insider-control"],
    Fig6Panel[Table[RunModelCached[mfDefault, kval, errDefault, aOutsider], {kval, kVals}],
      aOutsider, kVals, kSym, "e", "Outsider-control"]
  };
  rowG = {
    Fig6Panel[Table[RunModelCached[mfDefault, kDefault, errDefault, aInsider, pScale * ptest], {pScale, pScaleVals}],
      aInsider, pScaleVals, gSym, "c", "Insider-control"],
    Fig6Panel[Table[RunModelCached[mfDefault, kDefault, errDefault, aOutsider, pScale * ptest], {pScale, pScaleVals}],
      aOutsider, pScaleVals, gSym, "f", "Outsider-control"]
  };

  grid = GraphicsGrid[{rowMf, rowK, rowG}, Spacings -> {0.1, 0.1}, ImageSize -> 760];
  ExportPDF["Fig6_relatedness.pdf", grid];
  Print["  Done."];
];

(* ============================================================ *)
(* FIGURE S1  -- a-sweep summary                                 *)
(* ============================================================ *)

MakeFigS1[] := Module[{data, rvFloater, meanRel, corrRel, grid},
  Print["Generating Figure S1..."];

  data = Table[
    Module[{res, fWF, rVec, rFull, weights, sizes},
      res = RunModelCached[mfDefault, kDefault, errDefault, a];
      fWF = res[[4, 1]];
      rVec = res[[4, 2]];
      rFull = Join[{0}, rVec];
      weights = GroupSizeFrequencies[fWF];
      sizes = Range[1, nmax];
      {res[[4, 3, -1]], Total[weights rFull]/Total[weights],
        WeightedCorrelation[sizes, rFull, weights]}
    ],
    {a, aVals}
  ];

  rvFloater = ListLinePlot[Transpose[{aVals, data[[All, 1]]}],
    PlotStyle -> Directive[sweepColors[[1]], AbsoluteThickness[1.4]],
    PlotMarkers -> {sweepMarkers[[1, 1]], sweepMarkers[[1, 2]]},
    Frame -> True,
    FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
    LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 11],
    FrameLabel -> {mi["a"], "Floater reproductive value"},
    Epilog -> {Inset[Style["a", FontFamily -> "Times", FontSize -> 13],
               Scaled[{0.06, 0.93}], {Left, Top}]},
    ImageSize -> 260];

  meanRel = ListLinePlot[Transpose[{aVals, data[[All, 2]]}],
    PlotStyle -> Directive[sweepColors[[3]], AbsoluteThickness[1.4]],
    PlotMarkers -> {sweepMarkers[[3, 1]], sweepMarkers[[3, 2]]},
    Frame -> True,
    FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
    LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 11],
    FrameLabel -> {mi["a"], "Mean within-group relatedness"},
    Epilog -> {Inset[Style["b", FontFamily -> "Times", FontSize -> 13],
               Scaled[{0.06, 0.93}], {Left, Top}]},
    ImageSize -> 260];

  corrRel = ListLinePlot[Transpose[{aVals, data[[All, 3]]}],
    PlotStyle -> Directive[Black, AbsoluteThickness[1.4]],
    PlotMarkers -> {sweepMarkers[[4, 1]], sweepMarkers[[4, 2]]},
    Frame -> True,
    FrameStyle -> Directive[Black, AbsoluteThickness[0.8]],
    LabelStyle -> Directive[Black, FontFamily -> "Times", FontSize -> 11],
    FrameLabel -> {mi["a"], Row[{"corr(", mi["n"], ", ", mi["r"], ")"}]},
    PlotRange -> {Automatic, {-1, 1}},
    Epilog -> {Inset[Style["c", FontFamily -> "Times", FontSize -> 13],
               Scaled[{0.06, 0.93}], {Left, Top}]},
    ImageSize -> 260];

  grid = GraphicsRow[{rvFloater, meanRel, corrRel}, Spacings -> 0.3, ImageSize -> 800];
  ExportPDF["FigS1_outsiderControlSweep.pdf", grid];
  Print["  Done."];
];

(* ============================================================ *)
(* RUN ALL                                                       *)
(* ============================================================ *)

Print["Generating figures..."];
MakeFig2[];
MakeFig3[];
MakeFig4[];
MakeFig5[];
MakeFig6[];
MakeFigS1[];

Print[""];
Print["=== All figures exported! ==="];
