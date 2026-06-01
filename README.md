
# Group Size and Kin Structure in Cooperative Breeders

This repository contains the Wolfram Mathematica code used to generate the numerical results and figures for a theoretical study on how **group-size distributions** and **within-group kin structure (relatedness)** emerge from demographic processes and insider–outsider conflict over group membership.

---

## Contents

### Code files
- `Appendix_S1_GroupSizeModel.wl`
  Core model implementation (demographic, relatedness, and reproductive-value equilibria + iterative strategy update). Defines the default parameter vectors (`mtest`, `ptest`, `dtest`, `jtest`) and the main functions `sol`, `fullsol`, `upd`, and `run`.

- `Appendix_S2_FigureGeneration.wl`
  Figure-generation script. Loads Appendix S1, computes all manuscript scenarios with caching, and exports all figure files.

### Mapping to the manuscript
The code file names use "Appendix S" numbering. The numerical procedure these files implement is described in the **electronic supplementary material (ESM)**: the iterative algorithm corresponds to **ESM Methods S1 / Algorithm S1**, the event rates to **Table S1**, the relatedness recursion to **ESM §1.2**, and the reproductive-value system to **ESM §1.3**. Figures are numbered as in the main text (Figs 2–6) and the ESM (Fig S1); Fig 1 is a hand-drawn schematic and is not produced by this code.

---

## Requirements

- **Wolfram Mathematica** (or Wolfram Engine) **12.0+**
  (Developed/tested with Mathematica 13.x; the code uses standard built-ins such as `FindRoot`, `Nest`, `Tanh`, and plotting/export functions.)

No external packages are required.

---

## Quick start (run the model)

The example below reproduces the **baseline insider-control scenario** (the parameter set used for the insider panels of Fig. 2): `mf = 1`, `k = 7`, `err = 0.05`, `a = 0.2`.

### Option A: From a Mathematica notebook
Put both `.wl` files in the same folder, open a notebook in that folder, then run:

```mathematica
SetDirectory[NotebookDirectory[]];
Get["Appendix_S1_GroupSizeModel.wl"];

(* Baseline insider-control scenario *)
res = run[{mtest, ptest, 1, 7, 0.05, 0.2, dtest, jtest}, 1000];
```

### Option B: Command line (WolframScript)
From the folder containing the `.wl` files:

```bash
cd /path/to/code/folder
wolframscript -code 'Get["Appendix_S1_GroupSizeModel.wl"]; res = run[{mtest, ptest, 1, 7, 0.05, 0.2, dtest, jtest}, 1000]; Print[res[[1]]];'
```

(For generating figures, use `wolframscript -file Appendix_S2_FigureGeneration.wl`; the script sets the working directory automatically.)

---

## The `run` call

```mathematica
run[{mVec, pVec, mf, k, err, a, dInit, jInit}, nIterations]
```

**The eight elements of the first argument are positional and must be supplied in this exact order.** `err` is the **5th** element and `a` is the **6th** — they are easy to confuse (both are small positive scalars), so take care not to swap them.

| # | Argument | Meaning |
|---|----------|---------|
| 1 | `mVec` | Resident mortality hazards `{m1,...,m9}` for group sizes `n = 1..9`. Default: `mtest = Table[1, {n, 1, 9}]`. |
| 2 | `pVec` | Group productivity (birth rates) `{p1,...,p9}` for group sizes `n = 1..9`. Default: `ptest = Table[0.2 (n^1.5) ((9 - n)^0.75), {n, 1, 9}]` (note `p9 = 0`). |
| 3 | `mf` | Floater mortality hazard (per-capita). |
| 4 | `k` | Encounter rate between floaters and groups. |
| 5 | `err` | Logistic steepness / decision-noise parameter (**this is `ε` in the manuscript/ESM**, see note below). Smaller `err` ⇒ sharper, more step-like best responses. |
| 6 | `a` | Degree of **outsider control** over admission: `a ≈ 0` = insider control, `a ≈ 1` = outsider control. |
| 7 | `dInit` | Initial dispersal strategy `{d1,...,d8, 1}` (boundary `d9 = 1` enforced). Default: `dtest = {0.5,...,0.5, 1}`. |
| 8 | `jInit` | Initial floater-joining strategy `{j0,...,j8, 0}` (boundary `j9 = 0` enforced). Default: `jtest = {1,...,1, 0}`. |

`nIterations` is the number of best-response/equilibrium iterations (second argument to `run`).

### Two fixed numerical constants (not arguments)

- **Damping factor `η = 0.1`** is fixed internally in `upd` as a `0.9 · old + 0.1 · new` strategy update at every iteration (matching ESM Algorithm S1). It is *not* one of the `run` arguments — in the example call, the 6th value `0.2` is `a`, not `η`.
- **Best-response map.** Strategies are updated through `P = (1 + Tanh[Δ/err])/2`, where `Δ` is the collective inclusive-fitness change `Δ(n) = a·ΔI_out(n) + (1−a)·n·ΔI_in(n)`. This is the logistic map of the manuscript/ESM,
  `P_join(Δ) = 1/(1 + exp(−Δ/ε)) = (1 + tanh(Δ/(2ε)))/2`,
  with the correspondence **`ε = err/2`** (so the figure default `err = 0.05` corresponds to `ε = 0.025`).

---

## Output structure

`res` returned by `run[...]` is a 4-element list:

1. `res[[1]]` = final dispersal strategy `d` (length 9; group sizes **n = 1..9**, boundary `d[9] = 1` fixed).
2. `res[[2]]` = final floater-joining strategy `j` (length 10; group sizes **n = 0..9**, boundary `j[9] = 0` fixed).
3. `res[[3]]` = demographic + relatedness equilibrium:
   - `res[[3,1]]` = `{f0,f1,...,f9, fd}`, where `f0..f9` are equilibrium frequencies of territories with group size `n = 0..9` (summing to 1), and `fd` is the equilibrium **floater density** (mean floaters per territory).
   - `res[[3,2]]` = `{r2,r3,...,r9}`, mean within-group relatedness for sizes `n = 2..9` (`r1` is implicitly 1; `r0` is undefined).
4. `res[[4]]` = demographic + relatedness + reproductive-value equilibrium:
   - `res[[4,1]]` = `{f0..f9, fd}`
   - `res[[4,2]]` = `{r2..r9}`
   - `res[[4,3]]` = `{v1,v2,...,v9, vf}`, reproductive values for residents in sizes `n = 1..9` and for floaters (`vf`), normalized so that the **individual-weighted average reproductive value equals 1**, i.e. `(Σ_n n·f_n·v_n + fd·vf) / (Σ_n n·f_n + fd) = 1`.

---

## Reproducing all manuscript figures (Appendix S2)

### From a notebook
```mathematica
SetDirectory[NotebookDirectory[]];
Get["Appendix_S2_FigureGeneration.wl"];
```

### From command line
```bash
wolframscript -file Appendix_S2_FigureGeneration.wl
```

The script:
- Loads `Appendix_S1_GroupSizeModel.wl`.
- Uses a cache (`ResultCache`) to avoid rerunning identical scenarios.
- Pre-computes the baseline, the parameter sweeps, and the outsider-control sweep, then exports each figure as **both a `.pdf` and a `.svg`** to the working directory:

  | File (also exported as `.svg`) | Main text |
  |--------------------------------|-----------|
  | `Fig2_groupSizeDistribution.pdf` | Fig. 2 |
  | `Fig3_groupSize_and_floaterDensity.pdf` | Fig. 3 |
  | `Fig4_reproductiveValues.pdf` | Fig. 4 |
  | `Fig5_rateDecomposition.pdf` | Fig. 5 |
  | `Fig6_relatedness.pdf` | Fig. 6 |
  | `FigS1_outsiderControlSweep.pdf` | Fig. S1 (ESM) |

### Default parameters and sweeps used for the figures
- Control: `aInsider = 0.2`, `aOutsider = 0.8`.
- Baseline demography: `mfDefault = 1`, `kDefault = 7`, `errDefault = 0.05`.
- Floater-mortality sweep: `mfVals = {0.8, 1.0, 1.2, 1.5}`.
- Encounter-rate sweep: `kVals = {5, 7, 10, 15}`.
- Productivity-scaling sweep (`g` in the manuscript): `pScaleVals = {0.9, 1.0, 1.2, 1.5}`.
- Outsider-control sweep (Fig. S1): `aVals = Range[0, 1, 0.1]`.

### Speed / convergence toggle
In `Appendix_S2_FigureGeneration.wl`:
- `QuickMode = True` sets `nIterations = 200` (fast, for inspection).
- `QuickMode = False` sets `nIterations = 1000` (the published setting; this is the default in the script).

To reproduce the publication figures, keep `QuickMode = False` to ensure convergence.

---

## Troubleshooting

- The script sets `Off[FindRoot::lstol]`; the resulting "ltol/tolerance" messages are expected for this model and do not indicate failure.
- **`FindRoot` convergence failures** can still occur under some parameter combinations. Try:
  - Increasing `nIterations` (especially if strategies are still changing),
  - Using a larger `err` (smoother best-response mapping),
  - Starting from different initial strategies `dInit`, `jInit` (e.g., closer to 0/1),
  - Running fewer sweep points first (debug with a single scenario via `run[...]`).

---

## License / usage
All rights reserved; shared for peer review and reproducibility evaluation only.

Upon acceptance, this code will be released under a [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/) license.
