# Where this data comes from

`tests/manual/walkthrough.R` runs on real data when it finds it here. The
files themselves are not in this repository: they are published data that is
not ours to redistribute, and one of them is past the file size limit GitHub
will accept.

## The release

> Patel CJ, Pho N, McDuffie M, Easton-Marks J, Kothari C, Kohane IS, Avillach P
> (2016). **A database of human exposomes and phenomes from the US National
> Health and Nutrition Examination Survey.**
> *Scientific Data* **3**:160096. <https://doi.org/10.1038/sdata.2016.96>

Available from the Dryad deposit linked in that paper. The walkthrough needs
only `nh_99-06.Rdata` (22 MB).

## What each file is

| File | Size | Needed |
|---|---|---|
| `nh_99-06.Rdata` | 22 MB | **yes** — holds `MainTable`, `VarDescription`, `DemoVariables` |
| `MainTable.csv` | 147 MB | no — the same table as in the `.Rdata`, just far larger |
| `VarDescription.csv` | 1 MB | no — also inside the `.Rdata` |
| `DemoDescription.csv` | 4 KB | no — the demographic codebook, useful to read |
| `sdata201696.pdf` | 1.4 MB | no — the paper |

If you are short of space, `MainTable.csv` can go: the walkthrough never
reads it, and the `.Rdata` carries the same 41,474 x 1,191 table.

## What the walkthrough does with it

Adults with mortality follow-up, then five of the release's own variable
categories used as blocks:

| Block | Variables |
|---|---|
| `blood` | 8 haematology measures |
| `body_measures` | 7 anthropometric measures |
| `blood_pressure` | pulse, systolic, diastolic |
| `heavy_metals` | blood cadmium and lead |
| `housing` | 5 categorical housing variables |

The outcome is `MORTSTAT` with `PERMTH_INT` as follow-up time, which makes it
a survival design &mdash; the only setting where the engine can claim temporal
identification. Age and sex are covariates, and standing height is declared a
negative control, since it cannot plausibly be part of a mechanism leading to
death within the follow-up.

Two things are worth knowing about the preparation, both handled in
`make_nhanes_study()`:

- NHANES codes "refused" and "don't know" as 7, 9, 77 and 99 **inside**
  otherwise ordinary variables. Left alone, `house_age = 77` becomes a
  category holding 75 people and comes top of the evidence ranking on a single
  method. Those codes are set to `NA`.
- The full overlap is about 5,600 people and takes roughly a quarter of an
  hour. The walkthrough subsamples to 1,800, keeping every death. Set
  `CMO_FULL_COHORT <- TRUE` before sourcing to use all of it.

## Without the data

The walkthrough falls back to a small simulated study with a mechanism planted
in it, and says so at the top of the run. That fallback is worth keeping in
any case: real data has no known answer, so it can show the engine running but
not that it is right.
