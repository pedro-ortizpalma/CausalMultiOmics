# Contributing to CausalMultiOmics

Thanks for taking the time. This is a young package and the most useful
thing you can send is a case where it gives an answer you do not
believe.

## Reporting a bug

Open an issue with a [reprex](https://reprex.tidyverse.org) — a
self-contained snippet that reproduces the problem — and the output of
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html).
[`simulate_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/simulate_data.md)
generates a cohort with a known structure, so most reports can be
written against it without sharing any real data. **Please do not paste
patient-level data into an issue.** The objects this package returns
carry individual-level values by design: a `PreprocessingResult` holds
two full copies of the cohort and a `CMOResult` holds the outcome per
subject, so a [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
attached to an issue is a disclosure.

Before filing, run
[`cmo_setup()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_setup.md)
and include its output. Several reports that look like bugs are an
evidence generator silently skipped for a missing suggested package.

## Proposing a method

The package is a framework for evidence generators, not a fixed set of
nine. A new generator needs to say, for every relationship it reports:

- an estimate, its standard error and its sample size;
- which identification strategy, if any, would license reading it
  causally;
- what it assumes, in a form
  [`summary()`](https://rdrr.io/r/base/summary.html) can print to a
  non-statistician.

A method that returns a ranking without those is a good method but not
one this package can merge into a scored graph, because the score
weights agreement between generators and cannot weight an unquantified
one.

## Pull requests

``` r

devtools::document()   # roxygen2; never edit man/*.Rd by hand
devtools::test()
devtools::check()      # must come back 0 errors, 0 warnings
```

Conventions worth matching:

- Imports stay base R. Anything heavier goes in `Suggests`, guarded with
  `system.file(package = "x")` rather than
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) — the
  latter executes the package’s load code, which pulls every probed
  dependency into the session and lets a broken binary abort R instead
  of reporting itself as unavailable.
- [`summary()`](https://rdrr.io/r/base/summary.html) methods return a
  `summary.<Class>` object; the printing lives in
  `print.summary.<Class>()`.
- Every exported function needs a runnable `\examples` block and a
  `\seealso` that links it to its neighbours in the pipeline.
- New behaviour needs a test. The suite has no failures and should keep
  none.
- `NEWS.md` entries explain *why* the old behaviour was wrong, not just
  what changed.

## Scope

This package reports what the data can and cannot support. A change that
makes a result look more conclusive than its identification strategy
allows is out of scope even if it is correct arithmetic — that is the
one thing the package exists to avoid.
