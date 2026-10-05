---
name: Bug report
about: Something errors, or returns a number you do not believe
title: ''
labels: bug
assignees: ''
---

**What happened, and what you expected instead**

<!-- One or two sentences. If a number looks wrong rather than an error being
thrown, say which number and what you expected. -->

**Reproducible example**

<!-- Self-contained code that reproduces it. simulate_data() generates a
cohort with a known structure, so most reports can be written without any
real data.

Please do not attach patient-level data or a saveRDS() of a result object:
PreprocessingResult carries two full copies of the cohort and CMOResult
carries the outcome per subject. -->

```r
library(CausalMultiOmics)

data <- simulate_data(n = 100, blocks = list(main = 10), seed = 1)
# ...
```

**Output of `cmo_setup()`**

<!-- Several reports that look like bugs turn out to be an evidence generator
skipped for a missing suggested package. -->

```
```

**Output of `sessionInfo()`**

```
```
