# From blocks of data to a claim you could be wrong about

Most analysis packages fit a model and hand it back. This one does not,
because “the best model” is a poor answer to a biological question: the
model that predicts best is usually the one leaning hardest on whatever
was measured most accurately, which is a fact about the laboratory
rather than about the disease.

What comes out instead is a scored graph in which every arrow carries
its own evidence — which methods saw it, how large it was, how precisely
it was measured — and, the part that matters, what would have to be true
for it to mean what it appears to mean.

## Data with a known answer

Real data cannot show that an analysis is right. It has no answer to
check against. So this vignette plants one.

``` r

structure <- data.frame(
  from   = c("age", "age", "diet", "inflammation"),
  to     = c("diet", "disease", "inflammation", "disease"),
  effect = c(0.4, 0.5, 0.8, 0.7)
)

sim <- simulate_data(
  n = 400,
  blocks = list(
    clinical = c("diet", "inflammation"),
    proteins = 8
  ),
  dag = structure,
  outcome = "disease",
  missing = c(proteins = 0.1),
  seed = 1
)

sim
#> 
#> MultiOmicsData
#> ==============
#> 
#> Blocks:                2
#> Samples:               400
#> Features:              10
#> Metadata:              Yes
#> Preprocessing:         0
#> History:               2
```

Read the structure as biology. Age influences both what people eat and
whether they die, so it confounds. Diet drives inflammation, which in
turn drives the disease — so inflammation is a **mediator**, and the
eight proteins are noise with nothing to do with any of it.

The truth travels with the data:

``` r

sim$misc$simulation$planted_features
#> [1] "diet"         "inflammation"
sim$misc$simulation$noise_features
#> [1] "proteins_01" "proteins_02" "proteins_03" "proteins_04" "proteins_05"
#> [6] "proteins_06" "proteins_07" "proteins_08"
```

## Audit before you touch anything

[`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
changes nothing. It profiles each block, benchmarks candidate
transformations, and derives a preprocessing recipe with its reasoning
attached.

``` r

quality <- check_data(sim)

quality$summary$n_blocks
#> [1] 2
quality$summary$total_missing_percent
#> [1] 8
```

Two overlap figures are reported, and the second is the one an analysis
runs on:

``` r

quality$summary$shared_by_all
#> [1] 400

quality$summary$cumulative_overlap
#>      block samples shared_after lost
#> 1 clinical     400          400    0
#> 2 proteins     400          400    0
```

Every pair of twenty blocks can share the whole cohort while the twenty
together share none of it, because each can be missing a different part.
A pairwise matrix cannot express that; the running total can.

## Execute the plan, and nothing else

[`preprocess()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)
derives nothing of its own. Every method, parameter and threshold comes
from the recipe, and every stage stores its fitted model so the pipeline
can be replayed on a second cohort.

``` r

clean <- preprocess(sim, quality, plots = FALSE, quiet = TRUE)

clean$quality$table[, c("block", "missing_before", "missing_after")]
#>      block missing_before missing_after
#> 1 clinical              0             0
#> 2 proteins             10             0
```

A block going from 10% missing to 0% did not improve. It was filled in,
and that matters later.

## Build evidence

``` r

results <- analyze(
  clean,
  outcome = "disease",
  covariates = "age",
  effort = "standard",
  control = analysis_control(methods = c("association", "conditional")),
  plots = FALSE,
  quiet = TRUE
)

results
#> 
#> CMOResult
#> =========
#> 
#> Outcome:                       disease
#> Design:                        cross-sectional
#> Samples:                       400
#> Features analysed:             10 of 10
#> 
#> Methods run:                   2 of 2
#> Relationships found:           11
#> After integration:             6
#> With temporal precedence:      0
#> 
#> Causal paths:                  1
#> Plots:                         0
#> Tables:                        15
#> Runtime:                       2.01 s
```

The planted driver should be at the top, and the eight noise proteins
should not be:

``` r

head(results$interpretation$drivers[, c("source", "evidence_score",
                                        "identification")], 4)
#>         source evidence_score identification
#> 1 inflammation          46.87     adjustment
#> 2         diet          37.52     adjustment
#> 4  proteins_07           2.32     adjustment
#> 5  proteins_03           2.17     adjustment
```

Note the `identification` column. Every relationship records the
strategy that would license reading it causally, and `adjustment` means
measured confounders were handled and unmeasured ones cannot be. It is a
hypothesis, not a conclusion, however high it scores.

## What an adjustment is actually worth

Adjusting for a variable helps only when the variable is a confounder.
Adjusting for a mediator deletes part of the effect being measured;
adjusting for a collider manufactures an association out of nothing.
Both look like diligence and both make the estimate worse than doing
nothing.

Nothing in the data tells these apart — the correlation matrix is
identical in all three cases. Only a structure does, and a structure is
a claim about the world that has to come from you.

``` r

check_dag(structure, "diet", "disease", adjusted = "age")
#> 
#> Effect of diet on disease
#> ============================================================
#> 
#> Adjusted for : age
#> Verdict      : IDENTIFIED
#> 
#>   The adjustment closes every backdoor path from diet to
#>   disease under the DAG supplied.
#> 
#> A sufficient adjustment set
#> ------------------------------------------------------------
#>   age
#> 
#>   This holds only if the structure you supplied is correct.
#>   The data cannot confirm it.
```

And the same question with the mediator wrongly included:

``` r

check_dag(structure, "diet", "disease", adjusted = c("age", "inflammation"))
#> 
#> Effect of diet on disease
#> ============================================================
#> 
#> Adjusted for : age, inflammation
#> Verdict      : NOT identified
#> 
#>   Conditioning on inflammation is what breaks this: it
#>   should not be in the adjustment set at all.
#> 
#> Problems with the adjustment
#> ------------------------------------------------------------
#>   - Conditioned on inflammation, which lies on the path from
#>     diet to disease: part of the effect being measured has
#>     been removed.
#> 
#> A sufficient adjustment set
#> ------------------------------------------------------------
#>   age
```

Passing `dag = structure` to
[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
audits every relationship this way. It changes no estimate — a structure
is an interpretive claim, not a statistical one — but it changes what
may be concluded, and an adjustment that includes a mediator loses its
label rather than keeping it with a footnote.

## What was measured and what was invented

Imputation fills gaps with plausible numbers, which is usually right.
But once a gap is filled nothing downstream can tell a measurement from
an estimate, and the confidence interval comes out exactly as narrow as
if every value had been real.

``` r

affected <- Filter(function(e) length(e$quality_flags) > 0, results$evidence)

length(affected)
#> [1] 4

if (length(affected) > 0) affected[[1]]$quality_flags
#> [1] "11% of this variable was missing and was filled in by k-nearest-neighbours, borrowing from correlated features."
#> [2] "9% of this variable was missing and was filled in by k-nearest-neighbours, borrowing from correlated features." 
#> [3] "10% of this variable was missing and was filled in by k-nearest-neighbours, borrowing from correlated features."
#> [4] "8% of this variable was missing and was filled in by k-nearest-neighbours, borrowing from correlated features."
```

`data_quality` is the share of the data behind a relationship that was
measured rather than reconstructed. It scales the evidence score rather
than entering the confidence score, because those are different
problems: a wide interval is fixed by collecting more people and a
column that was half invented is fixed by nothing.

## Does one number describe everybody?

An average says nothing about whether the people it averages resemble
each other. A coefficient of 0.4 is equally compatible with 0.4 in
everyone, and with 2.0 in a tenth of them and nothing in the rest.

``` r

checked <- Filter(function(e) is.finite(e$share_driving_effect),
                  results$evidence)

for (e in head(checked, 3)) {
  cat(sprintf("%-16s halved by removing %d people (%.0f%%)\n",
              e$source, e$samples_driving_effect,
              100 * e$share_driving_effect))
}
#> inflammation     halved by removing 400 people (100%)
#> diet             halved by removing 83 people (21%)
```

For a relationship that holds across a group the answer is most of them,
because removing a handful barely shifts an average. A small answer
means the finding is carried by a minority nobody has identified.

## State something that could be wrong

``` r

h <- hypothesis(results)

h$claim
#> [1] "Higher inflammation goes with higher disease."
h$grade
#> [1] "an adjusted association"
```

The grade comes from identification alone. Precision, method agreement
and resampling stability describe how *well* an association was
estimated and say nothing about what it is evidence *of*; letting them
raise the grade would turn a well-measured correlation into a cause by
arithmetic.

The sentence follows the grade rather than the score. “Higher X goes
with higher Y” describes what was seen; “raising X would raise Y”
describes what would happen, and only identification licenses the
second.

The part a result never has is what would settle the argument:

``` r

h$settles
#> [1] "Measure a confounder of inflammation and disease. To erase this it would have to be associated with both at a risk ratio of at least 4.7; anything weaker leaves the relationship standing."
#> [2] "Measure inflammation before disease in the same people. Nothing in this data establishes which came first, and the reverse direction fits it equally well."
```

Each item comes from a specific weakness of this specific relationship
rather than from a stock paragraph of caution. A reader can act on
“measure a confounder associated with both at 3.2 or more”. Nobody can
act on “residual confounding cannot be excluded”.

## Take it somewhere new

A claim carries its own protocol — which outcome, which covariates,
which model — so what runs on a second cohort is what ran on the first.

``` r

validation <- simulate_data(
  n = 350,
  blocks = list(clinical = c("diet", "inflammation"), proteins = 8),
  dag = structure,
  outcome = "disease",
  seed = 2
)

tested <- test_hypothesis(h, apply_preprocessing(validation, clean,
                                                 quiet = TRUE))

tested$replication$verdict
#> [1] "replicated"
tested$replication$ratio
#> [1] 0.8993155
```

Replication is judged on direction and size, not on a p-value. The usual
way one is oversold is a direction that holds with an effect a fifth as
large, which a significance test calls a success; that case is reported
as `"same direction, much smaller"`.

## Two kinds of argument

[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
takes its many options in two groups, because they are two kinds of
decision.

[`analysis_assumptions()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_assumptions.md)
holds what you are claiming: the causal diagram, which variables are
modifiable, the reliability of an assay, which events compete with the
outcome. None of it can be derived from the data, all of it changes what
may be concluded, and getting one wrong makes the answer wrong.

[`analysis_control()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_control.md)
holds how much work to do: which generators, how many resamples, how
many features. Getting one wrong makes the answer slower or noisier,
never wrong.

``` r

analyze(
  clean,
  outcome = "disease",
  covariates = "age",
  assume = analysis_assumptions(
    dag = structure,
    reliability = c(inflammation = 0.85),
    modifiable = "clinical"
  ),
  control = analysis_control(
    methods = c("association", "survival"),
    max_features = 200
  )
)
```

Two of those deserve a word.

`reliability` corrects for the attenuation that measurement error
causes: a noisy exposure pulls its own coefficient toward zero by
exactly its reliability. The correction is reported beside the measured
estimate and never in place of it, and it widens the interval rather
than narrowing it — an attenuation correction that left the interval
alone would turn a noisy measurement into a stronger claim.

`positivity`, on by default, checks the identification condition nobody
mentions. Reading an adjusted estimate causally needs two things: no
unmeasured confounding, and variation in the exposure at every
combination of the adjustment set. Where the second fails the model does
not complain, it extrapolates.

``` r

checked <- Filter(function(e) !is.na(e$positivity), results$evidence)

for (e in head(checked, 3)) {
  cat(sprintf("%-14s positivity %-5s (%.0f%% of it free to vary)\n",
              e$source, e$positivity, 100 * e$residual_variation))
}
#> inflammation   positivity TRUE  (94% of it free to vary)
#> diet           positivity TRUE  (87% of it free to vary)
#> proteins_07    positivity TRUE  (100% of it free to vary)
```

Only relationships with the outcome are checked, since only those have
an adjustment set to be trapped by.

## Several outcomes at once

Studies rarely have one endpoint. Naming several runs each analysis on
its own terms and then corrects multiplicity across all of them together
— which running
[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
twice by hand does not, because a relationship that was one of fifty
tests is not one of a hundred, and correcting within each outcome
separately pretends the other analysis was never run.

``` r

analyze(clean, outcome = c("disease", "biomarker"), effort = "fast")
```

## Where to go next

- `report(results, file = "report.html")` writes a self-contained
  document intended for a reader who is not a statistician, with every
  finding carrying what would settle it.
- `explain(results, "diet")` gathers everything known about one
  variable, including what each method reported on its own before
  anything was merged.
- `counterfactual(results, modifiable = "clinical")` states the findings
  in the units the variables were measured in, and includes nothing you
  have not declared changeable.
- `results$consensus` and `results$modules` read the same evidence
  across resamples and at the resolution between a variable and a block.

Nothing here is offered as certainty. The package exists to make the gap
between what was measured and what may be claimed visible rather than
convenient.
