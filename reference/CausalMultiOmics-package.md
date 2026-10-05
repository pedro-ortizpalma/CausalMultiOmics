# CausalMultiOmics: Interpretable Causal Analysis of Multi-Block Omics Data

Turns complex multi-block datasets into a quantified, interpretable
account of the mechanisms leading to a phenotype or clinical event. The
workflow is four steps, each producing an object that records every
decision it took.

## The workflow


      load_data()      MultiOmicsData        blocks plus sample metadata
           |
      check_data()     CMOValidation         audit, diagnostics, recipes
           |
      preprocess()     PreprocessingResult   the plan, executed and recorded
           |
      analyze()        CMOResult             integrated evidence graph

Each step consumes the object the previous one produced. Nothing is
modified in place, and no step re-derives a decision an earlier one
already made:
[`preprocess()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)
executes the recipes
[`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
wrote, and
[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
starts from a preprocessing plan that is already fixed.

## What the result is

[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
does not return a model. It runs many analysis methods, collects the
relationships each reports, and integrates them into one scored directed
graph of evidence, from which mechanisms, mediators, hubs and candidate
biomarkers are extracted.

Three scores are reported per relationship and they measure different
things: **strength** is how large the effect is, **confidence** is how
precisely it was estimated, and **consistency** is how many methods
agreed on its direction.

## On causal language

Consistency between methods is not validity. Methods that share an
unmeasured confounder agree with one another while all being biased in
the same direction, so agreement is evidence of stability and nothing
more.

Every edge therefore carries an `identification` strategy — the reason,
if any, that would license reading it causally — together with the
assumptions that would have to hold. An edge identified as `"none"` or
`"adjustment"` is an association, however high it scores. This is
deliberate: the package is designed so that an over-claim is
structurally hard to make rather than merely discouraged in the
documentation.

## Reproducibility

Every stage stores the fitted model that produced it, so
[`apply_preprocessing()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/apply_preprocessing.md)
can replay a pipeline on an external cohort without re-deriving a single
decision from it. Seeds, package versions, runtimes and parameters are
recorded in each result object, and no function moves the caller's
random number generator.

## See also

[`load_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/load_data.md),
[`check_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md),
[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md),
[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`report`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/report.md)

## Author

**Maintainer**: Pedro Ortiz-Palma <ortiz9009@gmail.com>

Authors:

- Pedro Ortiz-Palma <ortiz9009@gmail.com>
