# CausalMultiOmics 0.0.0.9000

## Correctness

* `sqrt` and `vst` transformations no longer replace missing values with
  zero. `pmax(x, 0, na.rm = TRUE)` does not skip `NA`, it substitutes the
  other operand, so every missing value was silently imputed as zero before
  the transformation benchmark scored the block.
* `check_data()` no longer moves the caller's random number generator. Fold
  assignment and Shapiro subsampling run under a saved-and-restored
  `.Random.seed`, so results stay reproducible without disturbing the
  surrounding session.
* Order preservation is reported as `NA` when a transformation changes the
  shape of a block (`alr`, `ilr`) instead of correlating unrelated cells by
  position.
* Constant columns no longer decide the statistical type of a block. A
  column of 1s read as a proportion and a column of 7s as a count, dragging
  otherwise homogeneous blocks to `"mixed"` and cutting their candidate
  transformations down to the generic set.
* `.outcome_performance()` is documented for what it computes: a
  fold-averaged in-sample association, not cross-validation. The wording of
  the generated justification was corrected to match.

## Data integrity

* `load_data()` requires sample identifiers in the row names of every block.
  Blocks without them were labelled `"1"`, `"2"`, ... by position, which made
  unrelated blocks appear to share all their samples and silently broke
  matching against metadata.
* `MultiOmicsData$history` is the character log the class declares. The
  structured record of the call moved to `misc$load_data`.

## Robustness

* Quality-control checks tolerate an unpopulated `CMOValidation` instead of
  failing with "missing value where TRUE/FALSE needed".
* Per-block plots stay aligned with their labels when `check_data()` skips a
  block.

## API

* Added the `CMOResult()` constructor that `print.CMOResult()` and
  `summary.CMOResult()` already assumed.
* `NAMESPACE` registers all fifteen S3 methods and exports the `report()`
  generic. Nine methods and `report()` were previously unreachable from an
  installed package.

## Infrastructure

* Added a `testthat` suite covering the public API and carrying a regression
  test for each fix above.
* Declared `Imports` (grDevices, graphics, stats, utils) and `Suggests`
  (survival, testthat); filled in the package Title and Description.
