# CausalMultiOmics 0.1.0

## Two kinds of argument, kept apart

* `analyze()` had twenty-seven arguments, which is not a signature but a
  form. The count was not the problem: mixing two kinds of decision in one
  list was. It now takes eleven, with the rest in two groups.
* `analysis_assumptions()` holds what you are claiming — the causal diagram,
  which variables are modifiable, measurement reliability, competing events,
  structure-learning constraints. None can be derived from the data, all
  change what may be concluded, and getting one wrong makes the answer wrong.
* `analysis_control()` holds how much work to do — which generators, how many
  resamples, how many features. Getting one wrong makes the answer slower or
  noisier, never wrong.
* A clean break, with no deprecation path. The old names are gone rather than
  quietly accepted, and every call site in the package, the tests, the
  walkthroughs, the vignette and the README was rewritten.

## Positivity: the identification condition nothing checked

* Reading an adjusted estimate causally takes two conditions. The first, no
  unmeasured confounding, this package works hard at. The second is that at
  every combination of the adjustment variables the exposure actually varies,
  and until now nothing here said a word about it.
* Where it fails the model does not fail with it. It extrapolates, and hands
  back a coefficient with an interval like any other. If everyone over
  seventy is hypertensive, the effect of hypertension at seventy is not
  estimable from the data.
* Unlike unmeasured confounding this one is checkable, because it is a fact
  about the data in hand rather than about the world. Every relationship with
  the outcome now carries `positivity` and `residual_variation`: how much of
  the exposure is left to vary once the adjustment set has had its share.
* Strata in which a two-level exposure never varies are counted separately,
  since those people contribute no comparison and the model fills them in
  from its own shape.

## Measurement error

* `assume = analysis_assumptions(reliability = )` corrects for the
  attenuation a noisy exposure causes. Classical measurement error multiplies
  a coefficient by exactly the reliability of the measurement, so dividing by
  it undoes the damage.
* Reported beside the measured estimate and never in place of it: the
  correction rests on a number the caller supplied, and replacing the
  measured value would hide an assumption inside a result.
* The interval widens by the same factor. A correction that left it alone
  would turn a noisier measurement into a stronger claim.
* Nothing is guessed. A variable absent from `reliability` is left alone, and
  a value outside 0 to 1 is refused.

## Competing risks

* A Cox model treats everything that is not the event as censoring, and
  censoring means "still at risk, we just stopped looking". For someone who
  died of another cause that is false, and in a mortality study it is not a
  small falsehood.
* Survival edges now say so. Naming `competing =` reports how many people had
  one, and either way the edge states that what it carries is a
  cause-specific hazard rather than a risk — and that a cause-specific hazard
  can rise while the risk falls, if the competing cause rises faster.

## Several outcomes

* `outcome` accepts more than one name. Each is analysed on its own terms and
  multiplicity is corrected across all of them together.
* Running `analyze()` twice by hand gives the same estimates and the wrong
  error rate: a relationship that was one of fifty tests is not one of a
  hundred, and correcting within each outcome separately pretends the other
  analysis was never run.
* Every edge carries both figures, `fdr` for its own analysis and
  `fdr_across_outcomes` for the study, and both are computed from the same
  p-values so they can be read side by side.
* Nothing is modelled jointly, and the result says so: a relationship found
  for one outcome is no evidence about another.

## Data with a known answer

* `simulate_data()` builds a multi-block study from a causal structure you
  specify and records what it planted, so a result can be checked rather than
  admired. Real data cannot show that an analysis is right — it has no answer
  to check against — and every claim this package makes about recovering a
  mediator, catching an artefact of imputation or separating a latent process
  from its members needs a truth someone put there on purpose.
* It replaces thirty-five small generators written one at a time across the
  test suite and the walkthrough, each planting its own truth its own way.
  That duplication was the argument for writing it.
* Structure and magnitude arrive in one object, because they are the same
  claim: a `data.frame` of `from`, `to` and `effect`. Variables are generated
  in topological order, so an effect of 0.8 is what a regression coefficient
  will recover.
* Anything the structure names and no block claims becomes a metadata column.
  That is how covariates and effect modifiers arrive, and it is also how a
  mediator is made unmeasured.
* Covers what makes real data hard: latent processes spanning blocks,
  missingness at a stated rate or concentrated where the outcome is high,
  batch shifts, and blocks measured on different fractions of the cohort.
* `misc$simulation` records the structure, which features are pure noise,
  which cells were blanked, module membership, block coverage and the seed.
* The caller's random number generator is restored afterwards, so an example
  in a vignette cannot change every simulation the reader runs next.
* Refuses a cycle rather than looping over it: generating a variable needs
  its parents to exist first, and a cycle has no such order.

## How much of this is the analyst?

* `sensitivity()` refits one relationship under every defensible
  preprocessing choice and reports the spread. The E-value asks how much
  unmeasured confounding it would take to erase a finding; this asks the
  question next to it and rather more embarrassing. A conclusion that
  survives every reasonable decision is a different object from one that
  needed this one.
* The package can answer it where most cannot, because recipes are objects
  and pipelines replay: the variants are cheap to build and cheap to run.
* Estimates are reported per standard deviation of the exposure. A rank
  transformation puts a variable on a 1 to n scale and a log compresses it,
  so the raw coefficients down different paths are in different units —
  putting them in one column reported a 275-fold spread for a relationship
  that never moved, which is the very comparison the quantity families exist
  to prevent.
* Scaling is not varied, because it cannot change a per-standard-deviation
  estimate. Including it filled the table with exact duplicates, and a
  robustness check that counts the same answer twice reports a finding as
  steadier than it is.
* Sample filters are held fixed. Changing them changes who is in the study,
  and an estimate on a different population answers a different question.
* Stability here is not evidence that a relationship is real. A chance
  correlation is not created or destroyed by how the data was transformed,
  only re-expressed, so noise passes this check comfortably. It measures
  dependence on the analyst, and the object says so.

## Two analyses, side by side

* `compare_results()` answers the four questions a reader has when handed two
  results: which relationships appear in both, which reverse, which vanish,
  and which survive at a size that no longer means the same thing.
* Comparability is checked first and reported before the comparison. Two
  graphs can be compared only if the analyses estimated the same quantity on
  the same kind of outcome with the same adjustment; sixty per cent overlap
  between two different questions is a number about nothing.
* Size is reported as a ratio, because a relationship holding in both cohorts
  at a fifth of the size is the usual way a replication is oversold — both
  significant, both the same direction, and a comparison based on presence
  alone calls it agreement.
* A relationship missing from one of two analyses has not been refuted by it.
  It may simply not have cleared the threshold there, and the two lists are
  not a test of each other.
* Nothing is pooled. There is no `meta_analyze()` and there will not be:
  pooling needs everything comparability needs and more, and a function that
  pooled regardless would be the most dangerous thing in the package.

## Taking the graph elsewhere

* `export_graph()` writes the evidence graph as GraphML (Cytoscape, Gephi),
  Graphviz DOT, or JSON. Every edge attribute travels with it — the score,
  the three components it is made of, the identification, the data quality —
  because a graph exported with only its arrows arrives stripped of
  everything that made it worth exporting.
* JSON is written directly, so a user with none of the optional packages
  installed can still get the graph out.
* GML and Pajek are deliberately absent. Pajek carries no edge attributes and
  igraph's GML writer rejects the vertex table this package produces; a
  format that silently discards the answer or fails outright is worse than
  one that is not offered.

## Constraining structure learning

* `analyze(..., forbidden = , required = )` passes a blacklist and whitelist
  to Bayesian network structure learning. Ruling out relationships that
  cannot exist removes them from a superexponential search rather than from
  its output, which is worth more than any amount of extra computation.
* Both are claims about the world in the same way a DAG is, so both are
  recorded as assumptions on every edge they shaped. A constraint applied
  silently would let the caller's own belief reappear to them as a finding.

## Documentation

* A vignette, built from `simulate_data()` so every number in it is
  reproducible and every claim is checked against a planted truth. It walks
  from raw blocks to a stated hypothesis and its replication in a second
  cohort.

## A claim, rather than a body of evidence

* `hypothesis()` takes one relationship out of a result and states it as a
  claim: what is being asserted, at what strength, what supports it, what
  threatens it, and what would settle the argument. A graph of forty scored
  relationships is material from which statements can be made; the making is
  where the judgement is, and it was not represented anywhere.
* `settles` is the part a result never has. It is derived from the specific
  weaknesses of the specific relationship, so a clean claim gets a short list
  and a fragile one gets a pointed one: measure a confounder of at least this
  strength, measure the exposure first, replicate without these six people,
  test within one level of the moderator, separate this variable from the
  four it moves with. A reader can act on those and cannot act on "residual
  confounding cannot be excluded".
* The grade is set by identification alone. Precision, method agreement and
  resampling stability describe how well an association was estimated and say
  nothing about what it is evidence of; letting them raise the grade would
  turn a well-measured correlation into a cause by arithmetic. A confirming
  causal diagram does raise it, conditionally and explicitly, because that is
  the entire purpose of auditing an adjustment against a stated structure.
* The sentence follows the grade rather than the score, and the two readings
  get different grammar: "Higher X goes with higher Y" describes what was
  seen, "Raising X would raise Y" describes what would happen, and only
  identification licenses the second.
* `test_hypothesis()` takes a claim to a cohort it has never seen. The claim
  carries its own protocol — outcome, covariates, model — so what runs there
  is what was run here.
* Replication is judged on direction and size, not on a p-value. The most
  common way a replication is oversold is a direction that holds with an
  effect a fifth as large, which a significance test calls a success; that
  case is reported as "same direction, much smaller".
* Covariates the new cohort lacks are named rather than quietly dropped,
  since a model missing an adjustment is not the model the claim was made
  with.
* Every finding in the HTML report now carries what would settle it.

## The resolution between a feature and a block

* `result$modules` is a `ModuleGraph`: groups of variables that move together,
  summarised and tested as one thing. Fifty transcripts rising and falling
  together are one process measured fifty times; testing each separately
  answers a question nobody asked, pays the multiplicity penalty fifty times,
  and reports fifty findings where there is one.
* Modules come from the correlation between features, not from the evidence
  graph. A community detected on the evidence graph groups features that each
  have a link to the outcome, which they can do while being uncorrelated with
  each other, and the first principal component of such a group summarises
  nothing.
* Each module is represented by its first principal component, scaled to unit
  variance so its coefficient means what a feature's coefficient means. A raw
  component is about the square root of its eigenvalue wide, so a ten-feature
  module arrived three times wider than its own members and its estimate came
  out three times smaller for no reason but arithmetic — which, read beside
  the members, looked like the module disagreeing with what it is made of.
* The component's sign is fixed against the average of the module's own
  members. Left as `prcomp` returns it, the direction reported for every
  module is a coin flip.
* `variance_explained` says how much of a module the summary actually
  captures, and a module below the threshold is reported but marked as not
  cohering. Its first component is one direction through a cloud rather than
  a shared process, and that is itself a finding about the data.
* Modules spanning several blocks are flagged. They are the only thing at
  this resolution that could be a mechanism rather than an artefact of one
  platform.
* A module and its own members are the same measurements at two resolutions,
  not two findings. They agree by construction and neither confirms the
  other; the object, the printed output and the report all say so.
* A new figure draws each module beside the members it stands for, so a
  reader can see whether it represents them or averages over a disagreement.

## Does one number describe everybody?

* Every estimate is an average over the people measured, and an average says
  nothing about whether they resemble each other. A coefficient of 0.4 is
  compatible with 0.4 in everyone and with 2.0 in a tenth of them and nothing
  in the rest; the first is a property of the cohort, the second a property
  of ten people nobody has identified.
* `share_driving_effect` answers that without needing anything named: how
  much of the cohort would have to be removed to halve the estimate. For a
  relationship that holds broadly the answer is most of them, because
  removing a few people barely shifts an average. Samples are ordered by
  their influence on the coefficient, so it is the worst case rather than a
  typical one — a reader deciding whether to believe a result wants to know
  how fragile it could be.
* Computed from one-step influence and then confirmed by a single refit at
  the chosen count, since dfbetas are not additive and a running total would
  be an estimate presented as a measurement.
* An estimate indistinguishable from zero is not reported as fragile.
  Halving nothing costs nothing, and without that guard every null result
  would read as driven by a handful of people.
* Subgroup results now travel on the relationship they belong to rather than
  only in a table at the bottom. Nobody cross-references a table against the
  finding they are reading, so the finding carries `heterogeneity_moderator`,
  `heterogeneity_fdr`, `effect_by_group` and `consistent_across_groups`, and
  says when the direction reverses between levels.
* `effect_by_group` is structured data as well as a printable string. The
  string was all there was, and anything wanting to put the groups in a table
  had to parse it back apart.
* Both are shown on the finding itself, in `explain()`, in the score
  decomposition and in the graph edge table. A relationship halved by
  removing three people is a different object from one that survives losing
  half of them, and the score cannot tell them apart.

## Would this report repeat?

* `result$consensus` is a `ConsensusGraph`: what the graph looks like across
  resamples rather than in the one sample that happened to be collected.
  Per-edge stability answers "would this relationship come back?" one
  relationship at a time; it cannot answer "would this picture come back?".
  A graph whose edges are each recovered six times in ten is a stable
  structure if it is the same six edges every time and no structure at all
  if it is a different six, and a single drawing cannot tell those apart.
* Both disagreements with the report are surfaced. Relationships that were
  reported but rarely recur are the weakest thing in the document.
  Relationships that recur constantly but did not clear the threshold in the
  collected sample are the ones a reader cannot discover any other way.
* Recurrence is not recovery. A relationship appearing in nine resamples out
  of ten pointing a different way each time has been found nine times and
  established nothing, so consensus membership needs a consistent sign as
  well as a reappearance.
* Rank is tracked, not just presence: how often each headline finding kept
  its place, and the best and worst position it reached. Being third instead
  of second is not instability; falling out of the top ten is.
* Replicate graph sizes are kept rather than averaged. A graph that is 12
  edges in one resample and 40 in the next is not a graph, and a median
  hides exactly that.
* The caveat travels with the object and is printed with it. A bootstrap
  resamples the people who were measured, so it says how much the picture
  depends on which of them ended up in the study, and nothing about whether
  a relationship is real. A chance correlation in this sample recurs in
  almost every resample of it and looks perfectly stable. Null calibration,
  at `effort = "thorough"`, is what addresses that.
* Costs no additional model fits. The replicate graphs were already being
  built and discarded.
* A new figure draws each relationship as a line from its best to its worst
  rank with a dot at the median, fading as sign consistency falls. A short
  line on the left would have been reported whoever was sampled.

## Measured values and filled-in values

* Every relationship reports `data_quality`: the share of the data behind it
  that was measured rather than reconstructed. Imputation fills gaps with
  plausible numbers and from that point nothing downstream can tell a
  measurement from an estimate, so a variable that arrived a quarter empty
  reported exactly the same precision as one fully observed.
* `data_quality` scales `evidence_score` rather than entering `confidence`.
  Folding it into confidence would leave a reader unable to tell a wide
  interval from a column that was largely invented, and those have different
  remedies: the first is fixed by more samples and the second by nothing.
* Only imputed values count against it. Rows dropped for being incomplete
  cost sample size, which `confidence` already carries through *n*, and
  charging for them twice would double-count. On a dataset with nothing
  imputed every quality is 1 and no score moves.
* Imputation methods are not treated alike. Filling with the column mean
  collapses every gap onto one number and pulls associations toward the null;
  nearest-neighbour imputation borrows from correlated features and keeps
  most of the structure.
* `quality_flags` says it in words — which variable, how much of it, and by
  what method — because "38% of this was filled in with the column median" is
  something a reader can act on and 0.62 is not.
* An edge is worth no more than its worst-measured ingredient, endpoints and
  covariates alike, and `quality_limited_by` names it. Averaging would let a
  clean outcome and four clean covariates hide an exposure that was
  two-thirds reconstructed.
* Relationships are re-sorted after the discount, so the ranking cannot
  disagree with the scores printed beside it.

## Findings that exist only because the gaps were filled

* From `effort = "standard"`, the strongest relationships with the outcome
  are refitted using only the rows where the variable was actually measured,
  and `complete_case_estimate` is reported against the pooled one. A
  relationship that reverses direction without the filled-in rows was
  produced by the filling.
* One model fit per relationship, not a full analysis per replicate.
  Multiple imputation with Rubin's rules is the thorough version and costs
  *m* times the whole run; this catches the case that matters at a fixed
  cost.

## What each method actually measured

* Every observation records the quantity it estimated — a regression
  coefficient, a log odds ratio, a partial correlation, a permutation
  importance — and quantities are only pooled with others of the same family.
  Averaging a log hazard ratio with a correlation coefficient produced a
  headline number in no units at all, and it was the number the report led
  with.
* Quantities that are not effect estimates never enter the pooled estimate.
  A random forest importance is evidence that a variable matters and no
  evidence about how much or in which direction, so it now supports the edge
  without moving its magnitude.
* `pooled_from` and `not_pooled` say how many observations went into the
  headline estimate and which were left out for measuring something else.
  Both are shown in `explain()` and in the report.

## What the estimate would have to assume

* `analyze(..., dag = )` accepts a causal diagram — a `dagitty` object, a
  specification string, or a `data.frame` of `from`/`to` — and audits every
  relationship against it. Each edge gains `identifiable`,
  `identification_reason`, `required_adjustment` and `adjustment_problems`.
* `check_dag()` answers the same question before any model is fitted: given
  this structure, does adjusting for these variables identify this effect?
* An adjustment that includes a mediator, a collider, or anything the outcome
  causes downgrades the edge's `identification` to `"none"`. Both adjustments
  look like diligence and both make the estimate worse than leaving the
  variable alone; letting the edge keep its label would present the more
  misleading number as the more careful one. So does an adjustment that
  simply fails to close the backdoor paths.
* The diagram never touches an estimate. It is a claim about how the world
  works, not something recoverable from a correlation matrix, and it changes
  only what may be concluded. Without one, nothing is asserted either way and
  the report says why.

## Reading a result

* `explain()` gathers everything the engine knows about one variable into a
  single account: which methods supported it and at what level, its stability
  under resampling, what a confounder would have to look like to explain it
  away, the chains it sits on, and what each method reported on its own
  before anything was merged.
* `counterfactual()` expresses findings in the units the variable was
  measured in, by pushing the original value back through the stored
  preprocessing models. Nothing is included unless the user names it as
  something that could plausibly be acted on: a contrast for a genotype is
  arithmetic without meaning. The verb follows the identification label, so
  an adjusted association reads "is associated with" however high it scores.
* Every `EvidenceEdge` carries `contributions`, one row per contributing
  method with its own estimate, interval, p-value and sample size. A summary
  the reader cannot open is one they have to take on trust.
* The report explains where every number comes from, describes each method in
  plain language including what it cannot tell you, and shows the individual
  results behind each finding.

## Evidence hierarchy

* Methods carry an epistemic level from predictive to identified, and
  agreement between them is weighted by it. Five predictive methods
  concurring is weaker evidence than one longitudinal model plus one
  mediation analysis, and an unweighted vote said the opposite.
* Every relationship reports an E-value: how strong an unmeasured confounder
  would have to be, on both variables, to explain it away entirely.
* `direction_confidence` records how one-sided a disagreement about
  orientation was, since observational data rarely settles direction.
* Temporality is derived from the design rather than from the level. A
  mediation model assumes an ordering; it does not observe one.

## Cost control

* `effort` selects how much computation to spend, from `"fast"` to
  `"exhaustive"`, with every component separately overridable. Resampling and
  permutation are the only expensive parts, and both scale linearly.
* Bootstrap and cross-validation resampling report how often each
  relationship survived and how often it could have been seen at all.
* Null calibration runs the engine with the outcome shuffled, so a reader can
  tell whether twenty-five relationships is more than chance produces.
* Automatic model diagnostics, subgroup heterogeneity, negative controls and
  leave-one-block-out robustness.

## Blocks

* The screening budget is shared between blocks with a floor rather than
  ranked globally. A block of twenty thousand transcripts used to take every
  slot, and a five-variable clinical block disappeared from the analysis
  entirely, taking every cross-block mediation with it.
* Evidence between layers, block importance, per-kind scores, community
  composition and a block-level graph, in `result$network$blocks`.
* `cross_block` is reported beside the score and never inside it. Crossing
  layers makes a relationship more interesting, not better supported.

## Categorical variables

* Factors reach the analysis instead of being dropped at alignment. A
  variable with *k* categories becomes *k*-1 comparisons against its
  commonest level, and the reference travels with the column so the estimate
  can be read.
* Identifier-like columns and categories with too few observations are left
  out with a stated reason rather than silently.
* An unordered outcome with more than two categories is refused. Coding it
  1, 2, 3 would let every model run and assert that the third category is
  three times the first.
* `.duplicated_samples()` no longer mistakes a low-cardinality block for
  repeated specimens. A single genotype column taking three values made
  almost every row a duplicate of another, and preprocessing removed all but
  three samples.

## Figures

* Two circos plots: one with every variable as a tick inside its block, one
  summarising whole layers. Drawn in base graphics, so they cost no new
  dependency.


## Analysis engine

* `analyze()` builds evidence rather than fitting a model. Nine generators
  run against a `PreprocessingResult` — adjusted association, conditional
  independence, Cox, linear mixed models, bootstrap mediation, elastic net,
  random forest, Bayesian network structure learning and SEM — and each
  emits `EvidenceEdge` objects that an integrator merges into one scored
  directed graph.
* Every edge carries `identification` and `assumptions`: the strategy that
  would license reading it causally, and what would have to be true. An edge
  identified as `"none"` or `"adjustment"` is an association however high it
  scores, and says so when printed.
* Three scores are reported and kept apart on purpose. Strength is effect
  magnitude, confidence is estimation precision, consistency is how many
  methods agreed on the direction. Consistency is not validity: methods
  sharing an unmeasured confounder agree while all being biased, so it never
  feeds a causal claim.
* Reciprocal edges are resolved into a single direction. Keeping both put a
  two-cycle in the graph, which generated paths that read as mechanisms but
  were artefacts.
* `EvidenceGraph` carries communities (Louvain), centrality, and the ranked
  paths reaching the outcome, scored by their weakest link.
* The design is detected from `subject` and `time` and decides which
  generators apply: repeated measures unlock mixed models, a time/status
  pair unlocks survival, a single cross-section unlocks neither.
* Features are screened against the outcome before the pairwise stage, and
  the number never examined is reported as a limitation rather than hidden.
* `annotate_evidence()` attaches biological support from a local export.
  Kept out of `analyze()` so the analysis stays offline, deterministic and
  independent of a database version that the result cannot record.
* The generated report always states its limitations, including how many
  relationships rest on temporal precedence and how many samples the
  cross-block intersection cost.

## Preprocessing engine

* `preprocess()` executes the `PreprocessingRecipe` objects produced by
  `check_data()`. It derives nothing of its own: every method, parameter and
  threshold comes from the recipe, and the stage order comes from the
  recipe's `stage_order`.
* `apply_preprocessing()` replays a fitted pipeline on an external cohort
  using the stored models. Feature decisions are replayed so the columns
  match; sample decisions are not, because dropping rows from a validation
  set manufactures optimistic results.
* Every stage is a fit/apply pair and stores its fitted model, so the
  pipeline is reproducible by construction. A method that cannot express its
  decision as a transferable model is refused with an explanation rather
  than silently re-fitted (`mice`, `missForest`).
* `PreprocessingResult$steps` is the ordered record of what ran: stage,
  method, parameters, fitted model, dimensions before and after, what was
  removed and how long it took. The other slots are views over it.
* Methods live in a registry, so extending the engine means adding an entry.
  Currently implemented in base R, with no new hard dependencies:
  imputation (mean, median, mode, pseudocount, knn), the fifteen benchmark
  transformations plus arcsin, normalization (total sum, TIC, CPM, median,
  RLE, PQN, TMM, quantile), scaling (autoscaling, pareto, vast, range,
  robust) and feature selection (variance, correlation).

## Modality

* `load_data()` accepts a `modality` argument recording what each block
  actually is. The statistical type cannot recover it — RNA-seq counts and
  any other counts are identical as numbers — and it is what lets
  `check_data()` recommend modality-appropriate normalization.
* Modality never overrides a statistical constraint: size-factor methods are
  dropped when the block is not non-negative, and any magnitude-based
  normalization is dropped when the chosen transformation already discarded
  the magnitude scale.

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

## The samples every block has in common

* `check_data()` reports `shared_by_all` and `cumulative_overlap` alongside
  the pairwise matrix. The matrix cannot express the number an analysis
  actually runs on: every pair of twenty blocks can share the whole cohort
  while the twenty together share none of it, because each block can be
  missing a different part. A user reading the matrix has no way to see that
  coming, and it is what stops the analysis.
* A person counts as present in a block only where the block measured
  something on them. A module administered to ninety people but assembled
  against the full sample list carries a row for everyone with the rest left
  empty, and counting row names called it complete — so the overlap figures
  reported the whole cohort as shared and the collapse surfaced only after
  preprocessing dropped those rows, which is far too late for a check whose
  job is to run first.
* Both figures warn rather than error. Blocks that share nobody can still be
  preprocessed, and analysing a subset of them afterwards is an ordinary
  thing to want; refusing at `check_data()` would block that on the strength
  of a decision the user has not made yet. `analyze()` still refuses outright
  when the joint analysis is actually attempted. Blame is only assigned to a
  specific block when one block genuinely stands out; naming the top three
  when every block costs the same invents a culprit.
* The `analyze()` error now names which block to drop and what dropping it
  would leave, instead of reporting a count of zero and stopping.
* Blocks sharing no identifier at all with the largest one are diagnosed as
  a naming difference rather than a different cohort, with example
  identifiers from both sides. That is the common case and the one where a
  bare "0 samples shared" sends the reader to check data that is fine.
* When the raw blocks overlapped and the preprocessed ones do not,
  preprocessing is named as the cause and the identifiers are explicitly
  cleared. The pairwise matrix is computed before preprocessing and the
  alignment happens after it, so a healthy report followed by a failed
  analysis was previously unexplainable.

## Robustness

* `analyze()` says so when figures were requested and none could be drawn.
  The whole plot stage sits behind one guard that returns an empty list on
  failure, so a single broken figure silently produced a report with no
  pictures at all and nothing anywhere explaining why.
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
