## Start here

This guide assumes you know nothing: not about R, not about programming,
not about omics. Everything is explained from the beginning, and every
piece of code can be copied and run.

### The problem this library solves

Imagine you have measured thousands of things in a group of people. The
amount of each protein in their blood. The amount of each metabolite.
Which bacteria live in their gut. Their age, their weight, their
cholesterol. And then something happened to some of them — they
developed a disease, or they recovered, or they did not.

The question you actually care about is **why**. Which of those
thousands of measurements explain what happened? And not just which ones
are related to the outcome, but *through what chain*: does a bacterium
change a metabolite, which changes a protein, which changes the disease?

#### Why this is hard

With thousands of measurements and a few dozen people, you will find
relationships that are pure coincidence. Flip enough coins and some will
land heads ten times in a row. The difficulty is not finding
relationships; it is telling the real ones from the accidents, and then
not fooling yourself into thinking a real relationship means one thing
*causes* the other.

### What the library does

It takes you through four steps. Each one produces a result you can
inspect, and each one records every decision it made, so you can always
see why something happened.

1 **Check**

Looks at your data and finds the problems: missing values, variables
that never change, duplicated samples, odd distributions. It changes
nothing — it only reports.

2 **Prepare**

Cleans the data following a plan that step 1 wrote. Fills gaps, puts
everything on comparable scales, removes what is useless.

3 **Analyse**

Runs many different statistical methods, and combines what they all
found into one picture.

4 **Read**

Produces a report you can actually understand, including an honest
account of what the results do *not* prove.

### The one idea that makes this library different

Most tools give you a model and a list of numbers. This one gives you
**evidence**.

The difference matters. If you run six different statistical methods and
all six find the same relationship, that is more convincing than one
method finding it. But — and this is the part most tools get wrong — it
is *not* proof that one thing causes the other. If all six methods are
looking at the same data, and something you never measured is driving
both variables, all six will agree and all six will be wrong in the same
way.

So every relationship this library reports carries a label saying how
much you are allowed to claim. That label is not decoration. It is the
point.

## Getting set up

### What R is

R is a free program for working with data. You type instructions, it
carries them out. You will be typing instructions into a box and reading
what comes back.

1.  Download and install R from https://cran.r-project.org
2.  Download and install RStudio from
    https://posit.co/download/rstudio-desktop/ — this is a friendlier
    window for using R. Open RStudio, not R.
3.  In RStudio you will see a panel called **Console**. That is where
    you type. Paste a line, press Enter, and it runs.

### What a package is

R on its own can do arithmetic and statistics. A **package** adds new
abilities. CausalMultiOmics is a package. Installing it means
downloading its code once; loading it means switching it on for your
current session.

    # Install the helper that installs packages from source (once, ever)
    install.packages("devtools")

    # Install CausalMultiOmics from the folder you have it in
    devtools::install("path/to/CausalMultiOmics")

    # Switch it on (every time you start R)
    library(CausalMultiOmics)

**Anything after a \# is a note to a human.** R ignores it. You can
paste those lines too; nothing will happen.

### Checking it worked

    library(CausalMultiOmics)

    # Read the package's own front page
    ?CausalMultiOmics

If a help page opens, you are ready.

## A complete worked example

This runs from an empty R session to a finished report. Paste each block
in order. Nothing here needs any data of your own — we invent some.

### Step 0 — Invent some data

We will build a small dataset where we *already know* the answer, so we
can check the library finds it. We plant a chain: a protein affects a
metabolite, and the metabolite affects the outcome.

    library(CausalMultiOmics)

    set.seed(42)          # makes the random numbers repeatable
    n <- 90               # ninety people

    # The planted chain
    protein    <- rnorm(n)                              # a protein level
    metabolite <- 0.8 * protein + rnorm(n, sd = 0.6)    # affected by the protein
    outcome    <- 0.9 * metabolite + rnorm(n, sd = 0.7) # affected by the metabolite

    people <- paste0("P", 1:n)   # names for our ninety people

**What just happened.** rnorm(n) makes n random numbers. The \<- arrow
means "store this under that name". So protein now holds 90 numbers.

### Step 1 — Put the data into the library

Measurements come in **blocks**. One block per kind of measurement: one
for proteins, one for metabolites, one for clinical variables. Blocks do
not have to contain the same people.

    proteins <- data.frame(
      APOA1 = protein,
      CRP   = rnorm(n),          # a protein unrelated to anything
      row.names = people
    )

    metabolites <- data.frame(
      SCFA = metabolite,
      TMAO = rnorm(n),           # another unrelated one
      row.names = people
    )

    # Everything we know about the people themselves
    info <- data.frame(
      sample_id    = people,
      HDL_function = outcome,
      age          = rnorm(n, 55, 8)
    )

    study <- load_data(
      assays   = list(proteins = proteins, metabolites = metabolites),
      metadata = info,
      modality = c(proteins = "proteomics", metabolites = "metabolomics")
    )

    study

#### Row names are not optional

row.names = people labels each row with whose measurement it is. Without
it the library refuses to continue — on purpose. If blocks have no
labels, two unrelated blocks would look as if they shared all their
samples, and every result after that would be nonsense.

### Step 2 — Check the data

    quality <- check_data(study)

    quality              # a short summary
    summary(quality)     # everything it found

This changes nothing. It reports: how much is missing, which variables
never vary, whether samples are duplicated, what kind of numbers each
block contains, and a quality score out of 100. It also writes a
**plan** for cleaning the data — which you can read before anything is
applied:

    # The plan for the proteins block
    quality$recipes$proteins

### Step 3 — Clean the data

    clean <- preprocess(study, quality)

    clean                # what was done
    summary(clean)       # step by step

preprocess() decides nothing on its own. It executes the plan from step
2, in order, and records every step: what ran, with what settings, how
the data looked before and after.

### Step 4 — Analyse

    results <- analyze(
      clean,
      outcome    = "HDL_function",
      covariates = "age"
    )

    results
    summary(results)

This runs up to nine different statistical methods, collects every
relationship each one found, and merges them into a single scored map.

### Step 5 — Read the results

    # A report anyone can read, saved as a web page
    report(results, file = "my_report.html")

    # Or straight to the screen
    report(results, format = "console")

### Did it find what we planted?

    results$causal_paths

You should see APOA1 -\> SCFA -\> HDL_function near the top — the chain
we planted. The unrelated variables (CRP, TMAO) should score far lower.

### Step 6 — Ask why a variable is there

    explain(results, "SCFA")

This gathers everything the engine knows about one variable into a
single account: which methods supported it and at what kind of evidence,
how stable it was when the analysis was repeated on resampled data, what
a hidden factor would have to look like to explain it away, which chains
it sits on, and what each method reported *on its own* before anything
was merged.

### Step 7 — Ask what changing something would mean

    counterfactual(results, modifiable = "metabolites")

This expresses the findings in the units the variable was measured in:
“an increase of 18.7 in APOA1 from 135.7 is associated with a 75% lower
risk of the event”.

#### Why you must say what is changeable

Nothing is included unless you name it. That is deliberate: a contrast
for a genotype or an ancestry component is arithmetic without meaning,
and printing one invites a reading nothing can support. You are the one
who knows what could plausibly be acted on — a diet, a treatable protein
— and what could not.

Even for the variables you do name, the sentence says **is associated
with**, not **would reduce**, unless the evidence supports that word. It
describes how the outcome differs between people whose measurements
differ, which is not the same as what would happen if you changed one
person.

### Step 8 — Decide how much computation to spend

Everything expensive is off unless you ask for it, through one argument.

    analyze(clean, outcome = "HDL_function", effort = "fast")       # generators once
    analyze(clean, outcome = "HDL_function", effort = "standard")   # + diagnostics, 50 resamples
    analyze(clean, outcome = "HDL_function", effort = "thorough")   # + null calibration
    analyze(clean, outcome = "HDL_function", effort = "exhaustive") # 500 resamples, every method

Each piece can also be set on its own, so you never have to accept a
preset wholesale:

    analyze(clean, outcome = "HDL_function",
            effort       = "fast",
            resample     = 100,              # test stability anyway
            permutations = 50,               # and calibrate against noise
            negative_controls = "height",    # this should not appear
            heterogeneity     = c("sex"),    # does it hold in both groups?
            modifiable        = "metabolites")

**Cost.** Resampling and permutation are the only parts that take real
time, and both scale linearly with their number. A hundred resamples
costs roughly a hundred times one pass. Start with "fast" while you are
exploring.

### Applying the same cleaning to new people

If you later collect a second group and want to compare it fairly, you
must clean it *exactly* the same way — using the numbers learned from
the first group, not recalculated from the second.

    ready <- apply_preprocessing(new_study, clean)

**Why this matters.** If you re-cleaned the new group using its own
averages, the two groups would be on different scales and any comparison
would be meaningless. This function reuses the stored numbers.

## How the pieces fit together

Four functions, four results. Each function takes the result of the
previous one. Nothing is ever modified in place.

``` flow
       your measurements
              |
              v
      +----------------+
      |  load_data()   |
      +----------------+
              |
              v
       MultiOmicsData        the container: blocks + information about people
              |
              v
      +----------------+
      |  check_data()  |     looks, never touches
      +----------------+
              |
              v
       CMOValidation         problems found + a cleaning plan per block
              |
              v
      +----------------+
      |  preprocess()  |     executes the plan, records every step
      +----------------+
              |
              v
    PreprocessingResult      clean data + the exact recipe used
              |               |
              |               +--> apply_preprocessing()  same recipe, new people
              v
      +----------------+
      |   analyze()    |     runs many methods, merges the evidence
      +----------------+
              |
              v
         CMOResult           the evidence graph + interpretation
              |
              v
      +----------------+
      |   report()     |     a web page a human can read
      +----------------+

  and, on the same CMOResult, at any time:

      explain(result, "APOA1")            why is this variable here
      counterfactual(result, modifiable)  what changing it would mean
      annotate_evidence(result, refs)     attach outside knowledge
```

### Why the steps are separate

Each step is deliberately kept from doing the next one's job.

- **check_data() decides, preprocess() executes.** The cleaning plan is
  written down before anything is applied, so you can read it, change
  it, or disagree with it.
- **analyze() starts from cleaned data, not raw.** By the time analysis
  begins, every cleaning decision is already fixed and recorded. The
  analysis cannot quietly re-clean the data to suit itself.
- **Nothing is modified in place.** Your original data is never altered.
  Every step returns a new object.

### What a block is, beyond a group of columns

Each block is a layer of biology measured with one technology, and the
library treats it as one throughout.

- **Cleaning is per block.** Each gets its own data type detected, its
  own contest between reshapings, its own recipe and its own order of
  steps.
- **Declaring the modality picks the method.** Telling load_data() that
  a block is RNA-seq rather than proteomics changes which normalisation
  is recommended, because the numbers alone cannot tell them apart.
- **The screening budget is shared out.** Twenty thousand transcripts
  against five clinical variables is not a fair contest, so each block
  gets a quota with a floor rather than the biggest one taking every
  slot.
- **Results can be read one layer at a time.** Evidence between every
  pair of blocks, how much of the total each carries, and what each
  detected community is made of — in results\$network\$blocks.
- **Chains that cross layers are what mediation looks for.** A mediator
  inside the same block as its exposure is usually collinearity, not
  mechanism.

### Variables that are categories

Sex, diet, treatment group. Factors pass through loading and cleaning
untouched — nothing sensible comes of scaling a category — and are
encoded when the analysis starts. A variable with *k* categories becomes
*k*−1 yes/no comparisons against a reference, which is the commonest
category.

You see them named like diet=western, and the report words them
properly: *“Being western rather than mediterranean goes together with
lower HDL”*, not “higher diet=western”.

**Guards.** A column with more categories than people can sensibly
compare — a barcode, a sample ID — is treated as an identifier and left
out, and so are categories with too few observations to estimate
anything. Both are reported, never dropped silently.

## The objects

Each step returns an object — a labelled box holding everything that
step produced. You open a box with the \$ sign.

    results$evidence      # the "evidence" drawer of the results box
    results$graph$edges   # the "edges" drawer inside the "graph" drawer

Two commands work on every object:

    results             # short summary
    summary(results)    # everything, laid out

#### MultiOmicsData

Your data, organised. Produced by load_data().

- assays: The measurement blocks themselves
- metadata: What you know about each person
- modality: What kind of assay each block is
- sample_info, feature_info: Which samples and variables exist where
- history: A log of what has been done

#### CMOValidation

What the check found. Produced by check_data().

- valid: Whether the data is usable at all
- score: Overall quality, 0–100
- errors, warnings: Problems, by severity
- diagnostics: Full statistics per block
- recipes: The cleaning plan per block
- transformations: Which reshaping was tested and why one won
- plots, tables: Figures and summaries

#### BlockDiagnostics

Everything measured about one block. Lives inside
CMOValidation\$diagnostics.

- data_type: Counts, proportions, continuous…
- missing_percent: How much is absent
- mean, median, sd, skewness…: The usual statistics
- constant_features: Variables that never change
- pca, correlation: Structure between variables

#### PreprocessingRecipe

The cleaning plan for one block. A plan, not an action.

- imputation: How to fill gaps
- transformation: How to reshape the numbers
- normalization: How to make samples comparable
- scaling: How to make variables comparable
- stage_order: The order these must run in

#### PreprocessingResult

The plan, executed. Produced by preprocess().

- data: The cleaned data
- steps: Every step that ran, in order, with its settings
- models: The numbers learned, so they can be reused
- removed_samples, removed_features: What was dropped and why
- quality: Before and after comparison

#### EvidenceEdge

One relationship between two variables. The atom of the whole analysis.

- source, target: From what, to what

- estimate, direction: How strong, which way

- evidence_score: 0–100 overall

- strength, confidence, consistency: The three parts of that score

- identification:

  **How much you may claim about cause**

- assumptions: What would have to be true

#### EvidenceGraph

All the relationships together, as a map.

- nodes: The variables
- edges: The relationships
- paths: Chains leading to the outcome
- communities: Groups of variables that hang together

#### CMOResult

The finished analysis. Produced by analyze().

- evidence: Every merged relationship
- graph: The map
- causal_paths: Chains, best first
- interpretation: Drivers, mediators, hubs, candidates
- importance: Which variables matter most
- report: Findings and limitations

#### TransformationRecommendation

The contest between ways of reshaping one block, and who won.

- candidates: What was tried
- ranking, metrics: How each scored
- recommended: The winner
- justification: Why it won

#### CMOExplanation

Everything known about one variable. Produced by explain().

- edges: Every relationship it takes part in
- paths: Chains it sits on
- roles: Hub, mediator, bridge…
- encoding: Set when it came from a category

#### CMOCounterfactual

What changing something would mean, in original units. Produced by
counterfactual().

- table: One row per contrast
- statements: The same, as sentences with their caveats
- skipped: What was left out, and why

## Reading the scores honestly

This section is the most important one in the guide. If you read nothing
else, read this.

### The three numbers

Every relationship gets a score from 0 to 100. It is built from three
things that are kept separate on purpose:

|  |  |
|----|----|
| Strength | How **big** the relationship is. When one variable moves, how much does the other move? |
| Confidence | How **sure** we are of that number. More people and a tighter estimate mean higher confidence. |
| Consistency | How many of the methods **agreed**. Several different methods look at the same data; this counts how many saw the same thing in the same direction. |

#### Agreement is not proof

It is tempting to read "six methods out of six agreed" as "this must be
real, and it must be causal". It is not.

All six methods are looking at **the same data**. If something you never
measured is driving both variables, all six will find the relationship,
all six will agree, and all six will be wrong in exactly the same way.
Consistency tells you the finding is **stable**. It says nothing about
whether it is **true**.

Ice cream sales and sunburn rise together. Every method you throw at
that data will agree. Ice cream still does not cause sunburn — hot
weather causes both.

### The label that says what you may claim

Because of the above, every relationship carries an identification
label:

**none**

The two were seen to move together and nothing else was taken into
account. Weakest claim.

**adjustment**

Known factors were subtracted out. Stronger — but only for the factors
you actually measured.

**temporal**

One was measured before the other, so the relationship cannot run
backwards. Real progress, but a third factor driving both is still
possible.

**instrument**

A special variable was used that, under strong assumptions, does support
a causal reading.

An edge labelled none or adjustment is an **association**, no matter how
high its score. The library will tell you so:

    results$evidence[[1]]              # prints the label and a warning if weak
    summary(results$evidence[[1]])    # prints the full list of assumptions

### Not every method makes the same kind of claim

A random forest says a variable helps predict. A Cox model on exposures
measured before the event says something considerably stronger. Counting
those as one vote each would make five predictive methods agreeing look
like better evidence than one longitudinal model plus one mediation
analysis, which is backwards.

So every method carries a **level**, and Agreement is weighted by it:

|  |  |
|----|----|
| 1 · predictive | Random forest, elastic net. Says the variable carries information about the outcome; nothing about the size or the direction of an effect. |
| 2 · associational | Linear and logistic regression, partial correlation, structure learning. Estimates an effect, conditional at best on what was measured. |
| 3 · temporal | Survival models, mixed models on repeated measures. The exposure precedes the outcome, which rules out the relationship running backwards. |
| 4 · mechanistic | Mediation. Commits to a specific chain and estimates its parts. |
| 5 · identified | An instrument or a validated adjustment set. |

### Four more numbers worth knowing

#### E-value

How strong a hidden factor would have to be, on both the variable and
the outcome, to explain the whole relationship away. Near 1 means a
trivially weak one would do it; above 3 means it would have to be
stronger than most known risk factors. It does not say a hidden factor
exists — it says how much would be needed.

#### Recovered in

The analysis is repeated on many random re-samples of the same people.
This is how often the relationship survived. Something recovered in half
the resamples is fragile however high its score, and this is the one
number that catches that.

#### Direction confidence

When methods disagree about which way the arrow points, how one-sided
the disagreement was. 0.5 means it was a coin flip. Only measurement
order really settles direction; everything else is inference.

#### Found on shuffled data

The engine is run again with the outcome randomly reshuffled, which
destroys any real relationship. If it still finds twenty-two
relationships, your twenty-five mean very little. This is the single
most useful number about a whole graph, and you get it with permutations
= 50.

### What to do with this

A high score with adjustment is a good **hypothesis**. It tells you
where to look next — an experiment, a longitudinal follow-up, a
knockout. It is not a conclusion. The library is built to help you
generate hypotheses that are worth testing, not to hand you answers you
did not earn.

### Things that raise or lower your confidence

Two checks are worth asking for explicitly, because they catch failures
that no score will:

    analyze(clean, outcome = "event",
            negative_controls = c("height"),   # should NOT appear
            permutations      = 50)            # how much appears by chance

A **negative control** is something you know has no business being part
of the mechanism. If height turns up with a strong score, the problem is
not height: it is a sign that something other than biology — a batch
effect, a technical artefact — is driving the whole graph.

Prior biological knowledge can be attached too, with
annotate_evidence(). It is reported *beside* the score and never folded
into it, so that a relationship nobody has published yet is not
penalised for being new.

## What is in each file

The library is six files of R code. You never need to open them to use
it — this is here so you know where things live.

#### classes.R

12 functions · 1604 lines

Defines the shape of every object the library produces. No calculations
here at all: each function just builds an empty box with the right
labelled drawers, so that a half-finished analysis is still a
well-formed object.

#### data_loading.R

2 functions · 421 lines

Getting your data in. load_data() checks that the blocks are usable,
that every sample is labelled, and assembles them into one container. It
is the only place that touches your raw input.

#### validation.R

90 functions · 3815 lines

The inspection engine. Works out what kind of numbers each block holds,
measures its quality from every angle, runs a contest between ways of
reshaping it, and writes the cleaning plan. It never modifies anything.

#### preprocessing.R

39 functions · 2725 lines

The cleaning engine. First half is the catalogue of methods, each one a
fit/apply pair so the decision can be stored and reused; second half
executes a plan and records every step.

#### analysis.R

105 functions · 9886 lines

The evidence engine. Nine different statistical methods each report the
relationships they find, and an integrator merges them into one scored
map, keeping track of how much each relationship is allowed to claim.

#### methods.R

89 functions · 6363 lines

Everything to do with showing results: how each object prints and
summarises, plus the two HTML report builders.

#### comparison.R

4 functions · 382 lines

#### options.R

5 functions · 275 lines

#### positivity.R

6 functions · 386 lines

#### sensitivity.R

4 functions · 470 lines

#### simulation.R

5 functions · 539 lines

## Complete function reference

Every function in the library. Names beginning with a dot (.like_this)
are internal — they do the work behind the scenes and you never call
them directly. The seven without a dot are the ones you use.

public functions only

### classes.R 12 functions

Defines the shape of every object the library produces. No calculations
here at all: each function just builds an empty box with the right
labelled drawers, so that a half-finished analysis is still a
well-formed object.

`MultiOmicsData`internalclasses.R:17

Create a MultiOmicsData object

Core container storing every data block loaded into CausalMultiOmics.

``` sig
MultiOmicsData()
```

**Returns:** A MultiOmicsData object.

`BlockDiagnostics`internalclasses.R:72

Create a BlockDiagnostics object

Stores every statistic calculated for one data block.

``` sig
BlockDiagnostics()
```

**Returns:** A BlockDiagnostics object.

`TransformationRecommendation`internalclasses.R:221

Create a TransformationRecommendation object

Stores automatic transformation benchmarking for one data block.

``` sig
TransformationRecommendation()
```

**Returns:** A TransformationRecommendation object.

`PreprocessingRecipe`internalclasses.R:309

Create a PreprocessingRecipe object

Stores the preprocessing strategy recommended during validation or
specified manually by the user.

``` sig
PreprocessingRecipe()
```

**Returns:** A PreprocessingRecipe object.

`PreprocessingResult`internalclasses.R:496

Create a PreprocessingResult object

Stores every output generated during preprocessing.

``` sig
PreprocessingResult()
```

**Returns:** A PreprocessingResult object.

`CMOValidation`internalclasses.R:604

Create a CMOValidation object

Stores every result generated during data validation.

``` sig
CMOValidation()
```

**Returns:** A CMOValidation object.

`EvidenceEdge`internalclasses.R:928

Create an EvidenceEdge object

One directed relationship between two variables, as reported by one
analysis method. Every evidence generator emits these; the integrator
merges the ones that describe the same relationship and scores them.

``` sig
EvidenceEdge()
```

**Returns:** An EvidenceEdge object.

`EvidenceGraph`internalclasses.R:1227

Create an EvidenceGraph object

The integrated directed graph of evidence: variables as nodes,
integrated `EvidenceEdge` objects as edges, plus the structure derived
from them (communities, paths, centrality).

``` sig
EvidenceGraph()
```

**Returns:** An EvidenceGraph object.

`ConsensusGraph`internalclasses.R:1281

Create a ConsensusGraph object

What the graph looks like across resamples rather than in the one sample
that happened to be collected. Per-edge stability answers "would this
relationship come back?" one relationship at a time. It cannot answer
"would this picture come back?", and those are different questions: a
graph whose edges are each recovered six times in ten is a stable
structure if it is the same six edges every time and no structure at all
if it is a different six. A reader shown a single drawing has no way to
tell which they are looking at. Everything here is read off the
resampling that already runs, so it costs no additional model fits.

``` sig
ConsensusGraph()
```

**Returns:** A ConsensusGraph object.

`Hypothesis`internalclasses.R:1362

Create a Hypothesis object

A `CMOResult` is a body of evidence. A `Hypothesis` is one claim taken
out of it, carrying everything needed to decide whether to act on it,
argue with it, or go and settle it. The distinction matters because a
graph of forty scored relationships is not a scientific statement. It is
material from which statements can be made, and the making is where the
judgement lives: which relationship, at what strength, under which
assumptions, and what would have to be observed for it to be wrong. The
last of those is what separates this from a result. A finding says what
was seen; a hypothesis says what would change its author's mind. Both
are derived here from what the engine already knows about the
relationship, so the answer is specific to it rather than a paragraph of
generic caution.

``` sig
Hypothesis()
```

**Returns:** A Hypothesis object.

`ModuleGraph`internalclasses.R:1452

Create a ModuleGraph object

The same evidence read at the resolution between a single feature and a
whole block. Measured variables are rarely independent things. Fifty
transcripts moving together are one biological process measured fifty
times, and testing each separately answers a question nobody asked while
paying a multiplicity penalty fifty times over. A module is that
process, and its first principal component is the closest thing to
measuring it directly. The results here are not additional evidence.
They are the same measurements re-expressed, so a module and its members
agreeing is arithmetic rather than replication, and anything reading
this has to say so.

``` sig
ModuleGraph()
```

**Returns:** A ModuleGraph object.

`CMOResult`internalclasses.R:1512

Create a CMOResult object

Top-level container for a complete CausalMultiOmics analysis. The result
of `analyze()` is not a model but a body of evidence: every method that
ran contributes edges, the integrator merges and scores them, and the
graph plus its interpretation is what the user reads. Each slot is
filled by the stage that produces it, so a partially finished analysis
is represented by the slots that are still empty rather than by a
missing element.

``` sig
CMOResult()
```

**Returns:** A CMOResult object.

### data_loading.R 2 functions

Getting your data in. load_data() checks that the blocks are usable,
that every sample is labelled, and assembles them into one container. It
is the only place that touches your raw input.

`.has_sample_ids`internaldata_loading.R:24

Does a block carry real sample identifiers?

A matrix without rownames and a data.frame with automatic row names both
come out of `as.data.frame()` labelled "1", "2", ..., which is
indistinguishable from identifiers the user chose deliberately. That
matters: two unrelated blocks would then appear to share every sample,
the overlap matrix would be fabricated, and metadata matching would join
on positions instead of samples. The check has to run on the original
object, before conversion, because afterwards the information is gone.
`.row_names_info()` returns a negative row count exactly when the row
names are the automatic compact-integer kind.

``` sig
.has_sample_ids(x)
```

- x: A matrix or data.frame.

**Returns:** `TRUE` when the block has usable sample identifiers.

`load_data`publicdata_loading.R:99

Load Multi-Block Data

Creates a MultiOmicsData object from one or more data blocks. Each
element of `assays` represents an independent block of variables. Blocks
may correspond to any data modality (omics, imaging, clinical,
environmental, wearable devices, etc.) and are not required to contain
the same samples. This function only imports and organizes the data.
Validation, harmonization, preprocessing and integration are performed
by later functions in the analysis workflow. Every block must carry
sample identifiers in its row names. They are what links blocks to each
other and to `metadata`, so a block that relies on automatic row names
is rejected rather than silently labelled by position. Blocks may
optionally declare their assay `modality`. The statistical type of a
block can be detected from the numbers, but the modality cannot: RNA-seq
counts and any other counts look identical, and yet they call for
different normalization families. Declaring it lets `check_data()`
recommend modality-appropriate methods; leaving it out simply falls back
to type-driven defaults.

``` sig
load_data(assays, metadata = NULL, modality = NULL)
```

- assays: Named list of matrices or data frames, each with sample
  identifiers as row names.

- metadata:

  Optional sample metadata. A `sample_id` column is used to match rows
  against the identifiers found in `assays`.

- modality:

  Optional named character vector declaring the assay modality of each
  block, for example `c(rna = "rnaseq", prot = "proteomics")`. Names
  must match names in `assays`; blocks left out are recorded as
  `"unknown"`. Recognised values are `"rnaseq"`, `"proteomics"`,
  `"metabolomics"`, `"microbiome"`, `"methylation"`, `"clinical"`,
  `"imaging"` and `"other"`. Any other string is stored but behaves like
  `"unknown"`.

**Returns:** A MultiOmicsData object.

### validation.R 90 functions

The inspection engine. Works out what kind of numbers each block holds,
measures its quality from every angle, runs a contest between ways of
reshaping it, and writes the cleaning plan. It never modifies anything.

`.safe_try`internalvalidation.R:15

Evaluate an expression, falling back to a default

``` sig
.safe_try(expr, default = NULL)
```

Arguments: `expr, default`

`.with_preserved_seed`internalvalidation.R:42

Evaluate an expression without leaking RNG state into the caller's
session

Parts of the diagnostic engine need randomness (fold assignment, Shapiro
subsampling) and want it reproducible, but `check_data()` is a read-only
audit: it must not move the user's random number generator, or every
simulation run afterwards would silently change. The current
`.Random.seed` is saved, the expression is evaluated under `seed`, and
the original state is put back on exit.

``` sig
.with_preserved_seed(expr, seed = 1L)
```

- expr: Expression to evaluate.

- seed:

  Integer seed used while evaluating `expr`.

`.set_slot`internalvalidation.R:76

Assign a (possibly NULL) value to a list slot without deleting it

Ordinary `$<-` / `[[<-` assignment of `NULL` removes the element from a
list. Every class defined in classes.R has a fixed set of slots that
must always remain present (even when their value is `NULL`), so every
assignment that might carry a `NULL` value goes through this helper
instead.

``` sig
.set_slot(obj, name, value)
```

Arguments: `obj, name, value`

`.n_rows`internalvalidation.R:90

Row count that tolerates NULL and non-rectangular input

`nrow()` returns `NULL` for both, and `NULL == 0` is `logical(0)`, which
blows up the surrounding `if`.

``` sig
.n_rows(x)
```

Arguments: `x`

`.numeric_columns`internalvalidation.R:102

Names of numeric columns in a block

``` sig
.numeric_columns(block)
```

Arguments: `block`

`.numeric_matrix`internalvalidation.R:114

Numeric columns of a block as a matrix

``` sig
.numeric_matrix(block)
```

Arguments: `block`

`.pool_numeric`internalvalidation.R:128

Pool finite numeric values from a block

``` sig
.pool_numeric(block)
```

Arguments: `block`

`.block_modality`internalvalidation.R:144

Modality declared for a block, defaulting to "unknown"

Hand-built `MultiOmicsData` objects need not carry the slot at all, so
every read goes through here.

``` sig
.block_modality(object, block_name)
```

Arguments: `object, block_name`

`.find_metadata_column`internalvalidation.R:164

Find first metadata column matching patterns

``` sig
.find_metadata_column(metadata, patterns)
```

Arguments: `metadata, patterns`

`.detect_variable_type`internalvalidation.R:188

Detect the statistical nature of a single variable

``` sig
.detect_variable_type(x)
```

Arguments: `x`

`.detect_data_type`internalvalidation.R:243

Detect the statistical nature of a whole data block

``` sig
.detect_data_type(block)
```

- block: A data.frame representing one data block.

**Returns:** A character string: one of `"continuous"`, `"count"`,
`"binary"`, `"ordinal"`, `"proportion"`, `"compositional"`,
`"censored"`, `"mixed"`.

`.compute_basic_stats`internalvalidation.R:325

Compute basic descriptive statistics for a vector

``` sig
.compute_basic_stats(x)
```

Arguments: `x`

`.skewness`internalvalidation.R:364

Compute sample skewness of a vector

``` sig
.skewness(x)
```

Arguments: `x`

`.kurtosis`internalvalidation.R:384

Compute excess kurtosis of a vector

``` sig
.kurtosis(x)
```

Arguments: `x`

`.shapiro_p`internalvalidation.R:404

Shapiro-Wilk normality test p-value

``` sig
.shapiro_p(x)
```

Arguments: `x`

`.outliers_iqr`internalvalidation.R:425

Flag outliers using the IQR rule

``` sig
.outliers_iqr(x)
```

Arguments: `x`

`.compute_shape_stats`internalvalidation.R:447

Compute shape statistics and normality label

``` sig
.compute_shape_stats(x)
```

Arguments: `x`

`.distribution_profile`internalvalidation.R:469

Full distribution profile of a pooled numeric vector

Shape statistics plus the location/spread summaries that the
`PreprocessingRecipe` expected-quality slots are built from.

``` sig
.distribution_profile(x)
```

Arguments: `x`

`.compute_composition_stats`internalvalidation.R:491

Compute zero, sign, and sparsity statistics

``` sig
.compute_composition_stats(m)
```

Arguments: `m`

`.compute_missing_stats`internalvalidation.R:532

Compute missingness statistics for a block

``` sig
.compute_missing_stats(block)
```

Arguments: `block`

`.constant_features`internalvalidation.R:567

Detect constant features in a block

``` sig
.constant_features(block)
```

Arguments: `block`

`.near_constant_features`internalvalidation.R:579

Detect near-constant features above a threshold

``` sig
.near_constant_features(block, threshold = 0.95)
```

Arguments: `block, threshold`

`.duplicated_features`internalvalidation.R:607

Detect duplicated feature columns

``` sig
.duplicated_features(block)
```

Arguments: `block`

`.duplicated_samples`internalvalidation.R:621

Detect duplicated sample rows

``` sig
.duplicated_samples(block)
```

Arguments: `block`

`.compute_multivariate`internalvalidation.R:661

Compute covariance, correlation, and PCA

``` sig
.compute_multivariate(block, max_features = 200)
```

Arguments: `block, max_features`

`.discover_outcome`internalvalidation.R:713

Discover survival outcome columns in metadata

``` sig
.discover_outcome(object, block_name)
```

Arguments: `object, block_name`

`.build_block_diagnostics`internalvalidation.R:779

Build diagnostics summary for one block

``` sig
.build_block_diagnostics(block_name, block, modality = "unknown")
```

Arguments: `block_name, block, modality`

`.candidate_transformations`internalvalidation.R:863

List candidate transformations for a data type

``` sig
.candidate_transformations(data_type)
```

Arguments: `data_type`

`.t_shift_positive`internalvalidation.R:892

Shift values to be strictly positive

``` sig
.t_shift_positive(x)
```

Arguments: `x`

`.t_log`internalvalidation.R:906

Log transform after positive shift

``` sig
.t_log(x, base = exp(1))
```

Arguments: `x, base`

`.t_sqrt`internalvalidation.R:915

Square root, floored at zero

`na.rm` is deliberately left at its default: with `na.rm = TRUE`
`pmax()` does not skip missing values, it replaces them with the other
operand (0), which would silently impute every `NA` as zero.

``` sig
.t_sqrt(x)
```

Arguments: `x`

`.t_cuberoot`internalvalidation.R:921

Signed cube root transform

``` sig
.t_cuberoot(x)
```

Arguments: `x`

`.t_vst`internalvalidation.R:928

Anscombe variance-stabilizing transform

See `.t_sqrt()` for why `na.rm` is not passed to `pmax()`.

``` sig
.t_vst(x)
```

Arguments: `x`

`.t_rank`internalvalidation.R:934

Rank transform with averaged ties

``` sig
.t_rank(x)
```

Arguments: `x`

`.t_quantile`internalvalidation.R:940

Quantile-normalize via rank to normal

``` sig
.t_quantile(x)
```

Arguments: `x`

`.t_robust`internalvalidation.R:953

Median-MAD robust standardization

``` sig
.t_robust(x)
```

Arguments: `x`

`.t_boxcox`internalvalidation.R:968

Box-Cox transform with optimized lambda

``` sig
.t_boxcox(x)
```

Arguments: `x`

`.t_yeojohnson`internalvalidation.R:1003

Yeo-Johnson transform with optimized lambda

``` sig
.t_yeojohnson(x)
```

Arguments: `x`

`.yj_transform`internalvalidation.R:1025

Apply Yeo-Johnson formula to one value

``` sig
.yj_transform(x, lambda)
```

Arguments: `x, lambda`

`.t_clr`internalvalidation.R:1045

Centered log-ratio transform of a matrix

``` sig
.t_clr(m)
```

Arguments: `m`

`.t_alr`internalvalidation.R:1059

Additive log-ratio transform of a matrix

``` sig
.t_alr(m)
```

Arguments: `m`

`.t_ilr`internalvalidation.R:1073

Isometric log-ratio transform of a matrix

``` sig
.t_ilr(m)
```

Arguments: `m`

`.apply_transformation`internalvalidation.R:1101

Apply a named transformation to a data block

Only numeric columns are transformed; every other column is left
untouched. Row and column names are preserved.

``` sig
.apply_transformation(block, method)
```

Arguments: `block, method`

`.transformation_metrics`internalvalidation.R:1211

Compute quality metrics for a transformation

``` sig
.transformation_metrics(before, after)
```

Arguments: `before, after`

`.composite_score`internalvalidation.R:1253

Composite score for ranking a transformation

``` sig
.composite_score(metrics, outcome_perf = NA_real_)
```

Arguments: `metrics, outcome_perf`

`.outcome_performance`internalvalidation.R:1301

Fold-averaged association between a transformed block and the outcome

The sample is split into `k` folds and the feature-outcome association
is measured inside each fold, then averaged. Splitting keeps the
estimate from being dominated by a handful of influential samples, so
the result is a measure of how \*stably\* the association survives a
given transformation. This is deliberately NOT cross-validation: no
model is fitted on the training folds and evaluated on a held-out one.
Every quantity below is computed within the fold it is measured on, so
it carries the optimistic bias of an in-sample statistic and must not be
read as out-of-sample predictive performance. It is used only to rank
transformations against each other, where that bias applies equally to
every candidate.

``` sig
.outcome_performance(m, outcome)
```

- m: Numeric matrix of transformed features (rows = samples).

- outcome:

  Outcome descriptor from `.discover_outcome()`.

**Returns:** A scalar in `[0, 1]`, or `NA_real_` when the association
cannot be estimated.

`.benchmark_transformations`internalvalidation.R:1400

Benchmark candidate transformations for one data block

``` sig
.benchmark_transformations(block_name, block, data_type, outcome = NULL)
```

Arguments: `block_name, block, data_type, outcome`

`.compute_quality_score`internalvalidation.R:1570

Compute a 0-100 quality score for one block

``` sig
.compute_quality_score(diagnostic)
```

Arguments: `diagnostic`

`.stage_order`internalvalidation.R:1622

Intended order of the preprocessing stages for one data type

`PreprocessingRecipe` stores which method to use at every stage but not
the order in which the stages must run. For count data the library size
has to be normalized before any variance-stabilizing transformation is
applied; for every other data type the transformation comes first. The
resolved order is recorded in the recipe comments so that the plan
remains self-describing.

``` sig
.stage_order(data_type)
```

Arguments: `data_type`

`.resolve_normalization`internalvalidation.R:1666

Resolve the normalization stage against the chosen transformation

Rank-based transformations already impose a common distribution on every
feature, so following them with quantile normalization would discard the
transformation selected by the benchmark. Compositional and proportion
data are closed by construction and need no normalization.

``` sig
.resolve_normalization(data_type, features, transformation, modality = "unknown")
```

Arguments: `data_type, features, transformation, modality`

`.resolve_scaling`internalvalidation.R:1761

Resolve the scaling stage against the chosen transformation

Quantile and robust transformations already return centred, unit-spread
features, so an additional scaling step would be redundant.

``` sig
.resolve_scaling(data_type, transformation, outlier_rate)
```

Arguments: `data_type, transformation, outlier_rate`

`.build_recipe`internalvalidation.R:1784

Build an automatic PreprocessingRecipe for one block

``` sig
.build_recipe(block_name, diagnostic, transformation, metadata = NULL, modality = "unknown")
```

Arguments: `block_name, diagnostic, transformation, metadata, modality`

`.compute_qc_checks`internalvalidation.R:1991

Compute passed, failed, and skipped QC checks

``` sig
.compute_qc_checks(validation, dataset_score, has_outcome)
```

Arguments: `validation, dataset_score, has_outcome`

`.safe_record`internalvalidation.R:2104

Record a plot safely off-screen

``` sig
.safe_record(plot_fn)
```

Arguments: `plot_fn`

`.map_blocks`internalvalidation.R:2130

Apply a function over all blocks

``` sig
.map_blocks(assays, fn)
```

Arguments: `assays, fn`

`.plot_bar`internalvalidation.R:2143

Draw a labeled bar plot

``` sig
.plot_bar(values, labels, main, ylab = "")
```

Arguments: `values, labels, main, ylab`

`.plot_missing_heatmap_block`internalvalidation.R:2167

Plot missingness heatmap for a block

``` sig
.plot_missing_heatmap_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_missing_by_sample_block`internalvalidation.R:2191

Plot missing percent by sample

``` sig
.plot_missing_by_sample_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_missing_by_feature_block`internalvalidation.R:2211

Plot missing percent by feature

``` sig
.plot_missing_by_feature_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_missing_pattern_block`internalvalidation.R:2231

Plot top missingness patterns by sample

``` sig
.plot_missing_pattern_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_overlap_heatmap`internalvalidation.R:2252

Plot sample overlap heatmap between blocks

``` sig
.plot_overlap_heatmap(overlap)
```

Arguments: `overlap`

`.plot_overlap_network`internalvalidation.R:2278

Plot block overlap as a network

``` sig
.plot_overlap_network(overlap)
```

Arguments: `overlap`

`.plot_correlation_heatmap_block`internalvalidation.R:2329

Plot correlation heatmap for a block

``` sig
.plot_correlation_heatmap_block(block_name, correlation)
```

Arguments: `block_name, correlation`

`.plot_correlation_distribution_block`internalvalidation.R:2353

Plot distribution of pairwise correlations

``` sig
.plot_correlation_distribution_block(block_name, correlation)
```

Arguments: `block_name, correlation`

`.plot_histogram_block`internalvalidation.R:2375

Plot histogram of pooled values

``` sig
.plot_histogram_block(block_name, pooled)
```

Arguments: `block_name, pooled`

`.plot_density_block`internalvalidation.R:2394

Plot density curve of pooled values

``` sig
.plot_density_block(block_name, pooled)
```

Arguments: `block_name, pooled`

`.plot_qqplot_block`internalvalidation.R:2413

Plot Q-Q plot against normal

``` sig
.plot_qqplot_block(block_name, pooled)
```

Arguments: `block_name, pooled`

`.plot_boxplot_block`internalvalidation.R:2430

Plot boxplots of block features

``` sig
.plot_boxplot_block(block_name, block)
```

Arguments: `block_name, block`

`.violin_polygon`internalvalidation.R:2456

Compute violin plot polygon coordinates

``` sig
.violin_polygon(x, at, width = 0.4)
```

Arguments: `x, at, width`

`.plot_violin_block`internalvalidation.R:2476

Plot violin plots of block features

``` sig
.plot_violin_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_pca_block`internalvalidation.R:2513

Plot PCA scores, PC1 versus PC2

``` sig
.plot_pca_block(block_name, pca)
```

Arguments: `block_name, pca`

`.plot_scree_block`internalvalidation.R:2532

Plot PCA scree of variance explained

``` sig
.plot_scree_block(block_name, explained_variance)
```

Arguments: `block_name, explained_variance`

`.plot_dendrogram`internalvalidation.R:2548

Plot hierarchical clustering dendrogram

``` sig
.plot_dendrogram(m, main)
```

Arguments: `m, main`

`.plot_sample_clustering_block`internalvalidation.R:2567

Plot dendrogram clustering samples

``` sig
.plot_sample_clustering_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_feature_clustering_block`internalvalidation.R:2580

Plot dendrogram clustering features

``` sig
.plot_feature_clustering_block(block_name, block)
```

Arguments: `block_name, block`

`.plot_outliers_block`internalvalidation.R:2593

Plot pooled values highlighting outliers

``` sig
.plot_outliers_block(block_name, pooled, outlier_idx)
```

Arguments: `block_name, pooled, outlier_idx`

`.plot_transformation_scores_block`internalvalidation.R:2617

Plot composite scores per transformation

``` sig
.plot_transformation_scores_block(block_name, ranking)
```

Arguments: `block_name, ranking`

`.plot_transformation_comparison_block`internalvalidation.R:2631

Plot histograms before and after transform

``` sig
.plot_transformation_comparison_block(block_name, before, after)
```

Arguments: `block_name, before, after`

`.build_plots`internalvalidation.R:2659

Assemble all diagnostic plots for a dataset

``` sig
.build_plots(assays, diagnostics, transformations, overlap)
```

Arguments: `assays, diagnostics, transformations, overlap`

`.build_diagnostics_table`internalvalidation.R:2779

Build summary table of block diagnostics

``` sig
.build_diagnostics_table(diagnostics)
```

Arguments: `diagnostics`

`.build_transformations_table`internalvalidation.R:2810

Build summary table of transformations

``` sig
.build_transformations_table(transformations)
```

Arguments: `transformations`

`.build_recipes_table`internalvalidation.R:2834

Build summary table of preprocessing recipes

``` sig
.build_recipes_table(recipes)
```

Arguments: `recipes`

`.build_qc_table`internalvalidation.R:2861

Build table of QC check results

``` sig
.build_qc_table(qc)
```

Arguments: `qc`

`.generate_recommendations`internalvalidation.R:2883

Generate text recommendations per block

``` sig
.generate_recommendations(diagnostics, recipes, dataset_score)
```

Arguments: `diagnostics, recipes, dataset_score`

`.validate_block_structure`internalvalidation.R:2994

Validate multi-block data structure

``` sig
.validate_block_structure(object)
```

Arguments: `object`

`.validate_metadata`internalvalidation.R:3190

Validate metadata against block samples

``` sig
.validate_metadata(object)
```

Arguments: `object`

`.cumulative_overlap`internalvalidation.R:3271

How the shared-sample count collapses as blocks are added

The pairwise matrix is the wrong number to read and the easiest one to
reach for. Every pair of twenty blocks can share all 1800 samples while
the twenty of them together share none, because each block can be
missing a different ninety. An analysis needs the intersection of all of
them, so that is the figure that decides whether it can run. Blocks are
added largest first, which puts the collapse next to the block that
caused it rather than wherever it happened to fall in the list.

``` sig
.cumulative_overlap(assays)
```

- assays: A named list of blocks.

**Returns:** A data.frame with one row per block: the running
intersection after adding it and how many samples that cost.

`.explain_no_overlap`internalvalidation.R:3337

Say which blocks are responsible for an unusable intersection

A count of zero tells the user their analysis cannot run and nothing
about what to do next. What they need is the name of the block to drop,
so the cheapest useful answer is to work out what the intersection would
be without each one.

``` sig
.explain_no_overlap(id_sets, min_samples, original = NULL)
```

- id_sets: Sample identifiers per block.
- min_samples: How many are needed.
- original: The same blocks before preprocessing, when available.

**Returns:** A character vector of lines, ready to paste into an error.

`.compute_overlap_matrix`internalvalidation.R:3473

Compute sample overlap matrix between blocks

``` sig
.compute_overlap_matrix(object)
```

Arguments: `object`

`check_data`publicvalidation.R:3536

Validate a MultiOmicsData object

Performs a complete structural, quality and preprocessing-readiness
audit of a `MultiOmicsData` object. For every block this computes a full
`BlockDiagnostics` profile, benchmarks a battery of candidate
transformations via `TransformationRecommendation`, and derives an
automatic `PreprocessingRecipe`. The dataset is additionally scored for
overall quality, screened through a set of quality-control checks, and
summarized with diagnostic plots, tables and human-readable
recommendations. The function never modifies the input object.
Everything it produces is returned inside a single `CMOValidation`
object.

``` sig
check_data(object)
```

- object:

  A `MultiOmicsData` object.

**Returns:** A `CMOValidation` object.

### preprocessing.R 39 functions

The cleaning engine. First half is the catalogue of methods, each one a
fit/apply pair so the decision can be stored and reused; second half
executes a plan and records every step.

`.method_registry`internalpreprocessing.R:49

Build the preprocessing method registry

Returns a nested list `registry[[stage]][[method]]`, each entry a list
with `fit`, `apply`, an optional `requires` vector of package names, and
a human-readable `label`. Adding a method to the engine means adding an
entry here; nothing else in the pipeline needs to change.

``` sig
.method_registry()
```

**Returns:** A nested list of method definitions.

`.get_method`internalpreprocessing.R:67

Look a method up, with a useful error when it is missing

``` sig
.get_method(stage, method)
```

Arguments: `stage, method`

`.available_methods`internalpreprocessing.R:118

List everything the engine can execute

``` sig
.available_methods()
```

**Returns:** A data.frame with one row per registered method.

`.unsupported_method`internalpreprocessing.R:151

Declare a method that is recognised but cannot be replayed

Some well-known methods do not fit the fit/apply contract: they produce
a completed dataset rather than a transferable model. Registering them
with an explanatory error is better than leaving them out, because the
user gets told why instead of "unknown method".

``` sig
.unsupported_method(label, reason, alternatives)
```

Arguments: `label, reason, alternatives`

`.stage_matrix`internalpreprocessing.R:174

Numeric columns of a block, as a matrix

``` sig
.stage_matrix(x, cols)
```

Arguments: `x, cols`

`.stage_restore`internalpreprocessing.R:182

Write a numeric matrix back into the columns it came from

``` sig
.stage_restore(x, m, cols)
```

Arguments: `x, m, cols`

`.col_stat`internalpreprocessing.R:192

Column-wise summary that tolerates all-missing columns

``` sig
.col_stat(m, fun, fallback = 0)
```

Arguments: `m, fun, fallback`

`.row_totals`internalpreprocessing.R:212

Positive row totals, guarding against empty or zero-sum samples

``` sig
.row_totals(m)
```

Arguments: `m`

`.rank_reference`internalpreprocessing.R:224

Reference distribution used to map new values onto training ranks

``` sig
.rank_reference(x)
```

Arguments: `x`

`.rank_position`internalpreprocessing.R:240

Position of each value within a stored reference distribution

Returns a probability in (0, 1). This is what lets rank- and
quantile-based transformations be applied to data the model never saw: a
new value is placed against the training distribution rather than
re-ranked among its own peers.

``` sig
.rank_position(x, reference)
```

Arguments: `x, reference`

`.imputation_methods`internalpreprocessing.R:265

Build the imputation method registry

``` sig
.imputation_methods()
```

`.transformation_methods`internalpreprocessing.R:522

Build the transformation method registry

``` sig
.transformation_methods()
```

`.normalization_methods`internalpreprocessing.R:931

Build the normalization method registry

``` sig
.normalization_methods()
```

`.scaling_methods`internalpreprocessing.R:1187

Build the scaling method registry

``` sig
.scaling_methods()
```

`.batch_methods`internalpreprocessing.R:1260

Build the batch correction method registry

``` sig
.batch_methods()
```

`.batch_labels`internalpreprocessing.R:1356

Batch label per sample, aligned to the rows of a block

``` sig
.batch_labels(x, context)
```

Arguments: `x, context`

`.feature_selection_methods`internalpreprocessing.R:1387

Build the feature selection method registry

``` sig
.feature_selection_methods()
```

`.non_numeric_columns`internalpreprocessing.R:1496

List the non-numeric columns of a block

``` sig
.non_numeric_columns(x)
```

Arguments: `x`

`.new_step`internalpreprocessing.R:1509

Record one executed pipeline step

``` sig
.new_step(index, block, stage, method, parameters = list(), model = NULL, before = c(NA, NA), after = c(NA, NA), removed_samples = character(), removed_features = character(), messages = character(), runtime = NA_real_, replay = TRUE)
```

Arguments:
`index, block, stage, method, parameters, model, before, after, removed_samples, removed_features, messages, runtime, replay`

`.block_dim`internalpreprocessing.R:1541

Dimensions of a block as the step record stores them

``` sig
.block_dim(x)
```

Arguments: `x`

`.apply_keep`internalpreprocessing.R:1554

Subset a block to the features a filter kept

``` sig
.apply_keep(x, model)
```

Arguments: `x, model`

`.structural_feature_filter`internalpreprocessing.R:1568

Decide which features the structural filters remove

Constant, near-constant, duplicated, empty and mostly-missing features.
These are invariant to everything downstream, which is why they run
first.

``` sig
.structural_feature_filter(x, recipe, diagnostic = NULL)
```

Arguments: `x, recipe, diagnostic`

`.structural_sample_filter`internalpreprocessing.R:1615

Decide which samples the structural filters remove

``` sig
.structural_sample_filter(x, recipe)
```

Arguments: `x, recipe`

`.statistical_feature_filter`internalpreprocessing.R:1661

Decide which features the statistical filters remove

Variance, abundance and prevalence only mean something once the block
has been normalized and transformed onto a comparable scale, which is
why this runs late rather than alongside the structural filters.

``` sig
.statistical_feature_filter(x, recipe)
```

Arguments: `x, recipe`

`.outlier_filter`internalpreprocessing.R:1722

Flag or drop outlying samples

Never enabled automatically: discarding observations is a scientific
decision, not a data-cleaning one, so the recipe has to ask for it.

``` sig
.outlier_filter(x, recipe)
```

Arguments: `x, recipe`

`.stage_config`internalpreprocessing.R:1784

Method configured for a stage in a recipe

``` sig
.stage_config(recipe, stage)
```

Arguments: `recipe, stage`

`.with_batch_attr`internalpreprocessing.R:1814

Attach batch labels so the batch method can find them

``` sig
.with_batch_attr(x, context)
```

Arguments: `x, context`

`.preprocess_block`internalpreprocessing.R:1829

Run the full recipe for a single block

``` sig
.preprocess_block(block_name, x, recipe, context, index_offset = 0L)
```

Arguments: `block_name, x, recipe, context, index_offset`

`.resolve_plan`internalpreprocessing.R:1981

Turn whatever the user passed as a plan into a named list of recipes

``` sig
.resolve_plan(plan, object)
```

Arguments: `plan, object`

`.check_plan_matches`internalpreprocessing.R:2027

Refuse to execute a plan that does not describe this object

``` sig
.check_plan_matches(object, plan, recipes, force)
```

Arguments: `object, plan, recipes, force`

`preprocess`publicpreprocessing.R:2154

Execute a preprocessing plan

Runs the `PreprocessingRecipe` objects produced by `check_data()`
against the data they describe. The function decides nothing on its own:
every method, parameter and threshold it applies comes from the recipe,
and the stage order comes from the recipe's `stage_order`. Each stage is
fitted and then applied, and the fitted model is kept. That is what
makes the result reproducible: `apply_preprocessing()` can put an
external dataset through the identical pipeline without deriving a
single decision from it. Sample-level decisions (empty, duplicated or
outlying samples) are marked as non-replayable, because an external
cohort has its own samples. Feature-level decisions are replayable and
pin the feature set.

``` sig
preprocess(object, plan, blocks = NULL, plots = TRUE, force = FALSE, quiet = FALSE)
```

- object:

  A `MultiOmicsData` object.

- plan:

  A `CMOValidation` (the usual case, as returned by `check_data()`), a
  single `PreprocessingRecipe`, or a named list of recipes.

- blocks: Optional character vector restricting execution to some
  blocks. Blocks not covered are carried through untouched.

- plots: Whether to render before/after diagnostic plots.

- force: Execute even when the plan does not match the object, or when
  the validation reported errors. The mismatch is recorded in the
  result.

- quiet: Whether to suppress progress messages.

**Returns:** A `PreprocessingResult` object.

`apply_preprocessing`publicpreprocessing.R:2402

Replay a fitted pipeline on new data

Puts a new dataset through exactly the pipeline recorded in a
`PreprocessingResult`, reusing every fitted model rather than deriving
anything from the new data. This is what makes an external validation
cohort comparable with the cohort the pipeline was built on. Two rules
follow from that and are applied deliberately: \item Feature-level
decisions are replayed, so the output carries the same features in the
same order as the training data. \item Sample-level decisions are not
replayed. Removing a sample from a new cohort because a training sample
was removed would be meaningless, and silently dropping rows from a
validation set is a common way to produce optimistic results.

``` sig
apply_preprocessing(object, result, blocks = NULL, quiet = FALSE)
```

- object:

  A `MultiOmicsData` object holding the new data.

- result:

  A `PreprocessingResult` from `preprocess()`.

- blocks: Optional character vector restricting the replay.

- quiet: Whether to suppress progress messages.

**Returns:** A `MultiOmicsData` object carrying the processed blocks.

`.steps_removed`internalpreprocessing.R:2494

Collect removed samples or features by block

``` sig
.steps_removed(steps, field)
```

Arguments: `steps, field`

`.steps_by_stage`internalpreprocessing.R:2516

Extract recorded steps for one pipeline stage

``` sig
.steps_by_stage(steps, stage)
```

Arguments: `steps, stage`

`.preprocessing_statistics`internalpreprocessing.R:2535

Compute before and after preprocessing statistics per block

``` sig
.preprocessing_statistics(before, after, steps)
```

Arguments: `before, after, steps`

`.quality_comparison`internalpreprocessing.R:2562

Compare quality scores before and after preprocessing

``` sig
.quality_comparison(object, processed, plan, diagnostics)
```

Arguments: `object, processed, plan, diagnostics`

`.build_steps_table`internalpreprocessing.R:2611

Build a summary table of preprocessing steps

``` sig
.build_steps_table(steps)
```

Arguments: `steps`

`.build_removed_table`internalpreprocessing.R:2639

Build a table of removed samples and features

``` sig
.build_removed_table(steps, field, label)
```

Arguments: `steps, field, label`

`.build_preprocessing_plots`internalpreprocessing.R:2678

Build diagnostic plots comparing data before and after

``` sig
.build_preprocessing_plots(before, after, diagnostics)
```

Arguments: `before, after, diagnostics`

### analysis.R 105 functions

The evidence engine. Nine different statistical methods each report the
relationships they find, and an integrator merges them into one scored
map, keeping track of how much each relationship is allowed to claim.

`.detect_design`internalanalysis.R:37

Work out what kind of study this is

The available evidence generators depend on it: repeated measures unlock
longitudinal models and give temporal precedence real meaning, a
time/status pair unlocks survival models, and a single cross-section
supports neither.

``` sig
.detect_design(metadata, outcome, time = NULL, subject = NULL)
```

Arguments: `metadata, outcome, time, subject`

`.resolve_outcome`internalanalysis.R:122

Resolve the outcome from metadata

``` sig
.resolve_outcome(metadata, outcome, sample_ids)
```

Arguments: `metadata, outcome, sample_ids`

`.encode_categorical`internalanalysis.R:223

Turn categorical columns into something every method can read

A factor cannot go into a correlation, a penalised fit or a structure
search as it stands. Each level except one becomes its own yes/no
column, contrasted against the level left out. That reference level is
not arbitrary decoration: every estimate for this variable means
"compared to the reference", and the reference has to travel with the
column or the number cannot be read. The commonest level is used as the
reference, because contrasts against a rare category are estimated from
very few people and come out wide.

``` sig
.encode_categorical(block, max_levels = 10, min_per_level = 5)
```

- block: A data.frame.
- max_levels: Beyond this a column is treated as an identifier rather
  than a variable and left out.
- min_per_level: Levels rarer than this are folded into nothing: they
  produce an estimate no one should read.

**Returns:** A list of encoded columns and a record of what each one
means.

`.align_blocks`internalanalysis.R:315

Join the analysed blocks onto one sample x feature matrix

Blocks need not share samples, and preprocessing may have removed
different ones from each. Analysis needs a rectangle, so the
intersection is taken and reported: silently analysing whatever survived
would hide how much of the cohort the result actually rests on.

``` sig
.align_blocks(assays, blocks, min_samples = 10, original = NULL)
```

Arguments: `assays, blocks, min_samples, original`

`.build_covariates`internalanalysis.R:421

Covariate matrix aligned to the analysed samples

``` sig
.build_covariates(metadata, covariates, sample_ids)
```

Arguments: `metadata, covariates, sample_ids`

`.allocate_screening`internalanalysis.R:466

Share the screening budget out between blocks

Ranking every feature together and taking the best hands the whole
budget to whichever block has the most columns. Twenty thousand
transcripts against five clinical variables is not a fair contest: the
clinical block disappears from the analysis entirely, and with it every
cross-block mediation that would have run through it. Each block gets a
floor first, so a small block survives intact, and what is left over is
shared in proportion to size.

``` sig
.allocate_screening(feature_block, max_features, min_per_block = 10)
```

- feature_block: Named vector mapping feature to block.
- max_features: Total budget.
- min_per_block: Floor per block, or the whole block when smaller.

**Returns:** A named integer vector of quotas.

`.screen_features`internalanalysis.R:533

Reduce the feature space before the pairwise stage

Pairwise evidence over every feature of every block is quadratic: twenty
thousand features is four hundred million tests, which is neither
computable nor interpretable. Features are screened against the outcome
first and the graph is built among the survivors. The screen is recorded
so the reader knows what was never examined. The budget is shared
between blocks rather than handed to whichever has the most columns. See
`.allocate_screening()`.

``` sig
.screen_features(x, outcome, max_features, feature_block = NULL, min_per_block = 10, quiet = FALSE)
```

Arguments:
`x, outcome, max_features, feature_block, min_per_block, quiet`

`.quantity_registry`internalanalysis.R:637

The kinds of quantity a method can report

Each entry declares the scale the number lives on, which other
quantities it can be pooled with, whether its sign carries direction,
and what value means "no relationship".

``` sig
.quantity_registry()
```

**Returns:** A named list of quantity definitions.

`.quantity`internalanalysis.R:717

Look up one quantity, tolerating an unregistered name

``` sig
.quantity(name)
```

Arguments: `name`

`.available_quantities`internalanalysis.R:737

Everything the engine can currently integrate

``` sig
.available_quantities()
```

**Returns:** A data.frame, one row per registered quantity.

`observation`internalanalysis.R:774

One quantity, reported by one method, about one relationship

The unit the integration engine actually works with. A model does not
emit evidence: it emits an observation, and evidence is what several
observations become once they have been reconciled. Emitting one of
these is how a new method joins the engine. Declare the quantity and the
integrator handles the rest; it has no knowledge of which models exist.

``` sig
observation(source, target, quantity, estimate, generator, method, se = NA_real_, p_value = NA_real_, n = NA_integer_, ci = c(NA_real_, NA_real_), identification = "none", ...)
```

- source,target: The two variables.

- quantity:

  One of `.available_quantities()$quantity`.

- estimate: The number the method reported.

- generator,method: Who reported it.

- se,p_value,n,ci: The usual accompaniments, where the method provides
  them.

- identification: What would license a causal reading.

- ...: Further fields stored verbatim.

**Returns:** An Observation.

`.new_edge`internalanalysis.R:793

Build one EvidenceEdge

``` sig
.new_edge(source, target, estimate, generator, method, quantity = NA_character_, se = NA_real_, p_value = NA_real_, n = NA_integer_, ci = c(NA_real_, NA_real_), identification = "none", adjustment_set = character(), assumptions = character(), temporal = NA, source_block = NA_character_, target_block = NA_character_, effect_size = NA_real_, notes = character())
```

Arguments:
`source, target, estimate, generator, method, quantity, se, p_value, n, ci, identification, adjustment_set, assumptions, temporal, source_block, target_block, effect_size, notes`

`.identification_assumptions`internalanalysis.R:843

Assumptions implied by an identification strategy

``` sig
.identification_assumptions(identification, adjustment_set = character())
```

Arguments: `identification, adjustment_set`

`.evidence_registry`internalanalysis.R:892

The registry of evidence generators

``` sig
.evidence_registry()
```

`.evidence_association`internalanalysis.R:971

Generate association evidence edges per feature

``` sig
.evidence_association(context)
```

Arguments: `context`

`.evidence_conditional`internalanalysis.R:1043

Compute partial correlation evidence between features

``` sig
.evidence_conditional(context)
```

Arguments: `context`

`.evidence_survival`internalanalysis.R:1135

Generate survival evidence with temporal identification

``` sig
.evidence_survival(context)
```

Arguments: `context`

`.evidence_longitudinal`internalanalysis.R:1224

Generate longitudinal evidence from mixed models

``` sig
.evidence_longitudinal(context)
```

Arguments: `context`

`.evidence_mediation`internalanalysis.R:1309

Test mediation evidence via bootstrapped triples

``` sig
.evidence_mediation(context)
```

Arguments: `context`

`.evidence_elasticnet`internalanalysis.R:1458

Generate evidence via elastic net regression

``` sig
.evidence_elasticnet(context)
```

Arguments: `context`

`.evidence_randomforest`internalanalysis.R:1526

Generate evidence via random forest importance

``` sig
.evidence_randomforest(context)
```

Arguments: `context`

`.evidence_bayesnet`internalanalysis.R:1604

Generate evidence via Bayesian network structure

``` sig
.evidence_bayesnet(context)
```

Arguments: `context`

`.evidence_sem`internalanalysis.R:1734

Generate evidence via structural equation modeling

``` sig
.evidence_sem(context)
```

Arguments: `context`

`.edge_key`internalanalysis.R:1816

Key identifying a relationship regardless of which method reported it

``` sig
.edge_key(edge)
```

Arguments: `edge`

`.integrate_evidence`internalanalysis.R:1846

Merge the edges describing the same relationship and score them

Three scores come out, and they measure different things on purpose:
\item{strength{How large the effect is, standardised across methods,
combined with how stable it is.} confidence{How precisely it was
estimated: sample size, interval width, multiplicity-adjusted
significance.} consistency{How many of the methods that could see the
relationship agreed on its sign.} } Consistency deliberately does NOT
feed a causal claim. Methods that share an unmeasured confounder agree
with each other while all being biased in the same direction, so
agreement is evidence of stability and nothing more. The identification
strategy is tracked separately and reported beside the score.

``` sig
.integrate_evidence(edges, params, contributions = TRUE)
```

- edges: Edges emitted by the generators.
- params: Tuning parameters.
- contributions: Build the per-method table on each edge. Resampling
  integrates a whole graph per replicate and then reads four fields of
  each edge, so building that table five thousand times and discarding
  it was most of what resampling cost.

`.build_evidence_graph`internalanalysis.R:2258

Turn integrated evidence into a scored directed graph

``` sig
.build_evidence_graph(evidence, feature_block, outcome_name, params)
```

Arguments: `evidence, feature_block, outcome_name, params`

`.extract_paths`internalanalysis.R:2421

Rank the evidence paths that reach the outcome

``` sig
.extract_paths(graph, outcome_name, params)
```

Arguments: `graph, outcome_name, params`

`.consensus_importance`internalanalysis.R:2548

Consensus importance across every generator that produced one

``` sig
.consensus_importance(edges, outcome_name, feature_block)
```

Arguments: `edges, outcome_name, feature_block`

`.interpret_graph`internalanalysis.R:2605

Extract readable findings from the graph

``` sig
.interpret_graph(graph, importance, outcome_name, params)
```

Arguments: `graph, importance, outcome_name, params`

`analyze`publicanalysis.R:2816

Build quantified causal evidence from preprocessed multi-block data

`analyze()` does not return a model. It runs every applicable analysis
method, collects the relationships each one reports as `EvidenceEdge`
objects, integrates them into a single scored directed graph, and
extracts the findings that graph supports.

``` sig
analyze(object, outcome, time = NULL, subject = NULL, covariates = NULL, blocks = "all", effort = c("standard", "fast", "thorough", "exhaustive"), assume = NULL, control = NULL, plots = TRUE, quiet = FALSE)
```

- object:

  A `PreprocessingResult`, as returned by `preprocess()`. Preprocessing
  decisions are already fixed, which is why analysis starts here rather
  than from a `MultiOmicsData`.

- outcome: Name of the outcome column in the metadata. Several may be
  named, in which case each is analysed and multiplicity is corrected
  across all of them together rather than within each.

- time: Optional name of a time or follow-up column. Combined with a
  binary outcome this makes the design a survival one.

- subject: Optional name of a subject identifier column. Repeated values
  make the design longitudinal.

- covariates: Optional character vector of metadata columns to adjust
  for. These form the declared adjustment set.

- blocks:

  Blocks to analyse, or `"all"`.

- effort:

  How much computation to spend. `"fast"` runs the generators once and
  nothing else; `"standard"` adds model diagnostics and 50 resamples;
  `"thorough"` adds null calibration; `"exhaustive"` resamples every
  generator 500 times. Anything set explicitly in `control` wins over
  the preset.

- assume:

  What you are claiming, built with `analysis_assumptions`: the causal
  diagram, which variables are modifiable, measurement reliability,
  competing events. Every one is a statement the data cannot check, and
  every one changes what may be concluded.

- control:

  How much work to do, built with `analysis_control`: which generators,
  how many resamples, how many features. Nothing here changes what may
  be concluded.

- plots: Whether to render diagnostic plots.

- quiet: Whether to suppress progress messages.

**Returns:** A `CMOResult` object, or a `CMOMultiResult` when several
outcomes were named.

`annotate_evidence`publicanalysis.R:3780

Attach external biological support to an evidence graph

Kept out of `analyze()` on purpose. Pathway and interaction databases
are queried over the network, so folding them into the analysis would
make the same data produce different scores on different days, fail
without a connection, and depend on a database version that is not
recorded anywhere in the result. Called separately, the annotation is an
explicit, dated act: the source and the retrieval time are stored on
every edge it touches.

``` sig
annotate_evidence(result, annotations, weight = 0.1)
```

- result:

  A `CMOResult`.

- annotations:

  A data.frame with columns `source`, `target` and optionally `support`
  (0-1) and `database`. Supply this to annotate from a local export,
  which is the reproducible option.

- weight: How much the biological support contributes to the recomputed
  evidence score, between 0 and 1.

**Returns:** The `CMOResult` with biological support attached.

`.build_evidence_report`internalanalysis.R:3857

Compile limitations for the evidence report

``` sig
.build_evidence_report(result)
```

Arguments: `result`

`.build_analysis_plots`internalanalysis.R:3940

Build plots for evidence scores and importance

``` sig
.build_analysis_plots(graph, importance, block_evidence = NULL, outcome_name = NULL, consensus = NULL, modules = NULL, evidence = NULL)
```

Arguments:
`graph, importance, block_evidence, outcome_name, consensus, modules, evidence`

`.evidence_level`internalanalysis.R:4069

Epistemic level of an evidence generator

``` sig
.evidence_level(generator)
```

- generator: Generator name.

**Returns:** An integer from 1 to 5.

`.level_label`internalanalysis.R:4115

Name of an evidence level

``` sig
.level_label(level)
```

Arguments: `level`

`.level_weight`internalanalysis.R:4129

Weight a level contributes to the agreement score

Deliberately not linear. The step from "predicts" to "estimates an
effect" matters less than the step from "estimates an effect" to "the
exposure came first", because only the latter removes a rival
explanation.

``` sig
.level_weight(level)
```

Arguments: `level`

`.level_identification`internalanalysis.R:4147

Identification strategy, from the level and what the design supports

The level is not enough on its own, and treating it as a ladder to
identification is wrong. Mediation sits at level 4 because it commits to
a path structure, but on cross-sectional data it only assumes an
ordering, it does not observe one. Reading "level 4" as "temporal" would
let a mediation scan on a single time point claim measurement order it
never had. Temporality is a property of the design and is carried
separately by the generators that genuinely have it.

``` sig
.level_identification(level, has_adjustment, has_temporal = FALSE)
```

Arguments: `level, has_adjustment, has_temporal`

`.method_explanation`internalanalysis.R:4168

What each method actually does, for a reader who has not met it

A report that says a finding was "found by structural equation model"
has told a specialist something and a general reader nothing. Worse, an
unfamiliar name reads as authority. These descriptions say what the
method looks at and, more usefully, what it cannot see.

``` sig
.method_explanation(generator)
```

- generator: Generator name.

**Returns:** A list with a short label, what it does, and its main
limitation.

`.quantity_explanations`internalanalysis.R:4228

What each reported quantity means and where it comes from

``` sig
.quantity_explanations()
```

`.imputation_fidelity`internalanalysis.R:4337

How much of an imputed value survives as information

Imputation methods are not interchangeable. Filling with the column mean
collapses every missing cell onto a single number: the variance of that
part of the column becomes zero and any association computed through it
is pulled toward the null. Nearest-neighbour imputation borrows from
correlated features and keeps most of the structure, so more of the
filled portion is still carrying information. These are deliberately
coarse. The point is that the ranking between methods is right and the
direction of the penalty is right, not that 0.35 is measurable to two
digits.

``` sig
.imputation_fidelity(method)
```

- method: The imputation method recorded in the preprocessing step.

**Returns:** A factor between 0 and 1 applied to the imputed share of a
variable.

`.feature_quality`internalanalysis.R:4361

What preprocessing did to every feature that reached the analysis

Reads the original blocks and the executed steps, and returns one row
per surviving feature saying how much of it was measured, how much was
filled in, by what, and what else happened to its block.

``` sig
.feature_quality(prep)
```

- prep:

  A `PreprocessingResult`.

**Returns:** A data.frame keyed by feature name, or `NULL` when the
provenance is not recoverable.

`.imputation_label`internalanalysis.R:4465

Name an imputation method the way a reader would say it

``` sig
.imputation_label(method)
```

- method: The registry name.

**Returns:** A phrase that completes "was filled in by ...".

`.quality_edges`internalanalysis.R:4493

Charge every relationship for the part of it that was invented

An edge is worth no more than its worst-measured ingredient, so the
score takes the weakest link across both endpoints and everything
adjusted for. Taking an average instead would let a clean outcome and
four clean covariates hide an exposure that was two-thirds imputed,
which is the one thing a reader needs to be told.

``` sig
.quality_edges(edges, ftab, covariates = character())
```

- edges: Integrated edges.

- ftab:

  The table from `.feature_quality()`.

- covariates: Names of the variables adjusted for.

**Returns:** The edges, with quality fields set and `evidence_score`
scaled.

`.observed_mask`internalanalysis.R:4551

Which cells of the analysis matrix were measured rather than filled in

``` sig
.observed_mask(prep, samples, features)
```

- prep:

  A `PreprocessingResult`.

- samples: Sample identifiers in the order the analysis uses.

- features: Feature names in the order the analysis uses.

**Returns:** A logical matrix, `TRUE` where the original value was
observed, or `NULL` when the original data is not recoverable.

`.complete_case_sensitivity`internalanalysis.R:4608

Refit the strongest relationships using only rows that were measured

The cheapest honest answer to "did this come out of the imputation?".
Multiple imputation with Rubin's rules would be the thorough version and
costs a full analysis per replicate; this costs one model fit per
relationship and catches the case that matters, which is a finding that
exists only because the gaps were filled. Only relationships with the
outcome are checked, because only those have a single model that can be
refitted without re-deriving the whole graph.

``` sig
.complete_case_sensitivity(edges, x, outcome, covariates, mask, top_n = 20L)
```

- edges: Integrated edges.

- x: The analysis matrix.

- outcome: The resolved outcome.

- covariates:

  The covariate frame, or `NULL`.

- mask:

  The matrix from `.observed_mask()`.

- top_n: How many of the highest-scoring relationships to check.

**Returns:** The edges, with the complete-case fields set where
applicable.

`.hypothesis_settles`internalanalysis.R:4717

What would have to be observed for this claim to be wrong

Derived from the specific weaknesses of the specific relationship, so a
clean edge gets a short list and a fragile one gets a long, pointed one.

``` sig
.hypothesis_settles(edge, object)
```

- edge:

  An `EvidenceEdge`.

- object:

  The `CMOResult` it came from.

**Returns:** A character vector of concrete next measurements.

`.hypothesis_grade`internalanalysis.R:4885

What kind of statement the evidence licenses

The grade is not a quality score. It is the strongest sentence that can
be written without overstating, which is a different thing: a
well-measured, highly stable association is still an association.

``` sig
.hypothesis_grade(edge)
```

- edge:

  An `EvidenceEdge`.

**Returns:** A list with the grade and the reasons for it.

`.hypothesis_case`internalanalysis.R:4963

The case for and against, kept apart

``` sig
.hypothesis_case(edge, object)
```

- edge:

  An `EvidenceEdge`.

- object:

  The `CMOResult`.

**Returns:** A list of two character vectors.

`hypothesis`publicanalysis.R:5092

State one relationship as a claim that could be wrong

A `CMOResult` is a body of evidence. This takes one relationship out of
it and states it as a hypothesis: the claim at the strength the evidence
supports, the case for it, the case against it, and the specific
measurements that would settle the argument.

``` sig
hypothesis(object, feature = NULL)
```

- object:

  A `CMOResult`.

- feature: The variable to state a claim about. Defaults to the
  highest-scoring relationship with the outcome.

**Returns:** A `Hypothesis` object.

`test_hypothesis`publicanalysis.R:5221

Take a stated claim to a different cohort

The claim carries its own protocol: which outcome, which covariates,
which model. This runs exactly that on data it has never seen and
reports whether the relationship is there, at what size, and in which
direction. Replication is not agreement of p-values. A claim replicates
when the new estimate points the same way and is of a comparable size,
and the most common failure is a direction that holds with an effect a
fifth as large, which a significance test would call a success.

``` sig
test_hypothesis(hypothesis, object)
```

- hypothesis:

  A `Hypothesis` from `hypothesis`.

- object:

  A `PreprocessingResult` for the new cohort, ideally produced by
  `apply_preprocessing` so the columns are on the same scale as the ones
  the claim was made on.

**Returns:** The hypothesis, with its `replication` slot filled in.

`.detect_modules`internalanalysis.R:5391

Group features that move together

Correlation, not the evidence graph. A community detected on the
evidence graph groups features that each have a link to the outcome,
which they can do while being uncorrelated with each other; the first
principal component of such a group summarises nothing. A module has to
be a set of variables that vary together before summarising it means
anything.

``` sig
.detect_modules(x, min_correlation = 0.5, min_size = 3L, max_features = 2000L)
```

- x: The analysis matrix.
- min_correlation: Features join a module at this average correlation.
- min_size: Modules smaller than this are left as individual features.
- max_features: Above this the correlation matrix is too large to be
  worth computing, and the modules would be too many to read.

**Returns:** A named integer vector of module assignments, or `NULL`.

`.module_latent`internalanalysis.R:5468

Summarise each module by the one variable that best represents it

The first principal component, on standardised features so that a module
is not dominated by whichever of its members happens to have the largest
units. Its sign is arbitrary as `prcomp` returns it, which would make
the direction of every module's relationship with the outcome a coin
flip. It is fixed here so that a higher score means higher on the
majority of the module's features, which is the only reading that lets
the direction be reported at all.

``` sig
.module_latent(x, membership)
```

- x: The analysis matrix.

- membership:

  Module assignments from `.detect_modules()`.

**Returns:** A list with the score matrix and, per module, the share of
its own variance the score accounts for.

`.module_edges`internalanalysis.R:5543

Test each module against the outcome the way a feature is tested

``` sig
.module_edges(latent, outcome, covariates = NULL)
```

- latent: The score matrix.

- outcome: The resolved outcome.

- covariates:

  Covariate frame, or `NULL`.

**Returns:** A data.frame of module-level estimates.

`.build_module_graph`internalanalysis.R:5611

Assemble the module-level reading of the same data

``` sig
.build_module_graph(x, outcome, covariates = NULL, feature_block = list(), min_correlation = 0.5, min_variance_explained = 0.5)
```

- x: The analysis matrix.

- outcome: The resolved outcome.

- covariates:

  Covariate frame, or `NULL`.

- feature_block: Which block each feature came from.

- min_correlation:

  Passed to `.detect_modules()`.

- min_variance_explained: Below this a module's first component is not a
  summary of it, and the module is reported but not treated as coherent.

**Returns:** A `ModuleGraph`.

`.effect_concentration`internalanalysis.R:5780

How few people it would take to halve a relationship

For a relationship that holds across the cohort, removing a few people
barely moves the estimate: that is what an average does. For one
produced by a small unusual group, removing that group collapses it. The
number of samples that separates those two cases is a more useful
description of robustness than any p-value, and it is not a p-value in
disguise: a finding can be overwhelmingly significant and rest entirely
on nine people. The candidate set is ordered by influence on the
coefficient, so this is the worst case rather than a typical one. That
is deliberate. A reader deciding whether to believe a result wants to
know how fragile it could be, not how fragile a random deletion would
make it.

``` sig
.effect_concentration(y, x, covariates = NULL, binary = FALSE)
```

- y: Outcome values.

- x: The exposure column.

- covariates:

  Covariate frame, or `NULL`.

- binary: Whether the outcome is binary.

**Returns:** A list with the count and the share of the cohort it
represents, or `NULL` when the model could not be fitted.

`.edge_concentration`internalanalysis.R:5875

Attach the concentration check to the relationships that carry the
report

Restricted to relationships with the outcome, because only those have
one model that can be refitted without re-deriving the whole graph.

``` sig
.edge_concentration(edges, x, outcome, covariates, top_n = 20L)
```

- edges: Integrated edges.

- x: The analysis matrix.

- outcome: The resolved outcome.

- covariates:

  Covariate frame, or `NULL`.

- top_n: How many of the highest-scoring relationships to check.

**Returns:** The edges, with the concentration fields set where
applicable.

`.constraint_frame`internalanalysis.R:5951

Check a structure-learning constraint and normalise it

``` sig
.constraint_frame(arcs, label)
```

- arcs: A data.frame of from/to pairs, or NULL.
- label: Which argument it is, for the message.

**Returns:** A two-column data.frame, or NULL.

`.validate_dag`internalanalysis.R:5982

Refuse a causal structure that parsed into nothing

``` sig
.validate_dag(parsed)
```

- parsed:

  The result of a `dagitty` parse, possibly `NULL`.

**Returns:** The structure, unchanged, or an error.

`.parse_dag`internalanalysis.R:6014

Accept a causal structure in whichever form the user has it

A dagitty object, a dagitty string, or the simplest thing a biologist
actually has to hand: a table of arrows.

``` sig
.parse_dag(dag)
```

- dag:

  A dagitty object, a character DAG specification, or a data.frame with
  `from` and `to` columns.

**Returns:** A dagitty object, or NULL when nothing usable was supplied.

`.dag_audit`internalanalysis.R:6074

Is this effect identified by this adjustment, and if not, why not

``` sig
.dag_audit(dag, exposure, outcome, adjusted = character())
```

- dag: A dagitty object.
- exposure,outcome: Node names.
- adjusted: What was actually conditioned on.

**Returns:** A list with the verdict, the reason, what would have
sufficed, and any variable that should not have been adjusted for.

`.audit_edges`internalanalysis.R:6232

Apply the audit to every relationship the DAG can speak about

``` sig
.audit_edges(edges, dag, covariates)
```

Arguments: `edges, dag, covariates`

`.e_value`internalanalysis.R:6317

How strong an unmeasured confounder would have to be

The E-value (VanderWeele and Ding, 2017) is the minimum strength of
association, on the risk-ratio scale, that an unmeasured confounder
would need with both the exposure and the outcome to explain away an
observed association entirely. This does not say whether confounding is
present. It says how much would be required, which is a different and
more useful question: an E-value of 1.1 means a trivially weak
confounder suffices, while 5 means the confounder would have to be
stronger than most measured risk factors. Continuous effects are put on
the risk-ratio scale through the approximation `RR = exp(0.91 * d)` for
a standardised difference `d`, which is the conversion the original
authors propose.

``` sig
.e_value(estimate, se = NA_real_, scale = "standardised")
```

- estimate: Effect estimate.

- se: Standard error, used for the limit of the confidence interval.

- scale:

  Either `"log"` for coefficients already on a log scale (Cox, logistic)
  or `"standardised"` for standardised differences.

**Returns:** A list with the point E-value and the E-value for the
confidence limit closest to the null.

`.e_value_reading`internalanalysis.R:6363

Plain-language reading of an E-value

``` sig
.e_value_reading(e)
```

Arguments: `e`

`.direction_confidence`internalanalysis.R:6391

How well the data supports the orientation of an edge

Observational data rarely identifies which way an arrow points. When one
method reports A to B and another reports B to A, the engine keeps the
better-supported direction, and this records how close the call was: 0.5
means the two directions were equally supported and the orientation is
arbitrary. Temporal evidence is the exception. If the exposure was
measured before the outcome, the orientation is not in question.

``` sig
.direction_confidence(forward_score, reverse_score, temporal, level)
```

Arguments: `forward_score, reverse_score, temporal, level`

`.decompose_evidence`internalanalysis.R:6438

Open up an evidence score into the parts it was built from

The single number is convenient and hides everything. This returns the
components, each on a 0 to 1 scale, so a reader can see whether a score
of 70 rests on a large effect measured imprecisely or a small one
measured well. Biological support is reported here but never folded into
the primary score. Letting prior knowledge raise a score would make
already-published relationships outrank novel ones, which is
confirmation bias inside the metric of a tool meant for discovery.

``` sig
.decompose_evidence(edge)
```

- edge:

  An `EvidenceEdge`.

**Returns:** A data.frame of components with their values and weights.

`.summarise_conflict`internalanalysis.R:6514

Explain why methods disagreed about a relationship

Recording that a conflict exists is not much use on its own. What a
reader needs is the shape of the disagreement: whether the dissenters
are all predictive methods, whether the relationship only appears once
covariates are added, or whether a single method is out on its own.

``` sig
.summarise_conflict(group, agreeing, disagreeing)
```

Arguments: `group, agreeing, disagreeing`

`.check_negative_controls`internalanalysis.R:6586

Check whether variables that should not appear, do

A negative control is something the user knows has no business being
part of the mechanism. If it shows up with a strong score, the finding
is not the control's fault: it is a signal that the pipeline is picking
up structure that is not biological.

``` sig
.check_negative_controls(evidence, controls, outcome_name)
```

- evidence: Integrated evidence.
- controls: Character vector of feature names.
- outcome_name: Name of the outcome.

**Returns:** A list with the offending edges and a verdict.

`.predictive_versus_causal`internalanalysis.R:6656

Separate how useful a variable is from how well supported it is

These come apart more often than people expect. A variable can predict
well because it is a downstream consequence of the outcome, and a
variable with a well-supported effect can predict poorly because its
effect is small. Reporting them in one column invites the reader to
conflate them.

``` sig
.predictive_versus_causal(edges, outcome_name, feature_block)
```

Arguments: `edges, outcome_name, feature_block`

`.missing_report`internalanalysis.R:6724

Describe the missingness the analysis had to work around

Deliberately descriptive. Whether data is missing at random rather than
missing not at random cannot be decided from the observed data at all:
the evidence that would settle it is the part that is absent. Little's
test addresses only the stricter question of whether missingness is
completely at random, and even that is reported as a test result rather
than a verdict.

``` sig
.missing_report(x, dropped_samples, screening)
```

Arguments: `x, dropped_samples, screening`

`.fast_generators`internalanalysis.R:6818

Generators cheap enough to resample by default

``` sig
.fast_generators()
```

`.resample_estimate`internalanalysis.R:6824

Estimate how long resampling will take before committing to it

``` sig
.resample_estimate(seconds_per_replicate, n_replicates)
```

Arguments: `seconds_per_replicate, n_replicates`

`.resample_evidence`internalanalysis.R:6850

Re-run the cheap generators on resampled data

Reports, for every relationship, how often it was recovered AND how
often it could have been: a feature that goes constant in a resample
makes the edge untestable there, and counting that as a failure would
understate stability. The denominator is returned rather than hidden.

``` sig
.resample_evidence(context, generators, replicates, scheme = "bootstrap", folds = 10, quiet = FALSE)
```

- context: The analysis context.
- generators: Generator names to re-run.
- replicates: How many resamples.
- scheme: Either "bootstrap" or "cv".
- folds: Folds, when scheme is "cv".
- quiet: Suppress progress.

**Returns:** A list keyed by edge, each with found, evaluable and sign
counts.

`.build_consensus_graph`internalanalysis.R:7038

Assemble what the resampling says about the shape of the graph

The reported graph is one draw. This is the distribution it came from:
which relationships recur, which way they point when they do, where they
rank, and how much of the reported picture would survive being collected
again. The reported graph is deliberately not treated as the truth the
resamples are scored against. It is one of them, and the interesting
cases are the two disagreements: relationships that were reported but
rarely recur, and relationships that recur constantly but did not make
the report.

``` sig
.build_consensus_graph(resampling, evidence, threshold = 0.5, top_n = 10L)
```

- resampling:

  The list returned by `.resample_evidence()`.

- evidence: The integrated edges from the full sample.

- threshold: Fraction of replicates a relationship must appear in to
  join the consensus.

- top_n: How many of the reported relationships to track by rank.

**Returns:** A `ConsensusGraph`.

`.null_calibration`internalanalysis.R:7190

How many relationships the engine finds when there is nothing to find

The single most useful number a reader can have about a graph.
Twenty-five edges sounds like a result until you learn the same engine
produces twenty-two on the same data with the outcome shuffled. The
outcome is permuted, which destroys any relationship between the
features and the outcome while leaving the correlation structure among
the features intact. Feature-to-feature edges therefore survive
permutation by construction, and only edges reaching the outcome are
counted.

``` sig
.null_calibration(context, generators, permutations, observed_edges, quiet = FALSE)
```

Arguments: `context, generators, permutations, observed_edges, quiet`

`.heterogeneity`internalanalysis.R:7299

Does a relationship hold in every subgroup

Interaction tests are badly underpowered: a study sized to detect a main
effect is usually nowhere near able to detect that the effect differs
between groups. Subgroup findings from a scan like this are hypotheses
at best, and the wording says so rather than announcing that something
works "only in women".

``` sig
.heterogeneity(context, moderators, features, quiet = FALSE)
```

Arguments: `context, moderators, features, quiet`

`.edge_heterogeneity`internalanalysis.R:7427

Put the subgroup result on the relationship it belongs to

The table is a fine thing to read on its own and a poor way to be told
that the finding in front of you is an average over groups that
disagree. Nobody cross-references a table at the bottom of a report
against the card they are reading, so the finding has to carry it. Where
a feature was tested against several moderators, the strongest
interaction wins the slot. Reporting the weakest would bury the point.

``` sig
.edge_heterogeneity(edges, heterogeneity, outcome_name)
```

- edges: Integrated edges.

- heterogeneity:

  The list returned by `.heterogeneity()`.

- outcome_name: Name of the outcome.

**Returns:** The edges, with the heterogeneity fields set where
applicable.

`.model_diagnostics`internalanalysis.R:7490

Fit quality for the models behind the top relationships

Assumption checks are reported for what they are: a failed check does
not invalidate an estimate on its own, but it does tell the reader which
numbers to distrust.

``` sig
.model_diagnostics(context, features, quiet = FALSE)
```

Arguments: `context, features, quiet`

`.network_robustness`internalanalysis.R:7641

How much the picture depends on any one part of it

If dropping a whole assay barely changes the graph, the graph was not
really using it. If dropping one node collapses it, the result rests on
a single measurement.

``` sig
.network_robustness(graph, feature_block, outcome_name)
```

Arguments: `graph, feature_block, outcome_name`

`explain`publicanalysis.R:7754

Explain why a variable ended up where it did

Assembles everything the engine knows about one variable into a single
account: which methods supported it, at what epistemic level, how stable
it was under resampling, what a confounder would have to look like to
explain it away, and which paths it sits on.

``` sig
explain(object, feature)
```

- object:

  A `CMOResult`.

- feature: Name of the variable to explain.

**Returns:** A list, printed as a readable account.

`print.CMOExplanation`methodanalysis.R:7823

Print a readable account of one variable

``` sig
print.CMOExplanation(x, ...)
```

- x:

  A `CMOExplanation`.

- ...: Ignored.

**Returns:** The explanation, invisibly.

`.replay_value`internalanalysis.R:8056

Push a value through the stored preprocessing pipeline

Replays the feature-level steps recorded for one block, so a value in
original units can be located on the scale the analysis worked in.

``` sig
.replay_value(block_data, steps, block_name)
```

Arguments: `block_data, steps, block_name`

`.original_feature`internalanalysis.R:8089

Locate a feature's original name inside its block

``` sig
.original_feature(feature, block, assays)
```

Arguments: `feature, block, assays`

`.interpretable_generators`internalanalysis.R:8110

Which methods report a coefficient that means something per unit

A permutation importance and a network arc strength are not effects per
unit of anything, so they cannot answer this question at all.

``` sig
.interpretable_generators()
```

`.outcome_contrast`internalanalysis.R:8116

Translate an effect on the model scale into a statement about the
outcome

``` sig
.outcome_contrast(effect, se, outcome_type, design, baseline_risk = NULL)
```

Arguments: `effect, se, outcome_type, design, baseline_risk`

`.contrast_sentence`internalanalysis.R:8178

Wording that matches what the evidence actually supports

The verb is chosen by the identification label, not by the author. An
adjusted association gets "is associated with" however large its score,
because "would reduce" is a claim about intervening and nothing in an
observational adjustment licenses it.

``` sig
.contrast_sentence(variable, direction_word, amount, units, reference, contrast, identification, outcome_name)
```

Arguments:
`variable, direction_word, amount, units, reference, contrast, identification, outcome_name`

`counterfactual`publicanalysis.R:8281

What the results imply about changing a variable

Expresses each relationship as a contrast in the units the variable was
originally measured in: move this variable by this much, and the outcome
differs by this much.

``` sig
counterfactual(object, modifiable, change = "sd", direction = c("increase", "decrease"), baseline = "median", baseline_risk = NULL, min_identification = c("none", "adjustment", "temporal", "instrument"))
```

- object:

  A `CMOResult`.

- modifiable: Variables or block names to consider. Anything the user
  could plausibly act on: diet, a metabolite, a treatable protein.
  Required.

- change:

  How much to move each variable, in its original units. `"sd"` (the
  default) uses one standard deviation, `"iqr"` the interquartile range,
  a number applies that amount to everything, and a named vector sets it
  per variable.

- direction:

  Either `"increase"` or `"decrease"`.

- baseline:

  Reference point the change starts from, since non-linear preprocessing
  makes the answer depend on it. `"median"` or a number.

- baseline_risk: Observed risk of the event, used to turn an odds ratio
  into a risk ratio for a binary outcome. Taken from the data when
  absent.

- min_identification:

  Lowest identification strength to report: `"none"`, `"adjustment"`,
  `"temporal"` or `"instrument"`.

**Returns:** A `CMOCounterfactual` object, printed as readable
statements.

`print.CMOCounterfactual`methodanalysis.R:8582

Print counterfactual contrasts as readable statements

``` sig
print.CMOCounterfactual(x, ...)
```

- x:

  A `CMOCounterfactual`.

- ...: Ignored.

**Returns:** The object, invisibly.

`.is_cross_block`internalanalysis.R:8670

Does this relationship cross between measurement layers

Only meaningful between two features. An edge into the outcome always
crosses from a block to the outcome, so calling that cross-block would
make the field true for almost everything and mean nothing.

``` sig
.is_cross_block(source_block, target_block)
```

Arguments: `source_block, target_block`

`.block_evidence`internalanalysis.R:8689

Evidence summarised between every pair of blocks

``` sig
.block_evidence(evidence, outcome_name)
```

- evidence: Integrated evidence.
- outcome_name: Name of the outcome.

**Returns:** A data.frame, one row per ordered pair of blocks.

`.block_importance`internalanalysis.R:8748

How much of the total evidence each block accounts for

Answers the question a variable-level ranking cannot: which layer of
biology is carrying the result. A block can hold no single strong
variable and still dominate through many moderate ones, or the reverse.

``` sig
.block_importance(evidence, graph, feature_block, outcome_name)
```

Arguments: `evidence, graph, feature_block, outcome_name`

`.block_scores`internalanalysis.R:8836

Per-block summary across the dimensions the engine measures

Lets two blocks be compared on the same footing: one may predict well
while another carries the relationships with any claim to temporality.

``` sig
.block_scores(all_edges, feature_block, outcome_name)
```

Arguments: `all_edges, feature_block, outcome_name`

`.community_composition`internalanalysis.R:8890

What each detected community is made of

"Community 3" tells a reader nothing. "80% proteins, 15% metabolites"
tells them whether the engine found a module inside one layer or a
genuinely mixed one, which is the interesting case.

``` sig
.community_composition(graph, outcome_name)
```

Arguments: `graph, outcome_name`

`.block_graph`internalanalysis.R:8942

The same graph, with whole blocks as the nodes

Small enough to take in at a glance, which the variable-level graph
usually is not.

``` sig
.block_graph(block_evidence, block_importance)
```

Arguments: `block_evidence, block_importance`

`.arc_xy`internalanalysis.R:8998

Points along a circular arc

``` sig
.arc_xy(from, to, radius, n = 60)
```

Arguments: `from, to, radius, n`

`.chord_xy`internalanalysis.R:9014

A chord between two angles, bowed towards the centre

A straight line would cross the disc and make a dense graph unreadable.
The control point sits nearer the centre the further apart the two ends
are, so neighbours get a shallow arc and opposites a deep one, which is
what makes the bundle legible.

``` sig
.chord_xy(a1, a2, radius, n = 80)
```

Arguments: `a1, a2, radius, n`

`.circos_layout`internalanalysis.R:9048

Lay every node out around the circle, grouped by block

Sector size follows how many features a block contributed, so the figure
shows at a glance that a result rests on four hundred microbial taxa and
two clinical measurements.

``` sig
.circos_layout(nodes, outcome_name, gap = 0.04)
```

Arguments: `nodes, outcome_name, gap`

`.plot_modules`internalanalysis.R:9118

Each module beside the members it stands for

A bar of module-level estimates would show the summaries and hide what
they are summaries of. The question a reader has about a module is
whether it represents its members or averages over a disagreement among
them, and that is answerable only by drawing both.

``` sig
.plot_modules(modules, evidence, outcome_name)
```

- modules:

  A `ModuleGraph`.

- evidence: The integrated edges, for the members' own estimates.

- outcome_name: Name of the outcome.

**Returns:** A recorded plot, or NULL when there is nothing to draw.

`.plot_consensus`internalanalysis.R:9219

Where each relationship landed across the resamples

A bar chart of recurrence would say a relationship was found forty times
out of fifty and stop there. What a reader needs is whether it was near
the top every time or wandered from second to thirtieth, because those
produce the same count and completely different reports. Each
relationship is one horizontal line spanning its best to its worst rank,
with a dot at the median. A short line near the left is a finding that
would have been reported whoever happened to be sampled; a long line is
one that depended on it.

``` sig
.plot_consensus(consensus, outcome_name, max_edges = 25L)
```

- consensus:

  A `ConsensusGraph`.

- outcome_name: Name of the outcome, used only in the title.

- max_edges: How many relationships to draw, most consistent first.

**Returns:** A recorded plot, or NULL when there is nothing to draw.

`.plot_circos`internalanalysis.R:9299

One figure showing blocks, features and every relationship between them

``` sig
.plot_circos(graph, outcome_name, max_chords = 150, label_top = 25)
```

- graph:

  An `EvidenceGraph`.

- outcome_name: Name of the outcome.

- max_chords: Chords drawn, strongest first. Beyond a few hundred the
  figure stops being readable and starts being decoration.

- label_top: How many feature names to print. Every sector is always
  labelled.

**Returns:** A recorded plot, or NULL when there is nothing to draw.

`.plot_circos_blocks`internalanalysis.R:9439

The same figure with whole blocks as the only nodes

Small enough to take in at a glance, which the feature-level version
stops being once a block contributes more than a few dozen columns.

``` sig
.plot_circos_blocks(block_evidence, outcome_name)
```

Arguments: `block_evidence, outcome_name`

`check_dag`publicanalysis.R:9598

Check what a causal structure implies about an adjustment

Answers, before any model is fitted, the question that decides whether
an estimate means anything: given this causal structure, does adjusting
for these variables identify the effect of this exposure on this
outcome?

``` sig
check_dag(dag, exposure, outcome, adjusted = character())
```

- dag:

  A `dagitty` object, a dagitty specification string, or a data.frame
  with `from` and `to` columns.

- exposure,outcome: Variable names.

- adjusted: What you intend to condition on.

**Returns:** A `CMODagCheck` object, printed as a verdict with reasons.

`print.CMODagCheck`methodanalysis.R:9625

Print the verdict on an adjustment

``` sig
print.CMODagCheck(x, ...)
```

- x:

  A `CMODagCheck`.

- ...: Ignored.

**Returns:** The object, invisibly.

`.analyze_several`internalanalysis.R:9709

Analyse each outcome and correct multiplicity across all of them

Studies rarely have one endpoint. Running `analyze()` once per outcome
gives the right estimates and the wrong error rate: a relationship that
was one of fifty tests is not one of a hundred, and correcting within
each outcome separately pretends the other analyses were never run. So
each outcome is analysed on its own terms and the p-values are pooled
for one correction across the lot. Nothing else is shared: the outcomes
are not modelled jointly, and a relationship found for one says nothing
about the others.

``` sig
.analyze_several(object, outcome, time, subject, covariates, blocks, effort, assume, control, plots, quiet)
```

- object,time,subject,covariates,blocks,effort,assume,control,plots,quiet:

  As for `analyze`.

- outcome: Two or more outcome names.

**Returns:** A `CMOMultiResult`.

`.pool_multiplicity`internalanalysis.R:9749

One correction over every test in every outcome

``` sig
.pool_multiplicity(results, quiet = FALSE)
```

- results:

  Named list of `CMOResult`s.

- quiet: Suppress the summary line.

**Returns:** A `CMOMultiResult`.

`print.CMOMultiResult`methodanalysis.R:9848

Print an analysis of several outcomes

``` sig
print.CMOMultiResult(x, ...)
```

- x:

  A `CMOMultiResult`.

- ...: Ignored.

**Returns:** The object, invisibly.

### methods.R 89 functions

Everything to do with showing results: how each object prints and
summarises, plus the two HTML report builders.

`fmt_char`internalmethods.R:13

Format value as character or None placeholder

``` sig
fmt_char(x)
```

Arguments: `x`

`fmt_num`internalmethods.R:25

Format number with rounding or NA placeholder

``` sig
fmt_num(x, digits = 2)
```

Arguments: `x, digits`

`print.MultiOmicsData`methodmethods.R:54

Print a compact overview of a MultiOmicsData object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.MultiOmicsData(x, ...)
```

- x:

  A `MultiOmicsData` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.MultiOmicsData`methodmethods.R:123

Print everything stored in a MultiOmicsData object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.MultiOmicsData(object, ...)
```

- object:

  A `MultiOmicsData` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.BlockDiagnostics`methodmethods.R:230

Print a compact overview of a BlockDiagnostics object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.BlockDiagnostics(x, ...)
```

- x:

  A `BlockDiagnostics` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.BlockDiagnostics`methodmethods.R:301

Print everything stored in a BlockDiagnostics object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.BlockDiagnostics(object, ...)
```

- object:

  A `BlockDiagnostics` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.TransformationRecommendation`methodmethods.R:425

Print a compact overview of a TransformationRecommendation object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.TransformationRecommendation(x, ...)
```

- x:

  A `TransformationRecommendation` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.TransformationRecommendation`methodmethods.R:477

Print everything stored in a TransformationRecommendation object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.TransformationRecommendation(object, ...)
```

- object:

  A `TransformationRecommendation` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.PreprocessingRecipe`methodmethods.R:575

Print a compact overview of a PreprocessingRecipe object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.PreprocessingRecipe(x, ...)
```

- x:

  A `PreprocessingRecipe` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.PreprocessingRecipe`methodmethods.R:634

Print everything stored in a PreprocessingRecipe object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.PreprocessingRecipe(object, ...)
```

- object:

  A `PreprocessingRecipe` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.PreprocessingResult`methodmethods.R:795

Print a compact overview of a PreprocessingResult object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.PreprocessingResult(x, ...)
```

- x:

  A `PreprocessingResult` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.PreprocessingResult`methodmethods.R:853

Print everything stored in a PreprocessingResult object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.PreprocessingResult(object, ...)
```

- object:

  A `PreprocessingResult` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.CMOValidation`methodmethods.R:1006

Print a compact overview of a CMOValidation object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.CMOValidation(x, ...)
```

- x:

  A `CMOValidation` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.CMOValidation`methodmethods.R:1066

Print everything stored in a CMOValidation object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.CMOValidation(object, ...)
```

- object:

  A `CMOValidation` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.EvidenceEdge`methodmethods.R:1383

Print a compact overview of a EvidenceEdge object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.EvidenceEdge(x, ...)
```

- x:

  A `EvidenceEdge` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.EvidenceEdge`methodmethods.R:1462

Print everything stored in a EvidenceEdge object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.EvidenceEdge(object, ...)
```

- object:

  A `EvidenceEdge` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.Hypothesis`methodmethods.R:1621

Print a stated claim, its case, and what would settle it

``` sig
print.Hypothesis(x, ...)
```

- x:

  A `Hypothesis`.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.ModuleGraph`methodmethods.R:1728

Print the groups of features that move together

``` sig
print.ModuleGraph(x, ...)
```

- x:

  A `ModuleGraph`.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.ConsensusGraph`methodmethods.R:1818

Print what the resampling says about the shape of the graph

``` sig
print.ConsensusGraph(x, ...)
```

- x:

  A `ConsensusGraph`.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.EvidenceGraph`methodmethods.R:1934

Print a compact overview of a EvidenceGraph object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.EvidenceGraph(x, ...)
```

- x:

  A `EvidenceGraph` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.EvidenceGraph`methodmethods.R:1973

Print everything stored in a EvidenceGraph object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.EvidenceGraph(object, ...)
```

- object:

  A `EvidenceGraph` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.CMOResult`methodmethods.R:2039

Print a compact overview of a CMOResult object

Shows the few numbers that describe the object at a glance. Use
`summary()` for the full contents.

``` sig
print.CMOResult(x, ...)
```

- x:

  A `CMOResult` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`summary.CMOResult`methodmethods.R:2094

Print everything stored in a CMOResult object

Walks through every section the object carries. Use `print()` for a
one-screen overview instead.

``` sig
summary.CMOResult(object, ...)
```

- object:

  A `CMOResult` object.

- ...: Ignored.

**Returns:** The object, invisibly.

`.report_or`internalmethods.R:2272

Return default when value is missing

``` sig
.report_or(x, default = NA)
```

Arguments: `x, default`

`.report_rule`internalmethods.R:2281

Print a repeated character divider line

``` sig
.report_rule(width = 78, char = "=")
```

Arguments: `width, char`

`.report_title`internalmethods.R:2290

Print title framed by rule lines

``` sig
.report_title(text, width = 78)
```

Arguments: `text, width`

`.report_section`internalmethods.R:2302

Print underlined section heading

``` sig
.report_section(text, width = 78)
```

Arguments: `text, width`

`.report_bar`internalmethods.R:2312

Horizontal bar for a 0-100 score

``` sig
.report_bar(score, width = 40)
```

Arguments: `score, width`

`.report_grade`internalmethods.R:2332

Qualitative label for a 0-100 score

``` sig
.report_grade(score)
```

Arguments: `score`

`.report_pad`internalmethods.R:2347

Truncate and pad a string to an exact width

``` sig
.report_pad(x, n)
```

Arguments: `x, n`

`.report_bullets`internalmethods.R:2362

Print a character vector as a bulleted, wrapped list

``` sig
.report_bullets(x, prefix = "  - ", width = 78, max_items = Inf)
```

Arguments: `x, prefix, width, max_items`

`report`publicmethods.R:2407

Print a readable validation report

Generic for turning an analysis object into a human-readable report.

``` sig
report(object, ...)
```

- object: An object to report on.
- ...: Passed to methods.

**Returns:** The object, invisibly.

`report.CMOValidation`methodmethods.R:2452

Readable report for a CMOValidation object

Summarizes everything `check_data()` extracted: overall verdict and
quality score, dataset composition, per-block diagnostics, detected data
issues, sample overlap between blocks, the automatically derived
preprocessing plan, the transformation benchmark, quality-control
checks, the prioritized action list and the outputs available for
inspection. Unlike `summary()`, which dumps every stored component, this
method is organized for reading: it leads with the verdict, flags only
what needs attention, and tells the user where the remaining detail
lives.

``` sig
report.CMOValidation(object, file = NULL, format = c("html", "console"), sections = c("overview", "blocks", "issues", "overlap", "preprocessing",     "transformations", "qc", "actions", "outputs"), max_items = 12, width = 78, open = interactive(), prompt = TRUE, plot_width = 900, plot_height = 560, plot_res = 110, quiet = FALSE, ...)
```

- object:

  A `CMOValidation` object.

- file:

  Destination path for the HTML report. May be a file name or a
  directory. When `NULL` (the default) the user is prompted for a
  location in an interactive session, and the working directory is used
  otherwise. Ignored when `format = "console"`.

- format:

  Either `"html"` (the default) to write a self-contained interactive
  document, or `"console"` to print the plain-text report.

- open: Whether to open the saved report in a browser.

- prompt:

  Whether to ask for a destination when `file` is `NULL`. Set to `FALSE`
  for unattended scripts.

- plot_width,plot_height,plot_res: Pixel dimensions and resolution used
  when rasterizing the stored plots into the document.

- quiet: Whether to suppress progress messages.

- sections:

  Character vector selecting which sections to print. Any of
  `"overview"`, `"blocks"`, `"issues"`, `"overlap"`, `"preprocessing"`,
  `"transformations"`, `"qc"`, `"actions"`, `"outputs"`. Defaults to all
  of them.

- max_items: Maximum number of entries listed per itemized block
  (messages, recommendations, failed checks).

- width: Target line width.

- ...: Ignored.

**Returns:** The validation object, invisibly.

`.html_base64`internalmethods.R:2995

Base64-encode a raw vector using base R only

``` sig
.html_base64(bytes)
```

Arguments: `bytes`

`.html_escape`internalmethods.R:3025

Escape text for safe insertion into HTML

``` sig
.html_escape(x)
```

Arguments: `x`

`.html_id`internalmethods.R:3041

Make a string safe as an HTML id attribute

``` sig
.html_id(x)
```

Arguments: `x`

`.html_plot_uri`internalmethods.R:3053

Replay a recorded plot into an inline PNG data URI

``` sig
.html_plot_uri(recorded, width = 900, height = 560, res = 110)
```

Arguments: `recorded, width, height, res`

`.html_plot_items`internalmethods.R:3098

Flatten a plot slot into a named list of recorded plots

``` sig
.html_plot_items(slot)
```

Arguments: `slot`

`.html_table`internalmethods.R:3122

Render a data.frame as an interactive HTML table

``` sig
.html_table(df, caption = NULL)
```

Arguments: `df, caption`

`.html_list`internalmethods.R:3171

Render a character vector as an HTML list

``` sig
.html_list(x, class = "")
```

Arguments: `x, class`

`.html_card`internalmethods.R:3185

Render a labelled statistic card

``` sig
.html_card(label, value, sub = NULL)
```

Arguments: `label, value, sub`

`.html_section_overview`internalmethods.R:3204

Build HTML overview cards with quality gauge

``` sig
.html_section_overview(object)
```

Arguments: `object`

`.html_section_blocks`internalmethods.R:3257

Build HTML diagnostics table for each block

``` sig
.html_section_blocks(object)
```

Arguments: `object`

`.html_section_preprocessing`internalmethods.R:3329

Build HTML summary of preprocessing steps per block

``` sig
.html_section_preprocessing(object)
```

Arguments: `object`

`.html_section_transformations`internalmethods.R:3396

Build HTML panels comparing transformation candidates per block

``` sig
.html_section_transformations(object)
```

Arguments: `object`

`.html_section_qc`internalmethods.R:3431

Build HTML chip lists for QC flags

``` sig
.html_section_qc(object)
```

Arguments: `object`

`.html_section_actions`internalmethods.R:3465

Build HTML panels of recommended actions per block

``` sig
.html_section_actions(object)
```

Arguments: `object`

`.html_section_overlap`internalmethods.R:3488

Build HTML sample overlap table with note

``` sig
.html_section_overlap(object)
```

Arguments: `object`

`.html_section_plots`internalmethods.R:3563

Build HTML section embedding diagnostic plot images

``` sig
.html_section_plots(object, plot_width, plot_height, plot_res)
```

Arguments: `object, plot_width, plot_height, plot_res`

`.html_section_tables`internalmethods.R:3630

Build HTML section listing data and QC tables

``` sig
.html_section_tables(object)
```

Arguments: `object`

`.html_style`internalmethods.R:3659

Return CSS stylesheet for the HTML report

``` sig
.html_style()
```

`.html_script`internalmethods.R:3742

Return JavaScript for tab switching and table sorting

``` sig
.html_script()
```

`.report_html`internalmethods.R:3800

Build the complete self-contained HTML document

``` sig
.report_html(object, plot_width = 900, plot_height = 560, plot_res = 110, title = "CausalMultiOmics validation report")
```

Arguments: `object, plot_width, plot_height, plot_res, title`

`.report_destination`internalmethods.R:3884

Resolve the destination path for the HTML report

When `file` is `NULL` the user is prompted. In a non-interactive session
the prompt cannot block, so the default path is used and reported.

``` sig
.report_destination(file = NULL, default_name = "CausalMultiOmics_validation_report.html", prompt = TRUE)
```

Arguments: `file, default_name, prompt`

`.result_text`internalmethods.R:3962

User-facing wording for the analysis report

Keeping the prose in one table means it can be reviewed as prose, and a
translation is a matter of filling a column rather than hunting through
markup.

``` sig
.result_text(key)
```

Arguments: `key`

`.result_plain_direction`internalmethods.R:4106

Plain-language rendering of a direction

``` sig
.result_plain_direction(direction, source, target, encoding = NULL)
```

Arguments: `direction, source, target, encoding`

`.feature_encoding`internalmethods.R:4145

The encoding record for a feature, when it came from a category

``` sig
.feature_encoding(object, feature)
```

Arguments: `object, feature`

`.result_identification_label`internalmethods.R:4157

Plain-language label and explanation for an identification strategy

``` sig
.result_identification_label(identification)
```

Arguments: `identification`

`.result_identification_class`internalmethods.R:4174

Map identification strategy to strength category

``` sig
.result_identification_class(identification)
```

Arguments: `identification`

`.result_plain_statements`internalmethods.R:4196

Headline findings written for a non-specialist reader

`.interpret_graph()` phrases its statements for someone who knows what
an adjusted association is. That vocabulary is exactly what this report
exists to translate, so the general-audience version is generated from
the same facts in plainer words rather than reused verbatim.

``` sig
.result_plain_statements(object)
```

Arguments: `object`

`.result_score_bar`internalmethods.R:4273

A 0-100 score rendered as an inline bar

``` sig
.result_score_bar(score)
```

Arguments: `score`

`.result_style`internalmethods.R:4299

Return CSS styles for the results report

``` sig
.result_style()
```

`.html_result_overview`internalmethods.R:4370

Build HTML overview cards for analysis results

``` sig
.html_result_overview(object)
```

Arguments: `object`

`.html_result_howto`internalmethods.R:4411

Build HTML explanations of scoring and identification concepts

``` sig
.html_result_howto(object)
```

Arguments: `object`

`.html_result_spread_howto`internalmethods.R:4467

Explain, once, what an average does and does not say

``` sig
.html_result_spread_howto(object)
```

Arguments: `object`

`.html_result_quality_howto`internalmethods.R:4518

Explain, once, what the data-quality discount is and why it exists

``` sig
.html_result_quality_howto(object)
```

Arguments: `object`

`.html_result_dag_howto`internalmethods.R:4569

Explain the DAG audit, once, where the reader learns the vocabulary

The per-finding verdicts say what happened. This says what the words
mean, and — when no DAG was supplied — why every finding is silent about
it.

``` sig
.html_result_dag_howto(object)
```

Arguments: `object`

`.html_result_contributions`internalmethods.R:4631

What every method reported for one relationship, before merging

The headline numbers are a summary across methods. A summary the reader
cannot open is something they have to take on trust, which is the
opposite of what this report is for. Each finding therefore carries the
individual results underneath it, plus a plain description of what each
method does and what it cannot see.

``` sig
.html_result_contributions(object, source, target)
```

Arguments: `object, source, target`

`.html_result_settles`internalmethods.R:4727

What would have to be measured next to settle this one

The most actionable thing the engine produces and the only part of a
report a reader can act on directly. Everything else says what was seen;
this says what to do about it.

``` sig
.html_result_settles(object, source)
```

Arguments: `object, source`

`.html_result_spread`internalmethods.R:4758

Say whether this one number describes everybody

Only rendered when there is something to say. A box confirming that a
relationship is spread across the cohort, on every finding, teaches the
reader to skip the box that one day is not.

``` sig
.html_result_spread(object, source, target)
```

Arguments: `object, source, target`

`.html_result_quality`internalmethods.R:4827

Say which part of a relationship was measured and which was filled in

Rendered only when something behind the relationship was imputed. On a
complete dataset it would be a row of reassurances, and a reader learns
to skip a box that always says the same thing.

``` sig
.html_result_quality(object, source, target)
```

Arguments: `object, source, target`

`.html_result_dag_verdict`internalmethods.R:4890

State what the supplied DAG says about one relationship

Only ever rendered when the user supplied a causal structure. Without
one the section would be a row of shrugs, and a reader would learn to
skip it.

``` sig
.html_result_dag_verdict(object, source, target)
```

Arguments: `object, source, target`

`.html_result_findings`internalmethods.R:4935

Build HTML cards for key drivers found

``` sig
.html_result_findings(object)
```

Arguments: `object`

`.html_result_pathways`internalmethods.R:5013

Build HTML list of top causal path chains

``` sig
.html_result_pathways(object)
```

Arguments: `object`

`.html_result_modules`internalmethods.R:5060

Groups of variables that are really one thing measured several times

``` sig
.html_result_modules(object, plot_width, plot_height, plot_res)
```

Arguments: `object, plot_width, plot_height, plot_res`

`.html_result_consensus`internalmethods.R:5197

Would this report look the same with a different set of people?

The single most useful thing a reader can be told about a list of
findings and the one a single drawing cannot convey. Per-relationship
stability is already beside each finding; this is about the list as a
list.

``` sig
.html_result_consensus(object, plot_width, plot_height, plot_res)
```

Arguments: `object, plot_width, plot_height, plot_res`

`.html_result_network`internalmethods.R:5321

Build HTML network explanation with variable chips

``` sig
.html_result_network(object, plot_width, plot_height, plot_res)
```

Arguments: `object, plot_width, plot_height, plot_res`

`.html_result_blocks`internalmethods.R:5416

The same result read one measurement layer at a time

A hundred variable-to-variable relationships are hard to hold in the
head. "The microbiome contributes two thirds of the evidence, and most
of it runs to the metabolome" is not, and it is usually the question
that was being asked in the first place.

``` sig
.html_result_blocks(object)
```

Arguments: `object`

`.html_result_importance`internalmethods.R:5501

Build HTML section with consensus importance plot

``` sig
.html_result_importance(object, plot_width, plot_height, plot_res)
```

Arguments: `object, plot_width, plot_height, plot_res`

`.html_result_methods`internalmethods.R:5543

Build HTML table summarizing methods and skipped models

``` sig
.html_result_methods(object)
```

Arguments: `object`

`.html_result_data`internalmethods.R:5584

Build HTML summary of sample and variable screening

``` sig
.html_result_data(object)
```

Arguments: `object`

`.html_result_actionable`internalmethods.R:5626

What the results imply about changing something

Only rendered when the user has declared which variables could plausibly
be acted on. That declaration is the point: without it the section would
put a contrast for a genotype next to one for a diet, and a reader would
take both the same way.

``` sig
.html_result_actionable(object)
```

Arguments: `object`

`.html_result_provenance`internalmethods.R:5739

Explain every quantity the report puts in front of the reader

A number with no provenance is worse than no number: it carries the
authority of precision without the means to judge it. This section names
every quantity that appears anywhere in the document, says how it was
produced, and says what it does not mean.

``` sig
.html_result_provenance(object)
```

Arguments: `object`

`.html_result_limitations`internalmethods.R:5847

Build HTML glossary and limitations section

``` sig
.html_result_limitations(object)
```

Arguments: `object`

`.html_result_technical`internalmethods.R:5884

Build HTML technical tables and supporting figures

``` sig
.html_result_technical(object, plot_width, plot_height, plot_res)
```

Arguments: `object, plot_width, plot_height, plot_res`

`.report_html_result`internalmethods.R:5937

Assemble full HTML analysis result report

``` sig
.report_html_result(object, audience = "general", plot_width = 900, plot_height = 560, plot_res = 110, title = "CausalMultiOmics analysis report")
```

Arguments: `object, audience, plot_width, plot_height, plot_res, title`

`report.CMOResult`methodmethods.R:6062

Readable report for an analysis result

Turns a `CMOResult` into a self-contained HTML document written for a
reader who is not a statistician: findings first, in plain language,
with the technical tables tucked behind `audience = "technical"`. The
report states throughout what the numbers do and do not support. A
reader who does not know what an adjusted association is will otherwise
read a high score as proof of cause, so every finding carries a plain
label for how much can be claimed, and the reasoning behind that label
is explained in its own section rather than buried in a footnote.

``` sig
report.CMOResult(object, file = NULL, format = c("html", "console"), audience = c("general", "technical"), open = interactive(), prompt = TRUE, plot_width = 900, plot_height = 560, plot_res = 110, quiet = FALSE, ...)
```

- object:

  A `CMOResult` object.

- file:

  Destination path for the HTML report. May be a file name or a
  directory. When `NULL` the user is prompted in an interactive session
  and the working directory is used otherwise.

- format:

  Either `"html"` (the default) or `"console"` for a plain-text summary.

- audience:

  `"general"` (the default) writes for a non-specialist reader.
  `"technical"` adds a section with the complete tables and every
  diagnostic figure.

- open: Whether to open the saved report in a browser.

- prompt:

  Whether to ask for a destination when `file` is `NULL`. Set to `FALSE`
  for unattended scripts.

- plot_width,plot_height,plot_res: Pixel dimensions and resolution used
  when rasterizing the stored plots into the document.

- quiet: Whether to suppress progress messages.

- ...: Ignored.

**Returns:** The analysis object, invisibly.

`export_graph`publicmethods.R:6231

Write the evidence graph in a format another tool can open

The graph is the deliverable for a good many users, and most of them
will want to lay it out somewhere this package has no business trying to
be. Cytoscape and Gephi both read GraphML, which is the default. Every
edge attribute travels with it, so the evidence survives the export: the
score, the three components it is made of, the identification, the data
quality and the FDR are all readable in the receiving tool. A graph
exported with only its arrows would arrive stripped of everything that
made it worth reading.

``` sig
export_graph(object, file, format = c("graphml", "dot", "json"), min_score = 0)
```

- object:

  A `CMOResult`.

- file:

  Where to write. The extension is not checked against `format`; the
  format argument decides.

- format:

  One of `"graphml"` (Cytoscape, Gephi), `"dot"` (Graphviz) or `"json"`.
  The first two go through igraph; `"json"` is written directly and
  needs nothing installed. GML and Pajek are deliberately absent. Pajek
  carries no edge attributes, so a graph exported to it arrives as
  arrows with none of the evidence that made it worth exporting, and
  igraph's GML writer rejects the vertex table this package produces. A
  format that silently discards the answer or fails outright is worse
  than one that is not offered.

- min_score: Relationships scoring below this are left out. The default
  keeps everything.

**Returns:** The path, invisibly.

`.export_graph_json`internalmethods.R:6317

Write the graph as JSON, without needing igraph

Hand-rolled rather than pulling in a JSON package for one function. The
structure is nodes and links, which is what most JavaScript graph
libraries expect to be handed.

``` sig
.export_graph_json(nodes, edges, object, file)
```

- nodes,edges: The filtered tables.
- object: The result, for the provenance block.
- file: Where to write.

**Returns:** Nothing.

### comparison.R 4 functions

`compare_results`publiccomparison.R:65

Compare two analyses of the same question

Answers the four questions a reader has when handed two results: which
relationships appear in both, which reverse, which vanish, and which
survive at a size that no longer means the same thing.

``` sig
compare_results(a, b, names = c("first", "second"), tolerance = 0.5)
```

- a,b:

  Two `CMOResult` objects.

- names: What to call them in the output.

- tolerance: How far the second estimate may fall from the first, as a
  ratio, before the relationship is reported as agreeing in direction
  only. The default of 0.5 flags anything that halved.

**Returns:** A `CMOComparison`.

`.comparability`internalcomparison.R:184

Are these two results even comparable?

The check that has to come first. Two graphs can be compared only if the
analyses behind them estimated the same quantity on the same kind of
outcome with the same adjustment. Sixty per cent overlap between two
different questions is a number about nothing.

``` sig
.comparability(a, b)
```

- a,b: Two results.

**Returns:** A list with a verdict and the reasons for it.

`.comparison_edges`internalcomparison.R:262

The two columns a comparison needs from a result

``` sig
.comparison_edges(r)
```

- r: A CMOResult.

**Returns:** A data.frame of source, target, estimate and score.

`print.CMOComparison`methodcomparison.R:290

Print a comparison of two analyses

``` sig
print.CMOComparison(x, ...)
```

- x:

  A `CMOComparison`.

- ...: Ignored.

**Returns:** The object, invisibly.

### options.R 5 functions

`analysis_assumptions`publicoptions.R:73

What you are claiming, as opposed to what you measured

Everything here is a statement the data cannot check. A causal diagram
is a claim about biology. A variable declared modifiable is a claim that
someone could act on it. A reliability figure is a claim about the
assay. Each changes what the analysis is allowed to conclude, and each
is your responsibility rather than the engine's.

``` sig
analysis_assumptions(dag = NULL, modifiable = NULL, negative_controls = NULL, heterogeneity = NULL, reliability = NULL, competing = NULL, forbidden = NULL, required = NULL, positivity = TRUE, min_residual = 0.1)
```

- dag:

  Causal structure, as a `dagitty` object, a dagitty specification
  string, or a data.frame with `from` and `to`. Supplying one lets the
  engine check whether an adjustment identifies each effect rather than
  only recording that something was adjusted for. It never changes an
  estimate. See `check_dag`.

- modifiable: Variables or blocks someone could plausibly act on.
  Nothing is expressed as a contrast in original units unless it appears
  here: a contrast for a genotype is arithmetic without meaning.

- negative_controls: Variables that should show no relationship with the
  outcome. If they do, something other than biology is driving the
  graph.

- heterogeneity: Metadata columns to test as effect modifiers. Naming
  one asks whether each relationship differs across the groups it
  defines.

- reliability: Named vector giving the share of each variable's measured
  variance that is real, from duplicates or from the assay. Classical
  measurement error attenuates a coefficient by exactly this factor, and
  supplying it corrects for that. Nothing is guessed: a variable absent
  from this vector is left alone.

- competing: Metadata column marking competing events in a survival
  analysis. Without it, dying of another cause is treated as censoring,
  which assumes those people could still have had the outcome.

- forbidden,required:

  data.frames of `from`/`to` pairs that structure learning may not
  propose, or must include. Both are claims about the world in the same
  way a diagram is, and both are recorded as assumptions on every edge
  they shaped.

- positivity: Check whether each effect is estimable at all: whether the
  variables adjusted for leave the exposure any independent variation.
  An estimate without it is the model extrapolating.

- min_residual: Share of the exposure's variance that must survive
  adjustment before the estimate is treated as measurement rather than
  extrapolation.

**Returns:** An `analysis_assumptions` object.

`analysis_control`publicoptions.R:145

How much work to do, and on what

Nothing here changes what the analysis is allowed to conclude. These are
decisions about cost and scope: getting one wrong makes the answer
slower or noisier, never wrong.

``` sig
analysis_control(methods = NULL, goal = c("causal", "predictive"), resample = NULL, resample_scheme = c("bootstrap", "cv"), resample_methods = NULL, permutations = NULL, diagnostics = NULL, bootstrap = 200, max_features = 150, min_per_block = 10, min_evidence_score = 1, seed = 1L)
```

- methods: Which evidence generators to run. All applicable ones by
  default.

- goal:

  `"causal"` or `"predictive"`, which changes how features are screened.

- resample:

  How many resamples for stability. Taken from `effort` when left alone.

- resample_scheme:

  `"bootstrap"` or `"cv"`.

- resample_methods: Which generators to re-run on each resample.

- permutations: How many outcome permutations for null calibration.

- diagnostics: Run model diagnostics, effect concentration and the
  complete-case check.

- bootstrap: Resamples inside the mediation generator.

- max_features: Cap on features carried into the pairwise stage.

- min_per_block: Features guaranteed to each block during screening, so
  a large block cannot take every slot.

- min_evidence_score: Relationships scoring below this are dropped.

- seed:

  Passed to `set.seed()` where the engine needs randomness. The caller's
  generator is restored afterwards.

**Returns:** An `analysis_control` object.

`.as_group`internaloptions.R:185

Accept a group, or build the default one

``` sig
.as_group(value, builder, label)
```

- value: What the caller passed.
- builder: The constructor for that group.
- label: Its name, for the message.

**Returns:** A validated group object.

`print.analysis_assumptions`methodoptions.R:210

Print what is being assumed

``` sig
print.analysis_assumptions(x, ...)
```

- x:

  An `analysis_assumptions`.

- ...: Ignored.

**Returns:** The object, invisibly.

`print.analysis_control`methodoptions.R:256

Print how much work will be done

``` sig
print.analysis_control(x, ...)
```

- x:

  An `analysis_control`.

- ...: Ignored.

**Returns:** The object, invisibly.

### positivity.R 6 functions

`.residual_variation`internalpositivity.R:39

How much independent variation an exposure has left after adjustment

Positivity fails on a continuum rather than at a threshold, so the
useful quantity is how much of the exposure survives the adjustment.
Regress the exposure on the variables being adjusted for: whatever
variance is left is what an effect can be estimated from. If they
predict it almost perfectly there is nothing left, and the coefficient
that comes out is the model extrapolating into a region the data never
visited.

``` sig
.residual_variation(x, covariates)
```

- x: The exposure.
- covariates: A data.frame of what is being adjusted for, or NULL.

**Returns:** A list with the share of the exposure's variance left after
adjustment, or `NULL` when there is nothing to check.

`.empty_strata`internalpositivity.R:84

Strata of the adjustment set in which the exposure never varies

The textbook form of the problem, for an exposure with two levels. A
stratum containing only exposed people contributes nothing to a contrast
between exposed and unexposed, and a model that includes it is filling
in the missing half from the shape of its own equation. Continuous
covariates are cut into quantile bins, because exact strata of a
continuous variable each contain one person and would report every study
ever done as hopeless.

``` sig
.empty_strata(x, covariates, bins = 4L)
```

- x: The exposure.
- covariates: What is being adjusted for.
- bins: How many bins to cut a continuous covariate into.

**Returns:** A list describing the strata with no contrast, or `NULL`.

`.positivity`internalpositivity.R:144

Can this effect be estimated from this data at all?

``` sig
.positivity(x, covariates, min_residual = 0.1)
```

- x: The exposure.
- covariates: What is being adjusted for, or NULL.
- min_residual: Below this share of surviving variance, the estimate is
  treated as extrapolation rather than measurement.

**Returns:** A list with the verdict, the numbers behind it, and what to
say.

`.attenuation_correction`internalpositivity.R:227

Correct an estimate for the attenuation a noisy exposure causes

Classical measurement error in an exposure multiplies the coefficient by
the reliability of that exposure. Dividing by it undoes the attenuation.

``` sig
.attenuation_correction(estimate, se, reliability)
```

- estimate,se: The reported coefficient and its standard error.
- reliability: The share of the measured variance that is real, from
  duplicates or from the assay. Between 0 and 1.

**Returns:** A list with the corrected estimate, its standard error, and
what the correction assumed.

`.competing_risk_assumptions`internalpositivity.R:277

What a survival estimate assumes when other causes are possible

``` sig
.competing_risk_assumptions(competing = NULL, n_competing = 0L, n_total = 0L)
```

- competing: Name of a column marking competing events, or NULL.
- n_competing: How many people had one.
- n_total: How many people there are.

**Returns:** Assumption text for the edges of a survival analysis.

`.audit_estimability`internalpositivity.R:316

Attach the positivity check and any measurement-error correction

Both belong on the edge rather than in a table at the bottom, for the
same reason the DAG audit does: nobody cross-references a table against
the finding they are reading.

``` sig
.audit_estimability(edges, x, covariates, outcome_name, assume)
```

- edges: Integrated edges.
- x: The analysis matrix.
- covariates: Covariate frame, or NULL.
- outcome_name: Name of the outcome.
- assume: The assumptions the caller declared.

**Returns:** The edges, with the new fields set where they apply.

### sensitivity.R 4 functions

`sensitivity`publicsensitivity.R:85

Re-run one relationship under every defensible preprocessing choice

A multiverse analysis, restricted to the decisions this package makes on
the user's behalf. The relationship is refitted under each variant
recipe and the spread of the answer is reported.

``` sig
sensitivity(object, feature = NULL, variants = NULL, max_paths = 24L, quiet = FALSE)
```

- object:

  A `CMOResult`.

- feature: The variable whose relationship with the outcome to test.
  Defaults to the highest-scoring one.

- variants:

  Recipes to try, as produced by `.sensitivity_variants()`. Left alone
  by default, which builds them from the defensible alternatives for
  each block.

- max_paths: Cap on how many recipes to run.

- quiet: Suppress progress.

**Returns:** A `CMOSensitivity`.

`.sensitivity_result`internalsensitivity.R:245

Assemble the verdict from the paths that ran

``` sig
.sensitivity_result(paths, edge, outcome, block, attempted)
```

- paths: One row per recipe.
- edge: The relationship tested.
- outcome,block: Names.
- attempted: How many variants were tried.

**Returns:** A CMOSensitivity.

`.sensitivity_variants`internalsensitivity.R:330

Build the defensible alternatives to one recipe

Alternatives, not every combination. A grid over six imputations,
sixteen transformations and six scalings is five hundred paths, most of
them nobody would defend, and the resulting spread would say more about
the grid than about the data.

``` sig
.sensitivity_variants(recipe, max_paths = 24L)
```

- recipe: The recipe that was actually used.
- max_paths: Cap.

**Returns:** A list of variants, each with a recipe and a label. The
first is always the original, so the report can mark it.

`print.CMOSensitivity`methodsensitivity.R:405

Print how much of a finding is the analyst

``` sig
print.CMOSensitivity(x, ...)
```

- x:

  A `CMOSensitivity`.

- ...: Ignored.

**Returns:** The object, invisibly.

### simulation.R 5 functions

`simulate_data`publicsimulation.R:98

Generate a multi-block study with a known answer

Builds a `MultiOmicsData` from a causal structure you specify, and
records what it did so the result can be checked rather than admired.

``` sig
simulate_data(n = 200, blocks = list(main = 10), dag = NULL, outcome = "y", outcome_type = c("continuous", "binary", "survival"), modules = NULL, module_loading = 0.9, missing = 0, missing_pattern = c("random", "by_outcome"), batch = 0, batch_effect = 0.5, coverage = 1, noise = 1, seed = 1)
```

- n: Number of people.

- blocks:

  Named list. Each element is either a character vector of feature names
  to place in that block, or a single number meaning that many
  pure-noise features. Names used in `dag` must appear here to be
  measured; anything else in `dag` becomes metadata.

- dag:

  A data.frame with columns `from`, `to` and optionally `effect`
  (default 0.6). Both structure and magnitude in one object, because
  they are the same claim.

- outcome:

  Name of the outcome variable, which must appear in `dag` unless
  `effects_on_outcome` is used.

- outcome_type:

  `"continuous"`, `"binary"` or `"survival"`. Survival adds a time and a
  status column.

- modules: Named list of latent processes: each element is a character
  vector of feature names that will be driven by one unobserved
  variable. This is the shape a real biological process has, and it is
  not expressible as a DAG between measured variables.

- module_loading: How strongly each module drives its members.

- missing: Fraction of values to blank, either one number for every
  block or a named vector per block.

- missing_pattern:

  `"random"` blanks at random; `"by_outcome"` blanks preferentially
  where the outcome is high, which is the pattern imputation handles
  worst and the one that manufactures findings.

- batch: Number of batches, or 0 for none.

- batch_effect: Size of the shift between batches.

- coverage: Fraction of people each block measured, either one number or
  a named vector. Below 1 the blocks stop sharing a population, which is
  what makes sample alignment a real problem.

- noise: Standard deviation of the noise added to every generated
  variable.

- seed:

  Passed to `set.seed()`. The caller's random number generator is
  restored afterwards.

**Returns:** A `MultiOmicsData` with `misc$simulation` describing what
was planted.

`.simulation_structure`internalsimulation.R:351

Normalise the structure argument

``` sig
.simulation_structure(dag)
```

- dag: A data.frame, or NULL.

**Returns:** A data.frame with from, to and effect.

`.simulate_from_dag`internalsimulation.R:393

Generate variables in topological order

A variable with no parents is standard normal. A variable with parents
is the weighted sum of them plus noise, so an effect of 0.8 is eight
tenths of a standard deviation in the child per standard deviation in
the parent and a regression will recover roughly that number.

``` sig
.simulate_from_dag(structure_df, n, noise, outcome, outcome_type)
```

- structure_df: from/to/effect.
- n: Sample size.
- noise: Noise standard deviation.
- outcome: Name of the outcome.
- outcome_type: Its type.

**Returns:** A named list of numeric vectors.

`.simulation_outcome`internalsimulation.R:465

Turn the outcome's latent value into the requested kind of outcome

``` sig
.simulation_outcome(value, outcome, outcome_type, n)
```

- value: The generated numeric outcome.
- outcome: Its name.
- outcome_type: Which kind.
- n: Sample size.

**Returns:** A data.frame of the outcome columns.

`.simulation_per_block`internalsimulation.R:520

Accept one number or one per block

``` sig
.simulation_per_block(value, blocks, label, neutral = 0)
```

- value: Scalar or named vector.
- blocks: Block names.
- label: What it is, for the error message.
- neutral: What an unnamed block gets. Not the same for every argument:
  no missingness is 0 and full coverage is 1, so a shared default of
  zero silently reduced every unnamed block to ten people.

**Returns:** A named list, one entry per block.

## When something goes wrong

These are the messages you are most likely to see, what they mean, and
what to do. The library tries to fail with an explanation rather than a
code.

`Block 'x' has no sample identifiers.`

**Meaning.** One of your blocks has no row labels, so the library cannot
tell which measurement belongs to which person.

**Fix.** rownames(my_block) \<- my_ids before calling load_data().

**Why it refuses.** Without labels every block would be numbered 1, 2,
3… and two unrelated blocks would look as though they shared every
sample.

`Outcome 'y' has 4 unordered categories.`

**Meaning.** Your outcome has more than two categories with no natural
order (like colours, or treatment centres).

**Fix.** Turn it into a yes/no question — one category against all the
rest — or use a genuinely ordered variable.

**Why it refuses.** There is no honest way to put unordered categories
on a number line. The analysis would run and produce results that look
perfectly normal and mean nothing.

`Only 3 sample(s) are shared by the blocks a, b.`

**Meaning.** Your blocks barely overlap, so almost nobody has both kinds
of measurement.

**Fix.** Analyse fewer blocks at once, or check that the sample
identifiers really match between blocks (typos, prefixes, capitals).

`The plan does not match this object.`

**Meaning.** You are trying to apply a cleaning plan that was written
for different data.

**Fix.** Run check_data() again on the data you actually want to clean.

`'object' must be a PreprocessingResult.`

**Meaning.** You called analyze() on raw data instead of cleaned data.

**Fix.** Run preprocess() first. Analysis deliberately starts from data
whose cleaning is already fixed and recorded.

`'time = "x"' was ignored`

**Meaning.** A warning, not an error — the analysis ran. You gave a time
variable, but it changed nothing, because a time variable is only used
when paired with a yes/no event (survival) or with repeated measurements
per person (longitudinal).

**Fix.** Either supply a binary outcome alongside it, or a subject
column with repeated measurements.

`The knn method needs package 'x', which is not installed.`

**Meaning.** An optional method needs something you do not have.

**Fix.** install.packages("x"), or use a different method. The library
never installs anything behind your back.

### Getting help inside R

    ?analyze              # the manual page for a function
    ?CausalMultiOmics     # the package front page
    args(analyze)         # just the list of arguments
    example(load_data)    # run the documented example

## Glossary

- Assay / block: One kind of measurement made on your samples — all the
  proteins, or all the metabolites. Blocks are kept separate because
  they behave differently.

- Sample: One person, animal or specimen. Rows of your data.

- Feature / variable: One thing measured. Columns of your data.

- Metadata: What you know about the samples themselves: age, sex, group,
  outcome.

- Outcome: The thing you are trying to explain.

- Covariate: Something you want to account for so it does not confuse
  the picture — age, for instance.

- Missing value:

  A measurement that was not obtained. Written NA in R.

- Imputation: Filling in missing values with an estimate.

- Normalisation:

  Removing technical differences between *samples*, so two people can be
  compared.

- Scaling:

  Putting *variables* on a comparable footing, so a measurement in
  thousands does not drown one in decimals.

- Transformation: Reshaping numbers — for instance taking logarithms —
  so they behave better in statistical models.

- Batch effect:

  A difference caused by *when* or *where* samples were processed rather
  than by biology.

- Confounder: Something that affects two variables at once, making them
  look related when neither causes the other. The central problem of
  this whole field.

- Mediator: A variable that sits in the middle of a chain: A affects B,
  and B affects C.

- Hub: A variable connected to many others in the map.

- Bridge: A variable linking two different kinds of measurement — a
  protein connected to a metabolite, say.

- p-value: Roughly: how surprising this result would be if there were
  really no relationship. Small means surprising.

- FDR: A p-value corrected for the fact that you tested thousands of
  things and some will look impressive by luck alone.

- Bootstrap: Re-running an analysis on many random re-samples of your
  data, to see how much the answer wobbles.

- Cross-sectional: Everything measured once, at one moment.

- Longitudinal: The same people measured repeatedly over time.

- Survival analysis: Studying how long it takes for an event to happen,
  allowing for people who never had it during the study.

- Evidence level: What kind of claim a method can support, from
  predictive up to identified. Agreement between methods is weighted by
  it.

- E-value: How strong an unmeasured factor would have to be to explain a
  relationship away entirely.

- Resampling: Repeating the analysis on random re-samples of the same
  people, to see which findings survive.

- Null calibration: Running the engine on data with the outcome
  shuffled, to see how much it finds when there is nothing to find.

- Negative control: Something you know cannot be part of the mechanism.
  If it appears anyway, the pipeline is picking up something that is not
  biology.

- Circos plot: The circular figure: each arc is one kind of measurement,
  each thread a relationship. The one picture that shows the whole
  answer at once.

- Counterfactual: What the results imply about changing a variable,
  stated in the units it was measured in.
