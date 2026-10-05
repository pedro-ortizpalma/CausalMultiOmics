# Package index

## Load and audit

Bring blocks in, measure them, and get a preprocessing recipe. Nothing
is modified.

- [`load_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/load_data.md)
  : Load Multi-Block Data
- [`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
  : Validate a MultiOmicsData object
- [`simulate_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/simulate_data.md)
  : Generate a multi-block study with a known answer

## Preprocess

Execute the recipe, recording every step so it can be replayed on a
second cohort.

- [`preprocess()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)
  : Execute a preprocessing plan
- [`apply_preprocessing()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/apply_preprocessing.md)
  : Replay a fitted pipeline on new data

## State the question

What you believe and how it should be computed, written down before
anything is estimated.

- [`analysis_assumptions()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_assumptions.md)
  : What you are claiming, as opposed to what you measured
- [`analysis_control()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_control.md)
  : How much work to do, and on what
- [`check_dag()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_dag.md)
  : Check what a causal structure implies about an adjustment

## Analyse

Run the evidence generators and merge them into one scored graph.

- [`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
  : Build quantified causal evidence from preprocessed multi-block data
- [`annotate_evidence()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/annotate_evidence.md)
  : Attach external biological support to an evidence graph

## Read the result

Findings as prose, as a table, and as the figures the object already
carries.

- [`explain()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/explain.md)
  : Explain why a variable ended up where it did
- [`outcome_name()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/outcome_name.md)
  : The name of the outcome an analysis was run on
- [`cmo_plots()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_plots.md)
  : Which figures an object carries
- [`as.data.frame(`*`<CMOCounterfactual>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOCounterfactual.md)
  : The counterfactual contrasts, as a data frame
- [`as.data.frame(`*`<CMOResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOResult.md)
  : The relationships an analysis found, as a data frame
- [`as.data.frame(`*`<CMOSensitivity>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOSensitivity.md)
  : The analysis variants a sensitivity run fitted, as a data frame
- [`as.data.frame(`*`<CMOValidation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOValidation.md)
  : The block-level audit, as a data frame
- [`as.data.frame(`*`<EvidenceGraph>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.EvidenceGraph.md)
  : The edges of an evidence graph, as a data frame
- [`as.data.frame(`*`<PreprocessingResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.PreprocessingResult.md)
  : What each preprocessing stage did, as a data frame
- [`plot(`*`<CMOResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/plot.CMOResult.md)
  : Draw a figure recorded during an analysis
- [`plot(`*`<CMOValidation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/plot.CMOValidation.md)
  : Draw a figure recorded during the audit
- [`plot(`*`<PreprocessingResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/plot.PreprocessingResult.md)
  : Draw a figure recorded during preprocessing

## Robustness and action

Whether the result survives your preprocessing choices, what changing a
variable would imply, and whether a claim replicates.

- [`sensitivity()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/sensitivity.md)
  : Re-run one relationship under every defensible preprocessing choice
- [`counterfactual()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/counterfactual.md)
  : What the results imply about changing a variable
- [`hypothesis()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/hypothesis.md)
  : State one relationship as a claim that could be wrong
- [`test_hypothesis()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/test_hypothesis.md)
  : Take a stated claim to a different cohort
- [`compare_results()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/compare_results.md)
  : Compare two analyses of the same question

## Output

A self-contained HTML report, or the graph for Cytoscape and igraph.

- [`report()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/report.md)
  : Print a readable validation report
- [`report(`*`<CMOResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/report.CMOResult.md)
  : Readable report for an analysis result
- [`report(`*`<CMOValidation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/report.CMOValidation.md)
  : Readable report for a CMOValidation object
- [`export_graph()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/export_graph.md)
  : Write the evidence graph in a format another tool can open

## Installation

Which evidence generators this machine can actually run.

- [`cmo_setup()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_setup.md)
  : What this installation can run, and what it is missing

## Print and summary methods

S3 methods behind print() and summary() for every class the package
returns.

- [`print(`*`<analysis_assumptions>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.analysis_assumptions.md)
  : Print what is being assumed
- [`print(`*`<analysis_control>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.analysis_control.md)
  : Print how much work will be done
- [`print(`*`<BlockDiagnostics>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.BlockDiagnostics.md)
  : Print a compact overview of a BlockDiagnostics object
- [`print(`*`<cmo_setup>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.cmo_setup.md)
  : Print what this installation can run
- [`print(`*`<CMOComparison>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOComparison.md)
  : Print a comparison of two analyses
- [`print(`*`<CMOCounterfactual>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOCounterfactual.md)
  : Print counterfactual contrasts as readable statements
- [`print(`*`<CMODagCheck>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMODagCheck.md)
  : Print the verdict on an adjustment
- [`print(`*`<CMOExplanation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOExplanation.md)
  : Print a readable account of one variable
- [`print(`*`<CMOMultiResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOMultiResult.md)
  : Print an analysis of several outcomes
- [`print(`*`<CMOResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOResult.md)
  : Print a compact overview of a CMOResult object
- [`print(`*`<CMOSensitivity>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOSensitivity.md)
  : Print how much of a finding is the analyst
- [`print(`*`<CMOValidation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.CMOValidation.md)
  : Print a compact overview of a CMOValidation object
- [`print(`*`<ConsensusGraph>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.ConsensusGraph.md)
  : Print what the resampling says about the shape of the graph
- [`print(`*`<EvidenceEdge>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.EvidenceEdge.md)
  : Print a compact overview of a EvidenceEdge object
- [`print(`*`<EvidenceGraph>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.EvidenceGraph.md)
  : Print a compact overview of a EvidenceGraph object
- [`print(`*`<Hypothesis>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.Hypothesis.md)
  : Print a stated claim, its case, and what would settle it
- [`print(`*`<ModuleGraph>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.ModuleGraph.md)
  : Print the groups of features that move together
- [`print(`*`<MultiOmicsData>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.MultiOmicsData.md)
  : Print a compact overview of a MultiOmicsData object
- [`print(`*`<PreprocessingRecipe>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.PreprocessingRecipe.md)
  : Print a compact overview of a PreprocessingRecipe object
- [`print(`*`<PreprocessingResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.PreprocessingResult.md)
  : Print a compact overview of a PreprocessingResult object
- [`print(`*`<summary.BlockDiagnostics>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.BlockDiagnostics.md)
  : Print the summary of a BlockDiagnostics object
- [`print(`*`<summary.CMOResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.CMOResult.md)
  : Print the summary of a CMOResult object
- [`print(`*`<summary.CMOValidation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.CMOValidation.md)
  : Print the summary of a CMOValidation object
- [`print(`*`<summary.EvidenceEdge>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.EvidenceEdge.md)
  : Print the summary of a EvidenceEdge object
- [`print(`*`<summary.EvidenceGraph>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.EvidenceGraph.md)
  : Print the summary of a EvidenceGraph object
- [`print(`*`<summary.MultiOmicsData>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.MultiOmicsData.md)
  : Print the summary of a MultiOmicsData object
- [`print(`*`<summary.PreprocessingRecipe>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.PreprocessingRecipe.md)
  : Print the summary of a PreprocessingRecipe object
- [`print(`*`<summary.PreprocessingResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.PreprocessingResult.md)
  : Print the summary of a PreprocessingResult object
- [`print(`*`<summary.TransformationRecommendation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.TransformationRecommendation.md)
  : Print the summary of a TransformationRecommendation object
- [`print(`*`<TransformationRecommendation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.TransformationRecommendation.md)
  : Print a compact overview of a TransformationRecommendation object
- [`summary(`*`<BlockDiagnostics>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.BlockDiagnostics.md)
  : Assemble the full summary of a BlockDiagnostics object
- [`summary(`*`<CMOResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.CMOResult.md)
  : Assemble the full summary of a CMOResult object
- [`summary(`*`<CMOValidation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.CMOValidation.md)
  : Assemble the full summary of a CMOValidation object
- [`summary(`*`<EvidenceEdge>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.EvidenceEdge.md)
  : Assemble the full summary of a EvidenceEdge object
- [`summary(`*`<EvidenceGraph>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.EvidenceGraph.md)
  : Assemble the full summary of a EvidenceGraph object
- [`summary(`*`<MultiOmicsData>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.MultiOmicsData.md)
  : Assemble the full summary of a MultiOmicsData object
- [`summary(`*`<PreprocessingRecipe>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.PreprocessingRecipe.md)
  : Assemble the full summary of a PreprocessingRecipe object
- [`summary(`*`<PreprocessingResult>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.PreprocessingResult.md)
  : Assemble the full summary of a PreprocessingResult object
- [`summary(`*`<TransformationRecommendation>`*`)`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/summary.TransformationRecommendation.md)
  : Assemble the full summary of a TransformationRecommendation object
