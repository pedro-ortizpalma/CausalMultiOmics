# =============================================================================
# classes.R
# Core S3 classes for CausalMultiOmics
# =============================================================================

# =============================================================================
# MultiOmicsData
# =============================================================================

#' Create a MultiOmicsData object
#'
#' Core container storing every data block loaded into CausalMultiOmics.
#'
#' @return A MultiOmicsData object.
#' @keywords internal

MultiOmicsData <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # Input data
      # -----------------------------------------------------------------------

      assays = list(),

      metadata = NULL,

      sample_info = NULL,

      feature_info = NULL,

      # Named character vector, one entry per block. Records the assay
      # modality ("rnaseq", "proteomics", ...) which the statistical type
      # cannot recover: RNA-seq counts and any other counts are
      # indistinguishable from the numbers alone, yet they call for
      # different normalization families.

      modality = character(),

      # -----------------------------------------------------------------------
      # Processing
      # -----------------------------------------------------------------------

      preprocessing = list(),

      history = character(),

      misc = list()

    ),

    class = "MultiOmicsData"

  )

}

# =============================================================================
# BlockDiagnostics
# =============================================================================

#' Create a BlockDiagnostics object
#'
#' Stores every statistic calculated for one data block.
#'
#' @return A BlockDiagnostics object.
#' @keywords internal

BlockDiagnostics <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # General
      # -----------------------------------------------------------------------

      block = NULL,

      data_type = NULL,

      modality = NULL,

      objective = NULL,

      samples = NULL,

      features = NULL,

      observations = NULL,

      # -----------------------------------------------------------------------
      # Missing values
      # -----------------------------------------------------------------------

      missing_values = NULL,

      missing_percent = NULL,

      missing_by_sample = NULL,

      missing_by_feature = NULL,

      empty_samples = NULL,

      empty_features = NULL,

      # -----------------------------------------------------------------------
      # Data composition
      # -----------------------------------------------------------------------

      zero_percent = NULL,

      negative_percent = NULL,

      positive_percent = NULL,

      infinite_percent = NULL,

      sparsity = NULL,

      density = NULL,

      # -----------------------------------------------------------------------
      # Descriptive statistics
      # -----------------------------------------------------------------------

      mean = NULL,

      median = NULL,

      variance = NULL,

      sd = NULL,

      mad = NULL,

      cv = NULL,

      iqr = NULL,

      minimum = NULL,

      maximum = NULL,

      range = NULL,

      quantiles = NULL,

      # -----------------------------------------------------------------------
      # Distribution
      # -----------------------------------------------------------------------

      skewness = NULL,

      kurtosis = NULL,

      shapiro = NULL,

      normality = NULL,

      outliers = NULL,

      # -----------------------------------------------------------------------
      # Feature quality
      # -----------------------------------------------------------------------

      constant_features = NULL,

      near_constant_features = NULL,

      duplicated_features = NULL,

      duplicated_samples = NULL,

      # -----------------------------------------------------------------------
      # Correlation
      # -----------------------------------------------------------------------

      covariance = NULL,

      correlation = NULL,

      # -----------------------------------------------------------------------
      # Multivariate
      # -----------------------------------------------------------------------

      pca = NULL,

      explained_variance = NULL,

      # -----------------------------------------------------------------------
      # Additional statistics
      # -----------------------------------------------------------------------

      statistics = list()

    ),

    class = "BlockDiagnostics"

  )

}

# =============================================================================
# TransformationRecommendation
# =============================================================================

#' Create a TransformationRecommendation object
#'
#' Stores automatic transformation benchmarking for one data block.
#'
#' @return A TransformationRecommendation object.
#' @keywords internal

TransformationRecommendation <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # General
      # -----------------------------------------------------------------------

      block = NULL,

      detected_type = NULL,

      objective = NULL,

      automatic = TRUE,

      # -----------------------------------------------------------------------
      # Candidate transformations
      # -----------------------------------------------------------------------

      candidates = character(),

      # -----------------------------------------------------------------------
      # Benchmark results
      # -----------------------------------------------------------------------

      metrics = data.frame(),

      ranking = data.frame(),

      # -----------------------------------------------------------------------
      # Recommendation
      # -----------------------------------------------------------------------

      recommended = NULL,

      score = NULL,

      justification = character(),

      # -----------------------------------------------------------------------
      # Transformation model
      # -----------------------------------------------------------------------

      model = NULL,

      parameters = list(),

      # -----------------------------------------------------------------------
      # Diagnostics
      # -----------------------------------------------------------------------

      before = list(),

      after = list(),

      # -----------------------------------------------------------------------
      # Messages
      # -----------------------------------------------------------------------

      warnings = character(),

      errors = character(),

      messages = character()

    ),

    class = "TransformationRecommendation"

  )

}

# =============================================================================
# PreprocessingRecipe
# =============================================================================

#' Create a PreprocessingRecipe object
#'
#' Stores the preprocessing strategy recommended during validation or
#' specified manually by the user.
#'
#' @return A PreprocessingRecipe object.
#' @keywords internal

PreprocessingRecipe <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # General
      # -----------------------------------------------------------------------

      block = NULL,

      enabled = TRUE,

      automatic = TRUE,

      data_type = NULL,

      modality = NULL,

      objective = NULL,

      priority = NULL,

      comments = character(),

      # Executable order of the stages, resolved from the data type. The
      # recipe records which method runs at each stage; this records when.

      stage_order = character(),

      # -----------------------------------------------------------------------
      # Sample filtering
      # -----------------------------------------------------------------------

      remove_empty_samples = FALSE,

      remove_duplicate_samples = FALSE,

      sample_missing_threshold = NULL,

      sample_filter = NULL,

      # -----------------------------------------------------------------------
      # Feature filtering
      # -----------------------------------------------------------------------

      remove_empty_features = FALSE,

      remove_constant_features = FALSE,

      remove_near_constant_features = FALSE,

      remove_duplicate_features = FALSE,

      feature_missing_threshold = NULL,

      variance_threshold = NULL,

      abundance_threshold = NULL,

      prevalence_threshold = NULL,

      correlation_threshold = NULL,

      custom_filter = NULL,

      # -----------------------------------------------------------------------
      # Outliers
      # -----------------------------------------------------------------------
      #
      # Never selected automatically: dropping observations is a scientific
      # decision, not a data-cleaning one. Set by hand to enable.

      outlier_handling = NULL,

      outlier_threshold = NULL,

      # -----------------------------------------------------------------------
      # Missing value imputation
      # -----------------------------------------------------------------------

      imputation = NULL,

      imputation_parameters = list(),

      imputation_model = NULL,

      # -----------------------------------------------------------------------
      # Transformation
      # -----------------------------------------------------------------------

      transformation = NULL,

      transformation_candidates = NULL,

      transformation_model = NULL,

      transformation_parameters = list(),

      transformation_score = NULL,

      transformation_justification = character(),

      # -----------------------------------------------------------------------
      # Normalization
      # -----------------------------------------------------------------------

      normalization = NULL,

      normalization_parameters = list(),

      normalization_model = NULL,

      # -----------------------------------------------------------------------
      # Scaling
      # -----------------------------------------------------------------------

      scaling = NULL,

      scaling_parameters = list(),

      scaling_model = NULL,

      # -----------------------------------------------------------------------
      # Batch correction
      # -----------------------------------------------------------------------

      batch = NULL,

      batch_variable = NULL,

      batch_parameters = list(),

      batch_model = NULL,

      # -----------------------------------------------------------------------
      # Feature selection
      # -----------------------------------------------------------------------

      feature_selection = NULL,

      feature_selection_parameters = list(),

      feature_selection_model = NULL,

      # -----------------------------------------------------------------------
      # Dimensionality reduction
      # -----------------------------------------------------------------------

      dimensionality_reduction = NULL,

      dimensionality_parameters = list(),

      dimensionality_model = NULL,

      # -----------------------------------------------------------------------
      # Expected quality
      # -----------------------------------------------------------------------

      estimated_quality = NULL,

      expected_missing = NULL,

      expected_variance = NULL,

      expected_normality = NULL

    ),

    class = "PreprocessingRecipe"

  )

}

# =============================================================================
# PreprocessingResult
# =============================================================================

#' Create a PreprocessingResult object
#'
#' Stores every output generated during preprocessing.
#'
#' @return A PreprocessingResult object.
#' @keywords internal

PreprocessingResult <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # Input
      # -----------------------------------------------------------------------

      input = NULL,

      data = NULL,

      recipes = list(),

      # -----------------------------------------------------------------------
      # Pipeline record
      # -----------------------------------------------------------------------
      #
      # steps is the source of truth: an ordered list of everything that was
      # executed, each entry carrying its stage, method, parameters, the
      # fitted model, the dimensions before and after, what it removed and
      # how long it took. apply_preprocessing() replays this record against
      # new data, so the parallel views further down are derived from it and
      # exist only for convenience.

      steps = list(),

      # models[[block]][[stage]] -> the fitted model for that stage.

      models = list(),

      # -----------------------------------------------------------------------
      # Statistics
      # -----------------------------------------------------------------------

      statistics = list(),

      quality = list(),

      diagnostics = list(),

      # -----------------------------------------------------------------------
      # Derived views over steps
      # -----------------------------------------------------------------------

      removed_samples = list(),

      removed_features = list(),

      imputation = list(),

      transformations = list(),

      normalization = list(),

      scaling = list(),

      batch = list(),

      feature_selection = list(),

      dimensionality_reduction = list(),

      # -----------------------------------------------------------------------
      # Outputs
      # -----------------------------------------------------------------------

      plots = list(),

      tables = list(),

      reports = list(),

      # -----------------------------------------------------------------------
      # Execution
      # -----------------------------------------------------------------------

      execution = list(),

      logs = character(),

      history = character(),

      misc = list(),

      timestamp = Sys.time()

    ),

    class = "PreprocessingResult"

  )

}

# =============================================================================
# CMOValidation
# =============================================================================

#' Create a CMOValidation object
#'
#' Stores every result generated during data validation.
#'
#' @return A CMOValidation object.
#' @keywords internal

CMOValidation <- function() {

  structure(

    list(

      # =======================================================================
      # Validation status
      # =======================================================================

      valid = TRUE,

      score = NA_real_,

      errors = character(),

      warnings = character(),

      info = character(),

      # =======================================================================
      # Global summary
      # =======================================================================

      summary = list(

        n_blocks = NULL,

        n_samples = NULL,

        n_features = NULL,

        observations = NULL,

        total_missing = NULL,

        total_missing_percent = NULL,

        duplicated_samples = NULL,

        duplicated_features = NULL,

        block_summary = NULL,

        # Pairwise, which is what a matrix can express.

        overlap = NULL,

        # And the figure an analysis actually runs on. Every pair of twenty
        # blocks can share the whole cohort while the twenty together share
        # none of it, because each can be missing a different part. The
        # matrix above is silent about that; these are not.

        cumulative_overlap = NULL,

        shared_by_all = NULL,

        quality_score = NULL,

        n_errors = NULL,

        n_warnings = NULL

      ),

      # =======================================================================
      # Structural details
      # =======================================================================

      details = list(

        duplicated_samples = NULL,

        duplicated_features = NULL,

        missing_sample_ids = NULL,

        missing_feature_ids = NULL,

        missing_values = NULL,

        empty_samples = NULL,

        empty_features = NULL,

        metadata_only = NULL,

        block_only = NULL,

        unsupported_variables = NULL

      ),

      # =======================================================================
      # Diagnostics
      # =======================================================================

      diagnostics = list(),

      #
      # diagnostics[[block]]
      # -> BlockDiagnostics
      #

      # =======================================================================
      # Automatic preprocessing
      # =======================================================================

      recipes = list(),

      #
      # recipes[[block]]
      # -> PreprocessingRecipe
      #

      # =======================================================================
      # Automatic transformation benchmark
      # =======================================================================

      transformations = list(),

      #
      # transformations[[block]]
      # -> TransformationRecommendation
      #
      # =======================================================================
      # Recommendations
      # =======================================================================

      recommendations = list(

        global = character(),

        blocks = list()

      ),

      # =======================================================================
      # Quality Control
      # =======================================================================

      qc = list(

        passed = TRUE,

        score = NULL,

        failed_checks = character(),

        passed_checks = character(),

        skipped_checks = character()

      ),

      # =======================================================================
      # Graphical outputs
      # =======================================================================

      plots = list(

        # ---------------------------------------------------------------
        # Missing values
        # ---------------------------------------------------------------

        missing_heatmap = NULL,

        missing_by_block = NULL,

        missing_by_sample = NULL,

        missing_by_feature = NULL,

        missing_pattern = NULL,

        # ---------------------------------------------------------------
        # Sample overlap
        # ---------------------------------------------------------------

        overlap_heatmap = NULL,

        overlap_network = NULL,

        # ---------------------------------------------------------------
        # Correlation
        # ---------------------------------------------------------------

        correlation_heatmap = NULL,

        correlation_distribution = NULL,

        # ---------------------------------------------------------------
        # Distributions
        # ---------------------------------------------------------------

        histograms = NULL,

        density = NULL,

        qqplots = NULL,

        boxplots = NULL,

        violinplots = NULL,

        # ---------------------------------------------------------------
        # Multivariate
        # ---------------------------------------------------------------

        pca = NULL,

        scree = NULL,

        sample_clustering = NULL,

        feature_clustering = NULL,

        outliers = NULL,

        # ---------------------------------------------------------------
        # Transformation benchmark
        # ---------------------------------------------------------------

        transformation_scores = NULL,

        transformation_comparison = NULL

      ),

      # =======================================================================
      # Tables
      # =======================================================================

      tables = list(

        block_summary = NULL,

        diagnostics = NULL,

        recommendations = NULL,

        preprocessing = NULL,

        transformations = NULL

      ),

      # =======================================================================
      # Execution information
      # =======================================================================

      execution = list(

        runtime = NULL,

        started = NULL,

        finished = NULL,

        package_version = NULL,

        validation_version = "1.0.0",

        arguments = NULL,

        seed = NULL,

        session = NULL

      ),

      # =======================================================================
      # User information
      # =======================================================================

      history = character(),

      misc = list(),

      timestamp = Sys.time()

    ),

    class = "CMOValidation"

  )

}

# =============================================================================
# EvidenceEdge
# =============================================================================

#' Create an EvidenceEdge object
#'
#' One directed relationship between two variables, as reported by one
#' analysis method. Every evidence generator emits these; the integrator
#' merges the ones that describe the same relationship and scores them.
#'
#' @section What this object is allowed to claim:
#'
#' An edge records what was estimated AND what would have to be true for that
#' estimate to carry a causal reading. \code{identification} says which
#' strategy, if any, licenses the causal interpretation:
#'
#' \describe{
#'   \item{\code{"none"}}{Marginal association. No adjustment was made.}
#'   \item{\code{"adjustment"}}{Conditioned on \code{adjustment_set}. Causal
#'     only if that set blocks every backdoor path, which the data cannot
#'     confirm.}
#'   \item{\code{"temporal"}}{The exposure was measured before the outcome,
#'     which rules out reverse causation but not confounding.}
#'   \item{\code{"instrument"}}{An instrumental variable was used.}
#' }
#'
#' This matters because agreement between methods is easy to mistake for
#' validity. If several models share an unmeasured confounder they agree with
#' each other and are all wrong together: consistency measures stability, not
#' correctness. Keeping the assumptions attached to the estimate is what
#' stops a stability score from being read as a causal one.
#'
#' @return An EvidenceEdge object.
#' @keywords internal

EvidenceEdge <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # Relationship
      # -----------------------------------------------------------------------

      source = NULL,

      target = NULL,

      source_block = NULL,

      target_block = NULL,

      direction = NULL,

      # -----------------------------------------------------------------------
      # Estimate
      # -----------------------------------------------------------------------

      estimate = NA_real_,

      se = NA_real_,

      ci_lower = NA_real_,

      ci_upper = NA_real_,

      p_value = NA_real_,

      fdr = NA_real_,

      effect_size = NA_real_,

      n = NA_integer_,

      # -----------------------------------------------------------------------
      # Provenance
      # -----------------------------------------------------------------------

      method = NULL,

      generator = NULL,

      model = NULL,

      # -----------------------------------------------------------------------
      # What kind of number this is
      # -----------------------------------------------------------------------
      #
      # A log odds ratio, a log hazard ratio and a permutation importance are
      # not the same measurement, and averaging across them yields a number
      # that estimates nothing. The integrator pools only within a family that
      # shares a scale, and records here which family the reported estimate
      # came from and how many observations went into it.

      quantity = NA_character_,

      quantity_family = NA_character_,

      quantity_label = NA_character_,

      pooled_from = 0L,

      not_pooled = character(),

      # -----------------------------------------------------------------------
      # What licenses a causal reading
      # -----------------------------------------------------------------------

      identification = "none",

      adjustment_set = character(),

      assumptions = character(),

      temporal = NA,

      # -----------------------------------------------------------------------
      # Checked against a stated causal structure
      # -----------------------------------------------------------------------
      #
      # NA unless the user supplied a DAG. Without one the engine can say that
      # something was adjusted for and no more, which is a weak statement:
      # conditioning on a mediator removes part of the effect being measured,
      # and conditioning on a collider manufactures an association out of
      # nothing. Only a declared structure can tell those apart.

      identifiable = NA,

      identification_reason = character(),

      required_adjustment = character(),

      adjustment_problems = character(),

      # -----------------------------------------------------------------------
      # How much of this was measured rather than reconstructed
      # -----------------------------------------------------------------------
      #
      # A confidence interval is conditional on the values that went into it
      # being real. Imputation replaces missing cells with plausible numbers
      # and the interval afterwards knows nothing about it, so a variable that
      # was 40% filled in reports the same precision as one that was fully
      # observed. This records the difference.
      #
      # Only imputed values count against quality. Rows dropped for being
      # incomplete cost sample size, which `confidence` already reflects
      # through n; charging for them twice would double-count.

      data_quality = NA_real_,

      quality_flags = character(),

      quality_limited_by = NA_character_,

      # What the same model finds using only the rows that were actually
      # observed. If a relationship survives on real data alone it did not
      # come out of the imputation.

      complete_case_estimate = NA_real_,

      complete_case_n = NA_integer_,

      complete_case_agrees = NA,

      # Epistemic level of the strongest method that contributed, 1 to 5.
      # Agreement between methods is weighted by this: five predictive
      # methods concurring is weaker evidence than one longitudinal model
      # plus one mediation analysis, and an unweighted vote count says the
      # opposite.

      level = 2L,

      level_label = "associational",

      # -----------------------------------------------------------------------
      # How much unmeasured confounding it would take to explain this away
      # -----------------------------------------------------------------------

      e_value = NA_real_,

      e_value_ci = NA_real_,

      # -----------------------------------------------------------------------
      # Whether the arrow points the way it is drawn
      # -----------------------------------------------------------------------

      direction_confidence = NA_real_,

      direction_basis = "untested",

      # -----------------------------------------------------------------------
      # Survival under resampling
      # -----------------------------------------------------------------------

      bootstrap_stability = NA_real_,

      bootstrap_found = NA_integer_,

      bootstrap_evaluable = NA_integer_,

      # -----------------------------------------------------------------------
      # Integrated scores (filled by the evidence integrator)
      # -----------------------------------------------------------------------

      stability = NA_real_,

      strength = NA_real_,

      confidence = NA_real_,

      consistency = NA_real_,

      evidence_score = NA_real_,

      supporting_methods = character(),

      conflicting_methods = character(),

      conflict_summary = character(),

      # What each contributing method reported on its own, before merging.
      # The integrated numbers are a summary, and a summary that cannot be
      # opened is a claim the reader has to take on trust. One row per
      # method: its estimate, interval, p-value, sample size, epistemic
      # level, and whether it agreed with the majority.

      contributions = data.frame(),

      # -----------------------------------------------------------------------
      # Optional external support (filled by annotate_evidence())
      # -----------------------------------------------------------------------

      biological_support = NA_real_,

      biological_sources = character(),

      # -----------------------------------------------------------------------
      # Messages
      # -----------------------------------------------------------------------

      warnings = character(),

      notes = character()

    ),

    class = "EvidenceEdge"

  )

}

# =============================================================================
# EvidenceGraph
# =============================================================================

#' Create an EvidenceGraph object
#'
#' The integrated directed graph of evidence: variables as nodes, integrated
#' \code{EvidenceEdge} objects as edges, plus the structure derived from them
#' (communities, paths, centrality).
#'
#' @return An EvidenceGraph object.
#' @keywords internal

EvidenceGraph <- function() {

  structure(

    list(

      nodes = data.frame(),

      edges = data.frame(),

      evidence = list(),

      adjacency = NULL,

      communities = list(),

      paths = data.frame(),

      metrics = list(),

      igraph = NULL,

      parameters = list()

    ),

    class = "EvidenceGraph"

  )

}

# =============================================================================
# ConsensusGraph
# =============================================================================

#' Create a ConsensusGraph object
#'
#' What the graph looks like across resamples rather than in the one sample
#' that happened to be collected.
#'
#' Per-edge stability answers "would this relationship come back?" one
#' relationship at a time. It cannot answer "would this picture come back?",
#' and those are different questions: a graph whose edges are each recovered
#' six times in ten is a stable structure if it is the same six edges every
#' time and no structure at all if it is a different six. A reader shown a
#' single drawing has no way to tell which they are looking at.
#'
#' Everything here is read off the resampling that already runs, so it costs
#' no additional model fits.
#'
#' @return A ConsensusGraph object.
#' @keywords internal

ConsensusGraph <- function() {

  structure(

    list(

      # One row per relationship ever seen in any replicate: how often it
      # appeared, where it ranked when it did, and whether the replicates
      # agreed on which way it pointed.

      edges = data.frame(),

      # The relationships appearing in at least `threshold` of replicates.

      consensus = data.frame(),

      threshold = 0.5,

      # How many edges each replicate produced. A graph that is 12 edges in
      # one resample and 40 in the next is not a graph, and the median is not
      # the thing to report.

      sizes = integer(),

      # Of the relationships reported from the full sample, how many are in
      # the consensus, and how many consensus relationships never made it
      # into the report.

      agreement = list(),

      # How often the highest-scoring relationships kept their place.

      rank_stability = data.frame(),

      replicates = 0L,

      scheme = NA_character_,

      notes = character()

    ),

    class = "ConsensusGraph"

  )

}

# =============================================================================
# CMOResult
# =============================================================================

#' Create a CMOResult object
#'
#' Top-level container for a complete CausalMultiOmics analysis. The result
#' of \code{analyze()} is not a model but a body of evidence: every method
#' that ran contributes edges, the integrator merges and scores them, and the
#' graph plus its interpretation is what the user reads.
#'
#' Each slot is filled by the stage that produces it, so a partially finished
#' analysis is represented by the slots that are still empty rather than by a
#' missing element.
#'
#' @return A CMOResult object.
#' @keywords internal

CMOResult <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # Input
      # -----------------------------------------------------------------------

      input = NULL,

      data = NULL,

      design = list(),

      outcome = list(),

      # -----------------------------------------------------------------------
      # Evidence
      # -----------------------------------------------------------------------

      models = list(),

      effects = data.frame(),

      evidence = list(),

      graph = NULL,

      causal_paths = data.frame(),

      importance = data.frame(),

      network = list(),

      # What the graph looks like across resamples. Empty until resampling
      # runs, which is from effort = "standard" upward.

      consensus = NULL,

      # -----------------------------------------------------------------------
      # Assessment
      # -----------------------------------------------------------------------

      diagnostics = list(),

      performance = list(),

      interpretation = list(),

      # -----------------------------------------------------------------------
      # Outputs
      # -----------------------------------------------------------------------

      plots = list(),

      tables = list(),

      report = list(),

      # -----------------------------------------------------------------------
      # Execution
      # -----------------------------------------------------------------------

      parameters = list(),

      execution = list(),

      logs = character(),

      history = character(),

      misc = list(),

      timestamp = Sys.time()

    ),

    class = "CMOResult"

  )

}

# =============================================================================
# End of classes.R
# =============================================================================
