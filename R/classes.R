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

      objective = NULL,

      priority = NULL,

      comments = character(),

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

      custom_filter = NULL,

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
      # Statistics
      # -----------------------------------------------------------------------

      statistics = list(),

      quality = list(),

      diagnostics = list(),

      # -----------------------------------------------------------------------
      # Processing log
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

      history = character(),

      misc = list()

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

        overlap = NULL,

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
# CMOResult
# =============================================================================

#' Create a CMOResult object
#'
#' Top-level container for a complete CausalMultiOmics analysis. Each
#' analysis stage fills its own slot, so a partially finished workflow is
#' represented by the stages that are still empty rather than by a missing
#' element. \code{print()} and \code{summary()} report a stage as completed
#' exactly when its slot has non-zero length.
#'
#' @return A CMOResult object.
#' @keywords internal

CMOResult <- function() {

  structure(

    list(

      # -----------------------------------------------------------------------
      # Input
      # -----------------------------------------------------------------------

      data = NULL,

      validation = NULL,

      # -----------------------------------------------------------------------
      # Analysis stages
      # -----------------------------------------------------------------------

      integration = list(),

      latent = list(),

      causal = list(),

      network = list(),

      biomarkers = list(),

      prediction = list(),

      enrichment = list(),

      # -----------------------------------------------------------------------
      # Configuration and performance
      # -----------------------------------------------------------------------

      parameters = list(),

      performance = list(),

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

      history = character(),

      misc = list()

    ),

    class = "CMOResult"

  )

}

# =============================================================================
# End of classes.R
# =============================================================================
