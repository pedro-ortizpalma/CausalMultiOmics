# =============================================================================
# methods.R
# S3 methods for CausalMultiOmics
# =============================================================================

# =============================================================================
# Internal formatting helpers
# =============================================================================

fmt_char <- function(x) {

  if (is.null(x) || length(x) == 0 || all(is.na(x)))
    return("None")

  as.character(x)

}

fmt_num <- function(x, digits = 2) {

  if (is.null(x) || length(x) == 0 || all(is.na(x)))
    return("NA")

  format(round(x, digits), nsmall = digits)

}

# =============================================================================
# MultiOmicsData
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.MultiOmicsData <- function(x, ...) {

  cat("\n")
  cat("MultiOmicsData\n")
  cat("==============\n\n")

  cat(sprintf("%-22s %d\n",
              "Blocks:",
              length(x$assays)))

  if(is.null(x$sample_info)){

    cat(sprintf("%-22s Unknown\n",
                "Samples:"))

  }else{

    cat(sprintf("%-22s %d\n",
                "Samples:",
                length(unique(x$sample_info$sample_id))))

  }

  if(is.null(x$feature_info)){

    cat(sprintf("%-22s Unknown\n",
                "Features:"))

  }else{

    cat(sprintf("%-22s %d\n",
                "Features:",
                nrow(x$feature_info)))

  }

  cat(sprintf("%-22s %s\n",
              "Metadata:",
              ifelse(is.null(x$metadata),"No","Yes")))

  cat(sprintf("%-22s %d\n",
              "Preprocessing:",
              length(x$preprocessing)))

  cat(sprintf("%-22s %d\n",
              "History:",
              length(x$history)))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.MultiOmicsData <- function(object,...){

  cat("\n")
  cat("========== MultiOmicsData Summary ==========\n\n")

  cat("Blocks\n")
  cat("------\n")

  if(length(object$assays)==0){

    cat("None\n")

  }else{

    for(i in seq_along(object$assays)){

      x <- object$assays[[i]]

      cat(

        sprintf(

          "%-20s %8d samples x %8d features\n",

          names(object$assays)[i],

          nrow(x),

          ncol(x)

        )

      )

    }

  }

  cat("\n")

  cat("Metadata\n")
  cat("--------\n")

  if(is.null(object$metadata)){

    cat("None\n")

  }else{

    cat(nrow(object$metadata),"rows\n")

  }

  cat("\n")

  cat("Preprocessing\n")
  cat("-------------\n")

  if(length(object$preprocessing)==0){

    cat("None\n")

  }else{

    print(names(object$preprocessing))

  }

  cat("\n")

  cat("History\n")
  cat("-------\n")

  if(length(object$history)==0){

    cat("Empty\n")

  }else{

    cat(paste0("- ",object$history),sep="\n")

  }

  invisible(object)

}

# =============================================================================
# BlockDiagnostics
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.BlockDiagnostics <- function(x, ...) {

  cat("\n")
  cat("BlockDiagnostics\n")
  cat("================\n\n")

  cat(sprintf("%-28s %s\n",
              "Block:",
              x$block))

  cat(sprintf("%-28s %s\n",
              "Data type:",
              x$data_type))

  cat(sprintf("%-28s %s\n",
              "Objective:",
              x$objective))

  cat(sprintf("%-28s %d\n",
              "Samples:",
              x$samples))

  cat(sprintf("%-28s %d\n",
              "Features:",
              x$features))

  cat(sprintf("%-28s %.2f %%\n",
              "Missing values:",
              x$missing_percent))

  cat(sprintf("%-28s %.2f %%\n",
              "Zeros:",
              x$zero_percent))

  cat(sprintf("%-28s %.2f %%\n",
              "Negative values:",
              x$negative_percent))

  cat(sprintf("%-28s %.2f %%\n",
              "Infinite values:",
              x$infinite_percent))

  cat(sprintf("%-28s %d\n",
              "Constant features:",
              length(x$constant_features)))

  cat(sprintf("%-28s %d\n",
              "Near constant:",
              length(x$near_constant_features)))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.BlockDiagnostics <- function(object, ...) {

  cat("\n")
  cat("========== Block Diagnostics ==========\n\n")

  cat("General\n")
  cat("-------\n")

  cat(sprintf("%-25s %s\n","Block",object$block))
  cat(sprintf("%-25s %s\n","Data type",object$data_type))
  cat(sprintf("%-25s %d\n","Samples",object$samples))
  cat(sprintf("%-25s %d\n","Features",object$features))

  cat("\n")

  cat("Data quality\n")
  cat("------------\n")

  cat(sprintf("%-25s %.2f %%\n",
              "Missing",
              object$missing_percent))

  cat(sprintf("%-25s %.2f %%\n",
              "Zeros",
              object$zero_percent))

  cat(sprintf("%-25s %.2f %%\n",
              "Negative",
              object$negative_percent))

  cat(sprintf("%-25s %.2f %%\n",
              "Infinite",
              object$infinite_percent))

  cat(sprintf("%-25s %.2f\n",
              "Density",
              object$density))

  cat(sprintf("%-25s %.2f\n",
              "Sparsity",
              object$sparsity))

  cat("\n")

  cat("Distribution\n")
  cat("------------\n")

  cat(sprintf("%-25s %.4f\n","Mean",object$mean))
  cat(sprintf("%-25s %.4f\n","Median",object$median))
  cat(sprintf("%-25s %.4f\n","Variance",object$variance))
  cat(sprintf("%-25s %.4f\n","SD",object$sd))
  cat(sprintf("%-25s %.4f\n","MAD",object$mad))
  cat(sprintf("%-25s %.4f\n","CV",object$cv))
  cat(sprintf("%-25s %.4f\n","IQR",object$iqr))
  cat(sprintf("%-25s %.4f\n","Minimum",object$minimum))
  cat(sprintf("%-25s %.4f\n","Maximum",object$maximum))
  cat(sprintf("%-25s %.4f\n","Range",object$range))

  cat("\n")

  cat("Normality\n")
  cat("---------\n")

  cat(sprintf("%-25s %.4f\n",
              "Skewness",
              object$skewness))

  cat(sprintf("%-25s %.4f\n",
              "Kurtosis",
              object$kurtosis))

  cat(sprintf("%-25s %s\n",
              "Normality test",
              as.character(object$normality)))

  cat(sprintf("%-25s %d\n",
              "Outliers",
              length(object$outliers)))

  cat("\n")

  cat("Feature quality\n")
  cat("----------------\n")

  cat(sprintf("%-25s %d\n",
              "Constant",
              length(object$constant_features)))

  cat(sprintf("%-25s %d\n",
              "Near constant",
              length(object$near_constant_features)))

  cat(sprintf("%-25s %d\n",
              "Duplicated features",
              length(object$duplicated_features)))

  cat(sprintf("%-25s %d\n",
              "Duplicated samples",
              length(object$duplicated_samples)))

  invisible(object)

}

# =============================================================================
# TransformationRecommendation
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.TransformationRecommendation <- function(x, ...) {

  cat("\n")
  cat("TransformationRecommendation\n")
  cat("============================\n\n")

  cat(sprintf("%-28s %s\n",
              "Block:",
              x$block))

  cat(sprintf("%-28s %s\n",
              "Detected data type:",
              x$detected_type))

  cat(sprintf("%-28s %s\n",
              "Objective:",
              x$objective))

  cat(sprintf("%-28s %d\n",
              "Candidates tested:",
              length(x$candidates)))

  cat(sprintf("%-28s %s\n",
              "Recommended:",
              x$recommended))

  if(!is.null(x$score))
    cat(sprintf("%-28s %.2f\n",
                "Recommendation score:",
                x$score))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.TransformationRecommendation <- function(object, ...) {

  cat("\n")
  cat("===== Transformation Recommendation =====\n\n")

  cat("General\n")
  cat("-------\n")

  cat(sprintf("%-25s %s\n",
              "Block",
              object$block))

  cat(sprintf("%-25s %s\n",
              "Detected type",
              object$detected_type))

  cat(sprintf("%-25s %s\n",
              "Objective",
              object$objective))

  cat(sprintf("%-25s %d\n",
              "Candidates",
              length(object$candidates)))

  cat("\n")

  cat("Recommendation\n")
  cat("--------------\n")

  cat(sprintf("%-25s %s\n",
              "Selected",
              object$recommended))

  if(!is.null(object$score))
    cat(sprintf("%-25s %.2f\n",
                "Score",
                object$score))

  if(length(object$justification)>0){

    cat("\n")

    cat("Justification\n")
    cat("-------------\n")

    cat(paste0("- ",object$justification),
        sep="\n")

  }

  if(nrow(object$ranking)>0){

    cat("\n")

    cat("Ranking\n")
    cat("-------\n")

    print(object$ranking,
          row.names=FALSE)

  }

  if(nrow(object$metrics)>0){

    cat("\n")

    cat("Evaluation metrics\n")
    cat("------------------\n")

    print(object$metrics,
          row.names=FALSE)

  }

  invisible(object)

}

# =============================================================================
# PreprocessingRecipe
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.PreprocessingRecipe <- function(x, ...) {

  cat("\n")
  cat("PreprocessingRecipe\n")
  cat("===================\n\n")

  cat(sprintf("%-30s %s\n",
              "Block:",
              x$block))

  cat(sprintf("%-30s %s\n",
              "Automatic:",
              ifelse(isTRUE(x$automatic),"Yes","No")))

  cat(sprintf("%-30s %s\n",
              "Enabled:",
              ifelse(isTRUE(x$enabled),"Yes","No")))

  cat(sprintf("%-30s %s\n",
              "Objective:",
              x$objective))

  cat(sprintf("%-30s %s\n",
              "Transformation:",
              x$transformation))

  cat(sprintf("%-30s %s\n",
              "Normalization:",
              x$normalization))

  cat(sprintf("%-30s %s\n",
              "Scaling:",
              x$scaling))

  cat(sprintf("%-30s %s\n",
              "Imputation:",
              x$imputation))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.PreprocessingRecipe <- function(object, ...) {

  cat("\n")
  cat("========== Preprocessing Recipe ==========\n\n")

  cat("General\n")
  cat("-------\n")

  cat(sprintf("%-30s %s\n","Block",object$block))
  cat(sprintf("%-30s %s\n","Objective",object$objective))
  cat(sprintf("%-30s %s\n",
              "Automatic",
              ifelse(isTRUE(object$automatic),"Yes","No")))

  cat("\n")

  cat("Filtering\n")
  cat("---------\n")

  cat(sprintf("%-30s %s\n",
              "Remove empty samples",
              object$remove_empty_samples))

  cat(sprintf("%-30s %s\n",
              "Remove empty features",
              object$remove_empty_features))

  cat(sprintf("%-30s %s\n",
              "Remove constant features",
              object$remove_constant_features))

  cat(sprintf("%-30s %s\n",
              "Remove near constant",
              object$remove_near_constant_features))

  cat(sprintf("%-30s %s\n",
              "Remove duplicate samples",
              object$remove_duplicate_samples))

  cat(sprintf("%-30s %s\n",
              "Remove duplicate features",
              object$remove_duplicate_features))

  cat("\n")

  cat("Missing values\n")
  cat("--------------\n")

  cat(sprintf("%-30s %s\n",
              "Method",
              object$imputation))

  if(length(object$imputation_parameters)>0){

    print(object$imputation_parameters)

  }

  cat("\n")

  cat("Transformation\n")
  cat("----------------\n")

  cat(sprintf("%-30s %s\n",
              "Method",
              object$transformation))

  if(!is.null(object$transformation_score)){

    cat(sprintf("%-30s %.2f\n",
                "Score",
                object$transformation_score))

  }

  if(length(object$transformation_justification)>0){

    cat("\n")

    cat("Reason\n")
    cat("------\n")

    cat(paste0("- ",
               object$transformation_justification),
        sep="\n")

  }

  cat("\n")

  cat("Normalization\n")
  cat("-------------\n")

  cat(sprintf("%-30s %s\n",
              "Method",
              object$normalization))

  cat("\n")

  cat("Scaling\n")
  cat("-------\n")

  cat(sprintf("%-30s %s\n",
              "Method",
              object$scaling))

  cat("\n")

  cat("Batch correction\n")
  cat("----------------\n")

  cat(sprintf("%-30s %s\n",
              "Method",
              object$batch))

  cat(sprintf("%-30s %s\n",
              "Variable",
              object$batch_variable))

  cat("\n")

  cat("Feature selection\n")
  cat("-----------------\n")

  cat(sprintf("%-30s %s\n",
              "Method",
              object$feature_selection))

  cat("\n")

  cat("Expected quality\n")
  cat("----------------\n")

  cat(sprintf("%-30s %s\n",
              "Estimated quality",
              object$estimated_quality))

  invisible(object)

}

# =============================================================================
# PreprocessingResult
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.PreprocessingResult <- function(x, ...) {

  cat("\n")
  cat("PreprocessingResult\n")
  cat("===================\n\n")

  n.blocks <- if(is.null(x$data)) 0 else length(x$data$assays)

  cat(sprintf("%-30s %d\n",
              "Blocks:",
              n.blocks))

  cat(sprintf("%-30s %d\n",
              "Recipes:",
              length(x$recipes)))

  cat(sprintf("%-30s %d\n",
              "Removed samples:",
              sum(lengths(x$removed_samples))))

  cat(sprintf("%-30s %d\n",
              "Removed features:",
              sum(lengths(x$removed_features))))

  cat(sprintf("%-30s %d\n",
              "Plots:",
              length(Filter(Negate(is.null),x$plots))))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.PreprocessingResult <- function(object, ...) {

  cat("\n")
  cat("======= Preprocessing Result =======\n\n")

  cat("Recipes\n")
  cat("-------\n")

  if(length(object$recipes)==0){

    cat("None\n")

  }else{

    for(nm in names(object$recipes)){

      r <- object$recipes[[nm]]

      cat(

        sprintf(

          "%-20s %s -> %s -> %s\n",

          nm,

          r$transformation,

          r$normalization,

          r$scaling

        )

      )

    }

  }

  cat("\n")

  cat("Samples removed\n")
  cat("----------------\n")

  print(object$removed_samples)

  cat("\n")

  cat("Features removed\n")
  cat("-----------------\n")

  print(object$removed_features)

  cat("\n")

  cat("Statistics\n")
  cat("----------\n")

  if(length(object$statistics)>0)
    print(object$statistics)

  invisible(object)

}

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.CMOValidation <- function(x, ...) {

  cat("\n")
  cat("CMOValidation\n")
  cat("=============\n\n")

  cat(sprintf("%-30s %s\n",
              "Status:",
              ifelse(isTRUE(x$valid),
                     "VALID",
                     "INVALID")))

  cat(sprintf("%-30s %s\n",
              "Quality score:",
              fmt_num(x$score)))

  cat(sprintf("%-30s %s\n",
              "Blocks:",
              fmt_num(x$summary$n_blocks,0)))

  cat(sprintf("%-30s %s\n",
              "Samples:",
              fmt_num(x$summary$n_samples,0)))

  cat(sprintf("%-30s %s\n",
              "Features:",
              fmt_num(x$summary$n_features,0)))

  cat(sprintf("%-30s %d\n",
              "Errors:",
              length(x$errors)))

  cat(sprintf("%-30s %d\n",
              "Warnings:",
              length(x$warnings)))

  cat(sprintf("%-30s %d\n",
              "Recipes:",
              length(x$recipes)))

  invisible(x)

}

# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.CMOValidation <- function(object, ...) {

  cat("\n")
  cat("============= CMOValidation Summary =============\n\n")

  # =====================================================================
  # Status
  # =====================================================================

  cat("Validation status\n")
  cat("-----------------\n")

  cat(sprintf("%-30s %s\n",
              "Status",
              ifelse(isTRUE(object$valid),
                     "VALID",
                     "INVALID")))

  if(!is.na(object$score))
    cat(sprintf("%-30s %.2f /100\n",
                "Quality score",
                object$score))

  cat("\n")

  # =====================================================================
  # Dataset
  # =====================================================================

  cat("Dataset\n")
  cat("-------\n")

  cat(sprintf("%-30s %d\n",
              "Blocks",
              object$summary$n_blocks))

  cat(sprintf("%-30s %d\n",
              "Samples",
              object$summary$n_samples))

  cat(sprintf("%-30s %d\n",
              "Features",
              object$summary$n_features))

  cat(sprintf("%-30s %.2f %%\n",
              "Missing values",
              object$summary$total_missing_percent))

  cat("\n")

  # =====================================================================
  # Errors
  # =====================================================================

  cat("Errors\n")
  cat("------\n")

  if(length(object$errors)==0){

    cat("None\n")

  }else{

    cat(paste0("- ",object$errors),sep="\n")

  }

  cat("\n")

  # =====================================================================
  # Warnings
  # =====================================================================

  cat("Warnings\n")
  cat("--------\n")

  if(length(object$warnings)==0){

    cat("None\n")

  }else{

    cat(paste0("- ",object$warnings),sep="\n")

  }

  cat("\n")

  # =====================================================================
  # Information
  # =====================================================================

  cat("Information\n")
  cat("-----------\n")

  if(length(object$info)==0){

    cat("None\n")

  }else{

    cat(paste0("- ",object$info),sep="\n")

  }

  cat("\n")

  # =====================================================================
  # Block summary
  # =====================================================================

  cat("Block summary\n")
  cat("-------------\n")

  if(!is.null(object$summary$block_summary)){

    print(object$summary$block_summary)

  }else{

    cat("Not available\n")

  }

  cat("\n")

  # =====================================================================
  # Diagnostics
  # =====================================================================

  cat("Diagnostics\n")
  cat("-----------\n")

  if(length(object$diagnostics)==0){

    cat("None\n")

  }else{

    for(nm in names(object$diagnostics)){

      d <- object$diagnostics[[nm]]

      cat(

        sprintf(

          "%-20s %8d samples | %8d features | %6.2f %% missing\n",

          nm,

          d$samples,

          d$features,

          d$missing_percent

        )

      )

    }

  }

  cat("\n")

  # =====================================================================
  # Recommended preprocessing
  # =====================================================================

  cat("Automatic preprocessing\n")
  cat("-----------------------\n")

  if(length(object$recipes)==0){

    cat("None\n")

  }else{

    for(nm in names(object$recipes)){

      r <- object$recipes[[nm]]

      cat(

        sprintf(

          "%-18s Transform=%-12s Normalize=%-12s Scale=%-12s Impute=%s\n",

          nm,

          ifelse(is.null(r$transformation),"None",r$transformation),

          ifelse(is.null(r$normalization),"None",r$normalization),

          ifelse(is.null(r$scaling),"None",r$scaling),

          ifelse(is.null(r$imputation),"None",r$imputation)

        )

      )

    }

  }

  cat("\n")

  # =====================================================================
  # Transformation benchmark
  # =====================================================================

  cat("Transformation recommendations\n")
  cat("------------------------------\n")

  if(length(object$transformations)==0){

    cat("None\n")

  }else{

    for(nm in names(object$transformations)){

      tr <- object$transformations[[nm]]

      cat(

        sprintf(

          "%-20s %s\n",

          nm,

          tr$recommended

        )

      )

    }

  }

  cat("\n")

  # =====================================================================
  # Available plots
  # =====================================================================

  cat("Available plots\n")
  cat("---------------\n")

  p <- names(object$plots)[
    !vapply(object$plots,is.null,logical(1))
  ]

  if(length(p)==0){

    cat("None\n")

  }else{

    cat(paste0("- ",p),sep="\n")

  }

  cat("\n")

  # =====================================================================
  # Execution
  # =====================================================================

  cat("Execution\n")
  cat("---------\n")

  if(!is.null(object$execution$runtime))
    cat("Runtime :",object$execution$runtime,"\n")

  if(!is.null(object$execution$started))
    cat("Started :",object$execution$started,"\n")

  if(!is.null(object$execution$finished))
    cat("Finished:",object$execution$finished,"\n")

  cat("\n")

  cat("Timestamp\n")
  cat("---------\n")

  print(object$timestamp)

  invisible(object)

}

# =============================================================================
# CMOResult
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' @export

print.CMOResult <- function(x, ...) {

  cat("\n")
  cat("CMOResult\n")
  cat("=========\n\n")

  cat(sprintf("%-30s %s\n",
              "Input data:",
              ifelse(is.null(x$data),"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Integration:",
              ifelse(length(x$integration)==0,"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Latent representation:",
              ifelse(length(x$latent)==0,"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Causal analysis:",
              ifelse(length(x$causal)==0,"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Network:",
              ifelse(length(x$network)==0,"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Biomarkers:",
              ifelse(length(x$biomarkers)==0,"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Prediction:",
              ifelse(length(x$prediction)==0,"No","Yes")))

  cat(sprintf("%-30s %s\n",
              "Enrichment:",
              ifelse(length(x$enrichment)==0,"No","Yes")))

  cat(sprintf("%-30s %d\n",
              "Plots:",
              length(x$plots)))

  cat(sprintf("%-30s %d\n",
              "Tables:",
              length(x$tables)))

  cat(sprintf("%-30s %d\n",
              "Reports:",
              length(x$reports)))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' @export

summary.CMOResult <- function(object, ...) {

  cat("\n")
  cat("================== CMOResult Summary ==================\n\n")

  # =====================================================================
  # Workflow
  # =====================================================================

  cat("Workflow\n")
  cat("--------\n")

  steps <- c(

    Integration = length(object$integration)>0,

    Latent = length(object$latent)>0,

    Causal = length(object$causal)>0,

    Network = length(object$network)>0,

    Biomarkers = length(object$biomarkers)>0,

    Prediction = length(object$prediction)>0,

    Enrichment = length(object$enrichment)>0,

    Validation =
      !is.null(object$validation) &&
      length(object$validation) > 0

  )

  for(i in seq_along(steps)){

    cat(

      sprintf(

        "%-20s %s\n",

        names(steps)[i],

        ifelse(steps[i],"Completed","Not performed")

      )

    )

  }

  cat("\n")

  # =====================================================================
  # Parameters
  # =====================================================================

  cat("Parameters\n")
  cat("----------\n")

  if(length(object$parameters)==0){

    cat("None\n")

  }else{

    print(object$parameters)

  }

  cat("\n")

  # =====================================================================
  # Performance
  # =====================================================================

  cat("Performance\n")
  cat("-----------\n")

  if(length(object$performance)==0){

    cat("Not available\n")

  }else{

    print(object$performance)

  }

  cat("\n")

  # =====================================================================
  # Available outputs
  # =====================================================================

  cat("Outputs\n")
  cat("-------\n")

  cat(sprintf("%-25s %d\n",
              "Plots",
              length(object$plots)))

  cat(sprintf("%-25s %d\n",
              "Tables",
              length(object$tables)))

  cat(sprintf("%-25s %d\n",
              "Reports",
              length(object$reports)))

  cat("\n")

  # =====================================================================
  # History
  # =====================================================================

  cat("History\n")
  cat("-------\n")

  if(length(object$history)==0){

    cat("Empty\n")

  }else{

    cat(paste0("- ",object$history),
        sep="\n")

  }

  invisible(object)

}

# =============================================================================
# Validation report
# =============================================================================

# -----------------------------------------------------------------------------
# Internal report helpers
# -----------------------------------------------------------------------------

.report_or <- function(x, default = NA) {

  if (is.null(x) || length(x) == 0) default else x

}

.report_rule <- function(width = 78, char = "=") {

  cat(strrep(char, width), "\n", sep = "")

}

.report_title <- function(text, width = 78) {

  cat("\n")
  .report_rule(width, "=")
  cat(text, "\n", sep = "")
  .report_rule(width, "=")

}

.report_section <- function(text, width = 78) {

  cat("\n")
  cat(text, "\n", sep = "")
  cat(strrep("-", min(nchar(text), width)), "\n", sep = "")

}

#' Horizontal bar for a 0-100 score
#' @keywords internal
.report_bar <- function(score, width = 40) {

  if (is.null(score) || length(score) == 0 || is.na(score)) {
    return(paste0("[", strrep(" ", width), "]   NA"))
  }

  score <- max(0, min(100, score))
  filled <- round(width * score / 100)

  paste0(
    "[",
    strrep("#", filled),
    strrep(".", width - filled),
    sprintf("] %5.1f / 100", score)
  )

}

#' Qualitative label for a 0-100 score
#' @keywords internal
.report_grade <- function(score) {

  if (is.null(score) || length(score) == 0 || is.na(score)) return("unknown")

  if (score >= 85) return("excellent")
  if (score >= 70) return("good")
  if (score >= 50) return("acceptable")
  if (score >= 30) return("poor")

  "critical"

}

#' Truncate and pad a string to an exact width
#' @keywords internal
.report_pad <- function(x, n) {

  x <- as.character(x)
  x[is.na(x)] <- "-"

  ifelse(
    nchar(x) > n,
    paste0(substr(x, 1, n - 1), "~"),
    formatC(x, width = -n, flag = " ")
  )

}

#' Print a character vector as a bulleted, wrapped list
#' @keywords internal
.report_bullets <- function(x, prefix = "  - ", width = 78, max_items = Inf) {

  if (length(x) == 0) {
    cat("  (none)\n")
    return(invisible(NULL))
  }

  shown <- utils::head(x, max_items)

  for (item in shown) {

    wrapped <- strwrap(
      item,
      width = width,
      prefix = strrep(" ", nchar(prefix)),
      initial = prefix
    )

    cat(paste(wrapped, collapse = "\n"), "\n", sep = "")

  }

  if (length(x) > length(shown)) {
    cat(sprintf("  ... and %d more\n", length(x) - length(shown)))
  }

  invisible(NULL)

}

# -----------------------------------------------------------------------------
# report()
# -----------------------------------------------------------------------------

#' Print a readable validation report
#'
#' Generic for turning an analysis object into a human-readable report.
#'
#' @param object An object to report on.
#' @param ... Passed to methods.
#'
#' @return The object, invisibly.
#'
#' @export

report <- function(object, ...) {

  UseMethod("report")

}


#' Readable report for a CMOValidation object
#'
#' Summarizes everything \code{check_data()} extracted: overall verdict and
#' quality score, dataset composition, per-block diagnostics, detected data
#' issues, sample overlap between blocks, the automatically derived
#' preprocessing plan, the transformation benchmark, quality-control checks,
#' the prioritized action list and the outputs available for inspection.
#'
#' Unlike \code{summary()}, which dumps every stored component, this method
#' is organized for reading: it leads with the verdict, flags only what
#' needs attention, and tells the user where the remaining detail lives.
#'
#' @param object A \code{CMOValidation} object.
#' @param file Destination path for the HTML report. May be a file name or a
#'   directory. When \code{NULL} (the default) the user is prompted for a
#'   location in an interactive session, and the working directory is used
#'   otherwise. Ignored when \code{format = "console"}.
#' @param format Either \code{"html"} (the default) to write a self-contained
#'   interactive document, or \code{"console"} to print the plain-text report.
#' @param open Whether to open the saved report in a browser.
#' @param prompt Whether to ask for a destination when \code{file} is
#'   \code{NULL}. Set to \code{FALSE} for unattended scripts.
#' @param plot_width,plot_height,plot_res Pixel dimensions and resolution used
#'   when rasterizing the stored plots into the document.
#' @param quiet Whether to suppress progress messages.
#' @param sections Character vector selecting which sections to print. Any
#'   of \code{"overview"}, \code{"blocks"}, \code{"issues"}, \code{"overlap"},
#'   \code{"preprocessing"}, \code{"transformations"}, \code{"qc"},
#'   \code{"actions"}, \code{"outputs"}. Defaults to all of them.
#' @param max_items Maximum number of entries listed per itemized block
#'   (messages, recommendations, failed checks).
#' @param width Target line width.
#' @param ... Ignored.
#'
#' @return The validation object, invisibly.
#'
#' @exportS3Method report CMOValidation

report.CMOValidation <- function(object,
                                 file = NULL,
                                 format = c("html", "console"),
                                 sections = c("overview", "blocks", "issues",
                                              "overlap", "preprocessing",
                                              "transformations", "qc",
                                              "actions", "outputs"),
                                 max_items = 12,
                                 width = 78,
                                 open = interactive(),
                                 prompt = TRUE,
                                 plot_width = 900,
                                 plot_height = 560,
                                 plot_res = 110,
                                 quiet = FALSE,
                                 ...) {

  format <- match.arg(format)
  sections <- match.arg(sections, several.ok = TRUE)

  # ===========================================================================
  # HTML output
  # ===========================================================================

  if (identical(format, "html")) {

    path <- .report_destination(file = file, prompt = prompt)

    if (!isTRUE(quiet)) cat("Building HTML report...\n")

    html <- .report_html(
      object,
      plot_width = plot_width,
      plot_height = plot_height,
      plot_res = plot_res
    )

    con <- file(path, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(html), con)

    if (!isTRUE(quiet)) {

      cat(sprintf("Report saved to: %s\n", path))
      cat(sprintf("Size: %.1f KB\n", file.size(path) / 1024))

    }

    if (isTRUE(open)) {
      try(utils::browseURL(path), silent = TRUE)
    }

    attr(object, "report_path") <- path

    return(invisible(object))

  }

  # ===========================================================================
  # Console output
  # ===========================================================================

  blocks <- names(object$diagnostics)

  # ===========================================================================
  # Header and verdict
  # ===========================================================================

  .report_title("CausalMultiOmics - Data Validation Report", width)

  status <- if (isTRUE(object$valid)) "PASSED" else "FAILED"

  cat("\n")
  cat(sprintf("  %-18s %s\n", "Status", status))
  cat(sprintf("  %-18s %s\n", "Quality",
              .report_bar(object$score, width = min(40, width - 34))))
  cat(sprintf("  %-18s %s\n", "Assessment", .report_grade(object$score)))
  cat(sprintf("  %-18s %d error(s), %d warning(s)\n", "Findings",
              length(object$errors), length(object$warnings)))

  if (!isTRUE(object$valid)) {

    cat("\n")
    cat("  Validation failed. The errors below must be resolved before\n")
    cat("  preprocessing or integration can proceed.\n")

    .report_section("Blocking errors", width)
    .report_bullets(object$errors, width = width, max_items = max_items)

  }

  # Structural failure: check_data() returned early, nothing else was computed.

  if (length(blocks) == 0) {

    cat("\n")
    cat("  No per-block diagnostics were produced.\n")
    cat("\n")

    .report_rule(width, "=")
    cat("\n")

    return(invisible(object))

  }

  # ===========================================================================
  # Overview
  # ===========================================================================

  if ("overview" %in% sections) {

    .report_section("1. Dataset overview", width)

    s <- object$summary

    cat(sprintf("  %-22s %s\n", "Blocks", fmt_num(.report_or(s$n_blocks), 0)))
    cat(sprintf("  %-22s %s\n", "Unique samples", fmt_num(.report_or(s$n_samples), 0)))
    cat(sprintf("  %-22s %s\n", "Total features", fmt_num(.report_or(s$n_features), 0)))
    cat(sprintf("  %-22s %s\n", "Observations", fmt_num(.report_or(s$observations), 0)))
    cat(sprintf("  %-22s %s (%s values)\n", "Missing data",
                paste0(fmt_num(.report_or(s$total_missing_percent)), " %"),
                fmt_num(.report_or(s$total_missing), 0)))

    if (!is.null(object$execution$runtime)) {
      cat(sprintf("  %-22s %.2f s\n", "Validation runtime", object$execution$runtime))
    }

  }

  # ===========================================================================
  # Per-block summary
  # ===========================================================================

  if ("blocks" %in% sections) {

    .report_section("2. Blocks at a glance", width)

    cat(sprintf(
      "  %s %s %s %s %s %s\n",
      .report_pad("Block", 16), .report_pad("Type", 14),
      .report_pad("Samples", 8), .report_pad("Features", 9),
      .report_pad("Missing", 8), .report_pad("Quality", 9)
    ))

    cat("  ", strrep("-", 16 + 14 + 8 + 9 + 8 + 9 + 5), "\n", sep = "")

    for (nm in blocks) {

      d <- object$diagnostics[[nm]]
      q <- object$recipes[[nm]]$estimated_quality

      cat(sprintf(
        "  %s %s %s %s %s %s\n",
        .report_pad(nm, 16),
        .report_pad(.report_or(d$data_type, "-"), 14),
        .report_pad(fmt_num(d$samples, 0), 8),
        .report_pad(fmt_num(d$features, 0), 9),
        .report_pad(paste0(fmt_num(d$missing_percent, 1), "%"), 8),
        .report_pad(
          if (is.null(q) || is.na(q)) "-" else sprintf("%.0f (%s)", q, substr(.report_grade(q), 1, 4)),
          9
        )
      ))

    }

  }

  # ===========================================================================
  # Issues
  # ===========================================================================

  if ("issues" %in% sections) {

    .report_section("3. Data issues detected", width)

    any_issue <- FALSE

    for (nm in blocks) {

      d <- object$diagnostics[[nm]]

      flags <- character(0)

      if (length(d$constant_features) > 0)
        flags <- c(flags, sprintf("%d constant feature(s)", length(d$constant_features)))

      if (length(d$near_constant_features) > 0)
        flags <- c(flags, sprintf("%d near-constant feature(s)", length(d$near_constant_features)))

      if (length(d$duplicated_features) > 0)
        flags <- c(flags, sprintf("%d duplicated feature(s)", length(d$duplicated_features)))

      if (length(d$duplicated_samples) > 0)
        flags <- c(flags, sprintf("%d duplicated sample(s)", length(d$duplicated_samples)))

      if (length(d$empty_samples) > 0)
        flags <- c(flags, sprintf("%d empty sample(s)", length(d$empty_samples)))

      if (length(d$empty_features) > 0)
        flags <- c(flags, sprintf("%d empty feature(s)", length(d$empty_features)))

      if (!is.na(d$missing_percent) && d$missing_percent > 0)
        flags <- c(flags, sprintf("%.1f%% missing", d$missing_percent))

      if (length(d$outliers) > 0)
        flags <- c(flags, sprintf("%d outlying value(s)", length(d$outliers)))

      if (identical(d$normality, "non-normal"))
        flags <- c(flags, "non-normal distribution")

      if (length(flags) > 0) {

        any_issue <- TRUE
        cat(sprintf("\n  %s\n", nm))
        .report_bullets(flags, prefix = "    - ", width = width, max_items = max_items)

      }

    }

    if (!any_issue) cat("  No structural or distributional issues detected.\n")

    if (length(object$warnings) > 0) {

      cat("\n  Warnings raised during validation:\n")
      .report_bullets(object$warnings, prefix = "    - ", width = width, max_items = max_items)

    }

    cross <- character(0)

    if (length(.report_or(object$details$block_only, character(0))) > 0)
      cross <- c(cross, sprintf(
        "%d sample(s) in data blocks but absent from metadata",
        length(object$details$block_only)
      ))

    if (length(.report_or(object$details$metadata_only, character(0))) > 0)
      cross <- c(cross, sprintf(
        "%d sample(s) in metadata but absent from data blocks",
        length(object$details$metadata_only)
      ))

    if (length(cross) > 0) {

      cat("\n  Metadata consistency:\n")
      .report_bullets(cross, prefix = "    - ", width = width, max_items = max_items)

    }

  }

  # ===========================================================================
  # Sample overlap
  # ===========================================================================

  if ("overlap" %in% sections && !is.null(object$summary$overlap)) {

    ov <- object$summary$overlap

    .report_section("4. Sample overlap between blocks", width)

    if (nrow(ov) < 2) {

      cat("  Only one block; overlap is not applicable.\n")

    } else {

      print(ov)

      off <- ov[row(ov) != col(ov)]

      cat("\n")
      cat(sprintf("  %-26s %d\n", "Smallest pairwise overlap", min(off)))
      cat(sprintf("  %-26s %d\n", "Largest pairwise overlap", max(off)))

      if (min(off) == 0) {
        cat("\n  At least one pair of blocks shares no samples. Integration\n")
        cat("  across those blocks will not be possible without imputation\n")
        cat("  or a different sample-matching strategy.\n")
      }

    }

  }

  # ===========================================================================
  # Preprocessing plan
  # ===========================================================================

  if ("preprocessing" %in% sections) {

    .report_section("5. Recommended preprocessing plan", width)

    for (nm in blocks) {

      r <- object$recipes[[nm]]

      cat(sprintf("\n  %s\n", nm))

      cat(sprintf("    %-16s %s\n", "Impute", fmt_char(.report_or(r$imputation, "none"))))
      cat(sprintf("    %-16s %s\n", "Transform", fmt_char(.report_or(r$transformation, "none"))))
      cat(sprintf("    %-16s %s\n", "Normalize", fmt_char(.report_or(r$normalization, "none"))))
      cat(sprintf("    %-16s %s\n", "Scale", fmt_char(.report_or(r$scaling, "none"))))

      if (!is.null(r$batch))
        cat(sprintf("    %-16s %s (variable: %s)\n", "Batch", r$batch,
                    fmt_char(.report_or(r$batch_variable, "-"))))

      if (!is.null(r$feature_selection))
        cat(sprintf("    %-16s %s\n", "Feature select", r$feature_selection))

      if (!is.null(r$dimensionality_reduction))
        cat(sprintf("    %-16s %s (planned for the latent-representation stage)\n",
                    "Reduce", r$dimensionality_reduction))

      filters <- character(0)

      if (isTRUE(r$remove_constant_features)) filters <- c(filters, "constant features")
      if (isTRUE(r$remove_near_constant_features)) filters <- c(filters, "near-constant features")
      if (isTRUE(r$remove_duplicate_features)) filters <- c(filters, "duplicated features")
      if (isTRUE(r$remove_duplicate_samples)) filters <- c(filters, "duplicated samples")
      if (isTRUE(r$remove_empty_samples)) filters <- c(filters, "empty samples")
      if (isTRUE(r$remove_empty_features)) filters <- c(filters, "empty features")

      if (length(filters) > 0)
        cat(sprintf("    %-16s %s\n", "Remove", paste(filters, collapse = ", ")))

      order_note <- grep("stage order", r$comments, value = TRUE)

      if (length(order_note) > 0)
        cat(sprintf("    %-16s %s\n", "Order",
                    sub("^Intended stage order:\\s*", "", order_note[1])))

    }

  }

  # ===========================================================================
  # Transformation benchmark
  # ===========================================================================

  if ("transformations" %in% sections) {

    .report_section("6. Transformation benchmark", width)

    for (nm in blocks) {

      tr <- object$transformations[[nm]]

      cat(sprintf("\n  %s (%s data, %s)\n", nm,
                  fmt_char(.report_or(tr$detected_type, "-")),
                  fmt_char(.report_or(tr$objective, "-"))))

      cat(sprintf("    Selected: %s (score %s of %d candidate(s) tested)\n",
                  fmt_char(.report_or(tr$recommended, "-")),
                  fmt_num(.report_or(tr$score)),
                  length(tr$candidates)))

      if (is.data.frame(tr$ranking) && nrow(tr$ranking) > 1) {

        top <- utils::head(tr$ranking, 3)

        cat("    Ranking: ",
            paste(sprintf("%s (%.1f)", top$transformation, top$score), collapse = "  >  "),
            "\n", sep = "")

      }

      if (length(tr$justification) > 0)
        .report_bullets(tr$justification, prefix = "    - ", width = width, max_items = 3)

    }

  }

  # ===========================================================================
  # Quality control
  # ===========================================================================

  if ("qc" %in% sections) {

    .report_section("7. Quality control checks", width)

    qc <- object$qc

    cat(sprintf("  Overall: %s (%d passed, %d failed, %d skipped)\n",
                if (isTRUE(qc$passed)) "PASSED" else "FAILED",
                length(qc$passed_checks),
                length(qc$failed_checks),
                length(qc$skipped_checks)))

    if (length(qc$passed_checks) > 0) {
      cat("\n  Passed:\n")
      .report_bullets(qc$passed_checks, prefix = "    + ", width = width, max_items = max_items)
    }

    if (length(qc$failed_checks) > 0) {
      cat("\n  Failed:\n")
      .report_bullets(qc$failed_checks, prefix = "    ! ", width = width, max_items = max_items)
    }

    if (length(qc$skipped_checks) > 0) {
      cat("\n  Not applicable:\n")
      .report_bullets(qc$skipped_checks, prefix = "    . ", width = width, max_items = max_items)
    }

  }

  # ===========================================================================
  # Actions
  # ===========================================================================

  if ("actions" %in% sections) {

    .report_section("8. Suggested actions", width)

    for (nm in blocks) {

      msgs <- object$recommendations$blocks[[nm]]

      if (length(msgs) == 0) next

      cat(sprintf("\n  %s\n", nm))
      .report_bullets(msgs, prefix = "    - ", width = width, max_items = max_items)

    }

  }

  # ===========================================================================
  # Outputs
  # ===========================================================================

  if ("outputs" %in% sections) {

    .report_section("9. Available outputs", width)

    available <- names(object$plots)[
      !vapply(object$plots, function(p) is.null(p) || length(p) == 0, logical(1))
    ]

    cat(sprintf("  %d plot slot(s) populated:\n", length(available)))

    if (length(available) > 0) {

      cat(paste(strwrap(paste(available, collapse = ", "),
                        width = width, indent = 4, exdent = 4),
                collapse = "\n"), "\n", sep = "")

      # Some slots hold one plot for the whole dataset, others hold one plot
      # per block. Show the accessor that actually matches.

      example <- available[1]
      slot <- object$plots[[example]]

      cat("\n  Display one with, for example:\n")

      if (is.list(slot) && !inherits(slot, "recordedplot")) {

        cat(sprintf("    grDevices::replayPlot(x$plots$%s$%s)\n",
                    example, names(slot)[1]))

      } else {

        cat(sprintf("    grDevices::replayPlot(x$plots$%s)\n", example))

      }

    }

    tabs <- names(object$tables)[
      vapply(object$tables, function(t) is.data.frame(t) && nrow(t) > 0, logical(1))
    ]

    cat(sprintf("\n  %d table(s) populated: %s\n", length(tabs),
                if (length(tabs) > 0) paste(tabs, collapse = ", ") else "none"))

    cat("\n  Full detail is stored in: diagnostics, recipes, transformations,\n")
    cat("  details, summary and tables.\n")

  }

  cat("\n")
  .report_rule(width, "=")
  cat("\n")

  invisible(object)

}

# =============================================================================
# End of methods.R
# =============================================================================

# =============================================================================
# HTML validation report
# =============================================================================
# The generated document is fully self-contained: styles, scripts and images
# are inlined, so it opens correctly with no network access and no external
# CDN dependency.
# =============================================================================

# -----------------------------------------------------------------------------
# Encoding helpers
# -----------------------------------------------------------------------------

#' Base64-encode a raw vector using base R only
#' @keywords internal
.html_base64 <- function(bytes) {

  chars <- c(LETTERS, letters, as.character(0:9), "+", "/")

  n <- length(bytes)

  if (n == 0) return("")

  pad <- (3L - n %% 3L) %% 3L

  bytes <- c(bytes, as.raw(rep(0L, pad)))

  m <- matrix(as.integer(bytes), nrow = 3L)

  i1 <- m[1, ] %/% 4L
  i2 <- (m[1, ] %% 4L) * 16L + m[2, ] %/% 16L
  i3 <- (m[2, ] %% 16L) * 4L + m[3, ] %/% 64L
  i4 <- m[3, ] %% 64L

  s <- paste0(chars[i1 + 1L], chars[i2 + 1L], chars[i3 + 1L], chars[i4 + 1L],
              collapse = "")

  if (pad > 0L) s <- paste0(substr(s, 1L, nchar(s) - pad), strrep("=", pad))

  s

}

#' Escape text for safe insertion into HTML
#' @keywords internal
.html_escape <- function(x) {

  x <- as.character(x)
  x[is.na(x)] <- ""

  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)

  x

}

#' Make a string safe as an HTML id attribute
#' @keywords internal
.html_id <- function(x) {

  gsub("[^A-Za-z0-9_-]", "_", as.character(x))

}

# -----------------------------------------------------------------------------
# Plot rasterization
# -----------------------------------------------------------------------------

#' Replay a recorded plot into an inline PNG data URI
#' @keywords internal
.html_plot_uri <- function(recorded, width = 900, height = 560, res = 110) {

  if (is.null(recorded) || !inherits(recorded, "recordedplot")) return(NULL)

  if (length(recorded[[1]]) == 0) return(NULL)

  file <- tempfile(fileext = ".png")

  on.exit(unlink(file), add = TRUE)

  device_open <- FALSE

  ok <- tryCatch({

    grDevices::png(filename = file, width = width, height = height,
                   res = res, type = "cairo")

    device_open <- TRUE

    grDevices::replayPlot(recorded)

    grDevices::dev.off()

    device_open <- FALSE

    TRUE

  }, error = function(e) FALSE)

  # Only close the device if replaying failed before it was shut down.

  if (device_open && grDevices::dev.cur() > 1) {
    try(grDevices::dev.off(), silent = TRUE)
  }

  if (!ok || !file.exists(file) || file.size(file) == 0) return(NULL)

  bytes <- readBin(file, "raw", n = file.size(file))

  paste0("data:image/png;base64,", .html_base64(bytes))

}

#' Flatten a plot slot into a named list of recorded plots
#' @keywords internal
.html_plot_items <- function(slot) {

  if (is.null(slot)) return(list())

  if (inherits(slot, "recordedplot")) return(list(`All blocks` = slot))

  if (is.list(slot)) {

    keep <- vapply(slot, function(p) inherits(p, "recordedplot"), logical(1))

    return(slot[keep])

  }

  list()

}

# -----------------------------------------------------------------------------
# HTML fragments
# -----------------------------------------------------------------------------

#' Render a data.frame as an interactive HTML table
#' @keywords internal
.html_table <- function(df, caption = NULL) {

  if (!is.data.frame(df) || nrow(df) == 0) {
    return("<p class='muted'>No data available.</p>")
  }

  df[] <- lapply(df, function(col) {

    if (is.numeric(col)) {
      ifelse(is.na(col), "", format(round(col, 4), trim = TRUE))
    } else {
      as.character(col)
    }

  })

  head_cells <- paste0(
    "<th onclick='cmoSort(this)'>", .html_escape(names(df)), "</th>",
    collapse = ""
  )

  body_rows <- vapply(

    seq_len(nrow(df)),

    function(i) {
      paste0("<tr>",
             paste0("<td>", .html_escape(unlist(df[i, ], use.names = FALSE)), "</td>",
                    collapse = ""),
             "</tr>")
    },

    character(1)

  )

  paste0(
    if (!is.null(caption)) paste0("<p class='cap'>", .html_escape(caption), "</p>") else "",
    "<div class='tablewrap'><table class='data'><thead><tr>",
    head_cells,
    "</tr></thead><tbody>",
    paste(body_rows, collapse = ""),
    "</tbody></table></div>"
  )

}

#' Render a character vector as an HTML list
#' @keywords internal
.html_list <- function(x, class = "") {

  if (length(x) == 0) return("<p class='muted'>None.</p>")

  paste0(
    "<ul class='", class, "'>",
    paste0("<li>", .html_escape(x), "</li>", collapse = ""),
    "</ul>"
  )

}

#' Render a labelled statistic card
#' @keywords internal
.html_card <- function(label, value, sub = NULL) {

  paste0(
    "<div class='card'><div class='card-label'>", .html_escape(label), "</div>",
    "<div class='card-value'>", .html_escape(value), "</div>",
    if (!is.null(sub)) paste0("<div class='card-sub'>", .html_escape(sub), "</div>") else "",
    "</div>"
  )

}

# -----------------------------------------------------------------------------
# Section builders
# -----------------------------------------------------------------------------

#' @keywords internal
.html_section_overview <- function(object) {

  s <- object$summary

  score <- if (is.null(object$score) || is.na(object$score)) NA_real_ else object$score
  pct <- if (is.na(score)) 0 else max(0, min(100, score))

  gauge_class <- if (is.na(score)) "na" else
    if (score >= 85) "good" else if (score >= 70) "ok" else
      if (score >= 50) "warn" else "bad"

  cards <- paste0(
    .html_card("Blocks", .report_or(s$n_blocks, "-")),
    .html_card("Unique samples", .report_or(s$n_samples, "-")),
    .html_card("Total features", .report_or(s$n_features, "-")),
    .html_card("Observations", .report_or(s$observations, "-")),
    .html_card("Missing data",
               paste0(fmt_num(.report_or(s$total_missing_percent)), " %"),
               paste0(.report_or(s$total_missing, 0), " values")),
    .html_card("Errors / warnings",
               paste0(length(object$errors), " / ", length(object$warnings)))
  )

  paste0(
    "<section id='overview' class='active'><h2>Overview</h2>",

    "<div class='verdict ", if (isTRUE(object$valid)) "pass" else "fail", "'>",
    "<span class='badge'>", if (isTRUE(object$valid)) "VALIDATION PASSED" else "VALIDATION FAILED",
    "</span>",
    "<div class='gauge'><div class='gauge-fill ", gauge_class,
    "' style='width:", pct, "%'></div></div>",
    "<div class='gauge-label'>Quality score: <strong>",
    if (is.na(score)) "NA" else sprintf("%.1f", score),
    " / 100</strong> (", .report_grade(score), ")</div>",
    "</div>",

    "<div class='cards'>", cards, "</div>",

    if (length(object$errors) > 0)
      paste0("<h3>Blocking errors</h3>", .html_list(object$errors, "err")) else "",

    if (length(object$warnings) > 0)
      paste0("<h3>Warnings</h3>", .html_list(object$warnings, "warn")) else "",

    "</section>"
  )

}

#' @keywords internal
.html_section_blocks <- function(object) {

  blocks <- names(object$diagnostics)

  rows <- do.call(rbind, lapply(blocks, function(nm) {

    d <- object$diagnostics[[nm]]
    q <- object$recipes[[nm]]$estimated_quality

    data.frame(
      Block = nm,
      Type = .report_or(d$data_type, "-"),
      Samples = d$samples,
      Features = d$features,
      `Missing %` = round(.report_or(d$missing_percent, NA), 2),
      Constant = length(d$constant_features),
      `Near constant` = length(d$near_constant_features),
      Duplicated = length(d$duplicated_features),
      Outliers = length(d$outliers),
      Normality = .report_or(d$normality, "-"),
      Quality = if (is.null(q) || is.na(q)) NA else round(q, 1),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

  }))

  detail <- paste0(vapply(blocks, function(nm) {

    d <- object$diagnostics[[nm]]

    stats <- data.frame(
      Statistic = c("Mean", "Median", "SD", "MAD", "IQR", "CV",
                    "Minimum", "Maximum", "Range",
                    "Skewness", "Kurtosis", "Shapiro p",
                    "Zeros %", "Negatives %", "Infinite %",
                    "Sparsity", "Density"),
      Value = c(d$mean, d$median, d$sd, d$mad, d$iqr, d$cv,
                d$minimum, d$maximum, d$range,
                d$skewness, d$kurtosis, d$shapiro,
                d$zero_percent, d$negative_percent, d$infinite_percent,
                d$sparsity, d$density),
      stringsAsFactors = FALSE
    )

    stats$Value <- vapply(stats$Value, function(v) {
      if (is.null(v) || length(v) == 0 || is.na(v)) "NA" else sprintf("%.4f", v)
    }, character(1))

    paste0(
      "<details class='blockdetail'><summary>", .html_escape(nm),
      " &mdash; full diagnostics</summary>",
      .html_table(stats),
      "</details>"
    )

  }, character(1)), collapse = "")

  paste0(
    "<section id='blocks'><h2>Blocks</h2>",
    .html_table(rows, "Click a column header to sort."),
    "<h3>Per-block detail</h3>",
    detail,
    "</section>"
  )

}

#' @keywords internal
.html_section_preprocessing <- function(object) {

  blocks <- names(object$recipes)

  cards <- paste0(vapply(blocks, function(nm) {

    r <- object$recipes[[nm]]

    order_note <- grep("stage order", r$comments, value = TRUE)
    order_txt <- if (length(order_note) > 0)
      sub("^Intended stage order:\\s*", "", order_note[1]) else "-"

    steps <- c(
      Impute = .report_or(r$imputation, "none"),
      Transform = .report_or(r$transformation, "none"),
      Normalize = .report_or(r$normalization, "none"),
      Scale = .report_or(r$scaling, "none"),
      Batch = .report_or(r$batch, "none"),
      `Feature selection` = .report_or(r$feature_selection, "none"),
      `Dimensionality reduction` = .report_or(r$dimensionality_reduction, "none")
    )

    chips <- paste0(
      "<div class='chiprow'>",
      paste0("<span class='chip", ifelse(steps == "none", " off", ""), "'>",
             .html_escape(names(steps)), ": <b>", .html_escape(steps), "</b></span>",
             collapse = ""),
      "</div>"
    )

    filters <- character(0)
    if (isTRUE(r$remove_constant_features)) filters <- c(filters, "constant features")
    if (isTRUE(r$remove_near_constant_features)) filters <- c(filters, "near-constant features")
    if (isTRUE(r$remove_duplicate_features)) filters <- c(filters, "duplicated features")
    if (isTRUE(r$remove_duplicate_samples)) filters <- c(filters, "duplicated samples")
    if (isTRUE(r$remove_empty_samples)) filters <- c(filters, "empty samples")
    if (isTRUE(r$remove_empty_features)) filters <- c(filters, "empty features")

    paste0(
      "<div class='panel'><h3>", .html_escape(nm), "</h3>",
      chips,
      "<p class='muted'><b>Order:</b> ", .html_escape(order_txt), "</p>",
      if (length(filters) > 0)
        paste0("<p class='muted'><b>Remove:</b> ",
               .html_escape(paste(filters, collapse = ", ")), "</p>") else "",
      "<p class='muted'><b>Estimated quality after preprocessing:</b> ",
      .html_escape(if (is.null(r$estimated_quality) || is.na(r$estimated_quality))
        "NA" else sprintf("%.1f", r$estimated_quality)),
      "</p></div>"
    )

  }, character(1)), collapse = "")

  paste0(
    "<section id='preprocessing'><h2>Preprocessing plan</h2>",
    cards,
    "<h3>Summary table</h3>",
    .html_table(object$tables$preprocessing),
    "</section>"
  )

}

#' @keywords internal
.html_section_transformations <- function(object) {

  blocks <- names(object$transformations)

  panels <- paste0(vapply(blocks, function(nm) {

    tr <- object$transformations[[nm]]

    paste0(
      "<div class='panel'><h3>", .html_escape(nm), "</h3>",
      "<p><b>Selected:</b> <span class='chip'>",
      .html_escape(.report_or(tr$recommended, "-")), "</span> ",
      "<span class='muted'>score ", .html_escape(fmt_num(.report_or(tr$score))),
      " &middot; ", length(tr$candidates), " candidate(s) tested &middot; ",
      .html_escape(.report_or(tr$objective, "-")), "</span></p>",
      .html_list(tr$justification),
      "<details><summary>Full ranking</summary>",
      .html_table(tr$ranking),
      "</details></div>"
    )

  }, character(1)), collapse = "")

  paste0(
    "<section id='transformations'><h2>Transformation benchmark</h2>",
    panels,
    "</section>"
  )

}

#' @keywords internal
.html_section_qc <- function(object) {

  qc <- object$qc

  mk <- function(items, cls, label) {

    if (length(items) == 0) return("")

    paste0(
      "<h3>", label, "</h3><div class='chiprow'>",
      paste0("<span class='chip ", cls, "'>", .html_escape(items), "</span>",
             collapse = ""),
      "</div>"
    )

  }

  paste0(
    "<section id='qc'><h2>Quality control</h2>",
    "<p class='verdict-inline ", if (isTRUE(qc$passed)) "pass" else "fail", "'>",
    if (isTRUE(qc$passed)) "All applicable checks passed" else "One or more checks failed",
    "</p>",
    mk(qc$passed_checks, "pass", "Passed"),
    mk(qc$failed_checks, "fail", "Failed"),
    mk(qc$skipped_checks, "off", "Not applicable"),
    "</section>"
  )

}

#' @keywords internal
.html_section_actions <- function(object) {

  blocks <- names(object$recommendations$blocks)

  panels <- paste0(vapply(blocks, function(nm) {

    paste0("<div class='panel'><h3>", .html_escape(nm), "</h3>",
           .html_list(object$recommendations$blocks[[nm]]), "</div>")

  }, character(1)), collapse = "")

  paste0(
    "<section id='actions'><h2>Suggested actions</h2>",
    panels,
    "</section>"
  )

}

#' @keywords internal
.html_section_overlap <- function(object) {

  ov <- object$summary$overlap

  if (is.null(ov) || nrow(ov) == 0) {
    return("<section id='overlap'><h2>Sample overlap</h2><p class='muted'>Not available.</p></section>")
  }

  df <- as.data.frame(ov, stringsAsFactors = FALSE)
  df <- cbind(Block = rownames(ov), df, stringsAsFactors = FALSE)

  note <- ""

  if (nrow(ov) > 1) {

    off <- ov[row(ov) != col(ov)]

    note <- paste0(
      "<p class='muted'>Smallest pairwise overlap: <b>", min(off),
      "</b> &middot; largest: <b>", max(off), "</b></p>",
      if (min(off) == 0)
        "<p class='warnbox'>At least one pair of blocks shares no samples. Integration across those blocks will require a different sample-matching strategy.</p>"
      else ""
    )

  }

  paste0(
    "<section id='overlap'><h2>Sample overlap</h2>",
    .html_table(df),
    note,
    "</section>"
  )

}

#' @keywords internal
.html_section_plots <- function(object, plot_width, plot_height, plot_res) {

  slots <- names(object$plots)

  blocks <- character(0)
  panels <- character(0)

  for (slot in slots) {

    items <- .html_plot_items(object$plots[[slot]])

    if (length(items) == 0) next

    figs <- character(0)

    for (i in seq_along(items)) {

      uri <- .html_plot_uri(items[[i]], plot_width, plot_height, plot_res)

      if (is.null(uri)) next

      label <- if (is.null(names(items)) || is.na(names(items)[i]) ||
                   names(items)[i] == "") paste("Plot", i) else names(items)[i]

      figs <- c(figs, paste0(
        "<figure><img loading='lazy' src='", uri, "' alt='",
        .html_escape(paste(slot, label)), "'>",
        "<figcaption>", .html_escape(label), "</figcaption></figure>"
      ))

    }

    if (length(figs) == 0) next

    blocks <- c(blocks, slot)

    panels <- c(panels, paste0(
      "<div class='plotgroup' data-slot='", .html_id(slot), "'>",
      "<h3>", .html_escape(gsub("_", " ", slot)), "</h3>",
      "<div class='gallery'>", paste(figs, collapse = ""), "</div></div>"
    ))

  }

  if (length(panels) == 0) {
    return("<section id='plots'><h2>Plots</h2><p class='muted'>No plots were generated.</p></section>")
  }

  filter <- paste0(
    "<div class='filterbar'><input type='text' id='plotFilter' ",
    "placeholder='Filter plots by name...' oninput='cmoFilterPlots()'>",
    "<span class='muted' id='plotCount'></span></div>"
  )

  paste0(
    "<section id='plots'><h2>Plots</h2>",
    filter,
    paste(panels, collapse = ""),
    "</section>"
  )

}

#' @keywords internal
.html_section_tables <- function(object) {

  tabs <- names(object$tables)

  panels <- paste0(vapply(tabs, function(nm) {

    paste0("<details class='blockdetail'><summary>",
           .html_escape(gsub("_", " ", nm)), "</summary>",
           .html_table(object$tables[[nm]]), "</details>")

  }, character(1)), collapse = "")

  paste0(
    "<section id='tables'><h2>Tables</h2>", panels,
    "<h3>Quality control checks</h3>",
    .html_table(.build_qc_table(object$qc)),
    "</section>"
  )

}

# -----------------------------------------------------------------------------
# Style and behaviour
# -----------------------------------------------------------------------------

#' @keywords internal
.html_style <- function() {
"<style>
:root{--bg:#f6f7f9;--fg:#1c1f23;--muted:#6b7280;--line:#e2e5ea;--panel:#fff;
--accent:#2f6fed;--good:#1f9d55;--ok:#3b82f6;--warn:#d97706;--bad:#dc2626;}
*{box-sizing:border-box}
body{margin:0;font:15px/1.55 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
background:var(--bg);color:var(--fg)}
header{background:var(--panel);border-bottom:1px solid var(--line);padding:22px 28px;position:sticky;top:0;z-index:20}
header h1{margin:0;font-size:20px}
header .sub{color:var(--muted);font-size:13px;margin-top:4px}
nav{display:flex;flex-wrap:wrap;gap:6px;margin-top:14px}
nav button{background:transparent;border:1px solid var(--line);border-radius:999px;padding:6px 14px;
cursor:pointer;font-size:13px;color:var(--muted)}
nav button:hover{border-color:var(--accent);color:var(--accent)}
nav button.active{background:var(--accent);border-color:var(--accent);color:#fff}
main{max-width:1180px;margin:0 auto;padding:26px 28px 80px}
section{display:none}
section.active{display:block;animation:fade .18s ease}
@keyframes fade{from{opacity:0;transform:translateY(4px)}to{opacity:1;transform:none}}
h2{font-size:22px;margin:6px 0 18px}
h3{font-size:16px;margin:22px 0 10px}
.muted{color:var(--muted);font-size:13px}
.cap{color:var(--muted);font-size:12px;margin:0 0 6px}
.verdict{background:var(--panel);border:1px solid var(--line);border-left-width:5px;border-radius:10px;padding:18px 20px;margin-bottom:18px}
.verdict.pass{border-left-color:var(--good)}
.verdict.fail{border-left-color:var(--bad)}
.badge{display:inline-block;font-weight:700;letter-spacing:.04em;font-size:13px;margin-bottom:12px}
.verdict.pass .badge{color:var(--good)}
.verdict.fail .badge{color:var(--bad)}
.gauge{height:12px;background:var(--line);border-radius:999px;overflow:hidden}
.gauge-fill{height:100%;border-radius:999px;transition:width .6s ease}
.gauge-fill.good{background:var(--good)}.gauge-fill.ok{background:var(--ok)}
.gauge-fill.warn{background:var(--warn)}.gauge-fill.bad{background:var(--bad)}
.gauge-fill.na{background:var(--muted)}
.gauge-label{margin-top:8px;font-size:13px;color:var(--muted)}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:12px;margin:16px 0}
.card{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:14px 16px}
.card-label{font-size:12px;color:var(--muted);text-transform:uppercase;letter-spacing:.04em}
.card-value{font-size:22px;font-weight:600;margin-top:4px}
.card-sub{font-size:12px;color:var(--muted)}
.panel{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:16px 18px;margin-bottom:14px}
.panel h3{margin-top:0}
.tablewrap{overflow-x:auto;background:var(--panel);border:1px solid var(--line);border-radius:10px}
table.data{border-collapse:collapse;width:100%;font-size:13px}
table.data th,table.data td{padding:9px 12px;text-align:left;border-bottom:1px solid var(--line);white-space:nowrap}
table.data th{background:#fafbfc;cursor:pointer;user-select:none;font-weight:600;position:sticky;top:0}
table.data th:hover{color:var(--accent)}
table.data tbody tr:hover{background:#f3f6fd}
table.data tbody tr:last-child td{border-bottom:none}
ul{margin:6px 0;padding-left:20px}
ul.err li{color:var(--bad)}
ul.warn li{color:var(--warn)}
.chiprow{display:flex;flex-wrap:wrap;gap:6px;margin:8px 0}
.chip{background:#eef2fb;color:#25417f;border-radius:6px;padding:4px 10px;font-size:12px}
.chip.off{background:#f1f2f4;color:var(--muted)}
.chip.pass{background:#e6f5ec;color:#14663a}
.chip.fail{background:#fdeaea;color:#a01b1b}
.verdict-inline{font-weight:600}
.verdict-inline.pass{color:var(--good)}
.verdict-inline.fail{color:var(--bad)}
.warnbox{background:#fff7e8;border:1px solid #f0d9a8;border-radius:8px;padding:10px 14px;font-size:13px}
details.blockdetail{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:10px 16px;margin-bottom:10px}
details summary{cursor:pointer;font-weight:600;font-size:14px}
details[open] summary{margin-bottom:10px}
.filterbar{display:flex;align-items:center;gap:12px;margin-bottom:16px}
.filterbar input{flex:1;max-width:340px;padding:9px 12px;border:1px solid var(--line);border-radius:8px;font-size:14px}
.filterbar input:focus{outline:none;border-color:var(--accent)}
.gallery{display:grid;grid-template-columns:repeat(auto-fit,minmax(330px,1fr));gap:14px}
figure{margin:0;background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:10px;cursor:zoom-in}
figure img{width:100%;height:auto;display:block;border-radius:6px}
figcaption{font-size:12px;color:var(--muted);margin-top:8px;text-align:center}
.plotgroup{margin-bottom:26px}
#lightbox{display:none;position:fixed;inset:0;background:rgba(0,0,0,.85);z-index:100;
align-items:center;justify-content:center;cursor:zoom-out;padding:30px}
#lightbox img{max-width:100%;max-height:100%;border-radius:6px}
footer{color:var(--muted);font-size:12px;border-top:1px solid var(--line);padding:16px 28px;text-align:center}
</style>"
}

#' @keywords internal
.html_script <- function() {
"<script>
function cmoShow(id,btn){
  document.querySelectorAll('main section').forEach(function(s){s.classList.remove('active')});
  var t=document.getElementById(id); if(t)t.classList.add('active');
  document.querySelectorAll('nav button').forEach(function(b){b.classList.remove('active')});
  if(btn)btn.classList.add('active');
  window.scrollTo({top:0,behavior:'smooth'});
}
function cmoSort(th){
  var table=th.closest('table'), idx=Array.prototype.indexOf.call(th.parentNode.children,th);
  var body=table.tBodies[0], rows=Array.prototype.slice.call(body.rows);
  var asc=!(th.dataset.asc==='true'); th.dataset.asc=asc;
  rows.sort(function(a,b){
    var x=a.cells[idx].textContent.trim(), y=b.cells[idx].textContent.trim();
    var nx=parseFloat(x), ny=parseFloat(y);
    var both=!isNaN(nx)&&!isNaN(ny);
    if(both)return asc?nx-ny:ny-nx;
    return asc?x.localeCompare(y):y.localeCompare(x);
  });
  rows.forEach(function(r){body.appendChild(r)});
}
function cmoFilterPlots(){
  var q=document.getElementById('plotFilter').value.toLowerCase();
  var shown=0,total=0;
  document.querySelectorAll('.plotgroup').forEach(function(g){
    var gname=(g.dataset.slot||'').toLowerCase();
    var any=false;
    g.querySelectorAll('figure').forEach(function(f){
      total++;
      var cap=f.querySelector('figcaption').textContent.toLowerCase();
      var hit=(gname+' '+cap).indexOf(q)>-1;
      f.style.display=hit?'':'none';
      if(hit){any=true;shown++;}
    });
    g.style.display=any?'':'none';
  });
  document.getElementById('plotCount').textContent=shown+' of '+total+' plots';
}
document.addEventListener('DOMContentLoaded',function(){
  if(document.getElementById('plotFilter'))cmoFilterPlots();
  var lb=document.getElementById('lightbox');
  document.querySelectorAll('figure img').forEach(function(img){
    img.addEventListener('click',function(){
      lb.querySelector('img').src=img.src; lb.style.display='flex';
    });
  });
  if(lb)lb.addEventListener('click',function(){lb.style.display='none'});
});
</script>"
}

# -----------------------------------------------------------------------------
# Document assembly
# -----------------------------------------------------------------------------

#' Build the complete self-contained HTML document
#' @keywords internal
.report_html <- function(object,
                         plot_width = 900,
                         plot_height = 560,
                         plot_res = 110,
                         title = "CausalMultiOmics validation report") {

  has_blocks <- length(object$diagnostics) > 0

  parts <- list(overview = .html_section_overview(object))

  if (has_blocks) {

    parts$blocks <- .html_section_blocks(object)
    parts$overlap <- .html_section_overlap(object)
    parts$preprocessing <- .html_section_preprocessing(object)
    parts$transformations <- .html_section_transformations(object)
    parts$qc <- .html_section_qc(object)
    parts$actions <- .html_section_actions(object)
    parts$plots <- .html_section_plots(object, plot_width, plot_height, plot_res)
    parts$tables <- .html_section_tables(object)

  }

  labels <- c(overview = "Overview", blocks = "Blocks", overlap = "Overlap",
              preprocessing = "Preprocessing", transformations = "Transformations",
              qc = "Quality control", actions = "Actions",
              plots = "Plots", tables = "Tables")

  nav <- paste0(
    vapply(seq_along(parts), function(i) {
      id <- names(parts)[i]
      paste0("<button class='", if (i == 1L) "active" else "",
             "' onclick=\"cmoShow('", id, "',this)\">",
             .html_escape(labels[[id]]), "</button>")
    }, character(1)),
    collapse = ""
  )

  runtime <- if (!is.null(object$execution$runtime))
    sprintf("%.2f s", object$execution$runtime) else "-"

  generated <- format(
    if (!is.null(object$timestamp)) object$timestamp else Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  paste0(
    "<!DOCTYPE html>\n<html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", .html_escape(title), "</title>",
    .html_style(),
    "</head><body>",

    "<header><h1>", .html_escape(title), "</h1>",
    "<div class='sub'>Generated ", .html_escape(generated),
    " &middot; validation runtime ", .html_escape(runtime),
    " &middot; validation engine v",
    .html_escape(.report_or(object$execution$validation_version, "-")),
    "</div><nav>", nav, "</nav></header>",

    "<main>", paste(unlist(parts), collapse = ""), "</main>",

    "<div id='lightbox'><img alt='Enlarged plot'></div>",

    "<footer>Self-contained report generated by CausalMultiOmics. ",
    "No external resources are required to view this file.</footer>",

    .html_script(),
    "</body></html>"
  )

}

# -----------------------------------------------------------------------------
# Destination handling
# -----------------------------------------------------------------------------

#' Resolve the destination path for the HTML report
#'
#' When \code{file} is \code{NULL} the user is prompted. In a non-interactive
#' session the prompt cannot block, so the default path is used and reported.
#'
#' @keywords internal

.report_destination <- function(file = NULL,
                                default_name = "CausalMultiOmics_validation_report.html",
                                prompt = TRUE) {

  default_path <- file.path(getwd(), default_name)

  if (is.null(file) && isTRUE(prompt)) {

    if (interactive()) {

      cat("\nWhere should the HTML report be saved?\n")
      cat("Enter a file path, a directory, or press Enter for the default.\n")
      cat("Default: ", default_path, "\n", sep = "")

      answer <- readline("Path: ")

      answer <- trimws(answer)

      if (nzchar(answer)) file <- answer

    } else {

      message("Non-interactive session: writing the report to the default path.")

    }

  }

  if (is.null(file) || !nzchar(file)) file <- default_path

  file <- path.expand(file)

  # A directory was supplied: place the default file name inside it.

  if (dir.exists(file)) file <- file.path(file, default_name)

  if (!grepl("\\.html?$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".html")
  }

  parent <- dirname(file)

  if (!dir.exists(parent)) {

    created <- dir.create(parent, recursive = TRUE, showWarnings = FALSE)

    if (!created && !dir.exists(parent)) {
      stop(sprintf("Cannot create the directory '%s'.", parent))
    }

  }

  file

}

