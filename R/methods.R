# =============================================================================
# methods.R
# S3 methods for CausalMultiOmics
# =============================================================================

# =============================================================================
# Internal formatting helpers
# =============================================================================

#' Format value as character or None placeholder
#'
#' @noRd
fmt_char <- function(x) {

  if (is.null(x) || length(x) == 0 || all(is.na(x)))
    return("None")

  as.character(x)

}

#' Format number with rounding or NA placeholder
#'
#' @noRd
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

#' Print a compact overview of a MultiOmicsData object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{MultiOmicsData} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print everything stored in a MultiOmicsData object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{MultiOmicsData} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print a compact overview of a BlockDiagnostics object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{BlockDiagnostics} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print everything stored in a BlockDiagnostics object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{BlockDiagnostics} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print a compact overview of a TransformationRecommendation object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{TransformationRecommendation} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print everything stored in a TransformationRecommendation object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{TransformationRecommendation} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print a compact overview of a PreprocessingRecipe object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{PreprocessingRecipe} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print everything stored in a PreprocessingRecipe object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{PreprocessingRecipe} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print a compact overview of a PreprocessingResult object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{PreprocessingResult} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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
              "Steps executed:",
              length(x$steps)))

  cat(sprintf("%-30s %d\n",
              "Removed samples:",
              sum(lengths(x$removed_samples))))

  cat(sprintf("%-30s %d\n",
              "Removed features:",
              sum(lengths(x$removed_features))))

  cat(sprintf("%-30s %d\n",
              "Plots:",
              length(Filter(Negate(is.null),x$plots))))

  if(!is.null(x$execution$runtime))
    cat(sprintf("%-30s %.2f s\n",
                "Runtime:",
                x$execution$runtime))

  invisible(x)

}


# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' Print everything stored in a PreprocessingResult object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{PreprocessingResult} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

  # =====================================================================
  # Executed pipeline
  # =====================================================================

  cat("Pipeline\n")
  cat("--------\n")

  if(length(object$steps)==0){

    cat("Nothing was executed\n")

  }else{

    for(s in object$steps){

      cat(
        sprintf(
          "%3d  %-16s %-20s %-22s %4s x %-4s -> %4s x %-4s\n",
          s$step,
          s$block,
          s$stage,
          s$method,
          s$samples_before, s$features_before,
          s$samples_after, s$features_after
        )
      )

    }

  }

  cat("\n")

  cat("Samples removed\n")
  cat("----------------\n")

  if(length(object$removed_samples)==0){

    cat("None\n")

  }else{

    for(nm in names(object$removed_samples)){
      cat(sprintf("%-20s %d\n", nm, length(object$removed_samples[[nm]])))
    }

  }

  cat("\n")

  cat("Features removed\n")
  cat("-----------------\n")

  if(length(object$removed_features)==0){

    cat("None\n")

  }else{

    for(nm in names(object$removed_features)){
      cat(sprintf("%-20s %d\n", nm, length(object$removed_features[[nm]])))
    }

  }

  cat("\n")

  cat("Quality\n")
  cat("-------\n")

  if(is.data.frame(object$quality$table) && nrow(object$quality$table)>0){

    print(object$quality$table, row.names = FALSE)

  }else{

    cat("Not available\n")

  }

  if(length(object$logs)>0){

    cat("\n")

    cat("Log\n")
    cat("---\n")

    cat(paste0("- ",object$logs),sep="\n")

  }

  invisible(object)

}

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' Print a compact overview of a CMOValidation object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{CMOValidation} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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

#' Print everything stored in a CMOValidation object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{CMOValidation} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
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
# EvidenceEdge
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' Print a compact overview of a EvidenceEdge object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{EvidenceEdge} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.EvidenceEdge <- function(x, ...) {

  cat("\n")
  cat("EvidenceEdge\n")
  cat("============\n\n")

  cat(sprintf("  %s  ->  %s\n\n", x$source, x$target))

  cat(sprintf("%-24s %s\n", "Direction:", fmt_char(x$direction)))
  cat(sprintf("%-24s %s\n", "Estimate:", fmt_num(x$estimate, 4)))

  if (is.finite(x$ci_lower) && is.finite(x$ci_upper)) {
    cat(sprintf("%-24s [%s, %s]\n", "95% CI:",
                fmt_num(x$ci_lower, 4), fmt_num(x$ci_upper, 4)))
  }

  cat(sprintf("%-24s %s\n", "FDR:", fmt_num(x$fdr, 4)))
  cat(sprintf("%-24s %s\n", "Methods:",
              paste(x$supporting_methods, collapse = ", ")))

  cat("\n")

  cat(sprintf("%-24s %s\n", "Evidence score:", fmt_num(x$evidence_score, 1)))
  cat(sprintf("%-24s %s\n", "  strength", fmt_num(x$strength, 3)))
  cat(sprintf("%-24s %s\n", "  confidence", fmt_num(x$confidence, 3)))
  cat(sprintf("%-24s %s\n", "  consistency", fmt_num(x$consistency, 3)))

  if (is.finite(x$data_quality) && x$data_quality < 1) {
    cat(sprintf("%-24s %s  (scaled down, limited by %s)\n", "  data quality",
                fmt_num(x$data_quality, 3),
                .report_or(x$quality_limited_by, "an ingredient")))
  }

  cat("\n")

  cat(sprintf("%-24s %s\n", "Identification:", fmt_char(x$identification)))

  if (isTRUE(x$identifiable)) {

    cat("  The supplied DAG says this adjustment identifies the effect.\n")

  } else if (isFALSE(x$identifiable)) {

    cat("  The supplied DAG says this adjustment does NOT identify it.\n")

  } else if (!identical(x$identification, "temporal") &&
             !identical(x$identification, "instrument")) {

    cat("  This is an association, not an identified causal effect.\n")

  }

  if (length(x$identification_reason) > 0) {

    cat(paste(strwrap(x$identification_reason, width = 74, prefix = "  "),
              collapse = "\n"), "\n", sep = "")

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' Print everything stored in a EvidenceEdge object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{EvidenceEdge} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

summary.EvidenceEdge <- function(object, ...) {

  print(object)

  cat("\n")

  cat("Assumptions required for a causal reading\n")
  cat("----------------------------------------\n")

  if (length(object$assumptions) == 0) {

    cat("None recorded\n")

  } else {

    cat(paste0("- ", object$assumptions), sep = "\n")

  }

  if (length(object$adjustment_set) > 0) {

    cat("\n")
    cat("Adjusted for\n")
    cat("------------\n")
    cat(paste0("- ", object$adjustment_set), sep = "\n")

  }

  if (length(object$adjustment_problems) > 0) {

    cat("\n")
    cat("Problems with that adjustment\n")
    cat("-----------------------------\n")

    for (p in object$adjustment_problems) {
      cat(paste(strwrap(p, width = 74, prefix = "  ", initial = "- "),
                collapse = "\n"), "\n", sep = "")
    }

  }

  if (length(object$required_adjustment) > 0) {

    cat("\n")
    cat("A sufficient adjustment set, per the DAG\n")
    cat("---------------------------------------\n")
    cat("  ", paste(object$required_adjustment, collapse = ", "), "\n",
        sep = "")

    missing <- setdiff(object$required_adjustment, object$adjustment_set)

    if (length(missing) > 0) {
      cat("  missing from this model: ", paste(missing, collapse = ", "),
          "\n", sep = "")
    }

  }

  if (is.finite(object$share_driving_effect) ||
      is.finite(object$heterogeneity_fdr)) {

    cat("\n")
    cat("Does one number describe everybody\n")
    cat("---------------------------------\n")

    if (is.finite(object$share_driving_effect)) {

      cat(sprintf(
        "  Halved by removing %d sample(s), %.0f%% of the cohort.%s\n",
        object$samples_driving_effect, 100 * object$share_driving_effect,
        if (object$share_driving_effect <= 0.05)
          " That is a minority, not a group." else ""))

    }

    if (is.finite(object$heterogeneity_fdr)) {

      cat(sprintf("  Tested against %s: interaction FDR %.3g%s\n",
                  object$heterogeneity_moderator, object$heterogeneity_fdr,
                  if (isTRUE(object$heterogeneity_fdr < 0.05))
                    " - the groups do not agree" else " - no difference detected"))

      if (is.data.frame(object$effect_by_group) &&
          nrow(object$effect_by_group) > 0) {
        print(object$effect_by_group, row.names = FALSE, digits = 3)
      }

      if (!isTRUE(object$heterogeneity_fdr < 0.05)) {
        cat("  Interaction tests are badly underpowered, so this is weak\n")
        cat("  reassurance rather than evidence of a uniform effect.\n")
      }

    }

  }

  if (length(object$quality_flags) > 0) {

    cat("\n")
    cat("What was measured and what was filled in\n")
    cat("---------------------------------------\n")

    for (f in object$quality_flags) {
      cat(paste(strwrap(f, width = 74, prefix = "  ", initial = "- "),
                collapse = "\n"), "\n", sep = "")
    }

    if (is.finite(object$complete_case_estimate)) {

      cat(sprintf(
        "\n  Using only the %d measured rows the estimate is %s, against %s\n",
        object$complete_case_n,
        format(round(object$complete_case_estimate, 4)),
        format(round(object$estimate, 4))))

      cat(if (isTRUE(object$complete_case_agrees))
        "  overall: same direction, so this did not come out of the imputation.\n"
        else
          "  overall: the direction REVERSES without the filled-in rows.\n")

    }

  }

  if (length(object$conflicting_methods) > 0) {

    cat("\n")
    cat("Conflicting methods\n")
    cat("-------------------\n")
    cat(paste0("- ", object$conflicting_methods), sep = "\n")

  }

  if (length(object$notes) > 0) {

    cat("\n")
    cat("Notes\n")
    cat("-----\n")
    cat(paste0("- ", object$notes), sep = "\n")

  }

  invisible(object)

}

# =============================================================================
# Hypothesis
# =============================================================================

#' Print a stated claim, its case, and what would settle it
#'
#' @param x A \code{Hypothesis}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.Hypothesis <- function(x, ...) {

  wrap <- function(text, prefix = "  ", initial = prefix) {
    cat(paste(strwrap(text, width = 74, prefix = prefix, initial = initial),
              collapse = "\n"), "\n", sep = "")
  }

  cat("\n")
  cat("Hypothesis\n")
  cat(strrep("=", 74), "\n\n", sep = "")

  wrap(.report_or(x$claim, "No claim could be stated."))

  cat("\n")
  wrap(sprintf("This is %s.", .report_or(x$grade, "an association")))

  cat("\n")
  cat(sprintf("  %-22s %s\n", "Effect:",
              sprintf("%s (%s)", fmt_num(x$estimate, 4),
                      .report_or(x$quantity_label, "unspecified"))))

  if (all(is.finite(x$ci))) {
    cat(sprintf("  %-22s [%s, %s]\n", "95% CI:",
                fmt_num(x$ci[1], 4), fmt_num(x$ci[2], 4)))
  }

  cat(sprintf("  %-22s %s\n", "Identification:",
              .report_or(x$identification, "-")))

  if (length(x$grade_reasons) > 0) {
    cat("\n")
    for (r in x$grade_reasons) wrap(r, prefix = "    ", initial = "  - ")
  }

  section <- function(title, items, bullet = "  + ") {

    if (length(items) == 0) return(invisible(NULL))

    cat("\n")
    cat(title, "\n", sep = "")
    cat(strrep("-", 74), "\n", sep = "")

    for (i in items) wrap(i, prefix = "    ", initial = bullet)

  }

  section("What supports it", x$supports, "  + ")
  section("What threatens it", x$threatens, "  - ")

  # The part that makes this a hypothesis rather than a result: not what was
  # seen, but what would change the author's mind.
  section("What would settle it", x$settles, "  > ")

  if (isTRUE(x$replication$tested)) {

    r <- x$replication

    cat("\n")
    cat("Tested on another cohort\n")
    cat(strrep("-", 74), "\n", sep = "")

    cat(sprintf("  %-22s %s\n", "Verdict:", toupper(r$verdict)))
    cat(sprintf("  %-22s %s\n", "There:",
                sprintf("%s [%s, %s], n = %d, p = %s",
                        fmt_num(r$estimate, 4), fmt_num(r$ci[1], 4),
                        fmt_num(r$ci[2], 4), r$n,
                        format.pval(r$p_value, digits = 2, eps = 1e-16))))
    cat(sprintf("  %-22s %s\n", "Here:", fmt_num(r$original, 4)))
    cat(sprintf("  %-22s %s of the original\n", "Size:",
                if (is.finite(r$ratio)) sprintf("%.0f%%", 100 * r$ratio)
                else "-"))

    for (nt in r$notes) wrap(nt, prefix = "    ", initial = "  ! ")

  }

  cat("\n")

  if (length(x$provenance) > 0) {

    cat(sprintf("  From %s %s, %s sample(s), score %s.\n",
                .report_or(x$provenance$package, "?"),
                .report_or(x$provenance$version, "?"),
                .report_or(x$provenance$samples, "?"),
                fmt_num(.report_or(x$provenance$evidence_score, NA), 1)))

  }

  cat("\n")

  invisible(x)

}

# =============================================================================
# ModuleGraph
# =============================================================================

#' Print the groups of features that move together
#'
#' @param x A \code{ModuleGraph}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.ModuleGraph <- function(x, ...) {

  cat("\n")
  cat("ModuleGraph\n")
  cat("===========\n\n")

  if (!is.data.frame(x$modules) || nrow(x$modules) == 0) {

    cat(paste(strwrap(if (length(x$notes) > 0) x$notes[1] else
      "No modules were found.", width = 60, prefix = "  "),
      collapse = "\n"), "\n\n", sep = "")

    return(invisible(x))

  }

  cat(sprintf("%-28s %d\n", "Modules:", nrow(x$modules)))
  cat(sprintf("%-28s %d\n", "Coherent enough to use:", sum(x$modules$coherent)))
  cat(sprintf("%-28s %d\n", "Spanning several blocks:",
              sum(x$modules$cross_block)))
  cat(sprintf("%-28s %s at |r| >= %s\n", "Grouped by:",
              .report_or(x$method, "?"), fmt_num(x$height, 2)))

  cat("\nWhat each one is made of\n")
  cat(strrep("-", 60), "\n", sep = "")

  for (i in seq_len(nrow(x$modules))) {

    m <- x$modules[i, ]

    cat(sprintf("  %-12s %2d feature(s), %s%s\n", m$module, m$size,
                m$composition,
                if (m$cross_block) "  [crosses blocks]" else ""))

    cat(sprintf("               summarised by its first component, which\n"))
    cat(sprintf("               carries %s of its variance%s\n",
                if (is.finite(m$variance_explained))
                  sprintf("%.0f%%", 100 * m$variance_explained) else "?",
                if (!m$coherent) " - too little to stand for it" else ""))

    cat(sprintf("               %s\n", m$members))

  }

  if (is.data.frame(x$edges) && nrow(x$edges) > 0) {

    cat("\nEach module against the outcome\n")
    cat(strrep("-", 60), "\n", sep = "")

    show <- x$edges[, c("module", "estimate", "ci_lower", "ci_upper",
                        "fdr", "n", "coherent")]

    names(show) <- c("module", "estimate", "CI low", "CI high", "FDR", "n",
                     "coherent")

    print(show, row.names = FALSE, digits = 3)

  }

  if (length(x$notes) > 0) {

    cat("\nHow to read this\n")
    cat(strrep("-", 60), "\n", sep = "")

    for (nt in x$notes) {
      cat(paste(strwrap(nt, width = 60, prefix = "  "), collapse = "\n"),
          "\n", sep = "")
    }

  }

  cat("\n")

  invisible(x)

}

# =============================================================================
# ConsensusGraph
# =============================================================================

#' Print what the resampling says about the shape of the graph
#'
#' @param x A \code{ConsensusGraph}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.ConsensusGraph <- function(x, ...) {

  cat("\n")
  cat("ConsensusGraph\n")
  cat("==============\n\n")

  if (x$replicates == 0) {

    cat("  ", if (length(x$notes) > 0) x$notes[1] else
      "Nothing to report.", "\n\n", sep = "")

    return(invisible(x))

  }

  cat(sprintf("%-28s %d (%s)\n", "Replicates:", x$replicates,
              .report_or(x$scheme, "unknown scheme")))

  if (length(x$sizes) > 0) {
    cat(sprintf("%-28s %d to %d, median %d\n", "Relationships per replicate:",
                min(x$sizes), max(x$sizes), as.integer(stats::median(x$sizes))))
  }

  cat(sprintf("%-28s %.0f%% of replicates\n", "Consensus threshold:",
              100 * x$threshold))

  cat("\n")

  a <- x$agreement

  cat(sprintf("%-28s %d\n", "Reported from the sample:", a$reported))
  cat(sprintf("%-28s %d\n", "In the consensus:", a$consensus))
  cat(sprintf("%-28s %d\n", "In both:", a$both))
  cat(sprintf("%-28s %s\n", "Overlap (Jaccard):", fmt_num(a$jaccard, 2)))

  if (length(a$reported_only) > 0) {

    cat("\nReported, but rarely recur\n")
    cat(strrep("-", 60), "\n", sep = "")
    for (k in utils::head(a$reported_only, 10)) cat("  ", k, "\n", sep = "")
    if (length(a$reported_only) > 10)
      cat("  ... and ", length(a$reported_only) - 10, " more\n", sep = "")

  }

  if (length(a$consensus_only) > 0) {

    # The more interesting direction: the sample that was collected happened
    # not to show these, and a reader looking only at the report would never
    # learn they exist.

    cat("\nRecur, but were not reported\n")
    cat(strrep("-", 60), "\n", sep = "")
    for (k in utils::head(a$consensus_only, 10)) cat("  ", k, "\n", sep = "")
    if (length(a$consensus_only) > 10)
      cat("  ... and ", length(a$consensus_only) - 10, " more\n", sep = "")

  }

  if (is.data.frame(x$rank_stability) && nrow(x$rank_stability) > 0) {

    cat("\nDid the headline findings keep their place\n")
    cat(strrep("-", 60), "\n", sep = "")

    for (i in seq_len(nrow(x$rank_stability))) {

      r <- x$rank_stability[i, ]

      cat(sprintf("  %2d. %-32s seen %3.0f%%, top %.0f%%, median rank %s\n",
                  r$reported_rank,
                  paste(r$source, "->", r$target),
                  100 * r$frequency, 100 * r$kept_top,
                  fmt_num(r$median_rank, 1)))

    }

  }

  if (length(x$notes) > 0) {

    cat("\nWhat this does and does not tell you\n")
    cat(strrep("-", 60), "\n", sep = "")

    for (nt in x$notes) {
      cat(paste(strwrap(nt, width = 60, prefix = "  "), collapse = "\n"),
          "\n", sep = "")
    }

  }

  cat("\n")

  invisible(x)

}

# =============================================================================
# EvidenceGraph
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' Print a compact overview of a EvidenceGraph object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{EvidenceGraph} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.EvidenceGraph <- function(x, ...) {

  cat("\n")
  cat("EvidenceGraph\n")
  cat("=============\n\n")

  cat(sprintf("%-24s %d\n", "Nodes:", nrow(x$nodes)))
  cat(sprintf("%-24s %d\n", "Edges:", nrow(x$edges)))
  cat(sprintf("%-24s %d\n", "Communities:", length(x$communities$sizes)))
  cat(sprintf("%-24s %d\n", "Paths:", nrow(x$paths)))

  if (nrow(x$edges) > 0) {

    cat(sprintf("%-24s %d\n", "Temporal edges:", sum(x$edges$temporal)))
    cat(sprintf("%-24s %s\n", "Best evidence score:",
                fmt_num(max(x$edges$evidence_score), 1)))

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' Print everything stored in a EvidenceGraph object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{EvidenceGraph} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

summary.EvidenceGraph <- function(object, ...) {

  print(object)

  if (nrow(object$edges) == 0) {

    cat("\nNo relationships passed the reporting threshold.\n")
    return(invisible(object))

  }

  cat("\n")

  cat("Top relationships\n")
  cat("-----------------\n")

  top <- utils::head(object$edges[order(-object$edges$evidence_score), ], 10)

  print(
    top[, c("source", "target", "direction", "evidence_score",
            "n_methods", "identification")],
    row.names = FALSE
  )

  cat("\n")

  cat("Identification strategies\n")
  cat("-------------------------\n")

  print(table(object$edges$identification))

  if (nrow(object$paths) > 0) {

    cat("\n")
    cat("Top paths\n")
    cat("---------\n")

    print(utils::head(object$paths[, c("path", "weakest_link")], 5),
          row.names = FALSE)

  }

  invisible(object)

}

# =============================================================================
# CMOResult
# =============================================================================

# -----------------------------------------------------------------------------
# print()
# -----------------------------------------------------------------------------

#' Print a compact overview of a CMOResult object
#'
#' Shows the few numbers that describe the object at a glance. Use
#' \code{summary()} for the full contents.
#'
#' @param x A \code{CMOResult} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.CMOResult <- function(x, ...) {

  cat("\n")
  cat("CMOResult\n")
  cat("=========\n\n")

  cat(sprintf("%-30s %s\n", "Outcome:", fmt_char(x$outcome$name)))
  cat(sprintf("%-30s %s\n", "Design:", fmt_char(x$design$type)))
  cat(sprintf("%-30s %s\n", "Samples:", fmt_num(x$performance$samples, 0)))

  cat(sprintf("%-30s %s of %s\n", "Features analysed:",
              fmt_num(x$performance$features_retained, 0),
              fmt_num(x$performance$features_screened, 0)))

  cat("\n")

  cat(sprintf("%-30s %s\n", "Methods run:",
              fmt_num(x$performance$generators_run, 0)))
  cat(sprintf("%-30s %s\n", "Relationships found:",
              fmt_num(x$performance$edges_generated, 0)))
  cat(sprintf("%-30s %s\n", "After integration:",
              fmt_num(x$performance$edges_integrated, 0)))
  cat(sprintf("%-30s %s\n", "With temporal precedence:",
              fmt_num(x$performance$temporal_edges, 0)))

  cat("\n")

  cat(sprintf("%-30s %d\n", "Causal paths:", nrow(x$causal_paths)))
  cat(sprintf("%-30s %d\n", "Plots:", length(x$plots)))
  cat(sprintf("%-30s %d\n", "Tables:", length(x$tables)))

  if (!is.null(x$execution$runtime)) {
    cat(sprintf("%-30s %.2f s\n", "Runtime:", x$execution$runtime))
  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# summary()
# -----------------------------------------------------------------------------

#' Print everything stored in a CMOResult object
#'
#' Walks through every section the object carries. Use
#' \code{print()} for a one-screen overview instead.
#'
#' @param object A \code{CMOResult} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

summary.CMOResult <- function(object, ...) {

  cat("\n")
  cat("================== CMOResult Summary ==================\n\n")

  # =====================================================================
  # Study
  # =====================================================================

  cat("Study\n")
  cat("-----\n")

  cat(sprintf("%-26s %s\n", "Outcome", fmt_char(object$outcome$name)))
  cat(sprintf("%-26s %s\n", "Outcome type", fmt_char(object$outcome$type)))
  cat(sprintf("%-26s %s\n", "Design", fmt_char(object$design$type)))
  cat(sprintf("%-26s %s\n", "Samples", fmt_num(object$performance$samples, 0)))

  if (length(object$design$notes) > 0) {
    cat(paste0("  ", object$design$notes), sep = "\n")
  }

  cat("\n")

  # =====================================================================
  # Methods
  # =====================================================================

  cat("Evidence generators\n")
  cat("-------------------\n")

  if (length(object$models) == 0) {

    cat("None\n")

  } else {

    for (nm in names(object$models)) {

      cat(sprintf("%-22s %-32s %d relationship(s)\n",
                  nm, object$models[[nm]]$label,
                  object$models[[nm]]$n_edges))

    }

  }

  if (length(object$logs) > 0) {

    skipped <- grep("skipped|failed", object$logs, value = TRUE)

    if (length(skipped) > 0) {

      cat("\n")
      cat(paste0("  ", skipped), sep = "\n")

    }

  }

  cat("\n")

  # =====================================================================
  # Main findings
  # =====================================================================

  cat("Main findings\n")
  cat("-------------\n")

  if (length(object$interpretation$statements) == 0) {

    cat("None\n")

  } else {

    cat(paste0("- ", object$interpretation$statements), sep = "\n")

  }

  cat("\n")

  # =====================================================================
  # Drivers
  # =====================================================================

  cat("Strongest relationships with the outcome\n")
  cat("----------------------------------------\n")

  drivers <- object$interpretation$drivers

  if (is.data.frame(drivers) && nrow(drivers) > 0) {

    print(
      drivers[, c("source", "direction", "evidence_score", "n_methods",
                  "identification")],
      row.names = FALSE
    )

  } else {

    cat("None passed the reporting threshold\n")

  }

  cat("\n")

  # =====================================================================
  # Structure
  # =====================================================================

  cat("Graph\n")
  cat("-----\n")

  cat(sprintf("%-26s %d\n", "Nodes", object$network$n_nodes))
  cat(sprintf("%-26s %d\n", "Edges", object$network$n_edges))
  cat(sprintf("%-26s %d\n", "Communities",
              length(object$network$communities$sizes)))

  if (length(object$interpretation$hubs) > 0) {
    cat(sprintf("%-26s %s\n", "Hubs",
                paste(utils::head(object$interpretation$hubs, 5), collapse = ", ")))
  }

  if (length(object$interpretation$mediators) > 0) {
    cat(sprintf("%-26s %s\n", "Mediators",
                paste(utils::head(object$interpretation$mediators, 5), collapse = ", ")))
  }

  if (length(object$interpretation$bridges) > 0) {
    cat(sprintf("%-26s %s\n", "Cross-block bridges",
                paste(utils::head(object$interpretation$bridges, 5), collapse = ", ")))
  }

  cat("\n")

  # =====================================================================
  # Paths
  # =====================================================================

  cat("Top paths to the outcome\n")
  cat("------------------------\n")

  if (nrow(object$causal_paths) > 0) {

    print(utils::head(object$causal_paths[, c("path", "weakest_link")], 5),
          row.names = FALSE)

  } else {

    cat("None\n")

  }

  cat("\n")

  # =====================================================================
  # Limitations
  # =====================================================================

  cat("Limitations\n")
  cat("-----------\n")

  cat(paste0("- ", object$report$limitations), sep = "\n")

  invisible(object)

}

# =============================================================================
# Validation report
# =============================================================================

# -----------------------------------------------------------------------------
# Internal report helpers
# -----------------------------------------------------------------------------

#' Return default when value is missing
#'
#' @noRd
.report_or <- function(x, default = NA) {

  if (is.null(x) || length(x) == 0) default else x

}

#' Print a repeated character divider line
#'
#' @noRd
.report_rule <- function(width = 78, char = "=") {

  cat(strrep(char, width), "\n", sep = "")

}

#' Print title framed by rule lines
#'
#' @noRd
.report_title <- function(text, width = 78) {

  cat("\n")
  .report_rule(width, "=")
  cat(text, "\n", sep = "")
  .report_rule(width, "=")

}

#' Print underlined section heading
#'
#' @noRd
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

      # The number an analysis actually runs on, and the one the matrix
      # above cannot show: every pair can share everything while all of them
      # together share nothing.

      shared_all <- object$summary$shared_by_all

      if (!is.null(shared_all)) {

        cat(sprintf("  %-26s %d\n", "Present in EVERY block", shared_all))

        if (shared_all < min(off)) {

          cat("\n  An analysis across all blocks will use those ",
              shared_all, " sample(s),\n", sep = "")
          cat("  not the pairwise figures above.\n")

          cum <- object$summary$cumulative_overlap

          if (is.data.frame(cum) && nrow(cum) > 0) {
            cat("\n  Adding blocks largest first:\n")
            for (i in seq_len(nrow(cum))) {
              cat(sprintf("    + %-24s %d left%s\n", cum$block[i],
                          cum$shared_after[i],
                          if (cum$lost[i] > 0)
                            sprintf("  (-%d)", cum$lost[i]) else ""))
            }
          }

        }

      }

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

#' Build HTML overview cards with quality gauge
#'
#' @keywords internal
#' @noRd
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

#' Build HTML diagnostics table for each block
#'
#' @keywords internal
#' @noRd
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

#' Build HTML summary of preprocessing steps per block
#'
#' @keywords internal
#' @noRd
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

#' Build HTML panels comparing transformation candidates per block
#'
#' @keywords internal
#' @noRd
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

#' Build HTML chip lists for QC flags
#'
#' @keywords internal
#' @noRd
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

#' Build HTML panels of recommended actions per block
#'
#' @keywords internal
#' @noRd
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

#' Build HTML sample overlap table with note
#'
#' @keywords internal
#' @noRd
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

    shared_all <- object$summary$shared_by_all

    note <- paste0(
      "<p class='muted'>Smallest pairwise overlap: <b>", min(off),
      "</b> &middot; largest: <b>", max(off), "</b>",
      if (!is.null(shared_all))
        paste0(" &middot; present in <i>every</i> block: <b>", shared_all,
               "</b>") else "",
      "</p>",

      # The table above answers a question nobody asked. An analysis needs
      # the samples every block has at once, and that number can be far
      # smaller than any pair suggests without anything looking wrong.
      if (!is.null(shared_all) && shared_all < min(off))
        paste0("<p class='warnbox'>Every pair of blocks shares at least ",
               min(off), " samples, but only <b>", shared_all,
               "</b> are present in all of them at once. An analysis across ",
               "all blocks will use those ", shared_all,
               ", because each block is missing a different part of the ",
               "cohort.</p>") else "",

      if (min(off) == 0)
        "<p class='warnbox'>At least one pair of blocks shares no samples. Integration across those blocks will require a different sample-matching strategy.</p>"
      else ""
    )

  }

  cumulative <- object$summary$cumulative_overlap

  ladder <- if (is.data.frame(cumulative) && nrow(cumulative) > 1 &&
                cumulative$shared_after[nrow(cumulative)] <
                  min(cumulative$samples)) {

    names(cumulative) <- c("Block added", "Samples in it",
                           "Shared after adding it", "Cost")

    paste0("<h3>How the shared count falls as blocks are added</h3>",
           "<p class='muted'>Largest block first. The row where the count ",
           "drops is the block that cost you those samples.</p>",
           .html_table(cumulative))

  } else ""

  paste0(
    "<section id='overlap'><h2>Sample overlap</h2>",
    .html_table(df),
    note,
    ladder,
    "</section>"
  )

}

#' Build HTML section embedding diagnostic plot images
#'
#' @keywords internal
#' @noRd
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

#' Build HTML section listing data and QC tables
#'
#' @keywords internal
#' @noRd
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

#' Return CSS stylesheet for the HTML report
#'
#' @keywords internal
#' @noRd
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

#' Return JavaScript for tab switching and table sorting
#'
#' @keywords internal
#' @noRd
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


# =============================================================================
# Analysis report
# =============================================================================
# The audience for this document is not necessarily a statistician, and that
# changes what it has to do. A reader who does not know what an adjusted
# association is will read "evidence score 87" as "proven cause" unless the
# document actively stops them. So the plain-language layer is not decoration
# here: it is the part that keeps the report honest.
#
# Every user-facing string goes through .result_text() so the wording lives in
# one place.
# =============================================================================

#' User-facing wording for the analysis report
#'
#' Keeping the prose in one table means it can be reviewed as prose, and a
#' translation is a matter of filling a column rather than hunting through
#' markup.
#'
#' @keywords internal

.result_text <- function(key) {

  wording <- c(

    title = "Analysis report",

    intro = paste(
      "This report summarises the relationships found between the measured",
      "variables and the outcome studied. It was produced automatically:",
      "every number below comes from the data supplied, and nothing was",
      "chosen by hand."
    ),

    howto_title = "How to read this report",

    score_intro = paste(
      "Each relationship gets a score from 0 to 100 called the evidence",
      "score. It is built from three separate things, and it helps to keep",
      "them apart:"
    ),

    strength_label = "Size",
    strength_text = paste(
      "How big the relationship is. A large value means that when one",
      "variable changes, the other changes a lot."
    ),

    confidence_label = "Precision",
    confidence_text = paste(
      "How sure we are of the number itself. This grows with the number of",
      "samples and shrinks when the estimate is noisy."
    ),

    consistency_label = "Agreement",
    consistency_text = paste(
      "How many of the analysis methods saw the same thing. Several methods",
      "were run on the same data; this counts how many agreed."
    ),

    causation_title = "What this report can and cannot tell you",

    causation_text = paste(
      "Finding that two things go together is not the same as showing that",
      "one causes the other. Two variables can move together because a third",
      "thing drives both of them. Ice cream sales and sunburn rise together,",
      "but ice cream does not cause sunburn: hot weather causes both."
    ),

    agreement_warning = paste(
      "Agreement between methods does not fix this. If every method is",
      "looking at the same data, and something that was never measured is",
      "driving both variables, then every method will report the same",
      "relationship and every one of them will be misled in the same way.",
      "High agreement means the finding is stable, not that it is causal."
    ),

    identification_intro = paste(
      "Because of that, every relationship below is labelled with how much",
      "can be claimed about cause:"
    ),

    id_none_label = "Observed together",
    id_none_text = paste(
      "The two were seen to move together and nothing else was taken into",
      "account. This is the weakest claim."
    ),

    id_adjustment_label = "Other factors accounted for",
    id_adjustment_text = paste(
      "Known factors were subtracted out. This is stronger, but only for the",
      "factors that were actually measured. Anything not measured is still",
      "unaccounted for."
    ),

    id_temporal_label = "Measured in order",
    id_temporal_text = paste(
      "The first variable was measured before the second. That rules out the",
      "relationship running backwards, which is real progress, but it still",
      "does not rule out a third factor driving both."
    ),

    id_instrument_label = "Instrumented",
    id_instrument_text = paste(
      "A special kind of variable was used that, under strong assumptions,",
      "does support a causal reading."
    ),

    findings_title = "Main findings",
    findings_none = "No relationship passed the reporting threshold.",

    pathways_title = "Chains of relationships",
    pathways_intro = paste(
      "Sometimes a variable appears to act on the outcome through another",
      "one. These chains are shown below, strongest first. A chain is only as",
      "trustworthy as its weakest step, so that is what it is ranked by."
    ),
    pathways_none = "No chains of relationships were found.",

    network_title = "The overall picture",
    network_intro = paste(
      "All relationships together form a map. Variables that connect to many",
      "others are shown as hubs; variables that sit between two different",
      "kinds of measurement are shown as bridges."
    ),

    importance_title = "Which variables matter most",
    importance_intro = paste(
      "Several methods rank the variables by how useful they are. Because",
      "each method produces numbers on its own scale, the rankings are",
      "combined rather than the raw values. A value near 1 means the variable",
      "came out near the top in most methods."
    ),

    methods_title = "How the analysis was done",
    methods_intro = paste(
      "The following methods were run. Each one looks at the data",
      "differently, which is why agreement between them is reported."
    ),

    data_title = "What the analysis is based on",

    limitations_title = "Limitations",
    limitations_intro = paste(
      "Every analysis rests on things that could be otherwise. These apply to",
      "the results above and should be read alongside them, not after them."
    ),

    technical_title = "Technical detail",
    technical_intro = paste(
      "The complete tables behind the report, for readers who want them."
    ),

    glossary_title = "Glossary"

  )

  if (!(key %in% names(wording))) return(key)

  unname(wording[[key]])

}

#' Plain-language rendering of a direction
#' @keywords internal
.result_plain_direction <- function(direction, source, target,
                                    encoding = NULL) {

  # A category comparison needs different words from a measurement. "Higher
  # Sex=M goes together with higher HDL" is neither English nor true: the
  # column is a yes/no marker, and what it compares is being one category
  # rather than another.

  if (!is.null(encoding)) {

    if (is.na(direction)) {
      return(sprintf("Being %s rather than %s is related to %s",
                     encoding$level, encoding$reference, target))
    }

    return(sprintf(
      "Being %s rather than %s goes together with %s %s",
      encoding$level, encoding$reference,
      if (identical(direction, "positive")) "higher" else "lower",
      target
    ))

  }

  if (is.na(direction)) {
    return(sprintf("%s is related to %s", source, target))
  }

  if (identical(direction, "positive")) {
    sprintf("Higher %s goes together with higher %s", source, target)
  } else {
    sprintf("Higher %s goes together with lower %s", source, target)
  }

}

#' The encoding record for a feature, when it came from a category
#' @keywords internal
#' @noRd
.feature_encoding <- function(object, feature) {

  encoding <- object$data$encoding

  if (is.null(encoding) || !(feature %in% names(encoding))) return(NULL)

  encoding[[feature]]

}

#' Plain-language label and explanation for an identification strategy
#' @keywords internal
.result_identification_label <- function(identification) {

  switch(
    identification,
    none = .result_text("id_none_label"),
    adjustment = .result_text("id_adjustment_label"),
    temporal = .result_text("id_temporal_label"),
    instrument = .result_text("id_instrument_label"),
    identification
  )

}

#' Map identification strategy to strength category
#'
#' @keywords internal
#' @noRd
.result_identification_class <- function(identification) {

  switch(
    identification,
    none = "weak",
    adjustment = "medium",
    temporal = "strong",
    instrument = "strong",
    "weak"
  )

}

#' Headline findings written for a non-specialist reader
#'
#' \code{.interpret_graph()} phrases its statements for someone who knows
#' what an adjusted association is. That vocabulary is exactly what this
#' report exists to translate, so the general-audience version is generated
#' from the same facts in plainer words rather than reused verbatim.
#'
#' @keywords internal

.result_plain_statements <- function(object) {

  statements <- character(0)

  drivers <- object$interpretation$drivers
  outcome <- object$outcome$name

  if (is.data.frame(drivers) && nrow(drivers) > 0) {

    best <- drivers[1, ]

    statements <- c(statements, sprintf(
      "The clearest finding: %s. %d of the methods used agreed on this.",
      .result_plain_direction(best$direction, best$source, outcome,
                              .feature_encoding(object, best$source)),
      best$n_methods
    ))

    if (nrow(drivers) > 1) {

      statements <- c(statements, sprintf(
        "%d variable(s) in total showed a relationship with %s.",
        nrow(drivers), outcome
      ))

    }

  }

  mediators <- object$interpretation$mediators

  if (length(mediators) > 0 && nrow(object$causal_paths) > 0) {

    statements <- c(statements, sprintf(
      "Some variables appear to act on %s indirectly, through %s.",
      outcome, mediators[1]
    ))

  }

  # The headline number a non-specialist most needs is how much of this can
  # be read as cause, so it is stated in the summary rather than left to the
  # limitations section.

  total <- object$performance$edges_integrated
  temporal <- object$performance$temporal_edges

  if (isTRUE(total > 0)) {

    statements <- c(statements, if (isTRUE(temporal > 0)) {

      sprintf(
        paste("For %d of the %d relationships found, one variable was",
              "measured before the other, so the relationship cannot run",
              "backwards. For the remaining %d we can only say the variables",
              "move together."),
        temporal, total, total - temporal
      )

    } else {

      paste(
        "Everything here was measured at a single point in time. That means",
        "we can say these variables move together, but not which one comes",
        "first, and not that one causes the other."
      )

    })

  }

  statements

}

#' A 0-100 score rendered as an inline bar
#' @keywords internal
.result_score_bar <- function(score) {

  if (is.null(score) || length(score) == 0 || is.na(score)) {
    return("<span class='muted'>not available</span>")
  }

  pct <- max(0, min(100, score))

  level <- if (pct >= 60) "good" else if (pct >= 30) "ok" else "warn"

  paste0(
    "<div class='scorewrap'><div class='scorebar'><div class='scorefill ",
    level, "' style='width:", round(pct, 1), "%'></div></div>",
    "<span class='scorenum'>", sprintf("%.0f", pct), "</span></div>"
  )

}

# -----------------------------------------------------------------------------
# Supplementary style
# -----------------------------------------------------------------------------

#' Return CSS styles for the results report
#'
#' @keywords internal
#' @noRd
.result_style <- function() {
"<style>
.lead{font-size:16px;line-height:1.65;max-width:70ch}
.callout{background:#fff7e8;border:1px solid #f0d9a8;border-left-width:5px;
border-left-color:var(--warn);border-radius:8px;padding:14px 18px;margin:16px 0;max-width:80ch}
.callout h4{margin:0 0 8px;font-size:15px}
.callout p{margin:0 0 8px}
.callout p:last-child{margin-bottom:0}
.callout ul{margin:0 0 8px;padding-left:20px}
.callout.good{background:#f0f9f3;border-color:#bfe3cd;border-left-color:var(--good)}
.callout.danger{background:#fdf1f1;border-color:#f0c4c4;border-left-color:var(--bad)}
.explain{background:var(--panel);border:1px solid var(--line);border-radius:10px;
padding:16px 18px;margin-bottom:12px}
.explain h4{margin:0 0 6px;font-size:15px}
.explain p{margin:0;color:var(--muted);font-size:14px}
.finding{background:var(--panel);border:1px solid var(--line);border-radius:10px;
padding:16px 18px;margin-bottom:12px}
.finding-head{display:flex;flex-wrap:wrap;align-items:center;gap:10px;margin-bottom:10px}
.finding-title{font-size:16px;font-weight:600}
.badge-id{border-radius:999px;padding:3px 12px;font-size:12px;font-weight:600;white-space:nowrap}
.badge-id.weak{background:#f1f2f4;color:var(--muted)}
.badge-id.medium{background:#eef2fb;color:#25417f}
.badge-id.strong{background:#e6f5ec;color:#14663a}
.scorewrap{display:flex;align-items:center;gap:10px;min-width:180px}
.scorebar{flex:1;height:10px;background:var(--line);border-radius:999px;overflow:hidden;min-width:110px}
.scorefill{height:100%;border-radius:999px}
.scorefill.good{background:var(--good)}
.scorefill.ok{background:var(--ok)}
.scorefill.warn{background:var(--warn)}
.scorenum{font-weight:600;font-size:13px;min-width:26px;text-align:right}
.finding-detail{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));
gap:10px;margin-top:10px;font-size:13px;color:var(--muted)}
.finding-detail b{color:var(--fg);display:block;font-size:15px}
.chain{background:var(--panel);border:1px solid var(--line);border-radius:10px;
padding:14px 18px;margin-bottom:10px}
.chain-path{font-size:15px;font-weight:600;word-break:break-word}
.chain-meta{color:var(--muted);font-size:13px;margin-top:6px}
.glossary dt{font-weight:600;margin-top:10px}
.glossary dd{margin:2px 0 0 0;color:var(--muted);font-size:14px}
details.contrib{margin-top:12px;border-top:1px solid var(--line);padding-top:10px}
details.contrib>summary{cursor:pointer;font-size:13.5px;font-weight:600;color:var(--accent)}
details.contrib[open]>summary{margin-bottom:10px}
details.contrib .explain{margin-bottom:8px}
.provgrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:12px;margin:14px 0}
.provcard{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:14px 16px}
.provhead{display:flex;align-items:baseline;justify-content:space-between;gap:10px;margin-bottom:6px}
.provrange{font-size:11.5px;color:var(--muted);background:var(--line);border-radius:999px;padding:2px 9px;white-space:nowrap}
.provcard p{margin:0 0 6px;font-size:13.5px}
.provwatch{color:var(--muted);font-size:13px;margin:0}
.blocklist{margin:14px 0}
.blockrow{display:grid;grid-template-columns:150px 1fr 62px;grid-template-areas:
'name bar pct' 'name meta meta';gap:4px 12px;align-items:center;
padding:10px 0;border-bottom:1px solid var(--line)}
.blockrow:last-child{border-bottom:none}
.blockname{grid-area:name;font-weight:600;font-size:14px;word-break:break-word}
.blockbar{grid-area:bar;height:11px;background:var(--line);border-radius:999px;overflow:hidden}
.blockfill{height:100%;border-radius:999px;background:var(--accent)}
.blockpct{grid-area:pct;text-align:right;font-weight:600;font-size:13px}
.blockmeta{grid-area:meta;color:var(--muted);font-size:12.5px}
pre.flow{font-size:12.5px;line-height:1.4}
</style>"
}

# -----------------------------------------------------------------------------
# Sections
# -----------------------------------------------------------------------------

#' Build HTML overview cards for analysis results
#'
#' @keywords internal
#' @noRd
.html_result_overview <- function(object) {

  performance <- object$performance

  cards <- paste0(
    .html_card("Outcome studied", .report_or(object$outcome$name, "-")),
    .html_card("People or samples", .report_or(performance$samples, "-")),
    .html_card("Variables examined", .report_or(performance$features_retained, "-")),
    .html_card("Relationships found", .report_or(performance$edges_integrated, "-")),
    .html_card("Methods used", .report_or(performance$generators_run, "-")),
    .html_card("Measured in order",
               paste0(.report_or(performance$temporal_edges, 0), " of ",
                      .report_or(performance$edges_integrated, 0)))
  )

  statements <- .result_plain_statements(object)

  paste0(
    "<section id='overview' class='active'><h2>", .result_text("findings_title"),
    "</h2>",

    "<p class='lead'>", .html_escape(.result_text("intro")), "</p>",

    "<div class='cards'>", cards, "</div>",

    if (length(statements) > 0)
      paste0("<h3>In short</h3>", .html_list(statements)) else "",

    "<div class='callout'><h4>", .html_escape(.result_text("causation_title")),
    "</h4><p>", .html_escape(.result_text("causation_text")), "</p>",
    "<p>", .html_escape(.result_text("agreement_warning")), "</p></div>",

    "</section>"
  )

}

#' Build HTML explanations of scoring and identification concepts
#'
#' @keywords internal
#' @noRd
.html_result_howto <- function(object) {

  score_blocks <- paste0(
    "<div class='explain'><h4>", .result_text("strength_label"),
    "</h4><p>", .html_escape(.result_text("strength_text")), "</p></div>",
    "<div class='explain'><h4>", .result_text("confidence_label"),
    "</h4><p>", .html_escape(.result_text("confidence_text")), "</p></div>",
    "<div class='explain'><h4>", .result_text("consistency_label"),
    "</h4><p>", .html_escape(.result_text("consistency_text")), "</p></div>"
  )

  id_blocks <- paste0(
    vapply(
      c("none", "adjustment", "temporal", "instrument"),
      function(id) {
        paste0(
          "<div class='explain'><h4><span class='badge-id ",
          .result_identification_class(id), "'>",
          .html_escape(.result_identification_label(id)), "</span></h4>",
          "<p>", .html_escape(.result_text(paste0("id_", id, "_text"))),
          "</p></div>"
        )
      },
      character(1)
    ),
    collapse = ""
  )

  paste0(
    "<section id='howto'><h2>", .result_text("howto_title"), "</h2>",

    "<p class='lead'>", .html_escape(.result_text("score_intro")), "</p>",
    score_blocks,

    "<div class='callout'><h4>", .html_escape(.result_text("causation_title")),
    "</h4><p>", .html_escape(.result_text("causation_text")), "</p>",
    "<p>", .html_escape(.result_text("agreement_warning")), "</p></div>",

    "<p class='lead'>", .html_escape(.result_text("identification_intro")), "</p>",
    id_blocks,

    .html_result_spread_howto(object),

    .html_result_quality_howto(object),

    .html_result_dag_howto(object),

    "</section>"
  )

}

#' Explain, once, what an average does and does not say
#'
#' @keywords internal
#' @noRd
.html_result_spread_howto <- function(object) {

  checked <- Filter(function(e) is.finite(e$share_driving_effect),
                    object$evidence)

  if (length(checked) == 0) return("")

  concentrated <- Filter(function(e) isTRUE(e$share_driving_effect <= 0.05),
                         checked)

  tested <- Filter(function(e) is.finite(e$heterogeneity_fdr), object$evidence)
  differing <- Filter(function(e) isTRUE(e$heterogeneity_fdr < 0.05), tested)

  paste0(
    "<div class='callout'><h4>Does one number describe everybody?</h4>",

    "<p>Every figure in this report is an average over the people who were ",
    "measured, and an average says nothing about whether they resemble each ",
    "other. The same number can mean the relationship holds in everyone, or ",
    "that it is strong in a tenth of them and absent in the rest. Those are ",
    "different findings: the first is about the group, the second is about ",
    "ten people nobody has identified.</p>",

    "<p>So each of the <b>", length(checked), "</b> strongest relationships ",
    "was asked how much of the group would have to be removed to halve it. ",
    "For a relationship that holds broadly the answer is most of them, ",
    "because removing a few people barely shifts an average. <b>",
    length(concentrated), "</b> here were halved by removing 5% or less.</p>",

    if (length(tested) > 0)
      paste0("<p>Where you named something that might change the ",
             "relationship, it was also tested directly: <b>",
             length(differing), "</b> of <b>", length(tested),
             "</b> differ between the groups it defines. Detecting such a ",
             "difference needs far more data than detecting the relationship ",
             "itself, so a result of none is weak reassurance rather than ",
             "evidence that the effect is uniform.</p>") else
      paste0("<p>Nothing was named as a possible modifier, so no subgroup ",
             "was tested directly. Passing <code>heterogeneity = </code> a ",
             "metadata column asks whether each relationship differs across ",
             "it.</p>"),

    "</div>"
  )

}

#' Explain, once, what the data-quality discount is and why it exists
#'
#' @keywords internal
#' @noRd
.html_result_quality_howto <- function(object) {

  affected <- Filter(function(e) length(e$quality_flags) > 0, object$evidence)

  if (length(affected) == 0) return("")

  worst <- min(vapply(affected, function(e)
    .report_or(e$data_quality, 1), numeric(1)))

  checked <- Filter(function(e) is.finite(e$complete_case_estimate), affected)
  reversed <- Filter(function(e) isFALSE(e$complete_case_agrees), checked)

  paste0(
    "<div class='callout'><h4>Measured values and filled-in values</h4>",

    "<p>Some of the variables here arrived with gaps. Rather than throw those ",
    "people away, preprocessing estimated what the missing numbers probably ",
    "were. That is standard practice and usually the right call, but it has a ",
    "consequence nothing downstream can see: once a gap is filled, no model ",
    "can tell a measurement from an estimate, and the confidence intervals ",
    "come out exactly as narrow as if every value had been real.</p>",

    "<p><b>", length(affected), "</b> of the relationships in this report ",
    "involve a variable that was partly filled in. Their scores were scaled ",
    "down in proportion to how much was filled and how much the filling ",
    "method preserves &mdash; the most affected keeps <b>",
    .html_escape(sprintf("%.0f%%", 100 * worst)), "</b> of what its ",
    "statistics alone would earn. Nothing else about them was changed.</p>",

    if (length(checked) > 0)
      paste0("<p>Each was also refitted using only the people whose value was ",
             "actually measured. <b>", length(reversed), "</b> of <b>",
             length(checked), "</b> changed direction. A relationship that ",
             "reverses when the estimated values are removed was produced by ",
             "the filling, not found in the data.</p>") else "",

    "<p class='muted'>Sample size is a separate matter and is already in the ",
    "precision score. This is only about values that were reconstructed.</p>",

    "</div>"
  )

}

#' Explain the DAG audit, once, where the reader learns the vocabulary
#'
#' The per-finding verdicts say what happened. This says what the words mean,
#' and — when no DAG was supplied — why every finding is silent about it.
#'
#' @keywords internal
#' @noRd
.html_result_dag_howto <- function(object) {

  audited <- vapply(object$evidence,
                    function(e) !is.null(e$identifiable) &&
                      !is.na(e$identifiable), logical(1))

  if (length(audited) == 0 || !any(audited)) {

    return(paste0(
      "<div class='callout'><h4>No causal diagram was supplied</h4>",
      "<p>Every label above was inferred from the shape of the study alone: ",
      "what was measured, when, and what was adjusted for. That tells you a ",
      "model controlled for something; it cannot tell you whether the right ",
      "something was controlled for.</p>",
      "<p>If you are willing to commit to a diagram of what causes what, pass ",
      "it as <code>analyze(..., dag = )</code> and each relationship will be ",
      "checked against it. Adjusting for a variable that sits between the ",
      "exposure and the outcome, or for one the outcome itself causes, makes ",
      "an estimate worse rather than better, and nothing in the data will ",
      "reveal that.</p></div>"
    ))

  }

  n_ok <- sum(vapply(object$evidence,
                     function(e) isTRUE(e$identifiable), logical(1)))

  paste0(
    "<div class='callout'><h4>What the causal diagram check means</h4>",

    "<p>You supplied a diagram of what causes what, and each relationship was ",
    "checked against it: <b>", n_ok, "</b> of <b>", sum(audited),
    "</b> are identified by the variables that model adjusted for.</p>",

    "<p><b>Identified</b> means the adjustment closes every path that would ",
    "make the two variables move together for a reason other than one ",
    "affecting the other. <b>Not identified</b> means at least one such path ",
    "is still open, or that something harmful was adjusted for.</p>",

    "<p>Two adjustments do damage while looking like care. Conditioning on a ",
    "variable that lies on the path from cause to effect removes part of the ",
    "very effect being measured. Conditioning on a variable that the outcome ",
    "causes opens a path that was closed, manufacturing an association out of ",
    "nothing. In both cases the adjusted estimate is further from the truth ",
    "than the unadjusted one.</p>",

    "<p class='muted'>None of this is a statement about the data. It follows ",
    "entirely from the diagram you supplied, and the data cannot confirm that ",
    "diagram is right.</p></div>"
  )

}

#' What every method reported for one relationship, before merging
#'
#' The headline numbers are a summary across methods. A summary the reader
#' cannot open is something they have to take on trust, which is the opposite
#' of what this report is for. Each finding therefore carries the individual
#' results underneath it, plus a plain description of what each method does
#' and what it cannot see.
#'
#' @noRd
.html_result_contributions <- function(object, source, target) {

  edge <- NULL

  for (e in object$evidence) {
    if (identical(e$source, source) && identical(e$target, target)) {
      edge <- e
      break
    }
  }

  if (is.null(edge) || !is.data.frame(edge$contributions) ||
      nrow(edge$contributions) == 0) {
    return("")
  }

  contributions <- edge$contributions

  display <- data.frame(
    Method = contributions$method,
    `Kind of evidence` = contributions$level_label,
    `What it measured` = contributions$quantity,
    `Combined` = ifelse(contributions$pooled, "yes", "no"),
    Estimate = round(contributions$estimate, 4),
    `95% CI` = ifelse(
      is.finite(contributions$ci_lower) & is.finite(contributions$ci_upper),
      sprintf("%.3f to %.3f", contributions$ci_lower, contributions$ci_upper),
      "not reported"),
    `p-value` = ifelse(is.finite(contributions$p_value),
                       format.pval(contributions$p_value, digits = 2,
                                   eps = 1e-16), "not reported"),
    FDR = ifelse(is.finite(contributions$fdr),
                 format.pval(contributions$fdr, digits = 2, eps = 1e-16),
                 "not reported"),
    Samples = contributions$n,
    Agrees = ifelse(contributions$agrees, "yes", "no"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  methods_html <- paste0(vapply(unique(contributions$generator), function(g) {

    info <- .method_explanation(g)

    label <- contributions$method[contributions$generator == g][1]

    paste0(
      "<div class='explain'><h4>", .html_escape(label), "</h4>",
      "<p>", .html_escape(info$what), "</p>",
      if (nzchar(info$cannot))
        paste0("<p><b>What it cannot tell you.</b> ",
               .html_escape(info$cannot), "</p>") else "",
      "</div>"
    )

  }, character(1)), collapse = "")

  paste0(
    "<details class='contrib'><summary>What each method found on its own</summary>",

    "<p class='muted'>Each row is one method working alone, before anything ",
    "was combined. The third column says what kind of number it is: a log ",
    "hazard ratio and a drop in prediction accuracy are not the same ",
    "measurement and cannot be averaged together. Only rows marked ",
    "<b>combined</b> went into the headline estimate; the rest still count ",
    "towards agreement, because two methods pointing the same way is ",
    "informative even when their magnitudes are not.</p>",

    if (!is.null(edge$quantity_label) && !is.na(edge$quantity_label))
      paste0("<p class='muted'>Headline estimate: <b>",
             .html_escape(edge$quantity_label), "</b>, pooled from ",
             edge$pooled_from, " observation(s).",
             if (length(edge$not_pooled) > 0)
               paste0(" Left out: ",
                      .html_escape(paste(edge$not_pooled, collapse = ", ")),
                      ".") else "",
             "</p>") else "",

    .html_table(display),

    "<h4 style='margin-top:16px'>What these methods are</h4>",
    methods_html,

    "</details>"
  )

}

#' What would have to be measured next to settle this one
#'
#' The most actionable thing the engine produces and the only part of a
#' report a reader can act on directly. Everything else says what was seen;
#' this says what to do about it.
#'
#' @keywords internal
#' @noRd
.html_result_settles <- function(object, source) {

  h <- .safe_try(hypothesis(object, source), NULL)

  if (is.null(h) || length(h$settles) == 0) return("")

  paste0(
    "<details class='provenance'><summary>What would settle this</summary>",

    "<p class='muted'>Each of these follows from a specific weakness in this ",
    "relationship rather than from general caution, so they are in the order ",
    "the evidence puts them.</p>",

    "<ol>",
    paste0("<li>", vapply(h$settles, .html_escape, character(1)), "</li>",
           collapse = ""),
    "</ol>",

    "</details>"
  )

}

#' Say whether this one number describes everybody
#'
#' Only rendered when there is something to say. A box confirming that a
#' relationship is spread across the cohort, on every finding, teaches the
#' reader to skip the box that one day is not.
#'
#' @keywords internal
#' @noRd
.html_result_spread <- function(object, source, target) {

  edge <- Filter(function(e) identical(e$source, source) &&
                   identical(e$target, target), object$evidence)

  if (length(edge) == 0) return("")

  e <- edge[[1]]

  concentrated <- isTRUE(e$share_driving_effect <= 0.05)
  differs <- isTRUE(e$heterogeneity_fdr < 0.05)

  if (!concentrated && !differs) return("")

  groups <- if (differs && is.data.frame(e$effect_by_group) &&
                nrow(e$effect_by_group) > 0) {

    tab <- e$effect_by_group
    names(tab) <- c("Group", "People in it", "Relationship within it")
    .html_table(tab)

  } else ""

  paste0(
    "<div class='callout danger'>",

    "<h4>", if (differs)
      "This number is an average over groups that disagree"
    else "This number describes a few people, not the group", "</h4>",

    if (concentrated)
      paste0("<p>Removing the <b>", e$samples_driving_effect,
             "</b> most influential people &mdash; <b>",
             sprintf("%.0f%%", 100 * e$share_driving_effect),
             "</b> of everyone measured &mdash; halves this relationship. A ",
             "relationship that holds across a group does not behave like ",
             "that: removing a handful of people barely moves an average. ",
             "This one is carried by those few.</p>") else "",

    if (differs)
      paste0("<p>The relationship differs between levels of <b>",
             .html_escape(.report_or(e$heterogeneity_moderator, "a subgroup")),
             "</b>",
             if (isFALSE(e$consistent_across_groups))
               ", and the direction itself reverses between them" else "",
             ". The single figure above is their average, which may describe ",
             "nobody in particular.</p>") else "",

    groups,

    if (differs)
      paste0("<p class='muted'>Detecting a difference between groups needs ",
             "far more data than detecting the relationship itself, so this ",
             "is a hypothesis for a study designed to test it, not a ",
             "conclusion about those groups.</p>") else "",

    "</div>"
  )

}

#' Say which part of a relationship was measured and which was filled in
#'
#' Rendered only when something behind the relationship was imputed. On a
#' complete dataset it would be a row of reassurances, and a reader learns to
#' skip a box that always says the same thing.
#'
#' @keywords internal
#' @noRd
.html_result_quality <- function(object, source, target) {

  edge <- Filter(function(e) identical(e$source, source) &&
                   identical(e$target, target), object$evidence)

  if (length(edge) == 0) return("")

  e <- edge[[1]]

  if (length(e$quality_flags) == 0) return("")

  reversed <- isFALSE(e$complete_case_agrees)

  paste0(
    "<div class='callout ", if (reversed) "danger" else "", "'>",

    "<h4>", if (reversed)
      "This finding does not survive without the filled-in values"
    else "Part of this was filled in, not measured", "</h4>",

    "<ul>", paste0("<li>", vapply(e$quality_flags, .html_escape,
                                  character(1)), "</li>", collapse = ""),
    "</ul>",

    if (is.finite(e$data_quality) && e$data_quality < 1)
      paste0("<p>The score for this relationship was scaled down to <b>",
             .html_escape(sprintf("%.0f%%", 100 * e$data_quality)),
             "</b> of what the statistics alone would give it",
             if (!is.na(e$quality_limited_by))
               paste0(", set by <b>", .html_escape(e$quality_limited_by),
                      "</b>") else "",
             ".</p>") else "",

    if (is.finite(e$complete_case_estimate))
      paste0(
        "<p>Refitting on the <b>", e$complete_case_n,
        "</b> people whose value was actually measured gives <b>",
        .html_escape(format(round(e$complete_case_estimate, 4))),
        "</b>, against <b>",
        .html_escape(format(round(e$estimate, 4))),
        "</b> overall. ",
        if (reversed)
          paste0("The direction reverses, which means the relationship shown ",
                 "above is a product of how the gaps were filled rather than ",
                 "of what was measured. Treat it as an artefact until you can ",
                 "measure the missing values.")
        else
          paste0("Same direction, so this relationship is in the measured ",
                 "data and not an artefact of the filling."),
        "</p>") else "",

    "</div>"
  )

}

#' State what the supplied DAG says about one relationship
#'
#' Only ever rendered when the user supplied a causal structure. Without one
#' the section would be a row of shrugs, and a reader would learn to skip it.
#'
#' @keywords internal
#' @noRd
.html_result_dag_verdict <- function(object, source, target) {

  edge <- Filter(function(e) identical(e$source, source) &&
                   identical(e$target, target), object$evidence)

  if (length(edge) == 0) return("")

  e <- edge[[1]]

  if (is.null(e$identifiable) || is.na(e$identifiable)) return("")

  ok <- isTRUE(e$identifiable)

  missing <- setdiff(e$required_adjustment, e$adjustment_set)

  paste0(
    "<div class='callout ", if (ok) "good" else "danger", "'>",

    "<h4>", if (ok) "Your causal diagram supports this estimate"
    else "Your causal diagram does not support this estimate", "</h4>",

    "<p>", .html_escape(.report_or(e$identification_reason, "")), "</p>",

    if (length(e$adjustment_problems) > 0)
      paste0("<ul>", paste0("<li>", vapply(e$adjustment_problems,
                                           .html_escape, character(1)),
                            "</li>", collapse = ""), "</ul>") else "",

    if (length(missing) > 0)
      paste0("<p>Adjusting also for <b>",
             .html_escape(paste(missing, collapse = ", ")),
             "</b> would close the remaining paths.</p>") else "",

    "<p class='muted'>This verdict follows from the structure you supplied, ",
    "not from the data. The data cannot confirm that structure.</p>",

    "</div>"
  )

}

#' Build HTML cards for key drivers found
#'
#' @keywords internal
#' @noRd
.html_result_findings <- function(object) {

  drivers <- object$interpretation$drivers

  if (!is.data.frame(drivers) || nrow(drivers) == 0) {

    return(paste0(
      "<section id='findings'><h2>", .result_text("findings_title"),
      "</h2><p class='muted'>", .result_text("findings_none"), "</p></section>"
    ))

  }

  cards <- paste0(vapply(seq_len(nrow(drivers)), function(i) {

    row <- drivers[i, ]

    paste0(
      "<div class='finding'>",
      "<div class='finding-head'>",
      "<span class='finding-title'>",
      .html_escape(.result_plain_direction(row$direction, row$source,
                                            object$outcome$name,
                                            .feature_encoding(object, row$source))),
      "</span>",
      "<span class='badge-id ", .result_identification_class(row$identification),
      "'>", .html_escape(.result_identification_label(row$identification)),
      "</span>",
      "</div>",

      .result_score_bar(row$evidence_score),

      "<div class='finding-detail'>",
      "<div><b>", .html_escape(fmt_num(row$strength, 2)), "</b>",
      .result_text("strength_label"), "</div>",
      "<div><b>", .html_escape(fmt_num(row$confidence, 2)), "</b>",
      .result_text("confidence_label"), "</div>",
      "<div><b>", .html_escape(fmt_num(row$consistency, 2)), "</b>",
      .result_text("consistency_label"), "</div>",
      "<div><b>", row$n_methods, "</b>method(s) agreed</div>",
      "</div>",

      "<p class='muted' style='margin-top:10px'>Found by: ",
      .html_escape(row$methods), "</p>",

      if (nzchar(row$conflicts))
        paste0("<p class='muted'>Methods reporting the opposite direction: ",
               .html_escape(row$conflicts), "</p>") else "",

      .html_result_spread(object, row$source, object$outcome$name),

      .html_result_quality(object, row$source, object$outcome$name),

      .html_result_dag_verdict(object, row$source, object$outcome$name),

      .html_result_contributions(object, row$source, object$outcome$name),

      .html_result_settles(object, row$source),

      "</div>"
    )

  }, character(1)), collapse = "")

  paste0(
    "<section id='findings'><h2>", .result_text("findings_title"), "</h2>",
    "<p class='lead'>Relationships with <b>",
    .html_escape(object$outcome$name), "</b>, strongest evidence first.</p>",
    cards,
    "</section>"
  )

}

#' Build HTML list of top causal path chains
#'
#' @keywords internal
#' @noRd
.html_result_pathways <- function(object) {

  paths <- object$causal_paths

  if (!is.data.frame(paths) || nrow(paths) == 0) {

    return(paste0(
      "<section id='pathways'><h2>", .result_text("pathways_title"),
      "</h2><p class='muted'>", .result_text("pathways_none"), "</p></section>"
    ))

  }

  chains <- paste0(vapply(seq_len(min(nrow(paths), 10)), function(i) {

    row <- paths[i, ]

    paste0(
      "<div class='chain'>",
      "<div class='chain-path'>",
      # Escape first, then swap in the arrow entity: escaping afterwards
      # would turn the entity itself into visible text. The entity keeps the
      # source ASCII, which R CMD check requires.
      gsub("-&gt;", "&rarr;", .html_escape(row$path), fixed = TRUE),
      "</div>",
      .result_score_bar(row$weakest_link),
      "<div class='chain-meta'>Passes through: ",
      .html_escape(if (nzchar(row$mediators)) row$mediators else "nothing"),
      " &middot; ", row$length, " step(s)</div>",
      "</div>"
    )

  }, character(1)), collapse = "")

  paste0(
    "<section id='pathways'><h2>", .result_text("pathways_title"), "</h2>",
    "<p class='lead'>", .html_escape(.result_text("pathways_intro")), "</p>",
    chains,
    "</section>"
  )

}

#' Groups of variables that are really one thing measured several times
#'
#' @keywords internal
#' @noRd
.html_result_modules <- function(object, plot_width, plot_height, plot_res) {

  mg <- object$modules

  header <- "<section id='modules'><h2>Things measured many times</h2>"

  if (is.null(mg) || !is.data.frame(mg$modules) || nrow(mg$modules) == 0) {

    return(paste0(
      header,
      "<p class='lead'>Measured variables are often not separate things. ",
      "Fifty transcripts that rise and fall together are one process ",
      "measured fifty times, and reporting fifty findings about it would ",
      "overstate what was found.</p>",
      "<div class='callout'><h4>Nothing here needed grouping</h4><p>",
      .html_escape(if (length(mg$notes) > 0) mg$notes[1] else
        "No groups of variables moved together closely enough to be treated as one."),
      "</p></div></section>"
    ))

  }

  mods <- mg$modules

  figure <- if (!is.null(object$plots$modules)) {

    uri <- .html_plot_uri(object$plots$modules, plot_width,
                          max(plot_height, 420), plot_res)

    if (is.null(uri)) "" else paste0(
      "<figure><img loading='lazy' src='", uri,
      "' alt='Each group beside the variables it summarises'><figcaption>",
      "The diamond is the group taken as one thing; the grey dots are its ",
      "members tested one at a time. Dots scattered on both sides of zero ",
      "mean the group is averaging over variables that disagree.",
      "</figcaption></figure>")

  } else ""

  cards <- paste0(vapply(seq_len(nrow(mods)), function(i) {

    m <- mods[i, ]

    hit <- if (is.data.frame(mg$edges))
      mg$edges[mg$edges$module == m$module, , drop = FALSE] else data.frame()

    relation <- if (nrow(hit) == 1 && is.finite(hit$fdr)) {

      paste0("<p>Taken as one thing, it ",
             if (hit$fdr < 0.05)
               paste0("<b>is related to ", .html_escape(object$outcome$name),
                      "</b> (", if (hit$estimate >= 0) "higher" else "lower",
                      " group score goes with a higher outcome, FDR ",
                      format(signif(hit$fdr, 2)), ").")
             else paste0("shows no relationship with ",
                         .html_escape(object$outcome$name),
                         " (FDR ", format(signif(hit$fdr, 2)), ")."),
             "</p>")

    } else ""

    paste0(
      "<div class='finding'>",
      "<div class='finding-head'><span class='finding-title'>",
      .html_escape(m$module), " &mdash; ", m$size, " variables</span>",
      if (m$cross_block)
        "<span class='badge-id strong'>spans several kinds of measurement</span>"
      else "",
      "</div>",

      "<p class='muted'>", .html_escape(m$composition), "</p>",

      "<p>A single summary of this group captures <b>",
      if (is.finite(m$variance_explained))
        sprintf("%.0f%%", 100 * m$variance_explained) else "?",
      "</b> of how its members vary",
      if (!m$coherent)
        paste0(", which is too little for the summary to stand for them. ",
               "The figures below describe one direction through a cloud ",
               "rather than a shared process") else "",
      ".</p>",

      relation,

      "<p class='muted'>Members: ", .html_escape(m$members),
      if (m$size > 8) ", &hellip;" else "", "</p>",

      "</div>"
    )

  }, character(1)), collapse = "")

  paste0(
    header,

    "<p class='lead'>Measured variables are often not separate things. ",
    "Variables that rise and fall together are usually one underlying ",
    "process measured several times over. Grouping them and testing the ",
    "group asks the question directly, instead of asking it once per ",
    "variable and reporting the answer as many findings.</p>",

    "<div class='cards'>",
    .html_card("Groups found", as.character(nrow(mods))),
    .html_card("Hold together well", as.character(sum(mods$coherent)),
               "a summary represents them"),
    .html_card("Span measurement types", as.character(sum(mods$cross_block)),
               "candidates for a mechanism"),
    "</div>",

    figure,

    cards,

    "<div class='callout'><h4>These are not extra evidence</h4>",
    "<p>A group and the variables inside it are the same measurements at two ",
    "resolutions. If a group and its members both appear in this report they ",
    "agree because they are made of each other, and neither one confirms the ",
    "other. Count the finding once.</p>",
    "<p class='muted'>Groups were formed by clustering variables on how ",
    "strongly they move together, at a correlation of ",
    .html_escape(sprintf("%.2f", .report_or(mg$height, NA))),
    " or more. Variables that move independently were left alone.</p>",
    "</div>",

    "</section>"
  )

}

#' Would this report look the same with a different set of people?
#'
#' The single most useful thing a reader can be told about a list of findings
#' and the one a single drawing cannot convey. Per-relationship stability is
#' already beside each finding; this is about the list as a list.
#'
#' @keywords internal
#' @noRd
.html_result_consensus <- function(object, plot_width, plot_height, plot_res) {

  cg <- object$consensus

  header <- "<section id='consensus'><h2>Would this repeat?</h2>"

  if (is.null(cg) || .report_or(cg$replicates, 0) == 0) {

    return(paste0(
      header,
      "<div class='callout'><h4>This was not checked</h4>",
      "<p>Everything in this report comes from one set of people. Whether the ",
      "same relationships would come out of a different set was not tested, ",
      "because resampling was switched off.</p>",
      "<p>Re-running with <code>effort = \"standard\"</code> or higher repeats ",
      "the whole analysis on hundreds of resamples of the same data and ",
      "reports how much of this picture survives.</p></div>",
      "</section>"
    ))

  }

  a <- cg$agreement

  jaccard <- .report_or(a$jaccard, NA_real_)

  verdict <- if (!is.finite(jaccard)) "could not be summarised"
  else if (jaccard >= 0.8) "mostly the same list"
  else if (jaccard >= 0.5) "a recognisable but shifting list"
  else "a substantially different list"

  figure <- if (!is.null(object$plots$consensus)) {

    uri <- .html_plot_uri(object$plots$consensus, plot_width,
                          max(plot_height, 520), plot_res)

    if (is.null(uri)) "" else paste0(
      "<figure><img loading='lazy' src='", uri,
      "' alt='Rank of each relationship across resamples'><figcaption>",
      "Each line runs from the best to the worst position a relationship ",
      "reached across the resamples, with a dot at its usual position. A ",
      "short line on the left would have been reported whoever was sampled. ",
      "A long line means its place in this report depended on who was.",
      "</figcaption></figure>")

  } else ""

  listing <- function(title, keys, explanation) {

    if (length(keys) == 0) return("")

    paste0("<h3>", title, "</h3><p class='muted'>", explanation, "</p>",
           "<ul>",
           paste0("<li>",
                  vapply(utils::head(keys, 15),
                         function(k) gsub("-&gt;", "&rarr;", .html_escape(k),
                                          fixed = TRUE),
                         character(1)),
                  "</li>", collapse = ""),
           if (length(keys) > 15)
             paste0("<li>and ", length(keys) - 15, " more</li>") else "",
           "</ul>")

  }

  size_note <- if (length(cg$sizes) > 0) {
    sprintf(paste("Each resample produced between %d and %d relationships,",
                  "against the %d reported here."),
            min(cg$sizes), max(cg$sizes), a$reported)
  } else ""

  paste0(
    header,

    "<p class='lead'>The findings above come from the particular people who ",
    "ended up in this study. To see how much that mattered, the entire ",
    "analysis was run again on <b>", cg$replicates, "</b> resamples of the ",
    "same data. What came back was <b>", verdict, "</b>.</p>",

    "<div class='cards'>",
    .html_card("Reported here", as.character(a$reported)),
    .html_card("Recur reliably", as.character(a$consensus),
               sprintf("in %.0f%% of resamples or more", 100 * cg$threshold)),
    .html_card("In both", as.character(a$both)),
    .html_card("Overlap", if (is.finite(jaccard))
      sprintf("%.0f%%", 100 * jaccard) else "-",
      "of the two lists combined"),
    "</div>",

    if (nzchar(size_note))
      paste0("<p class='muted'>", .html_escape(size_note), "</p>") else "",

    figure,

    listing("Reported here, but rarely came back", a$reported_only,
            paste("These made this report and then failed to reappear in most",
                  "resamples. Treat them as the weakest thing in it.")),

    listing("Came back reliably, but are not in this report", a$consensus_only,
            paste("The opposite problem, and the one a reader cannot discover",
                  "any other way: these recur across resamples but happened",
                  "not to clear the threshold in the sample that was",
                  "collected.")),

    "<div class='callout'><h4>What this does and does not tell you</h4>",
    "<p>A resample draws from the people who were actually measured, so this ",
    "says how much the picture depends on which of them ended up in the ",
    "study. It is not evidence that any relationship is real.</p>",
    "<p>A variable with no connection to the outcome that happens to track it ",
    "in this sample will track it in almost every resample of that sample ",
    "too, and will look perfectly stable here. The question of whether these ",
    "relationships exceed what the same analysis finds on noise is a ",
    "different one, answered by null calibration at ",
    "<code>effort = \"thorough\"</code>.</p></div>",

    "</section>"
  )

}

#' Build HTML network explanation with variable chips
#'
#' @keywords internal
#' @noRd
.html_result_network <- function(object, plot_width, plot_height, plot_res) {

  interpretation <- object$interpretation

  listing <- function(label, values, explanation) {

    if (length(values) == 0) return("")

    paste0(
      "<div class='explain'><h4>", label, "</h4>",
      "<p>", .html_escape(explanation), "</p>",
      "<div class='chiprow'>",
      paste0("<span class='chip'>", .html_escape(utils::head(values, 8)),
             "</span>", collapse = ""),
      "</div></div>"
    )

  }

  # The circular figures come first: they are the only ones that show every
  # layer, its size and the traffic between layers in a single view.

  panel <- function(plot, caption, square = FALSE) {

    if (is.null(plot)) return("")

    uri <- .html_plot_uri(
      plot,
      if (square) max(plot_width, plot_height) else plot_width,
      if (square) max(plot_width, plot_height) else plot_height,
      plot_res
    )

    if (is.null(uri)) return("")

    paste0("<figure><img loading='lazy' src='", uri, "' alt='",
           .html_escape(caption), "'><figcaption>", caption,
           "</figcaption></figure>")

  }

  figure <- paste0(

    panel(object$plots$circos_blocks,
          paste("Each arc is one kind of measurement, sized by how much of the",
                "evidence it carries. Each ribbon is the evidence running",
                "between two of them."),
          square = TRUE),

    panel(object$plots$circos,
          paste("The same picture with every individual variable shown as a",
                "tick inside its arc, and every relationship as a thread.",
                "Thicker threads carry more evidence."),
          square = TRUE),

    panel(object$plots$network,
          "Every arrow is a relationship. Thicker arrows carry more evidence.")

  )

  paste0(
    "<section id='network'><h2>", .result_text("network_title"), "</h2>",
    "<p class='lead'>", .html_escape(.result_text("network_intro")), "</p>",
    figure,

    listing("Hubs", interpretation$hubs,
            "Connected to many other variables, so a change here is likely to be felt widely."),

    listing("Bridges", interpretation$bridges,
            "Sit between two different kinds of measurement and link them together."),

    listing("In the middle of a chain", interpretation$mediators,
            "Appear between a starting variable and the outcome, which is what a mechanism looks like."),

    listing("Associated with a higher outcome", interpretation$risk,
            "When these go up, the outcome tends to go up."),

    listing("Associated with a lower outcome", interpretation$protective,
            "When these go up, the outcome tends to go down."),

    .html_result_blocks(object),

    "</section>"
  )

}

#' The same result read one measurement layer at a time
#'
#' A hundred variable-to-variable relationships are hard to hold in the head.
#' "The microbiome contributes two thirds of the evidence, and most of it runs
#' to the metabolome" is not, and it is usually the question that was being
#' asked in the first place.
#'
#' @noRd
.html_result_blocks <- function(object) {

  blocks <- object$network$blocks

  if (is.null(blocks) || !is.data.frame(blocks$importance) ||
      nrow(blocks$importance) == 0) {
    return("")
  }

  importance <- blocks$importance

  bars <- paste0(vapply(seq_len(nrow(importance)), function(i) {

    row <- importance[i, ]

    paste0(
      "<div class='blockrow'>",
      "<div class='blockname'>", .html_escape(row$block), "</div>",
      "<div class='blockbar'><div class='blockfill' style='width:",
      max(row$share_percent, 0.5), "%'></div></div>",
      "<div class='blockpct'>", sprintf("%.1f%%", row$share_percent), "</div>",
      "<div class='blockmeta'>", row$features_analysed, " variables &middot; ",
      row$relationships, " relationships &middot; ",
      row$direct_to_outcome, " reaching the outcome</div>",
      "</div>"
    )

  }, character(1)), collapse = "")

  between <- if (is.data.frame(blocks$evidence) && nrow(blocks$evidence) > 0) {

    display <- blocks$evidence[
      blocks$evidence$from != blocks$evidence$to, , drop = FALSE]

    if (nrow(display) == 0) "" else {

      names_map <- c(from = "From", to = "To",
                     relationships = "Relationships",
                     mean_score = "Average evidence",
                     highest_level = "Strongest kind of evidence")

      display <- display[, names(names_map), drop = FALSE]
      names(display) <- unname(names_map)

      paste0("<h3>What runs between the layers</h3>",
             "<p class='muted'>Relationships inside a single layer are left ",
             "out here: two proteins moving together usually reflects them ",
             "being part of the same process, not one acting on the other.</p>",
             .html_table(display))

    }

  } else ""

  communities <- if (is.data.frame(blocks$communities) &&
                     nrow(blocks$communities) > 0) {

    display <- blocks$communities[, c("size", "composition", "kind"),
                                  drop = FALSE]
    names(display) <- c("Variables", "Made up of", "Reading")

    paste0("<h3>The groups it found</h3>",
           "<p class='muted'>The map splits into clusters of variables more ",
           "connected to each other than to the rest. A cluster spanning ",
           "several layers is the interesting case; one confined to a single ",
           "layer usually reflects how that layer was measured.</p>",
           .html_table(display))

  } else ""

  paste0(
    "<h3>Which layer carries the result</h3>",
    "<p class='muted'>A relationship between two layers belongs to both, so ",
    "its weight is split between them. These add to 100%.</p>",
    "<div class='blocklist'>", bars, "</div>",
    between,
    communities
  )

}

#' Build HTML section with consensus importance plot
#'
#' @keywords internal
#' @noRd
.html_result_importance <- function(object, plot_width, plot_height, plot_res) {

  importance <- object$importance

  figure <- ""

  if (!is.null(object$plots$importance)) {

    uri <- .html_plot_uri(object$plots$importance, plot_width, plot_height,
                          plot_res)

    if (!is.null(uri)) {
      figure <- paste0("<figure><img loading='lazy' src='", uri,
                       "' alt='Consensus importance'></figure>")
    }

  }

  table_html <- if (is.data.frame(importance) && nrow(importance) > 0) {

    display <- utils::head(importance, 20)
    names(display) <- c("Variable", "Measurement type", "Methods that ranked it",
                        "Combined ranking")

    .html_table(display)

  } else "<p class='muted'>No importance ranking was produced.</p>"

  paste0(
    "<section id='importance'><h2>", .result_text("importance_title"), "</h2>",
    "<p class='lead'>", .html_escape(.result_text("importance_intro")), "</p>",
    figure,
    table_html,
    "</section>"
  )

}

#' Build HTML table summarizing methods and skipped models
#'
#' @keywords internal
#' @noRd
.html_result_methods <- function(object) {

  models <- object$models

  rows <- if (length(models) > 0) {

    do.call(rbind, lapply(names(models), function(nm) {
      data.frame(
        Method = models[[nm]]$label,
        `Relationships reported` = models[[nm]]$n_edges,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))

  } else data.frame()

  skipped <- grep("skipped|failed", object$logs, value = TRUE)

  paste0(
    "<section id='methods'><h2>", .result_text("methods_title"), "</h2>",
    "<p class='lead'>", .html_escape(.result_text("methods_intro")), "</p>",
    .html_table(rows),

    if (length(skipped) > 0)
      paste0("<h3>Not run</h3>", .html_list(skipped)) else "",

    "<h3>Study design</h3>",
    "<p>The data was treated as <b>", .html_escape(object$design$type),
    "</b>.</p>",
    if (length(object$design$notes) > 0) .html_list(object$design$notes) else "",

    "</section>"
  )

}

#' Build HTML summary of sample and variable screening
#'
#' @keywords internal
#' @noRd
.html_result_data <- function(object) {

  screening <- object$data$screening

  cards <- paste0(
    .html_card("Samples analysed", length(object$data$samples)),
    .html_card("Samples set aside", length(object$data$dropped_samples),
               "not measured in every block"),
    .html_card("Variables examined", screening$retained,
               paste0("of ", screening$tested, " available"))
  )

  paste0(
    "<section id='data'><h2>", .result_text("data_title"), "</h2>",
    "<div class='cards'>", cards, "</div>",

    if (length(object$data$dropped_samples) > 0)
      paste0("<p class='muted'>Only samples present in every block can be ",
             "compared across blocks, so ", length(object$data$dropped_samples),
             " were set aside.</p>") else "",

    if (isTRUE(screening$screened))
      paste0("<div class='callout'><h4>Not everything was examined</h4><p>",
             "There were too many variables to test every possible pair, so ",
             screening$retained, " were carried forward on the strength of ",
             "their relationship with the outcome. The remaining ",
             screening$tested - screening$retained,
             " were not examined for indirect roles and could still matter.",
             "</p></div>") else "",

    "</section>"
  )

}
#' What the results imply about changing something
#'
#' Only rendered when the user has declared which variables could plausibly be
#' acted on. That declaration is the point: without it the section would put a
#' contrast for a genotype next to one for a diet, and a reader would take
#' both the same way.
#'
#' @noRd
.html_result_actionable <- function(object) {

  modifiable <- object$parameters$modifiable

  if (length(modifiable) == 0) return("")

  cf <- .safe_try(counterfactual(object, modifiable = modifiable), NULL)

  if (is.null(cf) || nrow(cf$table) == 0) {

    return(paste0(
      "<section id='actionable'><h2>What changing something would mean</h2>",
      "<p class='muted'>No relationship among the variables you marked as ",
      "changeable could be expressed in its original units.</p>",
      if (length(.report_or(cf$skipped, character(0))) > 0)
        .html_list(cf$skipped) else "",
      "</section>"
    ))

  }

  cards <- paste0(vapply(cf$statements, function(s) {

    row <- cf$table[cf$table$variable == s$variable, ][1, ]

    tone <- if (is.finite(row$percent_change) && row$percent_change < 0)
      "strong" else "medium"

    paste0(
      "<div class='finding'>",
      "<div class='finding-head'>",
      "<span class='finding-title'>", .html_escape(s$sentence), "</span>",
      "<span class='badge-id ", .result_identification_class(s$identification),
      "'>", .html_escape(.result_identification_label(s$identification)),
      "</span></div>",

      "<div class='finding-detail'>",
      "<div><b>", .html_escape(format(row$from)), "</b>starting value</div>",
      "<div><b>", .html_escape(format(row$to)), "</b>changed to</div>",
      "<div><b>", .html_escape(format(row$value)), "</b>",
      .html_escape(row$measure), "</div>",
      "<div><b>", .html_escape(sprintf("%.0f", s$evidence_score)),
      "</b>evidence score</div>",
      "</div>",

      if (is.finite(row$ci_lower))
        paste0("<p class='muted' style='margin-top:8px'>Range compatible with ",
               "the data: ", .html_escape(format(row$ci_lower)), " to ",
               .html_escape(format(row$ci_upper)), "</p>") else "",

      "<p class='muted'>Estimated by: ", .html_escape(row$method), "</p>",

      "<div class='callout' style='margin:10px 0 0'>",
      "<p>", .html_escape(s$caveat), "</p></div>",

      "</div>"
    )

  }, character(1)), collapse = "")

  baseline_note <- if (is.finite(.report_or(cf$baseline_risk, NA))) {
    sprintf(paste("In this group %.1f%% of people had the event. Percentages",
                  "below are changes relative to that."),
            100 * cf$baseline_risk)
  } else ""

  paste0(
    "<section id='actionable'><h2>What changing something would mean</h2>",

    "<p class='lead'>These are the relationships involving the variables you ",
    "marked as things that could plausibly be changed. Each is expressed in ",
    "the units the variable was measured in.</p>",

    "<div class='callout danger'><h4>Read this before the numbers</h4>",
    "<p>These are contrasts implied by the fitted models: they describe how ",
    "the outcome differs between people whose measurements differ by this ",
    "much. That is not the same as what would happen if you changed the ",
    "variable in one person.</p>",
    "<p>The two coincide only when the effect is identified, which for data ",
    "collected by observation it usually is not. Each statement below says ",
    "which case it is, and none of them says a change would cause anything ",
    "unless the evidence supports that word.</p></div>",

    if (nzchar(baseline_note))
      paste0("<p class='muted'>", .html_escape(baseline_note), "</p>") else "",

    cards,

    "<h3>All of them together</h3>",
    .html_table(cf$table[, c("variable", "block", "from", "to", "measure",
                             "value", "percent_change", "method",
                             "identification")]),

    if (length(cf$skipped) > 0)
      paste0("<h3>Left out</h3>", .html_list(cf$skipped)) else "",

    "<p class='muted'>Variables not marked as changeable were not considered ",
    "at all. A contrast for something nobody can act on, such as a genotype, ",
    "invites a reading that nothing in the data can support.</p>",

    "</section>"
  )

}

#' Explain every quantity the report puts in front of the reader
#'
#' A number with no provenance is worse than no number: it carries the
#' authority of precision without the means to judge it. This section names
#' every quantity that appears anywhere in the document, says how it was
#' produced, and says what it does not mean.
#'
#' @noRd
.html_result_provenance <- function(object) {

  quantities <- paste0(vapply(.quantity_explanations(), function(q) {

    paste0(
      "<div class='provcard'>",
      "<div class='provhead'><b>", .html_escape(q$term), "</b>",
      "<span class='provrange'>", .html_escape(q$short), "</span></div>",
      "<p>", .html_escape(q$what), "</p>",
      "<p class='provwatch'><b>Careful.</b> ", .html_escape(q$watch), "</p>",
      "</div>"
    )

  }, character(1)), collapse = "")

  # Which methods actually ran here, rather than the whole catalogue.
  used <- unique(unlist(lapply(object$evidence, function(e)
    if (is.data.frame(e$contributions) && nrow(e$contributions) > 0)
      e$contributions$generator else character(0))))

  if (length(used) == 0) used <- names(object$models)

  methods_html <- if (length(used) == 0) "" else paste0(
    vapply(used, function(g) {

      info <- .method_explanation(g)

      label <- object$models[[g]]$label
      if (is.null(label)) label <- g

      paste0(
        "<div class='provcard'>",
        "<div class='provhead'><b>", .html_escape(label), "</b>",
        "<span class='provrange'>level ", .evidence_level(g), ", ",
        .html_escape(.level_label(.evidence_level(g))), "</span></div>",
        "<p>", .html_escape(info$what), "</p>",
        if (nzchar(info$cannot))
          paste0("<p class='provwatch'><b>What it cannot tell you.</b> ",
                 .html_escape(info$cannot), "</p>") else "",
        "</div>"
      )

    }, character(1)), collapse = "")

  paste0(
    "<section id='provenance'><h2>Where these numbers come from</h2>",

    "<p class='lead'>Every quantity in this report is listed here with how it ",
    "was produced. If a number anywhere in the document is unclear, it is ",
    "explained below.</p>",

    "<h3>The path from your data to these results</h3>",

    "<pre class='flow'><code>your measurements
      |
      v
  checked for problems, then cleaned following a written plan
      |
      v
  ", length(used), " statistical methods, each run separately on the same data
      |
      v
  each reports the relationships it found, on its own scale
      |
      v
  results describing the same relationship are merged into one
      |
      v
  the merged relationship is scored, and labelled with how much
  can be claimed about cause
      |
      v
  what you are reading</code></pre>",

    "<h3>The methods behind these results</h3>",

    "<p>Each looks at the data differently, which is the point: agreement ",
    "between methods that share no assumptions says more than one method ",
    "repeated. Each carries a level describing what kind of claim it can ",
    "support, and agreement is weighted by that level rather than counted.</p>",

    "<div class='provgrid'>", methods_html, "</div>",

    "<h3>Every quantity, explained</h3>",

    "<div class='provgrid'>", quantities, "</div>",

    "<h3>Where to find the raw numbers</h3>",

    "<p>Nothing here is hidden. In R, on the object this report was built ",
    "from:</p>",

    "<pre><code>result$evidence[[1]]$contributions   # what every method said
result$effects                       # the same, for every relationship
result$tables$evidence               # the merged and scored relationships
result$diagnostics                   # model fit, resampling, calibration
explain(result, \"NAME\")              # everything known about one variable</code></pre>",

    "</section>"
  )

}


#' Build HTML glossary and limitations section
#'
#' @keywords internal
#' @noRd
.html_result_limitations <- function(object) {

  glossary <- c(
    "Outcome" = "The thing the analysis is trying to explain.",
    "Variable" = "Anything that was measured.",
    "Relationship" = "Two variables that change together in a way unlikely to be chance alone.",
    "Evidence score" = "A 0-100 summary combining size, precision and agreement.",
    "Adjusted" = "Other known factors were subtracted out before measuring the relationship.",
    "Confounder" = "Something not measured that drives two variables at once, making them look related.",
    "Mediator" = "A variable that sits between a cause and its outcome.",
    "Hub" = "A variable connected to many others."
  )

  glossary_html <- paste0(
    "<dl class='glossary'>",
    paste0("<dt>", .html_escape(names(glossary)), "</dt><dd>",
           .html_escape(unname(glossary)), "</dd>", collapse = ""),
    "</dl>"
  )

  paste0(
    "<section id='limitations'><h2>", .result_text("limitations_title"), "</h2>",
    "<p class='lead'>", .html_escape(.result_text("limitations_intro")), "</p>",
    .html_list(object$report$limitations, "warn"),

    "<h3>", .result_text("glossary_title"), "</h3>",
    glossary_html,

    "</section>"
  )

}

#' Build HTML technical tables and supporting figures
#'
#' @keywords internal
#' @noRd
.html_result_technical <- function(object, plot_width, plot_height, plot_res) {

  tables <- object$tables

  panels <- paste0(vapply(names(tables), function(nm) {

    paste0("<details class='blockdetail'><summary>",
           .html_escape(gsub("_", " ", nm)), "</summary>",
           .html_table(tables[[nm]]), "</details>")

  }, character(1)), collapse = "")

  figures <- character(0)

  for (nm in names(object$plots)) {

    # Already shown, full width, in the section about the overall picture.
    if (nm %in% c("network", "circos", "circos_blocks")) next

    uri <- .html_plot_uri(object$plots[[nm]], plot_width, plot_height, plot_res)

    if (is.null(uri)) next

    figures <- c(figures, paste0(
      "<figure><img loading='lazy' src='", uri, "' alt='",
      .html_escape(nm), "'><figcaption>",
      .html_escape(gsub("_", " ", nm)), "</figcaption></figure>"
    ))

  }

  paste0(
    "<section id='technical'><h2>", .result_text("technical_title"), "</h2>",
    "<p class='lead'>", .html_escape(.result_text("technical_intro")), "</p>",

    if (length(figures) > 0)
      paste0("<div class='gallery'>", paste(figures, collapse = ""), "</div>")
    else "",

    panels,
    "</section>"
  )

}

# -----------------------------------------------------------------------------
# Document assembly
# -----------------------------------------------------------------------------

#' Assemble full HTML analysis result report
#'
#' @keywords internal
#' @noRd
.report_html_result <- function(object,
                                audience = "general",
                                plot_width = 900,
                                plot_height = 560,
                                plot_res = 110,
                                title = "CausalMultiOmics analysis report") {

  parts <- list(
    overview = .html_result_overview(object),
    howto = .html_result_howto(object),
    findings = .html_result_findings(object),
    actionable = .html_result_actionable(object),
    pathways = .html_result_pathways(object),
    modules = .html_result_modules(object, plot_width, plot_height, plot_res),
    consensus = .html_result_consensus(object, plot_width, plot_height,
                                       plot_res),
    network = .html_result_network(object, plot_width, plot_height, plot_res),
    importance = .html_result_importance(object, plot_width, plot_height, plot_res),
    methods = .html_result_methods(object),
    provenance = .html_result_provenance(object),
    data = .html_result_data(object),
    limitations = .html_result_limitations(object)
  )

  if (identical(audience, "technical")) {

    parts$technical <- .html_result_technical(
      object, plot_width, plot_height, plot_res
    )

  }

  labels <- c(
    overview = "Summary", howto = "How to read this", findings = "Findings",
    actionable = "What to change", pathways = "Chains",
    modules = "Things measured many times",
    consensus = "Would this repeat?",
    network = "The big picture",
    importance = "What matters most", methods = "How it was done",
    provenance = "Where the numbers come from",
    data = "Data used", limitations = "Limitations",
    technical = "Technical detail"
  )

  nav <- paste0(
    vapply(seq_along(parts), function(i) {
      id <- names(parts)[i]
      paste0("<button class='", if (i == 1L) "active" else "",
             "' onclick=\"cmoShow('", id, "',this)\">",
             .html_escape(labels[[id]]), "</button>")
    }, character(1)),
    collapse = ""
  )

  generated <- format(
    if (!is.null(object$timestamp)) object$timestamp else Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  paste0(
    "<!DOCTYPE html>\n<html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", .html_escape(title), "</title>",
    .html_style(),
    .result_style(),
    "</head><body>",

    "<header><h1>", .html_escape(title), "</h1>",
    "<div class='sub'>Outcome: <b>",
    .html_escape(.report_or(object$outcome$name, "-")),
    "</b> &middot; generated ", .html_escape(generated),
    "</div><nav>", nav, "</nav></header>",

    "<main>", paste(unlist(parts), collapse = ""), "</main>",

    "<div id='lightbox'><img alt='Enlarged figure'></div>",

    "<footer>Produced by CausalMultiOmics. This file is self-contained: ",
    "no internet connection is needed to read it.</footer>",

    .html_script(),
    "</body></html>"
  )

}

# -----------------------------------------------------------------------------
# report.CMOResult()
# -----------------------------------------------------------------------------

#' Readable report for an analysis result
#'
#' Turns a \code{CMOResult} into a self-contained HTML document written for a
#' reader who is not a statistician: findings first, in plain language, with
#' the technical tables tucked behind \code{audience = "technical"}.
#'
#' The report states throughout what the numbers do and do not support. A
#' reader who does not know what an adjusted association is will otherwise
#' read a high score as proof of cause, so every finding carries a plain
#' label for how much can be claimed, and the reasoning behind that label is
#' explained in its own section rather than buried in a footnote.
#'
#' @param object A \code{CMOResult} object.
#' @param file Destination path for the HTML report. May be a file name or a
#'   directory. When \code{NULL} the user is prompted in an interactive
#'   session and the working directory is used otherwise.
#' @param format Either \code{"html"} (the default) or \code{"console"} for a
#'   plain-text summary.
#' @param audience \code{"general"} (the default) writes for a non-specialist
#'   reader. \code{"technical"} adds a section with the complete tables and
#'   every diagnostic figure.
#' @param open Whether to open the saved report in a browser.
#' @param prompt Whether to ask for a destination when \code{file} is
#'   \code{NULL}. Set to \code{FALSE} for unattended scripts.
#' @param plot_width,plot_height,plot_res Pixel dimensions and resolution used
#'   when rasterizing the stored plots into the document.
#' @param quiet Whether to suppress progress messages.
#' @param ... Ignored.
#'
#' @return The analysis object, invisibly.
#'
#' @seealso \code{\link{analyze}}
#'
#' @exportS3Method report CMOResult

report.CMOResult <- function(object,
                             file = NULL,
                             format = c("html", "console"),
                             audience = c("general", "technical"),
                             open = interactive(),
                             prompt = TRUE,
                             plot_width = 900,
                             plot_height = 560,
                             plot_res = 110,
                             quiet = FALSE,
                             ...) {

  format <- match.arg(format)
  audience <- match.arg(audience)

  # ===========================================================================
  # Console output
  # ===========================================================================

  if (identical(format, "console")) {

    width <- 78

    .report_title("CausalMultiOmics - Analysis Report", width)

    cat("\n")
    cat(sprintf("  %-20s %s\n", "Outcome", .report_or(object$outcome$name, "-")))
    cat(sprintf("  %-20s %s\n", "Design", .report_or(object$design$type, "-")))
    cat(sprintf("  %-20s %s\n", "Samples",
                fmt_num(object$performance$samples, 0)))
    cat(sprintf("  %-20s %s\n", "Relationships",
                fmt_num(object$performance$edges_integrated, 0)))

    .report_section("What was found", width)
    .report_bullets(object$interpretation$statements, width = width)

    .report_section("Strongest relationships", width)

    drivers <- object$interpretation$drivers

    if (is.data.frame(drivers) && nrow(drivers) > 0) {

      for (i in seq_len(nrow(drivers))) {

        row <- drivers[i, ]

        cat(sprintf(
          "  %-52s %5.1f  %s\n",
          .result_plain_direction(row$direction, row$source,
                                   object$outcome$name,
                                   .feature_encoding(object, row$source)),
          row$evidence_score,
          .result_identification_label(row$identification)
        ))

      }

    } else {

      cat("  ", .result_text("findings_none"), "\n", sep = "")

    }

    if (nrow(object$causal_paths) > 0) {

      .report_section("Chains", width)

      for (i in seq_len(min(5, nrow(object$causal_paths)))) {
        cat(sprintf("  %-58s %5.1f\n",
                    object$causal_paths$path[i],
                    object$causal_paths$weakest_link[i]))
      }

    }

    .report_section("Limitations", width)
    .report_bullets(object$report$limitations, width = width)

    cat("\n")
    .report_rule(width, "=")
    cat("\n")

    return(invisible(object))

  }

  # ===========================================================================
  # HTML output
  # ===========================================================================

  path <- .report_destination(
    file = file, prompt = prompt,
    default_name = "CausalMultiOmics_analysis_report.html"
  )

  if (!isTRUE(quiet)) cat("Building HTML report...\n")

  html <- .report_html_result(
    object,
    audience = audience,
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

  if (isTRUE(open)) try(utils::browseURL(path), silent = TRUE)

  attr(object, "report_path") <- path

  invisible(object)

}

# =============================================================================
# Taking the graph elsewhere
# =============================================================================

#' Write the evidence graph in a format another tool can open
#'
#' The graph is the deliverable for a good many users, and most of them will
#' want to lay it out somewhere this package has no business trying to be.
#' Cytoscape and Gephi both read GraphML, which is the default.
#'
#' Every edge attribute travels with it, so the evidence survives the export:
#' the score, the three components it is made of, the identification, the
#' data quality and the FDR are all readable in the receiving tool. A graph
#' exported with only its arrows would arrive stripped of everything that
#' made it worth reading.
#'
#' @param object A \code{CMOResult}.
#' @param file Where to write. The extension is not checked against
#'   \code{format}; the format argument decides.
#' @param format One of \code{"graphml"} (Cytoscape, Gephi), \code{"dot"}
#'   (Graphviz) or \code{"json"}. The first two go through \pkg{igraph};
#'   \code{"json"} is written directly and needs nothing installed.
#'
#'   GML and Pajek are deliberately absent. Pajek carries no edge attributes,
#'   so a graph exported to it arrives as arrows with none of the evidence
#'   that made it worth exporting, and igraph's GML writer rejects the vertex
#'   table this package produces. A format that silently discards the answer
#'   or fails outright is worse than one that is not offered.
#' @param min_score Relationships scoring below this are left out. The
#'   default keeps everything.
#'
#' @return The path, invisibly.
#'
#' @examples
#' \donttest{
#' sim <- simulate_data(n = 100, blocks = list(a = 4))
#' prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)
#' res <- analyze(prep, "y", methods = "association", effort = "fast",
#'                plots = FALSE, quiet = TRUE)
#'
#' export_graph(res, file.path(tempdir(), "graph.graphml"))
#' }
#'
#' @export

export_graph <- function(object, file,
                         format = c("graphml", "dot", "json"),
                         min_score = 0) {

  format <- match.arg(format)

  if (!inherits(object, "CMOResult")) {
    stop("'object' must be a CMOResult object.", call. = FALSE)
  }

  if (missing(file) || !is.character(file) || length(file) != 1) {
    stop("'file' must be a single path to write to.", call. = FALSE)
  }

  edges <- object$graph$edges

  if (!is.data.frame(edges) || nrow(edges) == 0) {
    stop("This result has no relationships to export.", call. = FALSE)
  }

  edges <- edges[.report_or(edges$evidence_score, 0) >= min_score, ,
                 drop = FALSE]

  if (nrow(edges) == 0) {
    stop(sprintf("No relationship scores %s or more.", format(min_score)),
         call. = FALSE)
  }

  nodes <- object$graph$nodes
  nodes <- nodes[nodes$name %in% c(edges$source, edges$target), , drop = FALSE]

  if (identical(format, "json")) {

    .export_graph_json(nodes, edges, object, file)

    return(invisible(file))

  }

  if (!requireNamespace("igraph", quietly = TRUE)) {

    stop(paste0("Writing ", format, " needs the 'igraph' package.\n",
                "  install.packages(\"igraph\"), or use format = \"json\"."),
         call. = FALSE)

  }

  # Rebuilt from the tables rather than reusing object$graph$igraph, because
  # that one was built before any filtering and carries every edge.

  carried <- edges[, c("source", "target",
                       intersect(c("evidence_score", "strength", "confidence",
                                   "consistency", "data_quality", "fdr",
                                   "identification", "level_label",
                                   "direction", "n_methods", "cross_block"),
                                 names(edges)))]

  # Logicals become 0 and 1 here rather than being converted by the writer,
  # which does it anyway and warns about it once per attribute.

  for (col in names(carried)) {
    if (is.logical(carried[[col]])) carried[[col]] <- as.integer(carried[[col]])
  }

  g <- igraph::graph_from_data_frame(d = carried, vertices = nodes,
                                     directed = TRUE)

  igraph::write_graph(g, file = file, format = format)

  invisible(file)

}

#' Write the graph as JSON, without needing igraph
#'
#' Hand-rolled rather than pulling in a JSON package for one function. The
#' structure is nodes and links, which is what most JavaScript graph
#' libraries expect to be handed.
#'
#' @param nodes,edges The filtered tables.
#' @param object The result, for the provenance block.
#' @param file Where to write.
#'
#' @return Nothing.
#' @keywords internal
#' @noRd
.export_graph_json <- function(nodes, edges, object, file) {

  quote_json <- function(x) {

    if (is.na(x)) return("null")

    if (is.numeric(x) || is.logical(x))
      return(if (is.logical(x)) tolower(as.character(x)) else
        format(x, scientific = FALSE, trim = TRUE))

    paste0("\"", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", x)), "\"")

  }

  row_json <- function(row) {
    paste0("{", paste(sprintf("\"%s\": %s", names(row),
                              vapply(row, quote_json, character(1))),
                      collapse = ", "), "}")
  }

  as_rows <- function(df) {
    paste(vapply(seq_len(nrow(df)),
                 function(i) row_json(as.list(df[i, , drop = FALSE])),
                 character(1)),
          collapse = ",\n    ")
  }

  writeLines(c(
    "{",
    sprintf("  \"outcome\": %s,", quote_json(.report_or(object$outcome$name,
                                                        NA_character_))),
    sprintf("  \"generated\": %s,",
            quote_json(format(.report_or(object$timestamp, Sys.time())))),
    sprintf("  \"package\": \"CausalMultiOmics %s\",",
            as.character(utils::packageVersion("CausalMultiOmics"))),
    "  \"nodes\": [",
    paste0("    ", as_rows(nodes)),
    "  ],",
    "  \"links\": [",
    paste0("    ", as_rows(edges)),
    "  ]",
    "}"
  ), con = file)

  invisible(NULL)

}
