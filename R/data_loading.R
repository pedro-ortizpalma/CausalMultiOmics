# =============================================================================
# LOAD DATA
# =============================================================================

#' Does a block carry real sample identifiers?
#'
#' A matrix without rownames and a data.frame with automatic row names both
#' come out of \code{as.data.frame()} labelled "1", "2", ..., which is
#' indistinguishable from identifiers the user chose deliberately. That
#' matters: two unrelated blocks would then appear to share every sample, the
#' overlap matrix would be fabricated, and metadata matching would join on
#' positions instead of samples. The check has to run on the original object,
#' before conversion, because afterwards the information is gone.
#'
#' \code{.row_names_info()} returns a negative row count exactly when the row
#' names are the automatic compact-integer kind.
#'
#' @param x A matrix or data.frame.
#'
#' @return \code{TRUE} when the block has usable sample identifiers.
#'
#' @keywords internal

.has_sample_ids <- function(x){

  if(is.matrix(x))
    return(!is.null(rownames(x)))

  if(is.data.frame(x))
    return(.row_names_info(x) > 0)

  FALSE

}

#' Load Multi-Block Data
#'
#' Creates a MultiOmicsData object from one or more data blocks.
#'
#' Each element of \code{assays} represents an independent block of variables.
#' Blocks may correspond to any data modality (omics, imaging, clinical,
#' environmental, wearable devices, etc.) and are not required to contain the
#' same samples.
#'
#' This function only imports and organizes the data. Validation,
#' harmonization, preprocessing and integration are performed by later
#' functions in the analysis workflow.
#'
#' Every block must carry sample identifiers in its row names. They are what
#' links blocks to each other and to \code{metadata}, so a block that relies
#' on automatic row names is rejected rather than silently labelled by
#' position.
#'
#' Blocks may optionally declare their assay \code{modality}. The statistical
#' type of a block can be detected from the numbers, but the modality cannot:
#' RNA-seq counts and any other counts look identical, and yet they call for
#' different normalization families. Declaring it lets \code{check_data()}
#' recommend modality-appropriate methods; leaving it out simply falls back
#' to type-driven defaults.
#'
#' @param assays Named list of matrices or data frames, each with sample
#'   identifiers as row names.
#' @param metadata Optional sample metadata. A \code{sample_id} column is
#'   used to match rows against the identifiers found in \code{assays}.
#' @param modality Optional named character vector declaring the assay
#'   modality of each block, for example
#'   \code{c(rna = "rnaseq", prot = "proteomics")}. Names must match names in
#'   \code{assays}; blocks left out are recorded as \code{"unknown"}.
#'   Recognised values are \code{"rnaseq"}, \code{"proteomics"},
#'   \code{"metabolomics"}, \code{"microbiome"}, \code{"methylation"},
#'   \code{"clinical"}, \code{"imaging"} and \code{"other"}. Any other string
#'   is stored but behaves like \code{"unknown"}.
#'
#' @return
#' A MultiOmicsData object.
#'
#' @examples
#' tr <- data.frame(
#'   Gene1 = rnorm(10),
#'   Gene2 = rnorm(10),
#'   row.names = paste0("S",1:10)
#' )
#'
#' pr <- data.frame(
#'   Prot1 = rnorm(8),
#'   Prot2 = rnorm(8),
#'   row.names = paste0("S",3:10)
#' )
#'
#' x <- load_data(
#'   assays = list(
#'     transcriptomics = tr,
#'     proteomics = pr
#'   )
#' )
#'
#' @export

load_data <- function(
    assays,
    metadata = NULL,
    modality = NULL){

  # ===========================================================================
  # Validate input
  # ===========================================================================

  if(missing(assays))
    stop("'assays' must be supplied.")

  if(!is.list(assays))
    stop("'assays' must be a list.")

  if(length(assays) == 0)
    stop("'assays' is empty.")

  if(is.null(names(assays)))
    stop("'assays' must be a named list.")

  if(any(names(assays) == ""))
    stop("Every assay must have a name.")

  # ===========================================================================
  # Remove NULL blocks
  # ===========================================================================

  assays <- assays[!vapply(assays,is.null,logical(1))]

  if(length(assays) == 0)
    stop("No valid data blocks were supplied.")

  # ===========================================================================
  # Convert every block to data.frame
  # ===========================================================================

  # Whether a block carries real identifiers can only be established on the
  # original object: as.data.frame() hands out positional row names and the
  # distinction is lost. The verdict is recorded here but acted on further
  # down, so that a block which is simply empty reports that first.

  has_ids <- vapply(assays, .has_sample_ids, logical(1))

  assays <- lapply(

    assays,

    function(x){

      if(!(is.matrix(x) || is.data.frame(x))){

        stop(
          "Each assay must be either a matrix or a data.frame."
        )

      }

      as.data.frame(
        x,
        check.names = FALSE
      )

    }

  )

  # ===========================================================================
  # Basic validation
  # ===========================================================================

{

    for(i in seq_along(assays)){

      block <- assays[[i]]

      block_name <- names(assays)[i]

      if(nrow(block) == 0){

        stop(
          sprintf(
            "Block '%s' contains no samples.",
            block_name
          )
        )

      }

      if(ncol(block) == 0){

        stop(
          sprintf(
            "Block '%s' contains no variables.",
            block_name
          )
        )

      }

      if(!has_ids[[i]]){

        stop(
          sprintf(
            paste0(
              "Block '%s' has no sample identifiers. Set them before ",
              "calling load_data(), e.g. rownames(x) <- ids.\n",
              "  Without them every block is labelled '1', '2', ..., which ",
              "makes unrelated blocks appear to share all their samples and ",
              "silently breaks matching against metadata."
            ),
            block_name
          )
        )

      }

      if(anyDuplicated(colnames(block))){

        stop(
          sprintf(
            "Duplicated feature names detected in '%s'.",
            block_name
          )
        )

      }

      # Identifiers are known to exist by the has_ids gate above, so only
      # their uniqueness is still open.

      if(anyDuplicated(rownames(block))){

        stop(
          sprintf(
            "Duplicated sample identifiers detected in '%s'.",
            block_name
          )
        )

      }

    }

  }

  # ===========================================================================
  # Sample information
  # ===========================================================================

  sample_info <- do.call(

    rbind,

    lapply(

      names(assays),

      function(block){

        data.frame(

          sample_id = rownames(assays[[block]]),

          block = block,

          stringsAsFactors = FALSE

        )

      }

    )

  )

  rownames(sample_info) <- NULL

  # ===========================================================================
  # Feature information
  # ===========================================================================

  feature_info <- do.call(

    rbind,

    lapply(

      names(assays),

      function(block){

        data.frame(

          feature_id = colnames(assays[[block]]),

          block = block,

          stringsAsFactors = FALSE

        )

      }

    )

  )

  rownames(feature_info) <- NULL

  # ===========================================================================
  # Block summary
  # ===========================================================================

  block_summary <- data.frame(

    block = names(assays),

    samples = vapply(
      assays,
      nrow,
      integer(1)
    ),

    features = vapply(
      assays,
      ncol,
      integer(1)
    ),

    stringsAsFactors = FALSE

  )

  # ===========================================================================
  # Modality
  # ===========================================================================

  block_modality <- stats::setNames(
    rep("unknown", length(assays)),
    names(assays)
  )

  if(!is.null(modality)){

    if(!(is.character(modality) || is.list(modality)))
      stop("'modality' must be a named character vector.")

    modality <- unlist(modality)

    if(is.null(names(modality)) || any(names(modality) == ""))
      stop("'modality' must be named after the blocks it describes.")

    unknown_blocks <- setdiff(names(modality), names(assays))

    if(length(unknown_blocks) > 0){

      stop(
        sprintf(
          "'modality' names blocks that are not in 'assays': %s.",
          paste(unknown_blocks, collapse = ", ")
        )
      )

    }

    block_modality[names(modality)] <- as.character(modality)

  }

  # ===========================================================================
  # Build object
  # ===========================================================================

  object <- MultiOmicsData()

  object$assays <- assays

  object$modality <- block_modality

  object$metadata <- metadata

  object$sample_info <- sample_info

  object$feature_info <- feature_info

  object$preprocessing <- list()

  # MultiOmicsData() declares history as character(), and summary() prints it
  # as a plain log. Storing a nested list here made it render as deparsed
  # R code, so the readable line stays in history and the structured record
  # of the call moves to misc.

  timestamp <- Sys.time()

  object$history <- sprintf(
    "load_data(): %d block(s), %d sample(s), %d feature(s) [%s]",
    length(assays),
    length(unique(sample_info$sample_id)),
    nrow(feature_info),
    format(timestamp, "%Y-%m-%d %H:%M:%S")
  )

  object$misc <- list(

    block_summary = block_summary,

    load_data = list(

      timestamp = timestamp,

      n_blocks = length(assays),

      summary = block_summary

    )

  )

  return(object)

}
