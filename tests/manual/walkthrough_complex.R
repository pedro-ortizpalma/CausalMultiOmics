# =============================================================================
# CausalMultiOmics - COMPLETE WALKTHROUGH
# =============================================================================
#
# Run this to watch the whole package work, end to end. Unlike the testthat
# suite, which hides output and only reports pass or fail, this prints
# everything: every object, every method, every step of the pipeline, and what
# happens when you misuse it on purpose.
#
# It is also meant to be read. Someone who has never done this kind of
# analysis should be able to follow the transcript from the top and come out
# understanding not just which function to call but what each step is for,
# what it can and cannot establish, and where the usual mistakes are. The
# explanations are printed alongside the output rather than left in comments,
# because the transcript is what a reader actually sees.
#
# Two kinds of data appear, on purpose.
#
# Real data - the NHANES 1999-2006 exposome release - runs the main pipeline.
# It has the properties that make a multi-block problem real: modules given to
# different subsamples, sentinel codes, wildly different variable types, and
# no known answer.
#
# That last property is why it is not enough. Real data can show the machinery
# running; it cannot show that the machinery is right, because there is
# nothing to check against. So wherever an output only means something
# relative to a known truth - a mediator being caught, a finding that exists
# only because of imputation, an effect present in one sex only, a latent
# process spanning two blocks - a small study is built with that truth planted
# in it, and the section says so.
#
#   Rscript tests/manual/walkthrough.R
#
# or, inside R from the package root:
#
#   source("tests/manual/walkthrough.R")
#
# Run only some sections:
#
#   CMO_SECTIONS <- c(1, 5, 9); source("tests/manual/walkthrough.R")
#
# A full transcript is written to tests/manual/output/ along with every HTML
# report the run produces.
# =============================================================================

CMO_START <- Sys.time()

# -----------------------------------------------------------------------------
# Load the package
# -----------------------------------------------------------------------------

if (!exists("CMO_SECTIONS")) CMO_SECTIONS <- NULL

.loaded <- FALSE

if ("CausalMultiOmics" %in% loadedNamespaces()) {

  .loaded <- TRUE

} else if (requireNamespace("devtools", quietly = TRUE)) {

  for (root in c(".", "..", "../..")) {
    if (file.exists(file.path(root, "DESCRIPTION"))) {
      devtools::load_all(root, export_all = TRUE, quiet = TRUE)
      .loaded <- TRUE
      break
    }
  }

}

if (!.loaded) {
  .loaded <- suppressWarnings(require("CausalMultiOmics", quietly = TRUE))
}

if (!.loaded) {
  stop("Could not load CausalMultiOmics. Run this from the package root.",
       call. = FALSE)
}

# Internals are exercised too, so they have to be visible. devtools::load_all()
# with export_all = TRUE gives that; a plain library() does not.
.internals_visible <- exists(".safe_try")

# Pasting this file into a session still holding an older build produces a
# spray of "could not find function" and "unused argument" errors that look
# like faults in the package. Checking once, up front, turns that into a
# single sentence naming the real cause.

.expected <- c("load_data", "check_data", "preprocess",
               "apply_preprocessing", "analyze", "explain",
               "counterfactual", "annotate_evidence", "report")

.absent <- .expected[!vapply(.expected, exists, logical(1))]

.stale <- length(.absent) > 0 ||
  !all(c("effort", "min_per_block", "modifiable") %in%
       names(formals(analyze)))

if (.stale) {

  stop(
    "The loaded CausalMultiOmics is older than this script.\n",
    if (length(.absent) > 0)
      paste0("  missing: ", paste(.absent, collapse = ", "), "\n") else "",
    "  Reload it:  devtools::load_all(\".\", export_all = TRUE)",
    call. = FALSE
  )

}

rm(.expected, .absent, .stale)

# -----------------------------------------------------------------------------
# Transcript
# -----------------------------------------------------------------------------

.outdir <- file.path("tests", "manual", "output")

if (!dir.exists(.outdir)) dir.create(.outdir, recursive = TRUE, showWarnings = FALSE)

.transcript <- file.path(.outdir, "walkthrough.txt")

.con <- file(.transcript, open = "wt")
sink(.con, split = TRUE)
sink(.con, type = "message")

on.exit({
  sink(type = "message"); sink(); close(.con)
}, add = TRUE)

# =============================================================================
# Helpers
# =============================================================================

.n_ok <- 0L
.n_fail <- 0L
.failures <- character(0)

rule <- function(char = "=", width = 78) cat(strrep(char, width), "\n", sep = "")

section <- function(number, title) {

  if (!is.null(CMO_SECTIONS) && !(number %in% CMO_SECTIONS)) return(FALSE)

  cat("\n\n")
  rule("=")
  cat(sprintf("SECTION %-3s %s\n", number, title))
  rule("=")

  TRUE

}

step <- function(...) {
  cat("\n")
  rule("-")
  cat(..., "\n", sep = "")
  rule("-")
}

note <- function(...) cat("\n  ", ..., "\n", sep = "")

#' A paragraph of explanation, wrapped.
#'
#' The transcript is meant to be readable by someone who has never seen the
#' package, and in places by someone who has never done this kind of analysis
#' at all. Those readers need prose, not a comment in the source they will
#' never open, so the explanations are printed alongside the output they
#' explain.
teach <- function(...) {

  text <- paste0(...)

  cat("\n")

  for (para in strsplit(text, "\n\n", fixed = TRUE)[[1]]) {

    if (!nzchar(trimws(para))) next

    cat(paste(strwrap(gsub("[[:space:]]+", " ", trimws(para)),
                      width = 76, prefix = "  "), collapse = "\n"),
        "\n\n", sep = "")

  }

}

#' A heading for a block of teaching, so the eye can find it in a long log.
lesson <- function(title) {
  cat("\n")
  cat("  ", strrep("~", 74), "\n", sep = "")
  cat("  ", title, "\n", sep = "")
  cat("  ", strrep("~", 74), "\n", sep = "")
}

#' Echo a call the way a user would type it, run it, print what comes back.
show <- function(expr, print_it = TRUE) {

  label <- paste(deparse(substitute(expr)), collapse = "\n  ")

  cat("\n> ", label, "\n", sep = "")

  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        cat("  [warning] ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      cat("  [ERROR] ", conditionMessage(e), "\n", sep = "")
      .n_fail <<- .n_fail + 1L
      .failures <<- c(.failures, label)
      structure(list(msg = conditionMessage(e)), class = "cmo_failed")
    }
  )

  if (inherits(value, "cmo_failed")) return(invisible(NULL))

  .n_ok <<- .n_ok + 1L

  if (print_it && !is.null(value)) print(value)

  invisible(value)

}

#' Assert that a call fails, and show the message the user would see.
must_fail <- function(expr, expect = NULL) {

  label <- paste(deparse(substitute(expr)), collapse = " ")

  cat("\n> ", label, "\n", sep = "")

  err <- tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))

  if (is.null(err)) {

    cat("  [FAIL] expected an error, none was raised\n")
    .n_fail <<- .n_fail + 1L
    .failures <<- c(.failures, paste("no error from:", label))
    return(invisible(FALSE))

  }

  short <- strsplit(err, "\n")[[1]][1]

  if (!is.null(expect) && !grepl(expect, err)) {

    cat("  [FAIL] message did not mention '", expect, "':\n    ", short, "\n", sep = "")
    .n_fail <<- .n_fail + 1L
    .failures <<- c(.failures, paste("wrong message:", label))
    return(invisible(FALSE))

  }

  cat("  [ok] refused: ", short, "\n", sep = "")
  .n_ok <<- .n_ok + 1L

  invisible(TRUE)

}

# =============================================================================
# The data used throughout
# =============================================================================
# Real data when it is available: the NHANES 1999-2006 exposome release from
#
#   Patel CJ et al. (2016). A database of human exposomes and phenomes from
#   the US National Health and Nutrition Examination Survey.
#   Scientific Data 3:160096.  tests/manual/test_data/sdata201696.pdf
#
# It suits this package unusually well. The variables come already sorted into
# categories that behave exactly like blocks, the modules were measured on
# overlapping but different subsets of people, so the sample alignment has real
# work to do, and mortality follow-up gives a genuine survival design, which is
# the only setting where the engine can claim temporal identification.
#
# When the file is absent the script falls back to a small simulated study
# with a mechanism planted in it. That is worth keeping either way: real data
# has no known answer, so it can show the engine running but not that it is
# right.
# =============================================================================

.nhanes_path <- file.path("tests", "manual", "test_data", "nh_99-06.Rdata")

# -----------------------------------------------------------------------------
# The blocks
# -----------------------------------------------------------------------------
# Twenty blocks covering all 1151 unique variables of the release, grouped by
# domain rather than by the 43 native categories: categories that measure the
# same kind of thing on the same kind of scale are merged, so that a block is
# something you can name in a sentence. The partition is exact - every variable
# appears in one block and only one.
#
# The list is deliberately larger than any single run will use. Which variables
# survive is decided below, from the data, by rules that never look at the
# outcome.
# -----------------------------------------------------------------------------

NHANES_BLOCKS <- list(
  # blood: 20 variables
  blood = c(
    "LBDBANO", "LBDEONO", "LBDLYMNO", "LBDMONO", "LBDNENO", "LBXBAPCT",
    "LBXEOPCT", "LBXHCT", "LBXHGB", "LBXLYPCT", "LBXMC", "LBXMCHSI",
    "LBXMCVSI", "LBXMOPCT", "LBXMPSI", "LBXNEPCT", "LBXPLTSI",
    "LBXRBCSI", "LBXRDW", "LBXWBCSI"
  ),

  # biochemistry: 56 variables
  biochemistry = c(
    "LBDHDD", "LBDHDL", "LBDLDL", "LBDP3", "LBDPCT", "LBXAPB", "LBXBAP",
    "LBXCPSI", "LBXCRP", "LBXEPP", "LBXFB", "LBXFER", "LBXGH", "LBXGLT",
    "LBXGLU", "LBXGLUSI", "LBXHCY", "LBXMMA", "LBXP1", "LBXP2",
    "LBXSAL", "LBXSAPSI", "LBXSASSI", "LBXSATSI", "LBXSBU", "LBXSC3SI",
    "LBXSCA", "LBXSCH", "LBXSCLSI", "LBXSCR", "LBXSCRINV", "LBXSGB",
    "LBXSGL", "LBXSGTSI", "LBXSIR", "LBXSKSI", "LBXSNASI", "LBXSOSSI",
    "LBXSPH", "LBXSTB", "LBXSTP", "LBXSTR", "LBXSUA", "LBXTC", "LBXTFR",
    "LBXTIB", "LBXTR", "PHAFSTHR", "PHAFSTMN", "URXCRS", "URXNT",
    "URXUCR", "URXUCRSI", "URXUMA", "URXUMASI", "URXUMS"
  ),

  # hormones: 8 variables
  hormones = c(
    "LBXFSH", "LBXIN", "LBXINSI", "LBXLH", "LBXPT21", "LBXSLDSI",
    "LBXT4", "LBXTSH"
  ),

  # body_measures: 19 variables
  body_measures = c(
    "BMXBMI", "BMXCALF", "BMXHEAD", "BMXHT", "BMXLEG", "BMXRECUM",
    "BMXSUB", "BMXTHICR", "BMXTRI", "BMXWAIST", "BMXWT", "DXDTOBMD",
    "DXDTOFAT", "DXDTOLE", "DXDTRLE", "DXXHEBMD", "DXXLSBMD",
    "DXXPEBMD", "DXXTRFAT"
  ),

  # blood_pressure: 4 variables
  blood_pressure = c(
    "BPXCHR", "BPXPLS", "MDIS", "MSYS"
  ),

  # physical_fitness: 15 variables
  physical_fitness = c(
    "CVDESVO2", "CVDR1DI", "CVDR1HR", "CVDR1SY", "CVDR2DI", "CVDR2HR",
    "CVDR2SY", "CVDS1DI", "CVDS1HR", "CVDS1SY", "CVDS2DI", "CVDS2HR",
    "CVDS2SY", "CVDVOMAX", "physical_activity"
  ),

  # nutrition: 284 variables
  nutrition = c(
    "ALANINE_mg", "ALCOHOL_PERCENT", "ALPHA_TOCOPHEROL_IU",
    "ALPHA_TOCOPHEROL_NA", "ALPHA_TOCOPHEROL_mg", "ARGININE_gm",
    "ARGININE_mg", "BETA_CAROTENE_IU", "BETA_CAROTENE_Unknown",
    "BETA_CAROTENE__", "BETA_CAROTENE_mcg", "BETA_CAROTENE_mg",
    "CAFFEINE_mg", "CALCIUM_PPM", "CALCIUM_Trace", "CALCIUM_Unknown",
    "CALCIUM_mg", "CHOLESTEROL_mg", "COPPER_Trace", "COPPER_Unknown",
    "COPPER_mg", "CYSTINE_mg", "DBD100", "DBQ095", "DIETARY_FIBER_mg",
    "DR1BWATR", "DR1CWATR", "DR1TACAR", "DR1TALCO", "DR1TATOA",
    "DR1TATOC", "DR1TB12A", "DR1TBCAR", "DR1TCAFF", "DR1TCALC",
    "DR1TCARB", "DR1TCARO", "DR1TCHL", "DR1TCHOL", "DR1TCOPP",
    "DR1TCRYP", "DR1TFA", "DR1TFDFE", "DR1TFF", "DR1TFIBE", "DR1TIRON",
    "DR1TKCAL", "DR1TLYCO", "DR1TLZ", "DR1TM161", "DR1TM181",
    "DR1TM201", "DR1TM221", "DR1TMAGN", "DR1TMFAT", "DR1TMOIS",
    "DR1TNIAC", "DR1TP182", "DR1TP183", "DR1TP184", "DR1TP204",
    "DR1TP205", "DR1TP225", "DR1TP226", "DR1TPFAT", "DR1TPHOS",
    "DR1TPOTA", "DR1TPROT", "DR1TRET", "DR1TS040", "DR1TS060",
    "DR1TS080", "DR1TS100", "DR1TS120", "DR1TS140", "DR1TS160",
    "DR1TS180", "DR1TSELE", "DR1TSFAT", "DR1TSODI", "DR1TSUGR",
    "DR1TTFAT", "DR1TTHEO", "DR1TVAIU", "DR1TVARA", "DR1TVARE",
    "DR1TVB1", "DR1TVB12", "DR1TVB2", "DR1TVB6", "DR1TVC", "DR1TVE",
    "DR1TVK", "DR1TWATE", "DR1TZINC", "DR1_320", "DR1_330", "DR1_330Z",
    "DRD320GW", "DRD330GW", "DRD340", "DRD350A", "DRD350AQ", "DRD350B",
    "DRD350BQ", "DRD350C", "DRD350CQ", "DRD350D", "DRD350DQ", "DRD350E",
    "DRD350EQ", "DRD350F", "DRD350FQ", "DRD350G", "DRD350GQ", "DRD350H",
    "DRD350HQ", "DRD350I", "DRD350IQ", "DRD350J", "DRD350JQ", "DRD350K",
    "DRD360", "DRD370A", "DRD370AQ", "DRD370B", "DRD370BQ", "DRD370C",
    "DRD370CQ", "DRD370D", "DRD370DQ", "DRD370E", "DRD370EQ", "DRD370F",
    "DRD370FQ", "DRD370G", "DRD370GQ", "DRD370H", "DRD370HQ", "DRD370I",
    "DRD370IQ", "DRD370J", "DRD370JQ", "DRD370K", "DRD370KQ", "DRD370L",
    "DRD370LQ", "DRD370M", "DRD370MQ", "DRD370N", "DRD370NQ", "DRD370O",
    "DRD370OQ", "DRD370P", "DRD370PQ", "DRD370Q", "DRD370QQ", "DRD370R",
    "DRD370RQ", "DRD370S", "DRD370SQ", "DRD370T", "DRD370TQ", "DRD370U",
    "DRD370UQ", "DRD370V", "DRDCWATR", "DRDDRSTZ", "DRDRESP", "DRDSDT1",
    "DRDSDT2", "DRDSDT3", "DRDSDT4", "DRDSDT5", "DRDSDT6", "DRDSDT7",
    "DRDSDT8", "DRDTSODI", "DSDCOUNT", "FOLIC_ACID_Unknown",
    "FOLIC_ACID_mcg", "GLYCINE_gm", "GLYCINE_mg", "HISTIDINE_mg",
    "IRON_Trace", "IRON_Unknown", "IRON_mg", "ISOLEUCINE_mg",
    "LBDATCSI", "LBDTLY", "LBXACY", "LBXALC", "LBXB12", "LBXBCC",
    "LBXBEC", "LBXCBC", "LBXCLC", "LBXCLZ", "LBXCRY", "LBXDTC",
    "LBXFOL", "LBXGTC", "LBXIRN", "LBXLCC", "LBXLUT", "LBXLUZ",
    "LBXLYC", "LBXPHE", "LBXPHF", "LBXRBF", "LBXRPL", "LBXRST",
    "LBXSEL", "LBXVB6", "LBXVIA", "LBXVIC", "LBXVID", "LBXVIE",
    "LBXZEA", "LEUCINE_mg", "LYSINE_mg", "L_ASPARTIC_ACID_gm",
    "L_ASPARTIC_ACID_mg", "L_GLUTAMINE_gm", "L_GLUTAMINE_mg",
    "MAGNESIUM_PPM", "MAGNESIUM_Unknown", "MAGNESIUM_mg",
    "METHIONINE_mg", "OMEGA_3_FATTY_ACIDS_Unknown",
    "OMEGA_3_FATTY_ACIDS_mg", "OMEGA_6_FATTY_ACIDS_NA",
    "OMEGA_6_FATTY_ACIDS_mg", "OMEGA_9_FATTY_ACIDS_NA",
    "OMEGA_9_FATTY_ACIDS_mg", "OTHER_FATTY_ACIDS_NA",
    "OTHER_FATTY_ACIDS_mg", "OTHER_OMEGA_3_FATTY_ACIDS_NA",
    "PHENYLALANINE_mg", "PHOSPHORUS_Trace", "PHOSPHORUS_Unknown",
    "PHOSPHORUS_mg", "POTASSIUM_mg", "PROLINE_mg", "PROTEIN_gm",
    "RIBOFLAVIN_Unknown", "RIBOFLAVIN_mg", "SELENIUM_Trace",
    "SELENIUM_Unknown", "SELENIUM_mcg", "SERINE_mg", "SODIUM_Trace",
    "SODIUM_mg", "THIAMIN_Unknown", "THIAMIN_mg", "THREONINE_mg",
    "TOTAL_CARBOHYDRATE_gm", "TRYPTOPHAN_mg", "TYROSINE_mg", "URXDAZ",
    "URXDMA", "URXEQU", "URXETD", "URXETL", "URXGNS", "VALINE_mg",
    "VITAMIN_A_IU", "VITAMIN_A_Unknown", "VITAMIN_A_mcg",
    "VITAMIN_A_mg", "VITAMIN_B_12_Unknown", "VITAMIN_B_12_mcg",
    "VITAMIN_B_6_Unknown", "VITAMIN_B_6_mg", "VITAMIN_C_Unknown",
    "VITAMIN_C_mg", "community_supply", "lite_salt", "no_salt",
    "ordinary_salt", "salt_substitute", "spring_water",
    "supplement_count", "well_water"
  ),

  # heavy_metals: 31 variables
  heavy_metals = c(
    "HRDHG", "HRXHG", "LBDDWS", "LBXBCD", "LBXBPB", "LBXDFS", "LBXDFSF",
    "LBXIHG", "LBXTHG", "URXUAB", "URXUAC", "URXUAS", "URXUAS3",
    "URXUAS5", "URXUBA", "URXUBE", "URXUCD", "URXUCO", "URXUCS",
    "URXUDMA", "URXUHG", "URXUIO", "URXUMMA", "URXUMO", "URXUPB",
    "URXUPT", "URXUSB", "URXUTL", "URXUTM", "URXUTU", "URXUUR"
  ),

  # pops: 79 variables
  pops = c(
    "LBD199", "LBX028", "LBX044", "LBX049", "LBX052", "LBX066",
    "LBX074", "LBX087", "LBX099", "LBX101", "LBX105", "LBX110",
    "LBX118", "LBX128", "LBX138", "LBX146", "LBX149", "LBX151",
    "LBX153", "LBX156", "LBX157", "LBX167", "LBX170", "LBX172",
    "LBX177", "LBX178", "LBX180", "LBX183", "LBX187", "LBX189",
    "LBX194", "LBX195", "LBX196", "LBX206", "LBX209", "LBXBB1",
    "LBXBR1", "LBXBR2", "LBXBR3", "LBXBR4", "LBXBR5", "LBXBR6",
    "LBXBR66", "LBXBR66L", "LBXBR7", "LBXBR8", "LBXBR9", "LBXD01",
    "LBXD02", "LBXD03", "LBXD04", "LBXD05", "LBXD07", "LBXEPAH",
    "LBXF01", "LBXF02", "LBXF03", "LBXF04", "LBXF05", "LBXF06",
    "LBXF07", "LBXF08", "LBXF09", "LBXF10", "LBXHXC", "LBXMPAH",
    "LBXPCB", "LBXPFBS", "LBXPFDE", "LBXPFDO", "LBXPFHP", "LBXPFHS",
    "LBXPFNA", "LBXPFOA", "LBXPFOS", "LBXPFSA", "LBXPFUA", "LBXTC2",
    "LBXTCD"
  ),

  # pesticides: 73 variables
  pesticides = c(
    "LBXALD", "LBXBHC", "LBXDIE", "LBXEND", "LBXGHC", "LBXHCB",
    "LBXHPE", "LBXMIR", "LBXODT", "LBXOXY", "LBXPDE", "LBXPDT",
    "LBXTNA", "URX14D", "URX1TB", "URX24D", "URX25T", "URX3TB",
    "URX4FP", "URXAAZ", "URXACE", "URXAPE", "URXATZ", "URXBSM",
    "URXCB3", "URXCBF", "URXCCC", "URXCHS", "URXCMH", "URXCPM",
    "URXDAM", "URXDCB", "URXDCZ", "URXDEE", "URXDIZ", "URXDPY",
    "URXDTZ", "URXEMM", "URXETU", "URXFRM", "URXHLS", "URXMAL",
    "URXMET", "URXMMI", "URXMSM", "URXMTM", "URXMTO", "URXNOS",
    "URXOMO", "URXOP1", "URXOP2", "URXOP3", "URXOP4", "URXOP5",
    "URXOP6", "URXOPM", "URXOPP", "URXOXS", "URXPAR", "URXPCP",
    "URXPIM", "URXPPX", "URXPRO", "URXPTU", "URXRIM", "URXSIS",
    "URXSMM", "URXSSF", "URXTCC", "URXTHF", "URXTRA", "URXTRN", "URXTRS"
  ),

  # vocs: 84 variables
  vocs = c(
    "LBX2DF", "LBXACR", "LBXGLY", "LBXV1A", "LBXV1D", "LBXV1E",
    "LBXV2A", "LBXV2C", "LBXV2E", "LBXV2P", "LBXV2T", "LBXV3A",
    "LBXV3B", "LBXV4A", "LBXV4C", "LBXV4E", "LBXV4T", "LBXVBF",
    "LBXVBM", "LBXVBZ", "LBXVCB", "LBXVCF", "LBXVCM", "LBXVCT",
    "LBXVDB", "LBXVDM", "LBXVDP", "LBXVEB", "LBXVHE", "LBXVMC",
    "LBXVME", "LBXVNB", "LBXVOX", "LBXVST", "LBXVTC", "LBXVTE",
    "LBXVTO", "LBXVXY", "LBXWBF", "LBXWBM", "LBXWCF", "LBXWCM",
    "LBXWIO", "LBXWME", "LBXWNO", "LBXWP8", "LBXZBZ", "LBXZCF",
    "LBXZDB", "LBXZEB", "LBXZMB", "LBXZOX", "LBXZTE", "LBXZTI",
    "LBXZTO", "LBXZXY", "SSMEL", "URXNO3", "URXP01", "URXP02", "URXP03",
    "URXP04", "URXP05", "URXP06", "URXP07", "URXP08", "URXP09",
    "URXP10", "URXP11", "URXP12", "URXP13", "URXP14", "URXP15",
    "URXP16", "URXP17", "URXP18", "URXP19", "URXP20", "URXP21",
    "URXP22", "URXP24", "URXSCN", "URXUP8", "URXUP8CA"
  ),

  # plastics_phenols: 22 variables
  plastics_phenols = c(
    "URX4TO", "URXBP3", "URXBPH", "URXBUP", "URXCNP", "URXCOP",
    "URXECP", "URXEPB", "URXMBP", "URXMC1", "URXMCP", "URXMEP",
    "URXMHH", "URXMHP", "URXMIB", "URXMNM", "URXMNP", "URXMOH",
    "URXMOP", "URXMPB", "URXMZP", "URXPPB"
  ),

  # infections: 69 variables
  infections = c(
    "LBDC1", "LBDC2", "LBDHBG", "LBDHCV", "LBDHD", "LBDHI", "LBDRUIU",
    "LBDSY3", "LBDSY4", "LBXBV", "LBXCD1", "LBXCD4", "LBXCD8", "LBXCH1",
    "LBXDY1", "LBXETA", "LBXETB", "LBXETC", "LBXETD", "LBXETH", "LBXHA",
    "LBXHBC", "LBXHBS", "LBXHE1", "LBXHE2", "LBXHP1", "LBXM1", "LBXMC1",
    "LBXMD1", "LBXME", "LBXME1", "LBXMF1", "LBXMG1", "LBXMI1", "LBXML1",
    "LBXMO1", "LBXMP1", "LBXMR1", "LBXMS1", "LBXMT1", "LBXMY1",
    "LBXMZ1", "LBXPVL", "LBXS06MK", "LBXS11MK", "LBXS16MK", "LBXS18MK",
    "LBXSY1", "LBXTO1", "LBXTO2", "LBXTO3", "LBXTO5", "LBXTSS", "LBXTV",
    "LBXVAR", "SXQ260", "SXQ265", "SXQ270", "TBQ020", "TBQ030",
    "TBQ040", "TBQ050", "TBQ060", "URXUCL", "URXUGC", "VARICELL",
    "hepa", "hepb", "pneu"
  ),

  # allergens: 20 variables
  allergens = c(
    "LBXE72", "LBXE74", "LBXF13", "LBXF24", "LBXID1", "LBXID2",
    "LBXIE1", "LBXIE5", "LBXIF1", "LBXIF2", "LBXIG2", "LBXIG5",
    "LBXIGE", "LBXII6", "LBXIM3", "LBXIM6", "LBXIT3", "LBXIT7",
    "LBXIW1", "LBXW11"
  ),

  # medication: 221 variables
  medication = c(
    "99999", "ACETAMINOPHEN__CODEINE",
    "ACETAMINOPHEN__CODEINE_PHOSPHATE", "ACETAMINOPHEN__HYDROCODONE",
    "ACETAMINOPHEN__HYDROCODONE_BITARTRATE", "ACETAMINOPHEN__OXYCODONE",
    "ACETAMINOPHEN__OXYCODONE_HYDROCHLORIDE",
    "ACETAMINOPHEN__PROPOXYPHENE",
    "ACETAMINOPHEN__PROPOXYPHENE_NAPSYLATE", "ALBUTEROL",
    "ALBUTEROL__IPRATROPIUM", "ALENDRONATE", "ALENDRONATE_SODIUM",
    "ALLOPURINOL", "ALPRAZOLAM", "AMITRIPTYLINE",
    "AMITRIPTYLINE_HYDROCHLORIDE", "AMLODIPINE", "AMLODIPINE_BESYLATE",
    "AMLODIPINE__BENAZEPRIL", "AMOXICILLIN",
    "AMOXICILLIN_TRIHYDRATE__CLAVULANATE_POTASSIUM",
    "AMOXICILLIN__CLAVULANATE", "AMPHETAMINE_ASPARTATE",
    "AMPHETAMINE__DEXTROAMPHETAMINE", "ANTIBIOTIC_UNSPECIFIED",
    "ASPIRIN", "ATENOLOL", "ATORVASTATIN", "ATORVASTATIN_CALCIUM",
    "AZITHROMYCIN", "AZITHROMYCIN_DIHYDRATE",
    "BECLOMETHASONE_DIPROPIONATE", "BENAZEPRIL",
    "BENAZEPRIL_HYDROCHLORIDE", "BUDESONIDE", "BUPROPION",
    "BUPROPION_HYDROCHLORIDE", "CAPTOPRIL", "CARVEDILOL", "CELECOXIB",
    "CEPHALEXIN", "CETIRIZINE", "CETIRIZINE_HYDROCHLORIDE",
    "CIMETIDINE_HYDROCHLORIDE", "CITALOPRAM", "CITALOPRAM_HYDROBROMIDE",
    "CLARITHROMYCIN", "CLONAZEPAM", "CLONIDINE", "CLOPIDOGREL",
    "CLOPIDOGREL_BISULFATE", "CONJUGATED_ESTROGENS", "CROMOLYN_SODIUM",
    "CYCLOBENZAPRINE", "CYCLOBENZAPRINE_HYDROCHLORIDE", "DESLORATADINE",
    "DIAZEPAM", "DIGOXIN", "DILTIAZEM", "DILTIAZEM_HYDROCHLORIDE",
    "DIVALPROEX_SODIUM", "DOXAZOSIN", "DOXAZOSIN_MESYLATE", "ENALAPRIL",
    "ENALAPRIL_MALEATE", "ESCITALOPRAM", "ESOMEPRAZOLE", "ESTRADIOL",
    "ESTROGENS__CONJUGATED",
    "ESTROGENS__CONJUGATED__MEDROXYPROGESTERONE_ACETATE",
    "ETHINYL_ESTRADIOL__LEVONORGESTREL",
    "ETHINYL_ESTRADIOL__NORETHINDRONE",
    "ETHINYL_ESTRADIOL__NORGESTIMATE", "EZETIMIBE",
    "EZETIMIBE__SIMVASTATIN", "FELODIPINE", "FENOFIBRATE",
    "FEXOFENADINE", "FEXOFENADINE_HYDROCHLORIDE", "FLUOXETINE",
    "FLUOXETINE_HYDROCHLORIDE", "FLUTICASONE", "FLUTICASONE_NASAL",
    "FLUTICASONE_PROPIONATE", "FLUTICASONE__SALMETEROL",
    "FLUVASTATIN_SODIUM", "FOSINOPRIL", "FOSINOPRIL_SODIUM",
    "FUROSEMIDE", "GABAPENTIN", "GEMFIBROZIL", "GLIMEPIRIDE",
    "GLIPIZIDE", "GLYBURIDE", "GLYBURIDE__METFORMIN",
    "HYDROCHLOROTHIAZIDE", "HYDROCHLOROTHIAZIDE__LISINOPRIL",
    "HYDROCHLOROTHIAZIDE__LOSARTAN",
    "HYDROCHLOROTHIAZIDE__LOSARTAN_POTASSIUM",
    "HYDROCHLOROTHIAZIDE__TRIAMTERENE",
    "HYDROCHLOROTHIAZIDE__VALSARTAN", "HYDROCODONE_UNSPECIFIED",
    "HYDROXYZINE", "HYDROXYZINE_HYDROCHLORIDE", "IBUPROFEN", "INSULIN",
    "INSULIN_GLARGINE", "INSULIN_ISOPHANE", "IPRATROPIUM_BROMIDE",
    "IRBESARTAN", "ISOSORBIDE_MONONITRATE", "ISOSORBIDE_UNSPECIFIED",
    "LANSOPRAZOLE", "LATANOPROST", "LATANOPROST_OPHTHALMIC",
    "LEVOTHYROXINE", "LEVOTHYROXINE_SODIUM", "LISINOPRIL", "LORATADINE",
    "LORATADINE__PSEUDOEPHEDRINE_SULFATE", "LORAZEPAM", "LOSARTAN",
    "LOSARTAN_POTASSIUM", "LOVASTATIN", "MECLIZINE_HYDROCHLORIDE",
    "MEDROXYPROGESTERONE_ACETATE", "METFORMIN",
    "METFORMIN_HYDROCHLORIDE", "METHYLPHENIDATE",
    "METHYLPHENIDATE_HYDROCHLORIDE", "METOPROLOL",
    "METOPROLOL_SUCCINATE", "METOPROLOL_TARTRATE",
    "METOPROLOL_UNSPECIFIED", "MOMETASONE_FUROATE_MONOHYDRATE",
    "MOMETASONE_NASAL", "MONTELUKAST", "MONTELUKAST_SODIUM", "NAPROXEN",
    "NIFEDIPINE", "NITROGLYCERIN", "OMEPRAZOLE", "OXYBUTYNIN",
    "PANTOPRAZOLE", "PANTOPRAZOLE_SODIUM", "PAROXETINE",
    "PAROXETINE_HYDROCHLORIDE", "PENICILLIN", "PHENYTOIN_SODIUM",
    "PIOGLITAZONE", "POLYETHYLENE_GLYCOL_3350", "POTASSIUM_CHLORIDE",
    "PRAVASTATIN", "PRAVASTATIN_SODIUM", "PREDNISONE", "PROPRANOLOL",
    "PROPRANOLOL_HYDROCHLORIDE", "QUINAPRIL", "QUINAPRIL_HYDROCHLORIDE",
    "RABEPRAZOLE", "RABEPRAZOLE_SODIUM", "RALOXIFENE_HYDROCHLORIDE",
    "RAMIPRIL", "RANITIDINE", "RANITIDINE_HYDROCHLORIDE", "RHQ510",
    "RHQ520", "RHQ540", "RHQ554", "RHQ556", "RHQ558", "RHQ562",
    "RHQ564", "RHQ566", "RHQ570", "RHQ572", "RHQ574", "RHQ580",
    "RHQ582", "RHQ584", "RHQ596", "RHQ598", "RHQ600", "ROFECOXIB",
    "ROSIGLITAZONE", "ROSIGLITAZONE_MALEATE", "SALMETEROL_XINAFOATE",
    "SERTRALINE", "SERTRALINE_HYDROCHLORIDE", "SIMVASTATIN",
    "SPIRONOLACTONE", "SULFAMETHOXAZOLE__TRIMETHOPRIM", "TAMSULOSIN",
    "TERAZOSIN", "TERAZOSIN_HYDROCHLORIDE", "THEOPHYLLINE",
    "TOLTERODINE_TARTRATE", "TRAMADOL", "TRAZODONE",
    "TRAZODONE_HYDROCHLORIDE", "TRIAMCINOLONE_ACETONIDE", "TRIAMTERENE",
    "VALDECOXIB", "VALSARTAN", "VENLAFAXINE",
    "VENLAFAXINE_HYDROCHLORIDE", "VERAPAMIL", "VERAPAMIL_HYDROCHLORIDE",
    "WARFARIN", "WARFARIN_SODIUM", "ZOLPIDEM", "ZOLPIDEM_TARTRATE",
    "age_started_birth_control", "age_stopped_birth_control",
    "how_long_estrogen", "how_long_estrogen_patch",
    "how_long_estrogen_progestin", "how_long_estrogen_progestin_patch",
    "how_long_progestin", "taking_birth_control"
  ),

  # disease_history: 40 variables
  disease_history = c(
    "any_cancer_self_report", "any_diabetes", "any_family_cad",
    "any_ht", "bladder_cancer_self_report", "blood_cancer_self_report",
    "bone_cancer_self_report", "brain_cancer_self_report",
    "breast_cancer_self_report", "cad", "cervix_cacner_self_report",
    "colon_cancer_self_report", "current_asthma",
    "esophagus_cancer_self_report", "ever_arthritis", "ever_asthma",
    "ever_osteo_arthritis", "ever_rheumatoid_arthritis",
    "gallbladder_cancer_self_report", "kidney_cancer_self_report",
    "larynx_cancer_self_report", "leukemia_self_report",
    "liver_cancer_self_report", "lung_cancer_self_report",
    "lymphoma_self_report", "melanoma_self_report",
    "mouth_cancer_self_report", "nervous_cancer_self_report",
    "other_cancer_self_report", "other_skin_cancer_self_report",
    "ovarian_cancer_self_report", "pancreatic_cancer_self_report",
    "prostate_cancer_self_report", "rectum_cancer_self_report",
    "skin_cancer_self_report", "soft_cancer_self_report",
    "stomach_cancer_self_report", "testis_cancer_self_report",
    "thyroid_cancer_self_report", "uterine_cancer_self_report"
  ),

  # smoking: 39 variables
  smoking = c(
    "LBXCOT", "SMD030", "SMD055", "SMD057", "SMD070", "SMD075",
    "SMD080", "SMD090", "SMD100CO", "SMD100FL", "SMD100MN", "SMD100NI",
    "SMD100TR", "SMD130", "SMD160", "SMD190", "SMD220", "SMD235",
    "SMD410", "SMD415", "SMD415A", "SMD415B", "SMD415C", "SMD430",
    "SMD440", "SMD450", "SMD641", "SMD650", "SMQ020", "SMQ040",
    "SMQ050", "SMQ077", "SMQ120", "SMQ150", "SMQ180", "SMQ210",
    "SMQ230", "cigarette_smoking", "current_past_smoking"
  ),

  # substance_use: 30 variables
  substance_use = c(
    "DUQ100", "DUQ110", "DUQ120", "DUQ130", "DUQ200", "DUQ210",
    "DUQ230", "DUQ240", "DUQ250", "DUQ260", "DUQ272", "DUQ280",
    "DUQ290", "DUQ300", "DUQ320", "DUQ330", "DUQ340", "DUQ352",
    "DUQ360", "DUQ370", "SXQ020", "SXQ280", "drink_five_per_day",
    "last_time_used_cocaine", "last_time_used_heroin",
    "last_time_used_marijuana", "last_time_used_meth",
    "quantity_drink_per_day", "total_days_5drink_year",
    "total_days_drink_year"
  ),

  # social_context: 34 variables
  social_context = c(
    "DED038Q", "anyone_to_help_social", "current_loud_noise",
    "ever_loud_noise_gt3", "ever_loud_noise_gt3_2",
    "first_degree_support", "home_painted_12mos", "house_age",
    "house_type", "how_many_years_in_house", "industry_agriculture",
    "industry_construction", "industry_manufacturing",
    "industry_mining", "industry_other", "industry_transport",
    "num_months_longest_job", "num_months_main_job",
    "number_close_friends", "occupation", "occupation_construction",
    "occupation_farm", "occupation_household", "occupation_laborer",
    "occupation_midmanage", "occupation_military", "occupation_repair",
    "occupation_transport", "old_paint_scraped",
    "paint_chipping_inside", "paint_chipping_outside",
    "private_water_source", "smell_tobacco", "use_water_treatment"
  ),

  # cognition_aging: 3 variables
  cognition_aging = c(
    "CFDFINSH", "CFDRIGHT", "TELOMEAN"
  )
)

# Modality of each block, for the `modality` argument of load_data()
NHANES_MODALITY <- c(
  blood = "clinical",
  biochemistry = "clinical",
  hormones = "clinical",
  body_measures = "clinical",
  blood_pressure = "clinical",
  physical_fitness = "clinical",
  nutrition = "diet",
  heavy_metals = "exposome",
  pops = "exposome",
  pesticides = "exposome",
  vocs = "exposome",
  plastics_phenols = "exposome",
  infections = "serology",
  allergens = "serology",
  medication = "other",
  disease_history = "other",
  smoking = "behaviour",
  substance_use = "behaviour",
  social_context = "other",
  cognition_aging = "clinical"
)

# Blocks whose mutual overlap defines the population everything else is
# measured on. These four were taken on essentially the whole examined sample,
# so intersecting them still leaves thousands of adults. Adding a laboratory
# subsample here - metals, pesticides, VOCs - would collapse the backbone to a
# few hundred people and quietly change what the whole walkthrough is about.
NHANES_CORE_BLOCKS <- c("blood", "biochemistry", "body_measures",
                        "blood_pressure")

# What a person could conceivably act on, or be helped to act on. Blood counts,
# serology, disease history and social position are not on that list; diet,
# smoking, weight, blood pressure, fitness and avoidable exposures are.
NHANES_MODIFIABLE <- c("body_measures", "blood_pressure", "physical_fitness",
                       "nutrition", "smoking", "substance_use", "heavy_metals")

# Standing height cannot plausibly be part of a mechanism leading to death over
# five years. If it turns up strongly, something other than biology is driving
# the graph. It is exempt from the variable cap so that the check is always
# available.
NHANES_NEGATIVE_CONTROLS <- "BMXHT"

# -----------------------------------------------------------------------------
# Column-level rules
# -----------------------------------------------------------------------------

#' How much a column varies, on a scale that does not depend on its units
#'
#' For anything categorical, the chance that two people drawn at random differ.
#' For anything continuous, the variance of the within-column rank, scaled so
#' that an evenly spread variable scores 1. Both live in [0, 1], so a serum
#' concentration and a yes/no answer can be ranked against each other.
.cmo_spread <- function(x) {

  x <- x[!is.na(x)]

  if (length(x) < 2) return(0)

  if (is.character(x) || is.factor(x) || length(unique(x)) <= 2) {
    p <- table(x) / length(x)
    return(as.numeric(1 - sum(p^2)))
  }

  r <- rank(x, ties.method = "average") / length(x)

  as.numeric(min(1, stats::var(r) * 12))

}

#' Blank NHANES "refused" and "don't know" codes
#'
#' The release stores these as 7, 9, 77, 99 inside otherwise ordinary
#' variables. Left alone they become categories of their own: house_age = 77
#' held 75 people out of five thousand and came top of the evidence ranking on
#' a single method.
#'
#' Blanking them everywhere would be worse than leaving them, because 9 is a
#' perfectly good answer to "how many cigarettes". So a code only counts as a
#' sentinel when the variable is short-scaled (few distinct values) and the
#' code sits well above everything else - the gap is what identifies it.
.cmo_blank_sentinels <- function(x, codes = c(7, 9, 77, 99, 777, 999),
                                 max_levels = 12, min_gap = 2) {

  if (!is.numeric(x)) return(list(x = x, blanked = 0L))

  u <- sort(unique(x[!is.na(x)]))

  if (length(u) < 3 || length(u) > max_levels) return(list(x = x, blanked = 0L))

  hit <- integer(0)

  for (code in rev(codes)) {

    if (!code %in% u) next

    below <- u[u < code & !(u %in% codes[codes < code])]

    if (!length(below)) next

    if (code - max(below) >= min_gap) hit <- c(hit, code)

  }

  if (!length(hit)) return(list(x = x, blanked = 0L))

  drop <- which(x %in% hit)
  x[drop] <- NA

  list(x = x, blanked = length(drop))

}

# -----------------------------------------------------------------------------
# The study
# -----------------------------------------------------------------------------

#' The largest set of blocks that still leaves a population to analyse
#'
#' Blocks are added to the core, most complete first, for as long as the
#' people present in every one of them stay above a floor. The order matters:
#' adding a laboratory subsample early would collapse the intersection and
#' then exclude every large block that came after it.
#'
#' This is the same arithmetic \code{check_data()} reports as
#' \code{cumulative_overlap}, done in advance so the walkthrough has a
#' workable set rather than discovering the problem twenty blocks in.
.viable_blocks <- function(assays, prefer = character(), floor = 400) {

  ids_of <- function(nm) {
    x <- assays[[nm]]
    rows <- rownames(x)
    # A person measured on nothing in this block is not in it, whatever the
    # row names say. load_data() gives every block every row and fills the
    # gaps with NA, so counting rows would call an empty block complete.
    if (is.null(rows) || nrow(x) == 0) return(character())
    rows[rowSums(!is.na(as.matrix(x))) > 0]
  }

  sizes <- vapply(names(assays), function(nm) length(ids_of(nm)), integer(1))

  # Preferred blocks are tried first, then everything else largest first.
  # Nothing is taken on trust: a preferred block that would break the
  # intersection is dropped like any other, because returning a set the
  # caller cannot analyse is worse than returning a smaller one.

  order_tried <- c(intersect(prefer, names(assays)),
                   setdiff(names(sizes)[order(sizes, decreasing = TRUE)],
                           prefer))

  keep <- character()
  shared <- NULL

  for (nm in order_tried) {

    here <- ids_of(nm)

    if (length(here) < floor) next

    candidate <- if (is.null(shared)) here else intersect(shared, here)

    if (length(candidate) >= floor) {
      keep <- c(keep, nm)
      shared <- candidate
    }

  }

  keep

}

#' Blocks drawn from the NHANES exposome release
#'
#' Twenty domains, filtered down to what is actually analysable. Three things
#' differ from a naive read of the release:
#'
#' 1. There is no rectangle across all twenty blocks. The modules were measured
#'    on different subsamples, so requiring everyone to have everything leaves
#'    nobody. Each block gets its own rectangle instead, drawn from a common
#'    backbone, which is exactly the sample-alignment problem the package
#'    exists to handle.
#'
#' 2. Variables are capped per block. Twelve hundred columns is not a
#'    walkthrough, it is an overnight job. The cap keeps the most complete and
#'    most variable columns, and never consults the outcome, so nothing is
#'    selected for looking good later.
#'
#' 3. Sentinel codes are cleaned everywhere, not just in housing.
#'
#' @param max_vars_per_block cap per block; Inf, or CMO_ALL_VARIABLES <- TRUE,
#'   keeps everything that survives the quality filters.
make_nhanes_study <- function(path = .nhanes_path,
                              blocks = NHANES_BLOCKS,
                              core_blocks = NHANES_CORE_BLOCKS,
                              max_vars_per_block = getOption(
                                "cmo.max.vars.per.block", Inf
                              ),
                              always_keep = NHANES_NEGATIVE_CONTROLS,
                              require_complete_core = TRUE,
                              core_coverage = 0.6,
                              flag_missing_above = 0.5,
                              flag_rare_below = 20,
                              max_n = 1800,
                              verbose = TRUE) {

  bag <- new.env(parent = emptyenv())
  load(path, envir = bag)

  main <- bag$MainTable
  vars <- bag$VarDescription[!duplicated(bag$VarDescription$var), ]

  flagged <- function(column) {
    if (!column %in% colnames(vars)) return(character(0))
    vars$var[!is.na(vars[[column]]) & vars[[column]] == 1]
  }

  questionnaire_vars <- flagged("is_questionnaire")
  binary_vars <- flagged("is_binary")
  ordinal_vars <- flagged("is_ordinal")

  # Adults with mortality follow-up. PERMTH_INT is months from interview to
  # death or censoring, MORTSTAT is whether the death happened.
  keep <- which(
    main$RIDAGEYR >= 18 &
      !is.na(main$RIDAGEYR) &
      !is.na(main$female) &
      !is.na(main$MORTSTAT) &
      !is.na(main$PERMTH_INT) &
      main$PERMTH_INT > 0
  )

  cohort <- main[keep, , drop = FALSE]

  blocks <- lapply(blocks, intersect, colnames(cohort))
  blocks <- blocks[vapply(blocks, length, integer(1)) > 0]

  if (!length(blocks)) stop("None of the requested variables are in the file.")

  present <- unlist(blocks, use.names = FALSE)

  # ---- sentinel codes, everywhere they can be identified ----

  blanked <- 0L
  blanked_vars <- character(0)

  for (v in intersect(present, questionnaire_vars)) {

    out <- .cmo_blank_sentinels(cohort[[v]])

    if (out$blanked > 0) {
      cohort[[v]] <- out$x
      blanked <- blanked + out$blanked
      blanked_vars <- c(blanked_vars, v)
    }

  }



  # ---- column quality: measured, reported, not acted on ----
  # check_data() is the thing that decides what is unusable. Dropping columns
  # here would hide from the audit exactly the cases it exists to catch, and
  # would silently change what the walkthrough is demonstrating. So this only
  # counts.

  n_cohort <- nrow(cohort)

  concern <- function(v) {

    x <- cohort[[v]]
    observed <- sum(!is.na(x))

    if (observed == 0) return("empty")
    if (observed < 2) return("one value observed")

    u <- unique(x[!is.na(x)])

    if (length(u) < 2) return("constant")

    if (observed / n_cohort < 1 - flag_missing_above) return("sparse")

    if (length(u) == 2 && min(table(x)) < flag_rare_below) return("rare level")

    ""

  }

  concerns <- vapply(present, concern, character(1))
  flagged_cols <- concerns[nzchar(concerns)]

  # ---- the cap: most complete first, then most variable, never the outcome ----

  capped <- 0L

  if (is.finite(max_vars_per_block)) {

    blocks <- lapply(blocks, function(v) {

      if (length(v) <= max_vars_per_block) return(v)

      completeness <- round(vapply(v, function(k)
        mean(!is.na(cohort[[k]])), numeric(1)), 2)

      spread <- vapply(v, function(k) .cmo_spread(cohort[[k]]), numeric(1))

      forced <- v %in% always_keep

      ranked <- v[order(!forced, -completeness, -spread, v)]

      capped <<- capped + length(v) - max_vars_per_block

      sort(ranked[seq_len(max_vars_per_block)])

    })

  }

  # ---- the backbone: who is measured on the core blocks ----

  # Who is in the study. Not "complete on every core variable" - no one is,
  # once the core blocks carry all their columns: BPXCHR is a child's heart
  # rate and is empty for every adult here, and the DXA scans were taken on a
  # subsample. One such column is enough to empty the intersection. So the rule
  # is coverage, not completeness: enough of the core measured to be worth
  # aligning the other blocks against.

  core <- intersect(names(blocks), core_blocks)

  if (isTRUE(require_complete_core) && length(core)) {

    core_vars <- unlist(blocks[core], use.names = FALSE)

    coverage <- rowMeans(!is.na(cohort[, core_vars, drop = FALSE]))

    if (max(coverage) < core_coverage) {
      stop(sprintf(
        "No adult reaches %.0f%% coverage of the %d core variables (best %.0f%%).",
        100 * core_coverage, length(core_vars), 100 * max(coverage)))
    }

    cohort <- cohort[coverage >= core_coverage, , drop = FALSE]

  }

  if (!nrow(cohort)) stop("No adult survived the backbone rule.")

  # A walkthrough is meant to be watched. A stratified subsample keeps every
  # death in and runs in a couple of minutes. Set CMO_FULL_COHORT <- TRUE to
  # use all of it.
  if (!isTRUE(getOption("cmo.full.cohort", exists("CMO_FULL_COHORT") &&
                        isTRUE(get("CMO_FULL_COHORT")))) &&
      nrow(cohort) > max_n) {

    set.seed(1)

    died <- which(cohort$MORTSTAT == 1)
    lived <- which(cohort$MORTSTAT == 0)

    take <- min(max_n - length(died), length(lived))

    cohort <- cohort[sort(c(died, sample(lived, take))), , drop = FALSE]

  }

  ids <- paste0("NH", cohort$SEQN)
  rownames(cohort) <- ids

  # ---- every block keeps every row, missing values included ----

  assays <- list()
  block_rows <- integer(0)
  block_complete <- integer(0)

  for (name in names(blocks)) {

    block <- cohort[, blocks[[name]], drop = FALSE]

    # Numeric codes that the codebook calls categories: house type 3 is not
    # three times house type 1. Left as numbers the engine would fit a slope
    # through arbitrary labels. Binary and ordinal codes are left alone -
    # for those the slope means something.
    block[] <- lapply(seq_along(block), function(i) {

      v <- colnames(block)[i]
      x <- block[[i]]
      observed <- x[!is.na(x)]

      nominal <- is.character(x) || is.factor(x) ||
        (v %in% questionnaire_vars &&
           !v %in% binary_vars && !v %in% ordinal_vars &&
           is.numeric(x) &&
           length(observed) > 0 &&
           length(unique(observed)) <= 8 &&
           all(observed == round(observed)))

      if (nominal) as.character(x) else x

    })

    assays[[name]] <- block
    block_rows[name] <- nrow(block)
    block_complete[name] <- sum(stats::complete.cases(block))

  }

  kept <- unlist(lapply(assays, colnames), use.names = FALSE)

  metadata <- data.frame(
    sample_id = ids,
    died = as.integer(cohort$MORTSTAT),
    follow_up_months = as.numeric(cohort$PERMTH_INT),
    age = as.numeric(cohort$RIDAGEYR),
    female = as.integer(cohort$female),
    sex = ifelse(cohort$female == 1, "female", "male"),
    survey = paste0("cycle_", cohort$SDDSRVYR),
    stringsAsFactors = FALSE
  )

  labels <- stats::setNames(vars$var_desc[match(kept, vars$var)], kept)

  if (isTRUE(verbose)) {

    cat("\nNHANES blocks\n")
    cat(strrep("-", 72), "\n", sep = "")
    cat(sprintf("  %-18s %6s %8s %9s  %s\n", "block", "vars", "rows",
                "complete", "modality"))

    for (name in names(assays)) {
      cat(sprintf("  %-18s %6d %8d %9d  %s\n", name, ncol(assays[[name]]),
                  block_rows[[name]], block_complete[[name]],
                  NHANES_MODALITY[[name]]))
    }

    cat(strrep("-", 72), "\n", sep = "")
    cat(sprintf("  %d variables in %d blocks, cohort of %d adults, %d deaths\n",
                length(kept), length(assays), nrow(cohort),
                sum(metadata$died)))

    if (capped > 0) {
      cat(sprintf("  %d variables held back by the per-block cap\n", capped))
    }

    cat(sprintf("  sentinel codes blanked: %d values in %d variables\n",
                blanked, length(blanked_vars)))

    if (length(flagged_cols)) {
      cat("  left in for check_data() to judge: ",
          paste(sprintf("%d %s", table(flagged_cols),
                        names(table(flagged_cols))), collapse = ", "),
          "\n", sep = "")
    }

    cat("\n")

  }

  # The function ended here without returning anything and without closing,
  # so make_simulated_study() below was being parsed as part of its body and
  # the file would not source at all. Everything the rest of the script reads
  # off STUDY is assembled here.

  list(
    source = paste("NHANES 1999-2006 (Patel et al., Scientific Data",
                   "3:160096)"),
    assays = assays,
    metadata = metadata,
    modality = NHANES_MODALITY[names(assays)],
    outcome = "died",
    time = "follow_up_months",
    covariates = c("age", "female"),

    # Standing height cannot plausibly be part of a mechanism leading to
    # death over this follow-up. If it surfaces strongly, something other
    # than biology is driving the graph.
    negative_controls = intersect(always_keep, kept),

    # What a person could conceivably act on, or be helped to act on.
    modifiable = intersect(NHANES_MODIFIABLE, names(assays)),

    labels = labels,
    ids = ids,

    # Which of the twenty can actually be analysed together. NHANES
    # administered its modules to different subsamples, so the intersection
    # of all twenty is empty: VOCs were measured on nine of these people and
    # allergens on none of them. Asking for all twenty is a real mistake a
    # user makes, and one section is devoted to watching the package explain
    # it, but the rest of the walkthrough needs a set that works.
    analysis_blocks = .viable_blocks(assays, core_blocks, floor = 400),

    # Kept for the sections that report on data preparation.
    preparation = list(
      blocks = vapply(assays, ncol, integer(1)),
      rows = block_rows,
      complete = block_complete,
      sentinels_blanked = blanked,
      sentinel_variables = blanked_vars,
      flagged_columns = flagged_cols,
      capped = capped
    )
  )

}

#' A small simulated study with a known answer
#'
#' Real data cannot show that the engine is right, only that it runs. This
#' plants a chain so the recovery can be checked.
#'
#' The defaults used to be read off STUDY, which does not exist yet the first
#' time this is called - the fallback path could not run at all. They are
#' arguments now, so the caller can align them when it wants to.

make_simulated_study <- function(n = 90, seed = 42,
                                 outcome = "died",
                                 negative_controls = "CRP",
                                 modifiable = c("proteins", "metabolites")) {

  set.seed(seed)

  protein <- rnorm(n)
  metabolite <- 0.8 * protein + rnorm(n, sd = 0.6)
  response <- 0.9 * metabolite + 0.2 * protein + rnorm(n, sd = 0.7)

  ids <- paste0("P", seq_len(n))

  counts <- data.frame(
    G1 = rpois(n - 5, 8), G2 = rpois(n - 5, 20), G3 = rpois(n - 5, 3),
    row.names = ids[6:n]
  )
  counts[3, ] <- NA

  microbiome <- as.data.frame(matrix(rgamma(n * 4, shape = 2), ncol = 4))
  microbiome <- microbiome / rowSums(microbiome)
  colnames(microbiome) <- paste0("OTU", 1:4)
  rownames(microbiome) <- ids

  list(
    source = "simulated, with a planted chain APOA1 -> SCFA -> HDL_function",
    assays = list(
      proteins = data.frame(
        APOA1 = protein, CRP = rnorm(n),
        ALB = rep(1, n),                  # constant: should be removed
        HP = c(rnorm(n - 2), NA, NA),     # has gaps
        row.names = ids
      ),
      metabolites = data.frame(SCFA = metabolite, TMAO = rnorm(n),
                               row.names = ids),
      transcripts = counts,
      microbiome = microbiome
    ),
    metadata = data.frame(
      sample_id = ids,
      HDL_function = response,
      died = as.integer(response > stats::median(response)),
      follow_up_months = rexp(n, 0.05) + 1,
      age = rnorm(n, 55, 9),
      sex = rep(c("male", "female"), length.out = n),
      batch = rep(c("B1", "B2"), length.out = n),
      subject = paste0("S", rep(seq_len(n / 2), each = 2)),
      stringsAsFactors = FALSE
    ),
    modality = c(proteins = "proteomics", metabolites = "metabolomics",
                 transcripts = "rnaseq", microbiome = "microbiome"),
    outcome = outcome,
    time = "follow_up_months",
    covariates = "age",
    negative_controls = negative_controls,
    modifiable = modifiable,
    labels = character(0),
    ids = ids,

    # All four blocks were simulated on the same people, so there is nothing
    # to choose between. The field exists so the sections downstream do not
    # have to know which study they were handed.
    analysis_blocks = c("proteins", "metabolites", "transcripts",
                        "microbiome")
  )

}

# STUDY <- if (file.exists(.nhanes_path)) {
#
#   tryCatch(make_nhanes_study(), error = function(e) {
#     cat("Could not read the NHANES file (", conditionMessage(e),
#         "); using simulated data.\n", sep = "")
#     make_simulated_study()
#   })
#
# } else {
#
#   make_simulated_study()
#
# }

STUDY <- if (file.exists(.nhanes_path)) {

  tryCatch(make_nhanes_study(max_vars_per_block = Inf), error = function(e) {
    cat("Could not read the NHANES file (", conditionMessage(e),
        "); using simulated data.\n", sep = "")
    make_simulated_study()
  })

} else {

  make_simulated_study()

}

# Kept so the sections that need a second cohort, or a known answer, still
# have one whichever dataset is in play. The outcome is matched to whatever
# STUDY ended up using; the block names cannot be, since they name different
# things in the two studies.
SIMULATED <- make_simulated_study(outcome = STUDY$outcome)

make_study <- function(n = 90, seed = 42) make_simulated_study(n, seed)

# -----------------------------------------------------------------------------
# Small studies built to show one thing each
# -----------------------------------------------------------------------------
#
# Real data cannot demonstrate a mechanism it does not contain. NHANES has no
# variable that is known to be a mediator, no relationship known to exist only
# in one sex, and no missing values known to have been filled in misleadingly.
# It can show the machinery running; it cannot show that the machinery is
# right, because there is no answer to check against.
#
# So where an output only means something against a known truth, the truth is
# planted. Each of these builds the smallest study that makes one behaviour
# visible, and the sections that use them say so.
# -----------------------------------------------------------------------------

#' Age confounds, inflammation mediates, a biomarker is downstream of the
#' disease. Three relationships with three different correct verdicts.
.cmo_dag_demo <- function(n = 250, seed = 3) {

  set.seed(seed)

  ids <- paste0("D", seq_len(n))

  age <- rnorm(n, 55, 10)
  protein <- 0.05 * age + rnorm(n)
  inflammation <- 0.8 * protein + rnorm(n, sd = 0.5)
  disease <- rbinom(n, 1, stats::plogis(0.4 * inflammation + 0.03 * age +
                                          rnorm(n)))

  obj <- load_data(
    list(blood = data.frame(protein = protein, inflammation = inflammation,
                            biomarker = 1.5 * disease + rnorm(n),
                            row.names = ids)),
    # inflammation is in the metadata as well so it can be named as a
    # covariate: adjusting for a mediator is only reachable if the mediator
    # can be offered as one.
    metadata = data.frame(sample_id = ids, disease = disease, age = age,
                          inflammation = inflammation,
                          stringsAsFactors = FALSE)
  )

  list(
    prep = preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE),
    structure = data.frame(
      from = c("age", "age", "protein", "inflammation", "disease"),
      to   = c("protein", "disease", "inflammation", "disease", "biomarker"),
      stringsAsFactors = FALSE
    )
  )

}

#' An exposure that is noise among the people it was measured on, whose
#' filled-in values track the outcome. The pooled estimate points one way and
#' the truth points the other.
.cmo_artefact_demo <- function(n = 300, seed = 7) {

  set.seed(seed)

  y <- rnorm(n)
  x <- rnorm(n)

  observed <- rep(c(TRUE, FALSE), each = n / 2)

  x[observed] <- -0.4 * y[observed] + rnorm(n / 2, sd = 0.5)
  x[!observed] <- 2.5 * y[!observed] + rnorm(n / 2, sd = 0.5)

  xm <- matrix(x, ncol = 1, dimnames = list(paste0("A", seq_len(n)), "exposure"))
  mask <- matrix(observed, ncol = 1, dimnames = dimnames(xm))

  edge <- .new_edge("exposure", "y", stats::coef(stats::lm(y ~ x))[[2]],
                    "association", "linear regression", quantity = "beta")
  edge$evidence_score <- 60

  list(
    edge = .complete_case_sensitivity(
      list(edge), xm,
      outcome = list(name = "y", values = y, type = "continuous"),
      covariates = NULL, mask = mask)[[1]]
  )

}

#' Three relationships of known shape: one uniform, one confined to men, one
#' manufactured by eight extreme individuals.
.cmo_shapes_demo <- function(n = 300, seed = 31) {

  set.seed(seed)

  ids <- paste0("H", seq_len(n))
  sex <- rep(c("F", "M"), length.out = n)

  uniform <- rnorm(n)
  subgroup <- rnorm(n)
  few <- rnorm(n)

  y <- 0.6 * uniform + 0.9 * subgroup * (sex == "M") + rnorm(n)

  few[1:8] <- 6
  y[1:8] <- y[1:8] + 9

  obj <- load_data(
    list(main = data.frame(uniform = uniform, subgroup = subgroup, few = few,
                           row.names = ids)),
    metadata = data.frame(sample_id = ids, y = y, sex = sex,
                          stringsAsFactors = FALSE)
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

  list(
    result = analyze(prep, "y", methods = "association", effort = "standard",
                     heterogeneity = "sex", plots = FALSE, quiet = TRUE)
  )

}

#' One latent process measured across two blocks, one decoy process confined
#' to a single block, and four variables that genuinely stand alone.
.cmo_module_demo <- function(n = 250, seed = 41) {

  set.seed(seed)

  ids <- paste0("M", seq_len(n))

  process <- rnorm(n)
  other <- rnorm(n)

  rna <- sapply(1:6, function(i) 0.9 * process + rnorm(n, sd = 0.45))
  prot <- sapply(1:4, function(i) 0.85 * process + rnorm(n, sd = 0.5))
  decoy <- sapply(1:5, function(i) 0.9 * other + rnorm(n, sd = 0.45))
  loners <- sapply(1:4, function(i) rnorm(n))

  y <- 1.1 * process + rnorm(n)

  rna <- cbind(rna, loners[, 1:2])
  colnames(rna) <- c(paste0("g", 1:6), paste0("solo_g", 1:2))
  rownames(rna) <- ids

  prot <- cbind(prot, decoy, loners[, 3:4])
  colnames(prot) <- c(paste0("p", 1:4), paste0("d", 1:5),
                      paste0("solo_p", 1:2))
  rownames(prot) <- ids

  obj <- load_data(list(rna = rna, prot = prot),
                   metadata = data.frame(sample_id = ids, y = y))

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

  list(
    modules = analyze(prep, "y", methods = "association", effort = "fast",
                      plots = FALSE, quiet = TRUE)$modules
  )

}

#' A claim, and three cohorts to take it to: one where it holds, one where it
#' is absent, and one where the direction holds at a fifth the size.
.cmo_replication_demo <- function(seed = 5) {

  cohort <- function(n, s, effect) {

    set.seed(s)
    ids <- paste0("R", s, "_", seq_len(n))

    age <- rnorm(n, 55, 10)
    protein <- 0.04 * age + rnorm(n)

    load_data(
      list(blood = data.frame(protein = protein, noise = rnorm(n),
                              row.names = ids)),
      metadata = data.frame(sample_id = ids,
                            y = effect * protein + 0.02 * age + rnorm(n),
                            age = age)
    )

  }

  discovery <- cohort(300, seed, 0.9)

  prep <- preprocess(discovery, check_data(discovery), plots = FALSE,
                     quiet = TRUE)

  result <- analyze(prep, "y", covariates = "age", methods = "association",
                    effort = "standard", plots = FALSE, quiet = TRUE)

  list(
    claim = hypothesis(result, "protein"),
    real = apply_preprocessing(cohort(280, 99, 0.9), prep, quiet = TRUE),
    absent = apply_preprocessing(cohort(280, 77, 0), prep, quiet = TRUE),
    weak = apply_preprocessing(cohort(600, 21, 0.15), prep, quiet = TRUE)
  )

}

cat("\n")
rule("=")
cat("CausalMultiOmics - COMPLETE WALKTHROUGH\n")
rule("=")
cat("\nPackage version : ",
    tryCatch(as.character(utils::packageVersion("CausalMultiOmics")),
             error = function(e) "development"), "\n", sep = "")
cat("R version       : ", R.version.string, "\n", sep = "")
cat("Internals visible: ", .internals_visible, "\n", sep = "")
cat("Transcript      : ", .transcript, "\n", sep = "")
cat("Sections        : ",
    if (is.null(CMO_SECTIONS)) "all" else paste(CMO_SECTIONS, collapse = ", "),
    "\n", sep = "")

# =============================================================================
# SECTION 1 - Every class, empty
# =============================================================================

if (section(1, "Every class, freshly constructed")) {

  note("Each class is a labelled box. Building one empty shows its shape.")

  constructors <- c("MultiOmicsData", "BlockDiagnostics",
                    "TransformationRecommendation", "PreprocessingRecipe",
                    "PreprocessingResult", "CMOValidation", "EvidenceEdge",
                    "EvidenceGraph", "CMOResult")

  for (cls in constructors) {

    step("Class: ", cls)

    obj <- tryCatch(get(cls)(), error = function(e) NULL)

    if (is.null(obj)) {
      cat("  [ERROR] constructor not reachable\n")
      .n_fail <- .n_fail + 1L
      next
    }

    .n_ok <- .n_ok + 1L

    cat("\nSlots (", length(obj), "):\n", sep = "")
    print(names(obj))

    cat("\nprint():\n")
    print(obj)

    cat("\nsummary():\n")
    tryCatch(print(summary(obj)),
             error = function(e) cat("  [ERROR] ", conditionMessage(e), "\n"))

  }

}

# =============================================================================
# SECTION 2 - load_data()
# =============================================================================

if (section(2, "load_data(): getting data in")) {

  step("The normal case")

  study <- show(load_data(assays = STUDY$assays, metadata = STUDY$metadata,
                          modality = STUDY$modality))

  assign("STUDY_OBJ", study, envir = globalenv())

  note("What it built:")
  show(names(study))
  show(study$modality)
  show(utils::head(study$sample_info))
  show(utils::head(study$feature_info))
  show(study$history)
  show(study$misc$block_summary)

  step("summary() of the container")
  show(summary(study), print_it = FALSE)

  step("Every way load_data() can refuse")

  must_fail(load_data(), "must be supplied")
  must_fail(load_data(1), "must be a list")
  must_fail(load_data(list()), "is empty")
  must_fail(load_data(list(a = NULL)), "No valid data blocks")
  must_fail(load_data(list(a = 1)), "matrix or a data")
  must_fail(load_data(list(data.frame(x = 1))), "named list")
  must_fail(load_data(list(a = data.frame(x = integer(0)))), "no samples")
  must_fail(load_data(list(a = data.frame(row.names = 1:5))), "no variables")

  note("A matrix with no row names is refused on purpose:")
  must_fail(load_data(list(a = matrix(rnorm(20), nrow = 5))),
            "no sample identifiers")

  note("Duplicated names, in both directions:")

  dup_cols <- data.frame(A = rnorm(5), B = rnorm(5))
  colnames(dup_cols) <- c("X", "X")
  rownames(dup_cols) <- paste0("S", 1:5)
  must_fail(load_data(list(bad = dup_cols)), "Duplicated feature names")

  dup_rows <- data.frame(A = rnorm(4))
  attr(dup_rows, "row.names") <- c("S1", "S1", "S2", "S3")
  must_fail(load_data(list(bad = dup_rows)), "Duplicated sample identifiers")

  note("Modality has to name blocks that exist:")
  must_fail(load_data(list(a = STUDY$assays[[1]]),
                      modality = c(nope = "rnaseq")), "not in 'assays'")

}

# =============================================================================
# SECTION 3 - check_data()
# =============================================================================

if (section(3, "check_data(): the inspection, which changes nothing")) {

  study <- get("STUDY_OBJ", envir = globalenv())

  step("Run the audit")

  quality <- show(check_data(study))

  assign("QUALITY", quality, envir = globalenv())

  step("The full summary")
  show(summary(quality), print_it = FALSE)

  step("What it decided per block")

  for (blk in names(quality$diagnostics)) {

    cat("\n---- ", blk, " ----\n", sep = "")

    d <- quality$diagnostics[[blk]]

    cat(sprintf("  type      : %s\n", d$data_type))
    cat(sprintf("  modality  : %s\n", d$modality))
    cat(sprintf("  size      : %d samples x %d features\n", d$samples, d$features))
    cat(sprintf("  missing   : %.2f%%\n", d$missing_percent))
    cat(sprintf("  constant  : %d\n", length(d$constant_features)))
    cat(sprintf("  normality : %s\n", d$normality))

  }

  step("One BlockDiagnostics in full")
  show(print(quality$diagnostics[[1]]), print_it = FALSE)
  show(summary(quality$diagnostics[[1]]), print_it = FALSE)

  step("One TransformationRecommendation: the contest between reshapings")
  show(print(quality$transformations[[1]]), print_it = FALSE)
  show(summary(quality$transformations[[1]]), print_it = FALSE)

  step("One PreprocessingRecipe: the plan, before anything is applied")
  show(print(quality$recipes[[1]]), print_it = FALSE)
  show(summary(quality$recipes[[1]]), print_it = FALSE)

  step("The plan for every block, side by side")
  show(quality$tables$preprocessing)

  step("Tables produced")
  show(names(quality$tables))
  show(quality$tables$block_summary)
  show(quality$tables$diagnostics)
  show(quality$tables$transformations)

  step("Quality control checks")
  show(quality$qc)

  step("Sample overlap between blocks")
  show(quality$summary$overlap)

  note("Note transcripts has fewer samples: the blocks do not all match.")

  step("Plots that were produced")
  show(names(Filter(function(p) length(p) > 0, quality$plots)))

  note("Display one with: grDevices::replayPlot(quality$plots$histograms[[1]])")

  step("check_data() refuses anything that is not a MultiOmicsData")

  must_fail(check_data(1), "must be a MultiOmicsData")
  must_fail(check_data(NULL), "must be a MultiOmicsData")
  must_fail(check_data(STUDY$assays), "must be a MultiOmicsData")

  step("Structural problems come back as an invalid result, not an exception")

  broken <- MultiOmicsData()
  broken$assays <- list()
  v <- show(check_data(broken))
  show(v$errors)

}

# =============================================================================
# SECTION 4 - preprocess()
# =============================================================================

if (section(4, "preprocess(): executing the plan")) {

  study <- get("STUDY_OBJ", envir = globalenv())
  quality <- get("QUALITY", envir = globalenv())

  step("Run it")

  clean <- show(preprocess(study, quality, plots = TRUE, quiet = FALSE))

  assign("CLEAN", clean, envir = globalenv())

  # Decided here rather than at the first analyze() call, because every
  # section downstream needs it and the decision belongs to the preprocessed
  # data: a person with one of eight hormone measurements looks measured
  # before the filters run and is gone afterwards.

  assign("ANALYSIS_BLOCKS",
         .viable_blocks(clean$data$assays,
                        prefer = intersect(STUDY$analysis_blocks,
                                           names(clean$data$assays)),
                        floor = 300),
         envir = globalenv())

  note("Blocks that can be analysed together: ",
       paste(get("ANALYSIS_BLOCKS", envir = globalenv()), collapse = ", "))

  step("The full summary, including the executed pipeline")
  show(summary(clean), print_it = FALSE)

  step("Every step that ran, as a table")
  show(clean$tables$steps)

  step("One step in full detail")
  show(clean$steps[[3]])

  note("Each step stores its fitted model, which is what makes the",
       " pipeline replayable on new data.")

  step("What was removed, and why")
  show(clean$tables$removed_features)
  show(clean$tables$removed_samples)

  step("Before and after")
  show(clean$tables$quality)

  step("The cleaned data")

  for (blk in names(clean$data$assays)) {
    cat(sprintf("\n  %-14s %d x %d, %d missing\n", blk,
                nrow(clean$data$assays[[blk]]),
                ncol(clean$data$assays[[blk]]),
                sum(is.na(clean$data$assays[[blk]]))))
  }

  show(utils::head(clean$data$assays[[1]], 4))

  step("The models it learned")
  show(names(clean$models))
  show(names(clean$models[[1]]))

  step("preprocess() refuses a plan that does not describe the data")

  other <- load_data(list(elsewhere = STUDY$assays[[1]]))
  must_fail(preprocess(other, quality, quiet = TRUE), "does not match")

  must_fail(preprocess(study), "must be supplied")
  must_fail(preprocess(1, quality), "must be a MultiOmicsData")
  must_fail(preprocess(study, "not a plan"), "must be a CMOValidation")

  step("A single recipe can be run on its own")
  show(preprocess(study, quality$recipes[[1]], plots = FALSE, quiet = TRUE))

}

# =============================================================================
# SECTION 5 - apply_preprocessing()
# =============================================================================

if (section(5, "apply_preprocessing(): the same recipe on new people")) {

  clean <- get("CLEAN", envir = globalenv())

  note("A second cohort. It must be cleaned using the numbers learned from",
       " the first, not recalculated from itself, or the two are not",
       " comparable.")

  fresh <- if (identical(STUDY$source, SIMULATED$source)) make_simulated_study(n = 30, seed = 999) else STUDY

  new_study <- show(load_data(fresh$assays, fresh$metadata, fresh$modality),
                    print_it = FALSE)

  ready <- show(apply_preprocessing(new_study, clean, quiet = FALSE))

  step("Same columns as the training data?")

  for (blk in names(ready$assays)) {

    trained <- clean$data$assays[[blk]]
    replayed <- ready$assays[[blk]]

    cat(sprintf("  %-14s trained %2dx%-2d  new %2dx%-2d  same features: %s\n",
                blk, nrow(trained), ncol(trained),
                nrow(replayed), ncol(replayed),
                identical(colnames(trained), colnames(replayed))))

  }

  step("Were any new samples dropped?")

  cat("  in :", nrow(fresh$assays[[1]]), "\n")
  cat("  out:", nrow(ready$assays[[1]]), "\n")

  note("None, by design. Dropping rows from a validation cohort because a",
       " training sample was dropped would manufacture optimistic results.")

  step("Guards")

  must_fail(apply_preprocessing(1, clean), "must be a MultiOmicsData")
  must_fail(apply_preprocessing(new_study, 1), "PreprocessingResult")
  must_fail(apply_preprocessing(new_study, PreprocessingResult()),
            "no steps to replay")

}

# =============================================================================
# SECTION 6 - The preprocessing method registry
# =============================================================================

if (section(6, "Everything the cleaning engine can execute")) {

  if (.internals_visible) {

    step("The registry")
    show(.available_methods())

    step("Unknown methods name the alternatives")
    must_fail(.get_method("imputation", "telepathy"), "Available")
    must_fail(.get_method("nonsense", "median"), "Unknown preprocessing stage")

    step("Methods that cannot be replayed explain why rather than pretending")

    entry <- .get_method("imputation", "mice")
    must_fail(entry$fit(data.frame(a = 1:5), list(), list()),
              "cannot be replayed")

  } else {

    note("Skipped: internals are not visible. Load with",
         " devtools::load_all('.', export_all = TRUE).")

  }

}

# =============================================================================
# SECTION 7 - analyze(), cross-sectional
# =============================================================================

if (section(7, "analyze(): building evidence")) {

  clean <- get("CLEAN", envir = globalenv())

  lesson("What analyze() is for, and what it refuses to be")

  teach(
    "Most analysis functions fit a model and hand it back. This one does not,
     because 'the best model' is a bad answer to a biological question: the
     model that predicts best is usually the one that leans hardest on
     whatever was measured most accurately, which is a fact about the
     laboratory rather than about the disease.

     Instead every applicable method runs, each reports the relationships it
     found, and an integrator merges them into one directed graph in which
     every arrow carries its own evidence: which methods saw it, how large it
     was, how precisely it was measured, and - the part that matters - what
     would have to be true for it to mean what it looks like it means.

     A relationship between two variables can arise three ways. One causes
     the other. Something else causes both. Or the sampling created it. No
     amount of statistics distinguishes these from a single cross-section,
     so the engine never claims to; it records which of the three it has
     ruled out and leaves the rest visible.")

  step("Run it")

  teach(
    "The blocks are named explicitly. NHANES gave its modules to different
     subsamples of people, so requiring everyone to have everything leaves
     nobody - a later section watches the package explain exactly that. Here
     the set is the largest one that still leaves a real population.

     The choice is made on the preprocessed blocks, not the raw ones, and the
     difference is large. Before preprocessing, a person with one of eight
     hormone measurements looks like someone who was measured. After it, the
     filter that drops mostly-empty rows has removed them, and the hormone
     block has gone from 1800 people to 413. Choosing blocks on the raw
     counts would pick a set that cannot be analysed.")

  analysis_blocks <- get("ANALYSIS_BLOCKS", envir = globalenv())

  cat("\n  blocks analysed: ",
      paste(analysis_blocks, collapse = ", "), "\n", sep = "")

  results <- show(analyze(clean, blocks = ANALYSIS_BLOCKS,
                          outcome = STUDY$outcome,
                          covariates = STUDY$covariates,
                          plots = TRUE, quiet = FALSE))

  assign("RESULTS", results, envir = globalenv())

  step("The full summary")
  show(summary(results), print_it = FALSE)

  step("Which methods ran, and which sat it out")
  show(results$models)
  show(results$logs)

  step("The evidence, merged and scored")
  show(results$graph$edges)

  step("Did it recover anything? (a planted chain, on simulated data)")
  show(results$causal_paths)

  step("One EvidenceEdge in full")
  show(print(results$evidence[[1]]), print_it = FALSE)
  show(summary(results$evidence[[1]]), print_it = FALSE)

  note("Note identification and assumptions. A high score with",
       " 'adjustment' is a hypothesis, not a conclusion.")

  step("The EvidenceGraph")
  show(print(results$graph), print_it = FALSE)
  show(summary(results$graph), print_it = FALSE)

  step("What the graph is made of")
  show(results$graph$nodes)
  show(results$network$communities$sizes)
  show(results$network$metrics$density)

  step("Consensus importance across methods")
  show(results$importance)

  step("Predicting well and being well supported are not the same thing")
  show(results$tables$standing)

  note("A variable can predict beautifully because it is downstream of the",
       " outcome, and one with a well-supported effect can predict poorly",
       " because the effect is small. Reporting them in one column invites",
       " the reader to conflate them.")

  step("The interpretation")
  show(results$interpretation$drivers)
  show(results$interpretation$mediators)
  show(results$interpretation$hubs)
  show(results$interpretation$bridges)
  show(results$interpretation$risk)
  show(results$interpretation$protective)
  show(results$interpretation$statements)

  step("Honest accounting")
  show(results$report$limitations)
  show(results$performance)

  step("Guards")

  study <- get("STUDY_OBJ", envir = globalenv())

  must_fail(analyze(study, STUDY$outcome), "must be a PreprocessingResult")
  must_fail(analyze(clean, blocks = ANALYSIS_BLOCKS, "no_such_column"), "not a column")
  must_fail(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, methods = "telepathy"),
            "Unknown generator")
  must_fail(analyze(clean, STUDY$outcome, blocks = "nope"), "Unknown block")

}

# =============================================================================
# SECTION 8 - analyze() under other designs
# =============================================================================

if (section(8, "The design decides which methods apply")) {

  clean <- get("CLEAN", envir = globalenv())

  step("Survival: an event plus a follow-up time")

  surv <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, outcome = STUDY$outcome, time = STUDY$time,
                       plots = FALSE, quiet = TRUE))

  cat("\n  design           :", surv$design$type, "\n")
  cat("  methods run      :", paste(names(surv$models), collapse = ", "), "\n")
  cat("  temporal edges   :", surv$performance$temporal_edges,
      "of", nrow(surv$graph$edges), "\n")

  show(table(surv$graph$edges$identification))

  note("Only here can a relationship claim temporal identification:",
       " the exposure was measured before the event.")

  step("Longitudinal: repeated measurements per subject")

  # A longitudinal design needs the same person measured more than once.
  # NHANES interviews each participant a single time, so passing subject =
  # there produces a cross-sectional analysis labelled as though it were
  # longitudinal, which teaches the reader the opposite of the truth. The
  # simulated study does have repeated measures, so the demonstration moves
  # there rather than being skipped.

  repeated_measures <- !is.null(STUDY$metadata$subject) &&
    any(table(STUDY$metadata$subject) > 1)

  if (repeated_measures) {

    long_input <- clean
    long_outcome <- STUDY$outcome

  } else {

    note("This dataset measures each person once, so it cannot show a",
         " longitudinal design. Switching to the simulated study, which",
         " has two measurements per subject.")

    sim <- load_data(SIMULATED$assays, SIMULATED$metadata,
                     SIMULATED$modality)

    long_input <- preprocess(sim, check_data(sim), plots = FALSE,
                             quiet = TRUE)
    long_outcome <- SIMULATED$outcome

  }

  long <- show(analyze(long_input, outcome = long_outcome,
                       subject = "subject", plots = FALSE, quiet = TRUE))

  cat("\n  design      :", long$design$type, "\n")
  cat("  methods run :", paste(names(long$models), collapse = ", "), "\n")
  show(long$design$notes)

  if (!identical(long$design$type, "longitudinal")) {
    cat("\n  [FAIL] subject was supplied but the design is",
        long$design$type, "\n")
    .n_fail <- .n_fail + 1L
    .failures <- c(.failures, "longitudinal design not detected")
  }


  step("Predictive goal: a narrower set of methods")

  pred <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, outcome = STUDY$outcome, goal = "predictive",
                       plots = FALSE, quiet = TRUE))

  cat("\n  methods run :", paste(names(pred$models), collapse = ", "), "\n")

  step("A single method, on demand")

  one <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, outcome = STUDY$outcome,
                      methods = "association", plots = FALSE, quiet = TRUE))

  cat("\n  methods run :", paste(names(one$models), collapse = ", "), "\n")

  step("Cross-sectional never claims temporal identification")

  results <- get("RESULTS", envir = globalenv())
  show(results$performance$temporal_edges)

}

# =============================================================================
# SECTION 9 - The evidence generators
# =============================================================================

if (section(9, "Every evidence generator, one by one")) {

  if (.internals_visible) {

    show(names(.evidence_registry()))

    registry <- .evidence_registry()

    for (nm in names(registry)) {

      entry <- registry[[nm]]

      missing <- entry$requires[
        !vapply(entry$requires, requireNamespace, logical(1), quietly = TRUE)]

      cat(sprintf("\n  %-14s %-38s needs: %-12s %s\n", nm, entry$label,
                  if (length(entry$requires)) paste(entry$requires, collapse = ",")
                  else "nothing",
                  if (length(missing)) "MISSING" else "available"))

    }

  } else {

    note("Skipped: internals not visible.")

  }

}

# =============================================================================
# SECTION 10 - annotate_evidence()
# =============================================================================

if (section(10, "annotate_evidence(): adding outside knowledge")) {

  results <- get("RESULTS", envir = globalenv())

  note("Kept out of analyze() on purpose, so the analysis stays offline",
       " and reproducible. Supply a local export rather than a live query.")

  edges <- results$graph$edges

  annotations <- data.frame(
    source = utils::head(edges$source, 2),
    target = utils::head(edges$target, 2),
    support = c(0.9, 0.6),
    database = c("local KEGG export", "local STRING export"),
    stringsAsFactors = FALSE
  )

  show(annotations)

  annotated <- show(annotate_evidence(results, annotations))

  step("What changed")

  touched <- Filter(function(e) is.finite(e$biological_support),
                    annotated$evidence)

  cat("  edges annotated:", length(touched), "\n\n")

  for (e in touched) {
    cat("  ", e$source, "->", e$target,
        " support", e$biological_support,
        " from", paste(e$biological_sources, collapse = ", "), "\n")
  }

  step("Guards")

  must_fail(annotate_evidence(1, annotations), "must be a CMOResult")
  must_fail(annotate_evidence(results, data.frame(a = 1)), "source")
  must_fail(annotate_evidence(results, annotations, weight = 2),
            "between 0 and 1")

}

# =============================================================================
# SECTION 11 - report()
# =============================================================================

if (section(11, "report(): making it readable")) {

  quality <- get("QUALITY", envir = globalenv())
  results <- get("RESULTS", envir = globalenv())

  step("The validation report, on screen")
  show(report(quality, format = "console"), print_it = FALSE)

  step("The analysis report, on screen")
  show(report(results, format = "console"), print_it = FALSE)

  step("Only some sections of the validation report")
  show(report(quality, format = "console", sections = c("overview", "qc")),
       print_it = FALSE)

  step("HTML: four documents")

  paths <- c(
    validation = file.path(.outdir, "validation-report.html"),
    analysis_general = file.path(.outdir, "analysis-general.html"),
    analysis_technical = file.path(.outdir, "analysis-technical.html")
  )

  show(report(quality, file = paths[["validation"]], open = FALSE,
              quiet = FALSE), print_it = FALSE)

  show(report(results, file = paths[["analysis_general"]], open = FALSE,
              quiet = FALSE), print_it = FALSE)

  show(report(results, file = paths[["analysis_technical"]], open = FALSE,
              quiet = FALSE, audience = "technical"), print_it = FALSE)

  step("What was written")

  for (nm in names(paths)) {
    cat(sprintf("  %-20s %7.1f KB  %s\n", nm,
                if (file.exists(paths[[nm]])) file.size(paths[[nm]]) / 1024 else NA,
                paths[[nm]]))
  }

  step("The figures behind the report")

  show(names(results$plots))

  note("circos and circos_blocks are the two that show the whole answer at",
       " once: which layers were measured, how big each is, and where the",
       " evidence runs between them.")

  for (nm in c("circos", "circos_blocks")) {

    if (is.null(results$plots[[nm]])) next

    file <- file.path(.outdir, paste0(nm, ".png"))

    ok <- tryCatch({
      grDevices::png(file, width = 1100, height = 1100, res = 130,
                     type = "cairo")
      grDevices::replayPlot(results$plots[[nm]])
      grDevices::dev.off()
      TRUE
    }, error = function(e) { try(grDevices::dev.off(), silent = TRUE); FALSE })

    cat(sprintf("  %-16s %s  %s\n", nm,
                if (ok && file.exists(file)) sprintf("%6.0f KB",
                                                     file.size(file) / 1024)
                else "  failed",
                file))

  }

  note("Open these in a browser. They are self-contained: no internet needed.")

  step("Guards")

  must_fail(report(results, format = "pdf"))
  must_fail(report(results, audience = "experts"))
  must_fail(report(get("STUDY_OBJ", envir = globalenv())), "no applicable method")

  step("Degenerate objects still produce a document")

  show(report(CMOValidation(), format = "console"), print_it = FALSE)
  show(report(CMOResult(), format = "console"), print_it = FALSE)

}

# =============================================================================
# SECTION 12 - How much computation to spend
# =============================================================================

if (section(12, "The effort dial: buying rigour with time")) {

  clean <- get("CLEAN", envir = globalenv())

  note("Everything expensive is opt-in through one argument, with each",
       " piece separately overridable. Resampling and permutation are the",
       " only parts that cost real time, and both scale linearly.")

  step("effort = 'fast': the generators once, nothing else")

  t0 <- Sys.time()
  fast <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, effort = "fast",
                       plots = FALSE, quiet = TRUE))
  cat(sprintf("\n  elapsed: %.1f s\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  step("effort = 'standard': adds model diagnostics and 50 resamples")

  t0 <- Sys.time()
  standard <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, effort = "standard",
                           plots = FALSE, quiet = TRUE))
  cat(sprintf("\n  elapsed: %.1f s\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  step("What each preset turns on")

  for (nm in c("fast", "standard")) {

    r <- get(nm)

    cat(sprintf("\n  %-10s resampling: %-5s  null calibration: %-5s  diagnostics: %s\n",
                nm,
                isTRUE(r$diagnostics$resampling$available),
                isTRUE(r$diagnostics$null_calibration$available),
                isTRUE(r$diagnostics$models$available)))

  }

  note("'thorough' adds null calibration; 'exhaustive' resamples every",
       " generator 500 times. Any component can be set on its own:",
       " resample =, permutations =, diagnostics =.")

  step("Overriding a single piece")

  custom <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, effort = "fast",
                         resample = 20, plots = FALSE, quiet = TRUE))

  cat("\n  resampling ran despite effort = 'fast': ",
      isTRUE(custom$diagnostics$resampling$available), "\n", sep = "")

  assign("RESAMPLED", standard, envir = globalenv())

}

# =============================================================================
# SECTION 13 - What a score is made of
# =============================================================================

if (section(13, "Opening up an evidence score")) {

  results <- get("RESULTS", envir = globalenv())

  note("Three numbers, kept apart on purpose. Size is how big the",
       " relationship is, Precision how tightly it was estimated, and",
       " Agreement how many methods saw the same thing.")

  step("The evidence hierarchy: not every method makes the same kind of claim")

  if (.internals_visible) {

    for (g in names(.evidence_registry())) {
      cat(sprintf("  %-16s level %d  %-14s weight %.1f\n",
                  g, .evidence_level(g), .level_label(.evidence_level(g)),
                  .level_weight(.evidence_level(g))))
    }

    note("Agreement is weighted by this. Five predictive methods concurring",
         " is weaker evidence than one longitudinal model plus one",
         " mediation analysis, and an unweighted vote says the opposite.")

  }

  step("Every relationship, with its level and identification")

  show(results$graph$edges[, c("source", "target", "level_label",
                               "identification", "consistency",
                               "evidence_score")])

  step("The score, opened up")

  if (.internals_visible) {
    show(.decompose_evidence(results$evidence[[1]]))
  }

  step("How much unmeasured confounding would explain it away")

  show(results$graph$edges[, c("source", "target", "e_value",
                               "direction_confidence", "bootstrap_stability")])

  note("An E-value near 1 means a trivially weak hidden factor would",
       " account for the whole thing. Direction confidence of 0.5 means the",
       " arrow could point either way.")

  step("Where methods disagreed, and why")

  conflicted <- Filter(function(e) length(e$conflicting_methods) > 0,
                       results$evidence)

  if (length(conflicted) == 0) {
    cat("  No relationship had methods pointing opposite ways.\n")
  } else {
    for (e in utils::head(conflicted, 3)) {
      cat("\n  ", e$source, " -> ", e$target, "\n", sep = "")
      cat(paste0("    ", e$conflict_summary), sep = "\n")
    }
  }

  step("What each method reported on its own, before merging")

  show(results$evidence[[1]]$contributions)

  note("Estimates are on each method's own scale and are not comparable",
       " between rows.")

}

# =============================================================================
# SECTION 14 - explain()
# =============================================================================

if (section(14, "explain(): why is this variable here")) {

  results <- get("RESULTS", envir = globalenv())

  candidates <- results$interpretation$drivers$source

  if (length(candidates) == 0) {

    note("No driver to explain in this run.")

  } else {

    step("Everything the engine knows about one variable")

    show(explain(results, candidates[1]), print_it = TRUE)

    step("It refuses a name it does not know, and says what it does know")

    must_fail(explain(results, "not_a_variable"), "no relationships")
    must_fail(explain(1, "x"), "must be a CMOResult")

  }

}

# =============================================================================
# SECTION 15 - counterfactual()
# =============================================================================

if (section(15, "counterfactual(): what changing something would mean")) {

  results <- get("RESULTS", envir = globalenv())

  note("Expressed in the units the variable was measured in. The engine",
       " pushes the original value through the stored preprocessing models",
       " to locate it on the scale the analysis worked in.")

  step("Only variables you declare as changeable are considered")

  must_fail(counterfactual(results), "must name the variables")

  note("Nothing is included by default on purpose: a contrast for a genotype",
       " is arithmetic without meaning, and printing one invites exactly the",
       " reading it cannot support.")

  step("Declaring blocks that could plausibly be acted on")

  cf <- show(counterfactual(results, modifiable = STUDY$modifiable),
             print_it = TRUE)

  step("A specific amount, in original units")

  show(counterfactual(results, modifiable = STUDY$modifiable[1],
                      change = "iqr")$table)

  step("A decrease instead")

  show(counterfactual(results, modifiable = STUDY$modifiable[1],
                      direction = "decrease")$table)

  step("Requiring stronger identification empties the table rather than relaxing")

  strict <- show(counterfactual(results, modifiable = STUDY$modifiable[1],
                                min_identification = "temporal"),
                 print_it = FALSE)

  cat("\n  rows: ", nrow(strict$table), "\n", sep = "")
  cat("  reason: ", paste(strict$skipped, collapse = "; "), "\n", sep = "")

  note("The verb is chosen by the identification label, never freely. An",
       " adjusted association reads 'is associated with', however high its",
       " score.")

}

# =============================================================================
# SECTION 16 - Categorical variables
# =============================================================================

if (section(16, "Factors, binary and with several categories")) {

  note("Factors survive loading and preprocessing untouched, which is right:",
       " nothing sensible comes of scaling a category. They are encoded at",
       " the analysis step.")

  set.seed(31)
  m <- 120
  ids <- paste0("C", seq_len(m))

  sex <- sample(c("F", "M"), m, TRUE)
  diet <- sample(c("mediterranean", "western", "vegetarian"), m, TRUE,
                 prob = c(.45, .35, .20))

  marker <- rnorm(m, 100, 15)
  y <- 0.03 * marker + 1.1 * (sex == "M") - 0.8 * (diet == "western") +
    rnorm(m)

  cat_obj <- show(load_data(
    list(
      blood = data.frame(MARKER = marker, OTHER = rnorm(m), row.names = ids),
      lifestyle = data.frame(sex = sex, diet = diet,
                             barcode = paste0("id_", seq_len(m)),
                             row.names = ids, stringsAsFactors = FALSE)
    ),
    metadata = data.frame(sample_id = ids, score = y,
                          stringsAsFactors = FALSE)
  ), print_it = FALSE)

  cat_prep <- preprocess(cat_obj, check_data(cat_obj), plots = FALSE,
                         quiet = TRUE)

  step("Factors are still factors after preprocessing")

  show(vapply(cat_prep$data$assays$lifestyle, class, character(1)))

  step("analyze() encodes them")

  cat_res <- show(analyze(cat_prep, "score", effort = "fast", plots = FALSE,
                          quiet = TRUE), print_it = FALSE)

  for (nm in names(cat_res$data$encoding)) {
    e <- cat_res$data$encoding[[nm]]
    cat(sprintf("  %-20s %s vs %s   (%d vs %d people)\n",
                nm, e$level, e$reference, e$n_level, e$n_reference))
  }

  note("A factor of k levels becomes k-1 comparisons against a reference,",
       " and the reference is the commonest level, because contrasts against",
       " a rare category are estimated from very few people.")

  step("What was left out, and why")

  show(grep("categor|identifier|fewer than", cat_res$logs, value = TRUE))

  step("The wording changes for a category")

  show(cat_res$report$main_findings)

  step("A category contrast is membership, not units")

  show(counterfactual(cat_res, modifiable = "diet")$table)

  step("An unordered outcome with more than two categories is refused")

  must_fail(
    analyze(cat_prep, "sex_is_not_here", quiet = TRUE),
    "not a column"
  )

}

# =============================================================================
# SECTION 17 - Blocks as units of evidence
# =============================================================================

if (section(17, "Reading the result one layer at a time")) {

  results <- get("RESULTS", envir = globalenv())

  step("How the screening budget was shared out")

  clean <- get("CLEAN", envir = globalenv())

  note("max_features caps how many variables reach the pairwise stage, and",
       " min_per_block is the floor each block keeps. Ranking every feature",
       " together would hand the whole budget to whichever block has the",
       " most columns, and a small clinical block would vanish.")

  tight <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, max_features = 8,
                        min_per_block = 2, effort = "fast",
                        plots = FALSE, quiet = TRUE), print_it = FALSE)

  show(tight$data$screening$allocation)

  cat("\n  every block survived: ",
      length(tight$data$screening$allocation) ==
        length(unique(tight$data$feature_block)), "\n", sep = "")

  note("Every block keeps at least one variable even when that pushes the",
       " total past max_features: losing a block entirely is worse than",
       " exceeding a target set for speed.")

  step("Evidence between every pair of layers")
  show(results$network$blocks$evidence)

  step("How much of the total each layer carries")
  show(results$network$blocks$importance)

  note("The shares add to 100: a relationship between two blocks belongs to",
       " both, so its score is split between them.")

  step("Each layer, by kind of evidence")
  show(results$network$blocks$scores)

  step("What each detected community is made of")
  show(results$network$blocks$communities)

  step("The graph with whole layers as the nodes")
  show(results$network$blocks$graph$nodes)
  show(results$network$blocks$graph$edges)

  step("Crossing layers is reported beside the score, never inside it")

  show(utils::head(results$graph$edges[, c("source", "target", "source_block",
                                            "target_block", "cross_block",
                                            "evidence_score")], 8))

  note("Crossing makes a relationship more interesting, not better",
       " supported. Folding it into the score would mix a thematic",
       " judgement into a measurement.")

}

# =============================================================================
# SECTION 18 - Robustness and diagnostics
# =============================================================================

if (section(18, "How much to trust any of this")) {

  resampled <- get("RESAMPLED", envir = globalenv())
  clean <- get("CLEAN", envir = globalenv())

  step("Would these relationships survive a different sample?")

  if (isTRUE(resampled$diagnostics$resampling$available)) {

    cat(sprintf("  %d %s replicates over: %s\n",
                resampled$diagnostics$resampling$replicates,
                resampled$diagnostics$resampling$scheme,
                paste(resampled$diagnostics$resampling$generators,
                      collapse = ", ")))

    stability <- vapply(resampled$evidence,
                        function(e) e$bootstrap_stability, numeric(1))

    cat(sprintf("  recovered every time : %d\n", sum(stability >= 0.99,
                                                     na.rm = TRUE)))
    cat(sprintf("  recovered under half : %d\n", sum(stability < 0.5,
                                                     na.rm = TRUE)))

  } else {

    cat("  Resampling did not run.\n")

  }

  step("How many relationships appear when there is nothing to find")

  calibrated <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, effort = "fast",
                             permutations = 15, plots = FALSE, quiet = TRUE),
                     print_it = FALSE)

  cal <- calibrated$diagnostics$null_calibration

  if (isTRUE(cal$available)) {

    cat(sprintf("\n  found here          : %d\n", cal$observed_edges))
    cat(sprintf("  found on shuffled   : %.1f on average, up to %d\n",
                cal$null_edges_mean, cal$null_edges_max))
    cat(sprintf("  empirical p         : %.3f\n", cal$p_more_edges))
    cat("\n  ", cal$verdict, "\n", sep = "")

    note("The single most useful number about a graph. Twenty-five edges",
         " sounds like a result until the same engine produces twenty-two on",
         " the same data with the outcome shuffled.")

  }

  step("Variables that should not appear, and did")

  controls <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, effort = "fast",
                           negative_controls = STUDY$negative_controls,
                           plots = FALSE, quiet = TRUE),
                   print_it = FALSE)$diagnostics$negative_controls

  show(controls$notes)
  show(controls$flagged)

  step("Does the relationship hold in every subgroup?")

  hetero <- show(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, effort = "fast",
                         heterogeneity = "sex", plots = FALSE, quiet = TRUE),
                 print_it = FALSE)$diagnostics$heterogeneity

  if (isTRUE(hetero$available)) {
    show(utils::head(hetero$table, 5))
    show(hetero$notes)
  }

  step("Fit quality and assumption checks")

  diagnostics <- get("RESAMPLED", envir = globalenv())$diagnostics$models

  if (isTRUE(diagnostics$available)) {
    show(utils::head(diagnostics$table, 5))
    show(diagnostics$notes)
  }

  step("What the missingness looked like")

  missing <- get("RESULTS", envir = globalenv())$diagnostics$missing_data

  cat(sprintf("  %.2f%% of cells missing, %.1f%% complete cases\n",
              missing$missing_percent, missing$complete_case_percent))

  show(missing$notes)

  step("How much the picture leans on any one part")

  robustness <- get("RESULTS", envir = globalenv())$diagnostics$network_robustness

  if (isTRUE(robustness$available)) {
    show(robustness$leave_one_block_out)
    show(robustness$notes)
  }

}

# =============================================================================
# SECTION 19 - Reproducibility
# =============================================================================

if (section(19, "Reproducibility guarantees")) {

  clean <- get("CLEAN", envir = globalenv())
  study <- get("STUDY_OBJ", envir = globalenv())

  step("Does the package disturb your random number generator?")

  set.seed(123); before <- runif(3)
  set.seed(123); invisible(check_data(study)); after_check <- runif(3)

  set.seed(123)
  invisible(analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, plots = FALSE, quiet = TRUE,
                    bootstrap = 30))
  after_analyze <- runif(3)

  cat("  after check_data() : ", identical(before, after_check), "\n", sep = "")
  cat("  after analyze()    : ", identical(before, after_analyze), "\n", sep = "")

  note("Both must be TRUE. An audit that moved your generator would",
       " silently change every simulation you ran afterwards.")

  if (!identical(before, after_check) || !identical(before, after_analyze)) {
    .n_fail <- .n_fail + 1L
    .failures <- c(.failures, "RNG state was modified")
  } else {
    .n_ok <- .n_ok + 2L
  }

  step("Does the same input give the same answer?")

  a <- analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, plots = FALSE, quiet = TRUE, bootstrap = 30)
  b <- analyze(clean, blocks = ANALYSIS_BLOCKS, STUDY$outcome, plots = FALSE, quiet = TRUE, bootstrap = 30)

  same <- identical(
    vapply(a$evidence, function(e) e$evidence_score, numeric(1)),
    vapply(b$evidence, function(e) e$evidence_score, numeric(1))
  )

  cat("  two identical runs agree: ", same, "\n", sep = "")

  if (same) .n_ok <- .n_ok + 1L else {
    .n_fail <- .n_fail + 1L
    .failures <- c(.failures, "analyze() is not deterministic")
  }

  step("What was recorded about this run")

  show(clean$execution)
  show(get("RESULTS", envir = globalenv())$execution)

}

# =============================================================================
# SECTION 20 - Sample alignment, and the mistake everyone makes once
# =============================================================================

if (section(20, "When blocks do not share people")) {

  clean <- get("CLEAN", envir = globalenv())

  lesson("Why twenty blocks can share nothing while every pair shares everything")

  teach(
    "This is the single most common way a multi-block analysis fails, and the
     reason is arithmetic rather than biology.

     A study rarely measures everything on everyone. NHANES is explicit about
     it: the examination was given to all participants, but the laboratory
     modules were assigned to subsamples. Volatile organic compounds went to
     one rotating subsample, allergen serology to another, heavy metals to a
     third. Each module covers thousands of people. No two modules cover the
     same thousands.

     Now consider what an analysis across all of them needs. To ask whether a
     metal concentration relates to blood pressure while adjusting for diet,
     one row must have all three. That row has to be in the intersection of
     every block used. And an intersection of twenty sets, each missing a
     different slice, empties out fast: if each block is missing a different
     5% of people, twenty of them can between them exclude everybody.

     The trap is that nothing looks wrong until it is too late. The overlap
     matrix a reader naturally consults is pairwise, and pairwise everything
     is fine - any two of these blocks share most of the cohort. A table of
     pairs cannot express an intersection of twenty.")

  step("What the pairwise matrix says")

  validation <- get("QUALITY", envir = globalenv())

  overlap <- validation$summary$overlap

  if (!is.null(overlap) && nrow(overlap) > 1) {

    off <- overlap[row(overlap) != col(overlap)]

    cat(sprintf("\n  smallest overlap between any two blocks : %d\n", min(off)))
    cat(sprintf("  largest overlap between any two blocks  : %d\n", max(off)))
    cat(sprintf("  present in EVERY block                  : %d\n",
                .report_or(validation$summary$shared_by_all, NA)))

    teach(
      "In this particular release the matrix does give the game away: one
       module shares nobody with anything, so the smallest pairwise figure is
       already zero. That is the easy case.")

  }

  step("The hard case, which the matrix cannot show at all")

  teach(
    "NHANES is unusually kind here. The dangerous version is the one where
     every pairwise figure is healthy and the set is still empty, and it
     needs constructing because a dataset that shows it is by definition one
     where nothing looks wrong.

     Eight blocks, each missing a different twenty-five people out of two
     hundred. Every pair shares at least 150. All eight together share
     none.")

  demo_ids <- paste0("Q", 1:200)

  demo_blocks <- lapply(seq_len(8), function(i) {
    keep <- setdiff(demo_ids, demo_ids[((i - 1) * 25 + 1):(i * 25)])
    matrix(rnorm(length(keep) * 3), length(keep), 3,
           dimnames = list(keep, paste0("v", 1:3)))
  })

  names(demo_blocks) <- paste0("b", seq_len(8))

  demo_obj <- load_data(
    demo_blocks,
    metadata = data.frame(sample_id = demo_ids, y = rnorm(200),
                          stringsAsFactors = FALSE))

  demo_v <- check_data(demo_obj)

  demo_off <- demo_v$summary$overlap[row(demo_v$summary$overlap) !=
                                       col(demo_v$summary$overlap)]

  cat(sprintf("\n    smallest pairwise overlap : %d\n", min(demo_off)))
  cat(sprintf("    largest pairwise overlap  : %d\n", max(demo_off)))
  cat(sprintf("    present in EVERY block    : %d\n",
              demo_v$summary$shared_by_all))

  cat("\n")
  show(demo_v$summary$cumulative_overlap)

  teach(
    "Nothing in the first two numbers hints at the third. Every pair looks
     complete because every pair IS complete; the losses are in different
     places and only compound when all eight are required at once. That is
     why the running total is reported next to the matrix rather than left
     for the reader to work out.")

  cat("\n  And what check_data() says about it:\n\n")

  for (w in grep("in every block", demo_v$warnings, value = TRUE)) {
    cat(paste(strwrap(w, width = 72, prefix = "    ", initial = "  ! "),
              collapse = "\n"), "\n", sep = "")
  }

  step("Where the count collapses, block by block")

  teach(
    "Blocks are added largest first. The row where the running total drops is
     the block that cost you those people, which is the thing you need to
     know and the thing a matrix cannot show.

     Note that a person counts as being in a block only if the block measured
     something on them. Their identifier appearing in it is not enough: a
     module administered to ninety people, assembled against the full sample
     list, carries a row for everyone with the other 1710 left empty. Counting
     row names would call it complete.")

  show(validation$summary$cumulative_overlap)

  step("And again after preprocessing, which is where it bites")

  teach(
    "The audit above runs on the raw blocks. Preprocessing then drops rows
     that are mostly empty, and for a study assembled from modules that lands
     entirely on the people a module did not cover. The same ladder on the
     preprocessed blocks is the one that decides whether an analysis can
     run.")

  show(.cumulative_overlap(clean$data$assays))

  step("Asking for all twenty anyway")

  teach(
    "This is what a user does on their first run: name no blocks and let the
     package use everything. Watch what it says. It does not merely report a
     count of zero and stop - a count of zero tells you the analysis cannot
     run and nothing about what to do next.")

  must_fail(
    analyze(clean, outcome = STUDY$outcome, covariates = STUDY$covariates,
            quiet = TRUE),
    "shared by all"
  )

  cat("\n  The full message:\n\n")

  msg <- tryCatch(
    analyze(clean, outcome = STUDY$outcome, covariates = STUDY$covariates,
            quiet = TRUE),
    error = conditionMessage)

  cat(paste0("  | ", strsplit(msg, "\n")[[1]]), sep = "\n")

  teach(
    "Three things are in there that a bare error would not have.

     The per-block counts, so the blocks measured on a handful of people are
     visible at a glance. The named blocks whose removal would restore a
     usable population, which is the actionable part. And the observation
     that the raw blocks did share a population before preprocessing, which
     rules out the explanation most people reach for first - that the sample
     identifiers do not match between files.")

  step("The same failure from a naming difference instead")

  teach(
    "The other way to get zero shared samples, and the one worth
     distinguishing: the two files describe the same people under different
     conventions. Nothing is wrong with the cohort at all.")

  ids <- paste0("P", 1:120)

  mismatched <- load_data(
    list(
      clinical = data.frame(bp = rnorm(120), bmi = rnorm(120),
                            row.names = ids),
      omics = data.frame(gene1 = rnorm(120), gene2 = rnorm(120),
                         row.names = sub("^P", "SUBJ-", ids))
    ),
    metadata = data.frame(sample_id = ids, y = rnorm(120),
                          stringsAsFactors = FALSE)
  )

  prepped <- preprocess(mismatched, check_data(mismatched), plots = FALSE,
                        quiet = TRUE, force = TRUE)

  cat("\n")

  msg2 <- tryCatch(
    analyze(prepped, "y", methods = "association", effort = "fast",
            plots = FALSE, quiet = TRUE),
    error = conditionMessage)

  cat(paste0("  | ", strsplit(msg2, "\n")[[1]]), sep = "\n")

  teach(
    "It names the diagnosis and shows identifiers from both sides rather than
     describing the difference, because seeing 'P1' beside 'SUBJ-1' settles
     it instantly and a sentence about naming conventions does not.")

  step("What to do about it")

  teach(
    "Three options, in the order worth trying.

     Drop the blocks that cost the most people. The message names them. This
     is usually right: a block measured on 113 of 1800 participants cannot
     contribute to an analysis of the other 1687 whatever you do to it.

     Analyse in groups. Nothing requires one analysis over all blocks. Two
     analyses over overlapping sets answer more than one analysis over an
     empty intersection.

     Loosen the sample filters in the recipe, but understand what you are
     buying: keeping rows that are mostly missing means imputing more of
     them, and the data-quality discount in a later section is there because
     that is not free.")

  cat("\n  The set this walkthrough settled on:\n")

  chosen <- get("ANALYSIS_BLOCKS", envir = globalenv())

  cat("    ", paste(chosen, collapse = ", "), "\n", sep = "")

  shared <- Reduce(intersect, lapply(chosen, function(b)
    rownames(clean$data$assays[[b]])))

  cat(sprintf("    %d blocks, %d people present in all of them\n",
              length(chosen), length(shared)))

}

# =============================================================================
# SECTION 21 - Causal diagrams and what an adjustment is worth
# =============================================================================

if (section(21, "check_dag(): what your adjustment is actually worth")) {

  lesson("Why 'we controlled for it' is not an argument on its own")

  teach(
    "Adjusting for a variable is the standard move in observational research
     and it is the one most often made without an argument. The reasoning
     usually stops at 'it could be related to both, so we controlled for it',
     and that reasoning is wrong often enough to matter.

     Whether adjusting helps depends entirely on where the variable sits in
     the causal structure, and there are three positions with three different
     consequences.

     A CONFOUNDER causes both the exposure and the outcome. Age causes both
     higher blood pressure and higher mortality, so the raw association
     between them is partly age. Adjusting for it removes that contamination.
     This is the case everyone has in mind.

     A MEDIATOR sits on the path between them. If smoking damages the lungs
     and damaged lungs kill, adjusting for lung function removes part of
     smoking's effect - the part that works through the lungs, which is most
     of it. The adjusted estimate is smaller than the truth, and the more
     careful the adjustment looks, the more of the effect it has deleted.

     A COLLIDER is caused by both. Adjusting for one opens a path that was
     closed and manufactures an association between things that have none.
     Among hospital patients, two unrelated diseases appear correlated,
     because being admitted required having something serious. Conditioning
     on admission created the correlation.

     The last two are the dangerous ones, because both look like diligence
     and both make the estimate worse than doing nothing at all. Nothing in
     the data distinguishes them. The correlation matrix is identical in all
     three cases; only the structure differs, and structure is a claim about
     the world, not a statistic.")

  step("A structure you are willing to defend")

  teach(
    "So the package will not guess. It takes the structure you are prepared
     to argue for and tells you what follows from it. A data.frame of arrows
     is enough - a dagitty object or specification string works too.")

  structure_df <- data.frame(
    from = c("age", "age", "smoking", "lung_function", "disease"),
    to   = c("smoking", "disease", "lung_function", "disease", "biomarker"),
    stringsAsFactors = FALSE
  )

  show(structure_df)

  teach(
    "Read as biology: age influences whether someone smokes and independently
     influences mortality, so it confounds. Smoking damages lung function
     which in turn kills, so lung function mediates. And the disease raises
     some biomarker, so the biomarker is downstream of the outcome
     entirely.")

  step("The confounder: adjusting helps")

  show(check_dag(structure_df, "smoking", "disease", adjusted = "age"))

  step("Adjusting for nothing")

  show(check_dag(structure_df, "smoking", "disease"))

  teach(
    "It names the variable that would fix it rather than reporting that the
     effect is unidentified and leaving you to work out which of your
     measured variables to reach for.")

  step("The mediator: adjusting deletes the effect")

  show(check_dag(structure_df, "smoking", "disease",
                 adjusted = c("age", "lung_function")))

  teach(
    "Note what the verdict leads with. It does not say 'a backdoor path
     remains open', which would send you hunting for a confounder you do not
     need. It names the conditioning that broke it.")

  step("The collider: adjusting invents an association")

  show(check_dag(structure_df, "smoking", "disease",
                 adjusted = c("age", "biomarker")))

  step("An effect nothing can identify")

  teach(
    "Some questions cannot be answered by any adjustment whatsoever. Asking
     what the biomarker does to the disease that produced it is one of them:
     the arrow points the other way and no set of control variables reverses
     an arrow.")

  show(check_dag(structure_df, "biomarker", "disease", adjusted = "age"))

  step("A variable the diagram does not mention")

  show(check_dag(structure_df, "not_in_the_diagram", "disease",
                 adjusted = "age"))

  teach(
    "It declines rather than guessing. A structure that says nothing about a
     variable licenses no conclusion about it, and inventing one would be the
     package overriding the user on the one input only the user can supply.")

  step("Guards")

  must_fail(check_dag(NULL, "a", "b"), "No usable causal structure")
  must_fail(.parse_dag(data.frame(a = 1, b = 2)), "'from' and 'to'")
  must_fail(.parse_dag("this is not a dag"), "Could not parse")
  must_fail(.parse_dag(42), "must be a dagitty object")

  teach(
    "That third one is worth a word. The dagitty package accepts text that is
     not a DAG without complaining and hands back a graph containing a single
     unnamed node. Passed on silently, that would audit every relationship as
     'not in the diagram', which reads like a finding about your model rather
     than the typo it is.")

  step("Inside analyze()")

  clean <- get("CLEAN", envir = globalenv())

  teach(
    "Supplying a diagram to analyze() audits every relationship against it.
     It changes no estimate - a structure is an interpretive claim, not a
     statistical one, and if it moved the numbers it would be fitting the
     data to the belief. It changes what may be concluded.")

  demo <- .cmo_dag_demo()

  plain <- show(analyze(demo$prep, "disease", covariates = "age",
                        methods = "association", effort = "fast",
                        plots = FALSE, quiet = TRUE), print_it = FALSE)

  audited <- show(analyze(demo$prep, "disease", covariates = "age",
                          dag = demo$structure, methods = "association",
                          effort = "fast", plots = FALSE, quiet = TRUE),
                  print_it = FALSE)

  cat("\n  Without a diagram, identifiable is unknown for every edge:\n")
  cat("    ", paste(unique(vapply(plain$evidence, function(e)
    as.character(e$identifiable), character(1))), collapse = ", "), "\n", sep = "")

  cat("\n  With one:\n\n")

  for (e in audited$evidence) {
    cat(sprintf("    %-12s -> %-10s  identification %-12s identifiable %s\n",
                e$source, e$target, e$identification, e$identifiable))
  }

  cat("\n")
  cat(paste0("  ", grep("DAG audit", audited$logs, value = TRUE)), sep = "\n")

  step("The estimates themselves are untouched")

  before <- sort(vapply(plain$evidence, function(e) e$estimate, numeric(1)))
  after <- sort(vapply(audited$evidence, function(e) e$estimate, numeric(1)))

  cat(sprintf("\n  identical: %s\n", isTRUE(all.equal(before, after))))

  step("Adjusting for the mediator, and being told")

  harmful <- show(analyze(demo$prep, "disease",
                          covariates = c("age", "inflammation"),
                          dag = demo$structure, methods = "association",
                          effort = "fast", plots = FALSE, quiet = TRUE),
                  print_it = FALSE)

  target <- Filter(function(e) identical(e$source, "protein"),
                   harmful$evidence)

  if (length(target) > 0) {

    e <- target[[1]]

    cat("\n    identification :", e$identification, " (downgraded from adjustment)\n")
    cat("    identifiable   :", e$identifiable, "\n")
    cat("    problem        :", paste(e$adjustment_problems, collapse = " "), "\n")
    cat("    warning        :", paste(e$warnings, collapse = " "), "\n")

  }

  teach(
    "The label is taken away, not just annotated. An estimate adjusted for a
     mediator is further from the truth than the unadjusted one, so letting
     it keep the 'adjusted' badge would present the more misleading number as
     the more careful one - which is exactly backwards, and exactly what a
     reader skimming badges would conclude.")

}

# =============================================================================
# SECTION 22 - Measured values and filled-in values
# =============================================================================

if (section(22, "Data quality: what was measured, what was invented")) {

  lesson("The uncertainty that disappears the moment you impute")

  teach(
    "Missing values are unavoidable in real cohorts, and the standard remedy
     is imputation: estimate what the missing number probably was, from the
     other people or the other variables, and carry on. It is usually the
     right call. Throwing away every person with one gap would cost most of
     the study.

     But it has a consequence nothing downstream can see. Once a gap is
     filled, no model can tell a measurement from an estimate. The confidence
     interval that comes out the far end is exactly as narrow as if every
     value had been real, because the uncertainty about the filling was
     discarded at the moment of filling and never came back.

     So a variable that arrived a quarter empty reports the same precision as
     one measured on everybody. Two findings that look equally solid are not,
     and nothing in the usual output says which is which.")

  step("What preprocessing actually did")

  clean <- get("CLEAN", envir = globalenv())

  show(clean$quality$table)

  teach(
    "Read the two missing columns together. A block going from 12% missing to
     0% did not improve: it was filled in. That is the entire point of this
     section.")

  step("Per feature, and by what method")

  provenance <- .feature_quality(clean)

  if (!is.null(provenance)) {

    imputed <- provenance[provenance$imputed, , drop = FALSE]

    cat(sprintf("\n  %d of %d features had values filled in.\n\n",
                nrow(imputed), nrow(provenance)))

    if (nrow(imputed) > 0) {
      show(utils::head(imputed[order(-imputed$missing_percent),
                               c("feature", "block", "missing_percent",
                                 "imputation_method", "quality")], 10))
    }

  }

  teach(
    "Methods are not interchangeable, and the quality column reflects that.
     Filling with the column mean collapses every gap onto a single number:
     the variance of that part of the column becomes zero and any
     relationship computed through it is dragged toward finding nothing.
     Nearest-neighbour imputation borrows from correlated variables and keeps
     most of the structure, so more of the filled portion still carries
     information.

     One thing that does NOT count against quality: values that were missing
     and never filled. Those rows are dropped by whichever model needed them,
     which costs sample size, and sample size is already in the precision
     score. Charging for it twice would be counting the same problem
     twice.")

  step("On the relationships")

  results <- get("RESULTS", envir = globalenv())

  affected <- Filter(function(e) length(e$quality_flags) > 0, results$evidence)

  cat(sprintf("\n  %d of %d relationships involve a variable partly filled in.\n",
              length(affected), length(results$evidence)))

  if (length(affected) > 0) {

    e <- affected[[1]]

    cat(sprintf("\n  %s -> %s\n", e$source, e$target))
    cat(sprintf("    data quality    : %.3f\n", e$data_quality))
    cat(sprintf("    limited by      : %s\n", e$quality_limited_by))
    for (f in e$quality_flags) cat("    ", f, "\n", sep = "")

  }

  teach(
    "An edge is worth no more than its worst-measured ingredient - both ends
     and everything adjusted for - and the limiting one is named. Averaging
     instead would let a clean outcome and four clean covariates hide an
     exposure that was two-thirds reconstructed, which is the one thing the
     reader needs told.

     The number scales the evidence score rather than entering the confidence
     score. Those are different problems with different remedies: a wide
     interval is fixed by collecting more people, and a column that was half
     invented is fixed by nothing.")

  step("Does it survive without the filled-in rows?")

  teach(
    "The cheapest honest check. Refit using only the people whose value was
     actually measured, and compare. Multiple imputation with Rubin's rules
     is the thorough version and costs a full analysis per replicate; this
     costs one model fit and catches the case that matters, which is a
     finding that exists only because the gaps were filled.")

  checked <- Filter(function(e) is.finite(e$complete_case_estimate),
                    results$evidence)

  for (e in utils::head(checked, 5)) {
    cat(sprintf("\n  %-24s pooled %8.4f   measured rows only %8.4f (n = %d)  %s\n",
                paste(e$source, "->", e$target), e$estimate,
                e$complete_case_estimate, e$complete_case_n,
                if (isTRUE(e$complete_case_agrees)) "same direction"
                else "REVERSES"))
  }

  step("A finding that is entirely an artefact of the filling")

  teach(
    "Real data rarely produces a clean example of the failure, so here is one
     built to order: an exposure that is pure noise among the people it was
     measured on, whose missing values happen to be filled in a way that
     tracks the outcome. The pooled estimate is positive. The truth, on the
     measured rows, is negative.")

  demo <- .cmo_artefact_demo()

  cat(sprintf("\n  pooled estimate                : %+.4f\n", demo$edge$estimate))
  cat(sprintf("  on the %d measured rows only   : %+.4f\n",
              demo$edge$complete_case_n, demo$edge$complete_case_estimate))
  cat(sprintf("  same direction                 : %s\n",
              demo$edge$complete_case_agrees))
  cat("\n")
  for (w in demo$edge$warnings) cat("  ! ", w, "\n", sep = "")

  step("Method fidelity, directly")

  for (m in c("mean", "median", "mode", "pseudocount", "knn", "unheard_of")) {
    cat(sprintf("  %-14s %.2f\n", m, .imputation_fidelity(m)))
  }

  teach(
    "Deliberately coarse. What has to be right is the ranking between methods
     and the direction of the penalty, not that 0.35 is measurable to two
     decimal places.")

}

# =============================================================================
# SECTION 23 - Would this repeat?
# =============================================================================

if (section(23, "The consensus graph: would this report repeat?")) {

  lesson("A picture from one sample, and the picture it came from")

  teach(
    "Everything in a report comes from the particular people who happened to
     end up in the study. Had recruitment gone slightly differently, would
     the same relationships be at the top?

     Per-relationship stability, which the package already reports, answers
     that one relationship at a time. It cannot answer it about the picture.
     A graph whose edges are each recovered six times in ten is a stable
     structure if it is the same six edges every time, and no structure at
     all if it is a different six. A single drawing cannot tell those
     apart.

     So the whole analysis is run again on hundreds of resamples of the same
     data, and what comes back is compared to what was reported - as sets,
     not one edge at a time. This costs nothing extra: the resampling was
     already running and the replicate graphs were already being built and
     thrown away.")

  resampled <- get("RESAMPLED", envir = globalenv())

  show(resampled$consensus)

  step("Both directions of disagreement")

  cg <- resampled$consensus

  if (!is.null(cg) && cg$replicates > 0) {

    a <- cg$agreement

    cat(sprintf("\n  reported here          : %d\n", a$reported))
    cat(sprintf("  recur reliably         : %d\n", a$consensus))
    cat(sprintf("  in both                : %d\n", a$both))
    cat(sprintf("  overlap (Jaccard)      : %.2f\n", .report_or(a$jaccard, NA)))

    teach(
      "The two disagreements are not equally visible to a reader, and the
       second is the interesting one.

       Relationships reported here that rarely came back are the weakest
       thing in the document, and at least a sceptical reader would think to
       ask about them.

       Relationships that came back reliably but are not in the report are
       invisible any other way. The sample that was collected happened not to
       show them clearly enough to clear the threshold. Nobody reading the
       report would ever learn they exist.")

    if (length(a$reported_only) > 0) {
      cat("\n  reported, rarely recur:\n")
      cat(paste0("    ", utils::head(a$reported_only, 8)), sep = "\n")
    }

    if (length(a$consensus_only) > 0) {
      cat("\n  recur, were not reported:\n")
      cat(paste0("    ", utils::head(a$consensus_only, 8)), sep = "\n")
    }

  }

  step("Recurrence is not recovery")

  teach(
    "A relationship that turns up in nine resamples out of ten pointing a
     different way each time has been found nine times and established
     nothing. Counting appearances alone would report it as highly stable, so
     consensus membership needs a consistent sign as well as a
     reappearance.")

  if (!is.null(cg) && is.data.frame(cg$edges) && nrow(cg$edges) > 0) {
    show(utils::head(cg$edges[, c("source", "target", "frequency",
                                  "consistent_frequency", "sign_agreement",
                                  "median_rank", "reported")], 10))
  }

  step("What this is not evidence of")

  teach(
    "This is the number most likely to be quoted out of context, so the
     object carries the caveat with it.

     A bootstrap resamples the people who were actually measured. It reports
     how much the picture depends on which of them ended up in the study, and
     nothing whatsoever about whether a relationship is real. A variable with
     no connection to the outcome that happens to track it in this sample
     will track it in almost every resample of that sample, and will look
     perfectly stable here.

     The question of whether these findings exceed what the same engine
     produces on noise is a different one, and null calibration answers it.
     That is the section on effort levels.")

  if (!is.null(cg)) for (nt in cg$notes) teach(nt)

}

# =============================================================================
# SECTION 24 - Does one number describe everybody?
# =============================================================================

if (section(24, "Heterogeneity: does one number describe everybody?")) {

  lesson("What an average conceals")

  teach(
    "Every estimate in this package is an average over the people measured,
     and an average says nothing about whether they resemble each other.

     A coefficient of 0.4 is equally compatible with 0.4 in everybody, and
     with 2.0 in a tenth of them and nothing in the rest. Those are different
     findings. The first is a property of the population and supports a
     population-level recommendation. The second is a property of a subgroup
     nobody has identified, and acting on it as though it were the first
     would treat nine people for no reason and under-treat the tenth.

     There are two ways to ask, and they need different evidence.")

  results <- get("RESULTS", envir = globalenv())

  step("Without naming anything: how many people carry it")

  teach(
    "The usual situation is not knowing what modifies the effect, so the
     first check assumes nothing. How much of the cohort would have to be
     removed to halve the estimate?

     For a relationship that holds broadly the answer is most of them,
     because removing a handful of people barely shifts an average. For one
     produced by a small unusual group, removing that group collapses it. The
     samples are taken in order of their influence, so this is the worst
     case - which is what someone deciding whether to believe a result needs,
     rather than what a random deletion would do.")

  checked <- Filter(function(e) is.finite(e$share_driving_effect),
                    results$evidence)

  for (e in utils::head(checked, 8)) {
    cat(sprintf("  %-30s halved by removing %4d people (%.1f%%)%s\n",
                paste(e$source, "->", e$target),
                e$samples_driving_effect, 100 * e$share_driving_effect,
                if (isTRUE(e$share_driving_effect <= 0.05)) "  <- a minority"
                else ""))
  }

  teach(
    "One guard is worth knowing about. An estimate indistinguishable from
     zero is exempt, because halving nothing costs nothing and without that
     rule every null result would be reported as driven by a handful of
     people.")

  lesson("Why every real relationship above is flagged, and why that is right")

  teach(
    "Every one of those is marked as resting on a minority, which looks like
     a broken measure until you calibrate it. So here it is calibrated:
     simulated data with the same number of people and the same kind of
     outcome, at four known effect sizes.")

  set.seed(1)

  n_cal <- 687

  for (b in c(0.2, 0.5, 1.0, 2.0)) {

    x <- rnorm(n_cal)
    y <- rbinom(n_cal, 1, stats::plogis(-1.2 + b * x))

    co <- summary(stats::glm(y ~ x, family = stats::binomial()))$coefficients
    cc <- .effect_concentration(y, x, binary = TRUE)

    cat(sprintf("    true effect %.1f  ->  z = %4.1f   halved by %s\n", b,
                co[2, 1] / co[2, 2],
                if (is.null(cc)) "- (too close to zero to judge)"
                else sprintf("%3d people (%.1f%%)", cc$k, 100 * cc$share)))

  }

  teach(
    "The measure scales with strength exactly as it should: a relationship at
     z = 4.7 is halved by 4% of the cohort, one at z = 12.5 needs half of
     them. So the reading of the real data is not that the measure is broken.
     It is that these associations between diet, routine bloods and five-year
     mortality are all marginal - z of three to five - and a marginal
     estimate genuinely is held up by whoever sits furthest from the middle.

     That is worth sitting with, because it is what screening 150 variables
     against a hard outcome in 687 people actually buys you. Nothing here is
     wrong. The engine found what is there, and what is there is thin. A
     paper reporting the top of that list as a discovery would be reporting
     the fifteen most unusual people in the study.")

  step("The three shapes, side by side")

  teach(
    "Real data cannot show all three cleanly, so here they are built to
     order: one relationship uniform across everybody, one present only in
     men, and one manufactured by eight extreme individuals.")

  shapes <- .cmo_shapes_demo()

  for (e in shapes$result$evidence) {

    cat(sprintf("\n  %s -> %s   estimate %+.3f\n", e$source, e$target,
                e$estimate))
    cat(sprintf("    halved by removing : %s people (%s)\n",
                e$samples_driving_effect,
                if (is.finite(e$share_driving_effect))
                  sprintf("%.1f%%", 100 * e$share_driving_effect) else "-"))
    cat(sprintf("    differs by sex     : FDR %s\n",
                format(signif(e$heterogeneity_fdr, 3))))

    if (is.data.frame(e$effect_by_group) && nrow(e$effect_by_group) > 0) {
      for (g in seq_len(nrow(e$effect_by_group))) {
        cat(sprintf("      %-4s n = %-4d slope %+.3f\n",
                    e$effect_by_group$group[g], e$effect_by_group$n[g],
                    e$effect_by_group$slope[g]))
      }
    }

  }

  teach(
    "Look at what the two checks catch. The uniform relationship needs a
     sixth of the cohort removed and shows no subgroup difference. The one
     built into men only reverses sign between the groups. The one carried by
     eight people is halved by removing five.

     And note the middle case is flagged by BOTH checks. That is not a
     coincidence: a pooled estimate over groups that disagree is a
     near-cancellation, and a near-cancellation is by nature held up by
     whoever is most extreme.")

  step("Naming a candidate modifier")

  teach(
    "The second way needs you to guess what modifies the effect. When you
     have a candidate - sex, age group, treatment arm - name it and every
     relationship is tested against it directly.")

  hetero <- get("RESAMPLED", envir = globalenv())$diagnostics$heterogeneity

  if (isTRUE(hetero$available)) {
    show(utils::head(hetero$table[, c("feature", "moderator", "interaction_p",
                                      "interaction_fdr", "groups", "slopes",
                                      "same_sign")], 8))
  }

  teach(
    "Read a null here with care. Detecting that an effect DIFFERS between
     groups needs several times the data that detecting the effect itself
     needs - a study powered to find a main effect is usually nowhere near
     able to find an interaction. So 'no difference detected' is weak
     reassurance, not evidence that the effect is uniform, and the package
     says so rather than letting the absence read as a finding.")

}

# =============================================================================
# SECTION 25 - Things measured many times
# =============================================================================

if (section(25, "Modules: the level between a variable and a block")) {

  lesson("Fifty transcripts are not fifty findings")

  teach(
    "Measured variables are rarely independent things.

     Fifty transcripts that rise and fall together are one biological process
     measured fifty times. Testing each separately answers a question nobody
     asked, pays the multiplicity penalty fifty times over, and reports fifty
     findings where there is one. Worse, it makes the process look
     overwhelming in a ranked list simply because it was measured often.

     Two resolutions already exist here. The single variable, and the block -
     but a block is whatever you called a block when you loaded the data,
     which is a fact about how the files arrived rather than about biology.
     The missing level is the one the data can define for itself.")

  results <- get("RESULTS", envir = globalenv())

  show(results$modules)

  step("Why correlation and not the evidence graph")

  teach(
    "The package already detects communities in the evidence graph, and it
     would have been cheaper to reuse them. It would also have been wrong.

     A community there groups variables that each have a link to the outcome.
     They can do that while being completely uncorrelated with each other -
     five independent risk factors form a community without being one thing.
     Summarising such a group by its first principal component summarises
     nothing.

     A module has to be a set of variables that actually vary together before
     summarising it means anything, so modules are clustered on correlation.
     Variables that move in opposite directions count as together: two
     measures of the same thing on reversed scales are one thing, and the
     sign is settled afterwards.")

  step("The summary, and how much it captures")

  mg <- results$modules

  if (is.data.frame(mg$modules) && nrow(mg$modules) > 0) {

    show(mg$modules[, c("module", "size", "blocks", "composition",
                        "variance_explained", "coherent", "cross_block")])

    teach(
      "The variance column is the honesty check. If a module's first
       component carries 85% of how its members vary, it genuinely stands for
       them. If it carries 30%, it is one direction through a cloud and the
       'module effect' summarises nothing - which is reported rather than
       hidden, because a group that failed to cohere is itself a finding
       about the data.

       Modules spanning several blocks are flagged. They are the only thing
       at this resolution that could be a mechanism rather than an artefact
       of one measurement platform: transcripts moving with metabolites is a
       claim about biology, transcripts moving with transcripts might be a
       claim about the array.")

  }

  step("Each module against the outcome")

  if (is.data.frame(mg$edges) && nrow(mg$edges) > 0) {
    show(mg$edges[, c("module", "estimate", "ci_lower", "ci_upper", "fdr",
                      "n", "coherent", "cross_block")])
  }

  step("A planted process, recovered")

  teach(
    "Real data has no known answer, so here is a case where the answer is
     known: one latent process measured six times in one block and four times
     in another, a second process confined to one block and unrelated to the
     outcome, and four variables that genuinely stand alone.")

  planted <- .cmo_module_demo()

  show(planted$modules$modules[, c("module", "size", "blocks",
                                   "variance_explained", "cross_block")])

  cat("\n  membership:\n")
  mb <- planted$modules$membership
  for (k in sort(unique(stats::na.omit(mb)))) {
    cat(sprintf("    module_%d : %s\n", k,
                paste(names(mb)[which(mb == k)], collapse = ", ")))
  }

  cat("\n  left out of every module: ",
      paste(names(mb)[is.na(mb)], collapse = ", "), "\n", sep = "")

  show(planted$modules$edges[, c("module", "estimate", "fdr", "cross_block")])

  teach(
    "The cross-block module is the planted process and it is related to the
     outcome. The single-block one is the decoy and it is not. The four
     independent variables were left alone rather than forced somewhere.")

  step("What a module is not")

  teach(
    "A module and the variables inside it are the same measurements at two
     resolutions, not two findings. If both appear in a report they agree
     because they are made of each other, and neither confirms the other.
     Count the finding once.

     One technical detail decides whether the two can even be compared. A raw
     first principal component is about the square root of its eigenvalue
     wide, so a ten-variable module arrives roughly three times wider than
     its own members, and its coefficient comes out three times smaller for
     no reason but arithmetic. Read beside the members, that looks like the
     module contradicting what it is made of. The component is scaled to unit
     variance so that its coefficient means what a variable's coefficient
     means: change per standard deviation.")

}

# =============================================================================
# SECTION 26 - From evidence to a claim
# =============================================================================

if (section(26, "hypothesis(): stating something that could be wrong")) {

  lesson("A result is not yet a statement")

  teach(
    "A graph of forty scored relationships is not a scientific statement. It
     is material from which statements can be made, and everything
     interesting happens in the making: which relationship, at what strength,
     under which assumptions, and what would have to be observed for it to be
     wrong.

     That last question is the one a result never answers and a hypothesis
     must. It is also the one a reader can act on. 'Residual confounding
     cannot be excluded' is true of almost every observational finding ever
     published and tells nobody what to do next; 'measure a confounder
     associated with both at 4.4 or more' names an experiment.")

  results <- get("RESULTS", envir = globalenv())

  step("The strongest claim in the analysis")

  h <- show(hypothesis(results))

  step("What decides the grade")

  teach(
    "Identification, and nothing else.

     Precision, agreement between methods and stability under resampling all
     describe how WELL an association was estimated. None of them says
     anything about what it is evidence OF. Letting them raise the grade
     would turn a well-measured correlation into a cause by arithmetic, which
     is the single most common way a paper overstates its result.

     So a relationship found by six methods, surviving every resample, with a
     confidence interval a tenth as wide as its estimate, is still an
     association if nothing identified it. The sentence changes with the
     grade rather than with the score - 'higher X goes with higher Y'
     describes what was seen, 'raising X would raise Y' describes what would
     happen, and only identification licenses the second.")

  step("A named relationship instead of the top one")

  available <- unique(vapply(
    Filter(function(e) identical(e$target, results$outcome$name),
           results$evidence),
    function(e) e$source, character(1)))

  if (length(available) > 1) show(hypothesis(results, available[2]))

  step("What would settle it")

  teach(
    "Each item comes from a specific weakness of this specific relationship
     rather than from a stock paragraph of caution, which is why a clean
     claim gets a short list and a fragile one gets a pointed one.")

  for (item in h$settles) {
    cat("\n")
    cat(paste(strwrap(item, width = 72, prefix = "    ", initial = "  > "),
              collapse = "\n"), "\n", sep = "")
  }

  step("A claim with a diagram behind it")

  teach(
    "Supplying a causal diagram that confirms the adjustment does raise the
     grade, conditionally and explicitly. That is the entire purpose of
     auditing an adjustment against a stated structure; leaving the grade
     alone would make the audit decorative.")

  demo <- .cmo_dag_demo()

  identified <- analyze(demo$prep, "disease", covariates = "age",
                        dag = demo$structure, methods = "association",
                        effort = "fast", plots = FALSE, quiet = TRUE)

  show(hypothesis(identified, "protein"))

  step("Taking a claim to a cohort it has never seen")

  teach(
    "The claim carries its own protocol - which outcome, which covariates,
     which model - so what runs on the new data is what ran on the old. This
     is the reproducibility promise made concrete rather than asserted.")

  replication <- .cmo_replication_demo()

  cat("\n  A cohort where the relationship is real:\n")
  show(test_hypothesis(replication$claim, replication$real))

  cat("\n  A cohort where it is not:\n")
  absent <- test_hypothesis(replication$claim, replication$absent)

  cat(sprintf("    verdict : %s\n", absent$replication$verdict))
  cat(sprintf("    there   : %+.4f     here: %+.4f\n",
              absent$replication$estimate, absent$replication$original))
  cat(sprintf("    ratio   : %.3f\n", absent$replication$ratio))

  cat("\n  A cohort where the direction holds but the effect is a fifth the size:\n")
  weak <- test_hypothesis(replication$claim, replication$weak)

  cat(sprintf("    verdict : %s\n", weak$replication$verdict))
  cat(sprintf("    ratio   : %.3f\n", weak$replication$ratio))

  teach(
    "That last case is the one worth dwelling on. The direction held and the
     result is statistically significant, so a replication judged on p-values
     would be announced as a success. The effect is a fifth the size. Judging
     on direction and magnitude reports it as what it is.")

  step("When the new cohort cannot answer the question")

  cohort <- replication$real

  no_outcome <- cohort
  no_outcome$metadata[[replication$claim$protocol$outcome]] <- NULL

  must_fail(test_hypothesis(replication$claim, no_outcome), "column to test")

  must_fail(test_hypothesis(replication$claim, "not a cohort"),
            "after preprocessing")

  must_fail(test_hypothesis("not a hypothesis", cohort), "must be a Hypothesis")

  must_fail(hypothesis(results, "no_such_variable"), "no relationship with")

  must_fail(hypothesis("not a result"), "must be a CMOResult")

  step("Covariates the new cohort does not have")

  teach(
    "Named rather than quietly dropped. A model missing an adjustment is not
     the model the claim was made with, and reporting the comparison without
     saying so would be the most flattering possible reading.")

  stripped <- cohort
  stripped$metadata$age <- NULL

  partial <- test_hypothesis(replication$claim, stripped)

  cat(sprintf("\n    used    : %s\n",
              paste(partial$replication$covariates_used, collapse = ", ")))
  cat(sprintf("    missing : %s\n",
              paste(partial$replication$covariates_missing, collapse = ", ")))
  for (nt in partial$replication$notes) cat("    ! ", nt, "\n", sep = "")

}

# =============================================================================
# SECTION 27 - Inventory
# =============================================================================

if (section(27, "What the package contains")) {

  step("Everything exported")

  exported <- tryCatch(
    sort(getNamespaceExports("CausalMultiOmics")),
    error = function(e) character(0)
  )

  if (length(exported) > 0) {
    show(exported)
  } else {
    note("Namespace exports unavailable under load_all(); see NAMESPACE.")
  }

  step("Functions per source file")

  if (dir.exists("R")) {

    counts <- vapply(list.files("R", pattern = "[.]R$", full.names = TRUE),
                     function(f) {
                       length(grep("^[a-zA-Z._][a-zA-Z0-9._]*\\s*<-\\s*function",
                                   readLines(f, warn = FALSE)))
                     }, integer(1))

    names(counts) <- basename(names(counts))

    print(counts)
    cat("\n  total:", sum(counts), "\n")

  }

  step("S3 methods registered for dispatch")

  generics <- c("print", "summary", "report")
  classes <- c("MultiOmicsData", "BlockDiagnostics",
               "TransformationRecommendation", "PreprocessingRecipe",
               "PreprocessingResult", "CMOValidation", "CMOResult",
               "EvidenceEdge", "EvidenceGraph",
               "ConsensusGraph", "ModuleGraph", "Hypothesis",
               "CMODagCheck", "CMOExplanation", "CMOCounterfactual")

  for (g in generics) {
    for (cls in classes) {
      m <- tryCatch(utils::getS3method(g, cls, optional = TRUE),
                    error = function(e) NULL)
      if (!is.null(m)) cat(sprintf("  %-8s %s\n", g, cls))
    }
  }

}

# =============================================================================
# Scoreboard
# =============================================================================

cat("\n\n")
rule("=")
cat("WALKTHROUGH COMPLETE\n")
rule("=")

cat(sprintf("\n  calls that succeeded : %d\n", .n_ok))
cat(sprintf("  calls that failed    : %d\n", .n_fail))

if (.n_fail > 0) {
  cat("\n  FAILURES:\n")
  cat(paste0("    - ", .failures, collapse = "\n"), "\n")
}

cat(sprintf("\n  elapsed : %.1f s\n",
            as.numeric(difftime(Sys.time(), CMO_START, units = "secs"))))

cat("\n  Transcript : ", .transcript, "\n", sep = "")
cat("  HTML reports and figures: ", .outdir, "\n", sep = "")

cat("\n")
if (.n_fail == 0) cat("  Everything worked.\n") else
  cat("  Something is wrong - see the failures above.\n")
rule("=")
cat("\n")
