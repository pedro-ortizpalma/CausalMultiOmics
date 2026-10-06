test_that("every class has a working print() and summary()", {

  objects <- list(
    MultiOmicsData = MultiOmicsData(),
    BlockDiagnostics = BlockDiagnostics(),
    TransformationRecommendation = TransformationRecommendation(),
    PreprocessingRecipe = PreprocessingRecipe(),
    PreprocessingResult = PreprocessingResult(),
    CMOValidation = CMOValidation(),
    CMOResult = CMOResult()
  )

  for (nm in names(objects)) {

    expect_no_error(capture.output(print(objects[[nm]])))
    expect_no_error(capture.output(summary(objects[[nm]])))

  }

})

test_that("S3 methods are registered for dispatch, not merely defined", {

  # export() alone is not enough: without S3method() in NAMESPACE dispatch
  # falls through to the default and the methods are dead code.

  generics <- list(
    c("print", "MultiOmicsData"), c("print", "BlockDiagnostics"),
    c("print", "TransformationRecommendation"), c("print", "PreprocessingRecipe"),
    c("print", "PreprocessingResult"), c("print", "CMOValidation"),
    c("print", "CMOResult"),
    c("summary", "MultiOmicsData"), c("summary", "BlockDiagnostics"),
    c("summary", "TransformationRecommendation"), c("summary", "PreprocessingRecipe"),
    c("summary", "PreprocessingResult"), c("summary", "CMOValidation"),
    c("summary", "CMOResult"),
    c("report", "CMOValidation")
  )

  for (g in generics) {

    expect_false(
      is.null(utils::getS3method(g[1], g[2], optional = TRUE)),
      label = paste0(g[1], ".", g[2], " is registered")
    )

  }

})

test_that("report() renders the console format", {

  v <- check_data(cmo_object())

  out <- capture.output(report(v, format = "console"))

  expect_true(length(out) > 40)
  expect_true(any(grepl("Data Validation Report", out)))
  expect_true(any(grepl("PASSED", out)))

})

test_that("report() console honours the section selection", {

  v <- check_data(cmo_object())

  for (sec in c("overview", "blocks", "issues", "overlap", "preprocessing",
                "transformations", "qc", "actions", "outputs")) {

    expect_no_error(capture.output(
      report(v, format = "console", sections = sec)
    ))

  }

  expect_error(report(v, format = "console", sections = "not_a_section"))
  expect_error(report(v, format = "pdf"))

})

test_that("report() returns its input unmodified", {

  v <- check_data(cmo_object())

  out <- capture.output(back <- report(v, format = "console", sections = "qc"))

  expect_identical(back, v)

})

test_that("report() writes a self-contained HTML document", {

  v <- check_data(cmo_object())
  path <- file.path(tempdir(), "cmo-test-report.html")

  on.exit(unlink(path), add = TRUE)

  report(v, file = path, open = FALSE, quiet = TRUE)

  expect_true(file.exists(path))

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(txt, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(txt, "</html>", fixed = TRUE)
  # Everything above holds on any machine. The embedded figures need a working
  # PNG device, which a Mac without XQuartz does not have.
  if (!is.na(.png_device_type())) {
    expect_match(txt, "data:image/png;base64,", fixed = TRUE)
  }

  # No CDN, no remote fonts: the file has to open with no network at all.
  expect_false(grepl("(src|href)=[\"']https?://", txt))

})

test_that("report() survives a degenerate validation", {

  bare <- file.path(tempdir(), "cmo-test-bare.html")
  on.exit(unlink(bare), add = TRUE)

  expect_no_error(capture.output(report(CMOValidation(), format = "console")))
  expect_no_error(report(CMOValidation(), file = bare, open = FALSE, quiet = TRUE))

  expect_true(file.exists(bare))

})

test_that("report() has no method for other classes", {

  expect_error(report(cmo_object()), "no applicable method")

})

test_that(".report_destination() resolves paths", {

  dir <- file.path(tempdir(), "cmo-dest")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  expect_match(.report_destination("noext", prompt = FALSE), "\\.html$")
  expect_match(.report_destination("keep.htm", prompt = FALSE), "\\.htm$")

  expect_equal(
    basename(.report_destination(dir, prompt = FALSE)),
    "CausalMultiOmics_validation_report.html"
  )

  deep <- file.path(tempdir(), "cmo-deep", "nested")
  unlink(deep, recursive = TRUE)

  invisible(.report_destination(file.path(deep, "r.html"), prompt = FALSE))
  expect_true(dir.exists(deep))

})

test_that("the base64 encoder matches the RFC 4648 vectors", {

  expect_equal(.html_base64(charToRaw("Man")), "TWFu")
  expect_equal(.html_base64(charToRaw("Ma")), "TWE=")
  expect_equal(.html_base64(charToRaw("M")), "TQ==")
  expect_equal(.html_base64(raw(0)), "")
  expect_equal(.html_base64(charToRaw("hello world")), "aGVsbG8gd29ybGQ=")

})

test_that("HTML escaping is applied and not doubled", {

  expect_equal(.html_escape("<script>"), "&lt;script&gt;")
  expect_equal(.html_escape("a & b"), "a &amp; b")
  expect_equal(.html_escape("&<"), "&amp;&lt;")
  expect_equal(.html_escape(NA), "")
  expect_equal(.html_id("a b/c"), "a_b_c")

})
