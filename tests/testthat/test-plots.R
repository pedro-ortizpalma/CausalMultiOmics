test_that("cmo_plots() lists what an object carries", {

  data <- simulate_data(n = 60, blocks = list(main = 6), seed = 7)
  audit <- check_data(data)

  names <- cmo_plots(audit)

  expect_type(names, "character")
  expect_true(length(names) > 0)
  # Only the slots that actually hold a recorded figure are listed: an audit
  # run with plots = FALSE keeps the named slots but records nothing.
  expect_true(all(names %in% names(audit$plots)))
  expect_true(all(vapply(names, function(nm) {
    slot <- audit$plots[[nm]]
    inherits(slot, "recordedplot") ||
      (is.list(slot) && any(vapply(slot, inherits, logical(1), "recordedplot")))
  }, logical(1))))

})


test_that("plot() without a name lists instead of failing", {

  data <- simulate_data(n = 60, blocks = list(main = 6), seed = 8)
  audit <- check_data(data)

  # Base R answers plot(list) with a message about components 'x' and 'y',
  # which tells a user nothing. Listing is the useful answer to "what can I
  # see?".
  shown <- utils::capture.output(plot(audit))

  expect_true(any(grepl("figure\\(s\\) available", shown)))
  expect_true(any(grepl("Draw one with", shown)))

})


test_that("plot() names the alternatives when asked for a figure that is absent", {

  data <- simulate_data(n = 60, blocks = list(main = 6), seed = 9)
  audit <- check_data(data)

  expect_error(plot(audit, "not_a_figure"), "There is no figure called")
  expect_error(plot(audit, "not_a_figure"), "Available:")

})


test_that("plot() asks for a block when the figure is drawn per block", {

  data <- simulate_data(n = 60, blocks = list(a = 5, b = 5), seed = 10)
  audit <- check_data(data)

  per_block <- Filter(function(slot)
    is.list(slot) && !inherits(slot, "recordedplot") && length(slot) > 1,
    audit$plots)

  skip_if(length(per_block) == 0, "no per-block figure in this audit")

  name <- names(per_block)[1]

  expect_error(plot(audit, name), "drawn once per block")
  expect_error(plot(audit, name, block = "not_a_block"), "has no")

})


test_that("plot() draws a recorded figure to a device", {

  data <- simulate_data(n = 60, blocks = list(main = 6), seed = 11)
  audit <- check_data(data)

  file <- tempfile(fileext = ".png")

  grDevices::png(file, width = 400, height = 400)
  on.exit({
    if (!is.null(grDevices::dev.list())) grDevices::dev.off()
    unlink(file)
  }, add = TRUE)

  drawn <- plot(audit, "missing_by_block")

  grDevices::dev.off()

  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
  expect_true(inherits(drawn, "recordedplot") ||
                inherits(drawn, "recordedPlot"))

})


test_that("an object without figures says so instead of erroring obscurely", {

  data <- simulate_data(n = 60, blocks = list(main = 6), seed = 12)

  ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)

  fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = "association"))

  expect_error(plot(fit, "network"), "carries no figures")
  expect_message(cmo_plots(fit), "carries no figures")

})



test_that("the PNG device type is probed rather than assumed", {

  type <- .png_device_type()

  # Either a type that works on this machine, or an honest NA. Never a guess:
  # the previous code assumed "cairo", which is unavailable on a Mac without
  # XQuartz and on any R built without it, and the report came out silently
  # short of every figure.
  expect_true(is.na(type) ||
              type %in% c("quartz", "cairo", "Xlib", "windows", "cairo-png"))

  # The probe must leave the device stack exactly as it found it. A device left
  # open sends every later plot to the wrong place, and R CMD check fails on a
  # device it did not open itself.
  before <- length(grDevices::dev.list())
  invisible(.png_device_type())
  expect_equal(length(grDevices::dev.list()), before)

  # And whatever it names has to actually open.
  if (!is.na(type)) {

    file <- tempfile(fileext = ".png")
    # Big enough for the default margins. A tiny device makes plot.new() fail
    # with "figure margins too large", which would be a failure of the test
    # rather than of the device it is supposed to be checking.
    grDevices::png(file, width = 400, height = 300, type = type)
    graphics::plot(1)
    grDevices::dev.off()

    expect_true(file.exists(file))
    expect_gt(file.size(file), 0)

    unlink(file)

  }

})
