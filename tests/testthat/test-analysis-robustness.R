# Regression tests for defects found by probing the engine with degenerate
# and adversarial input rather than by reading it.

test_that("an unordered outcome is refused rather than silently ranked", {

  # Regression: as.factor() ordered the categories alphabetically and every
  # model downstream treated "blue < green < red < yellow" as a scale. The
  # analysis ran and produced results that looked entirely normal.

  meta <- data.frame(
    sample_id = paste0("S", 1:40),
    colour = rep(c("red", "green", "blue", "yellow"), 10),
    stringsAsFactors = FALSE
  )

  expect_error(
    .resolve_outcome(meta, "colour", meta$sample_id),
    "unordered categories"
  )

  expect_error(
    .resolve_outcome(meta, "colour", meta$sample_id),
    "one category against the rest"
  )

})

test_that("two-category and numeric outcomes are still accepted", {

  meta <- data.frame(
    sample_id = paste0("S", 1:40),
    yes_no = rep(c("yes", "no"), 20),
    score = rnorm(40),
    graded = rep(1:4, 10),
    stringsAsFactors = FALSE
  )

  expect_equal(.resolve_outcome(meta, "yes_no", meta$sample_id)$type, "binary")
  expect_equal(.resolve_outcome(meta, "score", meta$sample_id)$type, "continuous")

  # An ordered numeric grading is a scale, so it stays allowed.
  expect_equal(.resolve_outcome(meta, "graded", meta$sample_id)$type, "continuous")

})

test_that("a time variable that changes nothing is reported, not swallowed", {

  # Regression: `time` was accepted, used by nothing, and never mentioned.

  meta <- data.frame(sample_id = paste0("S", 1:20), t = 1:20,
                     stringsAsFactors = FALSE)

  expect_warning(
    design <- .detect_design(meta, list(type = "continuous"), time = "t"),
    "was ignored"
  )

  expect_equal(design$type, "cross-sectional")
  expect_true(any(grepl("no time structure", design$notes)))

  # It stays silent when the time variable does drive the design.
  expect_silent(
    .detect_design(meta, list(type = "binary"), time = "t")
  )

})

test_that("path search is bounded on a dense graph", {

  skip_if_not_installed("igraph")

  # Regression: all_simple_paths() is exponential in density. A fifteen-node
  # dense graph took twenty seconds; a real one would not return.

  nodes <- paste0("V", 1:14)

  edges <- expand.grid(source = nodes, target = c(nodes, "OUT"),
                       stringsAsFactors = FALSE)
  edges <- edges[edges$source != edges$target, ]

  set.seed(1)
  edges$evidence_score <- runif(nrow(edges), 10, 90)

  graph <- EvidenceGraph()
  graph$edges <- edges
  graph$nodes <- data.frame(name = c(nodes, "OUT"), stringsAsFactors = FALSE)
  graph$igraph <- igraph::graph_from_data_frame(
    edges[, c("source", "target", "evidence_score")],
    directed = TRUE, vertices = graph$nodes["name"]
  )

  elapsed <- system.time(
    result <- .extract_paths(graph, "OUT",
                             list(max_path_length = 4, max_paths = 50,
                                  max_path_edges = 60))
  )[["elapsed"]]

  expect_lt(elapsed, 5)

  expect_true(result$search$trimmed)
  expect_equal(result$search$edges_searched, 60)
  expect_lt(result$search$edges_searched, result$search$edges_available)

})

test_that("a small graph is searched whole and says so", {

  skip_if_not_installed("igraph")

  edges <- data.frame(
    source = c("A", "B"), target = c("B", "OUT"),
    evidence_score = c(50, 60), stringsAsFactors = FALSE
  )

  graph <- EvidenceGraph()
  graph$edges <- edges
  graph$nodes <- data.frame(name = c("A", "B", "OUT"), stringsAsFactors = FALSE)
  graph$igraph <- igraph::graph_from_data_frame(
    edges, directed = TRUE, vertices = graph$nodes["name"]
  )

  result <- .extract_paths(graph, "OUT",
                           list(max_path_length = 4, max_paths = 50,
                                max_path_edges = 60))

  expect_false(result$search$trimmed)
  expect_equal(nrow(result$paths), 1)
  expect_equal(result$paths$path[1], "A -> B -> OUT")

})

test_that("mediation screens candidates before bootstrapping them", {

  # Regression: bootstrapping every cross-block triple is quadratic in the
  # candidates and linear in the resamples. Twenty candidates cost forty
  # thousand model fits and about forty seconds.

  set.seed(4)

  n <- 80
  x <- matrix(rnorm(n * 20), nrow = n)
  colnames(x) <- paste0("M", 1:20)
  rownames(x) <- paste0("S", seq_len(n))

  context <- list(
    x = x,
    outcome = list(values = rnorm(n)),
    feature_block = stats::setNames(rep(c("a", "b"), each = 10), colnames(x)),
    params = list(mediation_candidates = colnames(x), bootstrap = 200,
                  seed = 1, max_mediation_tests = 30)
  )

  elapsed <- system.time(edges <- .evidence_mediation(context))[["elapsed"]]

  expect_lt(elapsed, 20)
  expect_true(is.list(edges))

})

test_that("an integrated edge reports no p-value rather than an infinite one", {

  # Regression: min(all-NA, na.rm = TRUE) is Inf, and Inf then travelled into
  # the reported tables as though it were a p-value.

  a <- .new_edge("A", "B", estimate = 0.5, generator = "g1", method = "m1")
  b <- .new_edge("A", "B", estimate = 0.6, generator = "g2", method = "m2")

  merged <- .integrate_evidence(list(a, b), list(min_evidence_score = 0))

  expect_length(merged, 1)
  expect_true(is.na(merged[[1]]$p_value))
  expect_false(is.infinite(merged[[1]]$p_value))

})

test_that("a real p-value still survives integration", {

  a <- .new_edge("A", "B", estimate = 0.5, p_value = 0.01,
                 generator = "g1", method = "m1")
  b <- .new_edge("A", "B", estimate = 0.6, p_value = 0.20,
                 generator = "g2", method = "m2")

  merged <- .integrate_evidence(list(a, b), list(min_evidence_score = 0))

  expect_equal(merged[[1]]$p_value, 0.01)

})

# =============================================================================
# The work that is skipped is work nobody reads
# =============================================================================

test_that("resampling skips the per-method table, and reporting keeps it", {

  # Resampling integrates a whole graph per replicate and then reads four
  # fields of each edge. Building the contributions table five thousand times
  # and discarding it was most of what resampling cost, so it is skipped
  # there. If it were skipped anywhere the reader looks, every finding in the
  # report would lose the numbers behind it.

  sim <- simulate_data(
    n = 300, blocks = list(a = 8),
    dag = data.frame(from = "a_01", to = "y", effect = 0.7),
    outcome = "y", seed = 3)

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "y", effort = "standard", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = c("association", "conditional")))

  # Every reported edge keeps its own evidence.
  for (e in res$evidence) {
    expect_s3_class(e$contributions, "data.frame")
    expect_gt(nrow(e$contributions), 0)
  }

  # And resampling still produced everything it is there for.
  expect_true(all(vapply(res$evidence,
                         function(e) is.finite(e$bootstrap_stability),
                         logical(1))))

  expect_gt(res$consensus$replicates, 0)
  expect_gt(nrow(res$consensus$edges), 0)

})

test_that("skipping it changes nothing about the answer", {

  edges <- list(
    .new_edge("x", "y", 0.8, "association", "linear regression",
              quantity = "beta", se = 0.1, p_value = 1e-6, n = 100L),
    .new_edge("x", "y", 0.7, "conditional", "partial correlation",
              quantity = "partial_correlation", se = 0.1, p_value = 1e-5,
              n = 100L)
  )

  params <- list(min_evidence_score = 0, seed = 1)

  with_table <- .integrate_evidence(edges, params, contributions = TRUE)[[1]]
  without <- .integrate_evidence(edges, params, contributions = FALSE)[[1]]

  for (field in c("estimate", "evidence_score", "strength", "confidence",
                  "consistency", "level", "identification", "e_value")) {
    expect_equal(with_table[[field]], without[[field]], info = field)
  }

  expect_gt(nrow(with_table$contributions), 0)
  expect_equal(nrow(without$contributions), 0)

})
