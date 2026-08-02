# Shared fixtures. Deliberately small so the suite stays fast, but dirty
# enough to exercise the missing-value, constant-feature and multi-block
# paths that the diagnostics engine branches on.

cmo_block <- function(n = 20, seed = 42) {

  set.seed(seed)

  data.frame(
    G1 = rnorm(n),
    G2 = rnorm(n),
    G3 = rep(1, n),                # constant feature
    G4 = c(rnorm(n - 1), NA),      # missing value
    row.names = paste0("S", seq_len(n))
  )

}

cmo_counts <- function(n = 18, seed = 43) {

  set.seed(seed)

  data.frame(
    P1 = rpois(n, 5),
    P2 = rpois(n, 10),
    P3 = rpois(n, 3),
    row.names = paste0("S", seq_len(n))
  )

}

cmo_compositional <- function(n = 20, p = 5, seed = 44) {

  set.seed(seed)

  m <- matrix(rgamma(n * p, shape = 2), ncol = p)
  m <- m / rowSums(m)

  m <- as.data.frame(m)
  colnames(m) <- paste0("OTU", seq_len(p))
  rownames(m) <- paste0("S", seq_len(n))

  m

}

cmo_metadata <- function(n = 20, seed = 45) {

  set.seed(seed)

  data.frame(
    sample_id = paste0("S", seq_len(n)),
    group = rep(c("A", "B"), length.out = n),
    batch = rep(c("X", "Y"), length.out = n),
    stringsAsFactors = FALSE
  )

}

cmo_object <- function() {

  load_data(
    assays = list(
      transcriptomics = cmo_block(),
      proteomics = cmo_counts(),
      microbiome = cmo_compositional()
    ),
    metadata = cmo_metadata()
  )

}
