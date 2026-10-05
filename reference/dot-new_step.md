# Record one executed pipeline step

Record one executed pipeline step

## Usage

``` r
.new_step(
  index,
  block,
  stage,
  method,
  parameters = list(),
  model = NULL,
  before = c(NA, NA),
  after = c(NA, NA),
  removed_samples = character(),
  removed_features = character(),
  messages = character(),
  runtime = NA_real_,
  replay = TRUE
)
```
