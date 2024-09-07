Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    initialize = function(p,
                          mapping,
                          ...) {
      self$p <- p
      self$m <- mapping
      params <- process_qrow_params(self$p, self$m)
      obj <- params |>
        lapply(\(x) Qtab$new(x, mapping))
      self$qtabs <- tibble::tibble(params, obj)
    }
  )
)
