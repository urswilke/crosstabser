Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    log = list(warn = NULL, error = NULL),
    initialize = function(p,
                          mapping,
                          ...) {
      self$p <- p
      self$m <- mapping

      params <- process_qrow_params(self$p, self$m)

      obj <- withCallingHandlers(
        tryCatch({
          params |>
            lapply(\(x) Qtab$new(x, mapping))
        },
        error = function(e) {
          self$log$error <- capture.output(e)[-1] |> paste(collapse = "\n")
          obj <- list(NULL)
        }),
        warning = function(w) {
          self$log$warn <- capture.output(w)[-1] |> paste(collapse = "\n")
          tryInvokeRestart("muffleWarning")
        }
      )

      self$qtabs <- tibble::tibble(params, obj)
    }
  )
)
