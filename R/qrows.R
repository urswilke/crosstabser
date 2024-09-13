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
      obj <- list(NULL) |> rep(length(params))
      error <- warn <- NULL

      obj <- withCallingHandlers(
        tryCatch({
          params |>
            lapply(\(x) Qtab$new(x, mapping))
        },
        error = function(e) {
          error <<- capture.output(e) |> paste(collapse = "\n")
          obj <- list(NULL)
        }),
        warning = function(w) {
          warn <<- capture.output(w) |> paste(collapse = "\n")
          tryInvokeRestart("muffleWarning")
        }
      )

      self$log$warn <- warn
      self$log$error <- error
      self$qtabs <- tibble::tibble(params, obj)
    }
  )
)
