Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    log = list(warn = NULL, error = NULL),
    initialize = function(df,
                          mapping,
                          ...) {
      self$p <- preprocess_qrows_params(df, mapping)
      self$m <- mapping

      params <- process_qrow_params(self$p, self$m)

      verbose <- mapping$params$verbose
      obj <- tryCatch(
        error = function(e) {
          self$log$error <- utils::capture.output(e)[-1] |> paste(collapse = "\n")
          if (verbose) e |> conditionMessage() |> message()
          obj <- list(NULL)
        },
        withCallingHandlers(
          # message = <WE-COULD-ALSO-LOG-MESSAGES..._FUN()>,
          warning = function(w) {
            self$log$warn <- utils::capture.output(w)[-1] |> paste(collapse = "\n")
            if (verbose) w |> conditionMessage() |> message()
            tryInvokeRestart("muffleWarning")
          },
          params |>
            lapply(\(x) Qtab$new(x, mapping))
        )
      )

      self$qtabs <- obj
    }
  )
)
