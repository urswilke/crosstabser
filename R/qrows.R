Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    log = list(warn = NULL, error = NULL),
    crosstabs = NULL,
    initialize = function(df,
                          mapping,
                          ...) {
      self$p <- preprocess_qrows_params(df, mapping)[[1]]
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

      if (self$m$params$qrow_db_write) {
        self$write_to_db()
      }

    },
    aggregate_5_tables = function() {
      aggregate_5_tables_(self)
      add_columns_for_tablebook(self, BookNo = self$m$options$V_BookNo)
      invisible(self)
    },
    write_to_db = function() {
      self$aggregate_5_tables()
      write_to_db_(
        self,
        dsn = self$m$params$database_dsn,
        errors = list(self) |> lapply(\(x) x$log$error),
        warns = list(self) |> lapply(\(x) x$log$warn),
        book_no = self$m$options$V_BookNo,
        questno = self$p$Abbreviation
      )
      invisible(self)
    }
  )
)
