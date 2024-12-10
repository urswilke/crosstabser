Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    log = list(warn = NULL, error = NULL),
    crosstabs = NULL,
    initialize = function(df_qrow,
                          mapping,
                          ...) {
      self$p <- process_qrow_params(df_qrow, mapping)
      self$m <- mapping

      params <- gen_qtabs_params(self$p, self$m)

      verbose <- mapping$params$verbose
      self$qtabs <- if (self$m$params$error_out == "unsafe") {
        params |>
          lapply(\(x) Qtab$new(x, mapping))
      } else {
        tryCatch(
          error = function(e) {
            if (mapping$params$debug) browser()
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
      }


      if (self$m$params$qrow_db_write) {
        self$write_to_db()
      }

    },
    assemble_crosstab_data = function() {
      self$crosstabs <- assemble_crosstab_data_(self)
      add_columns_for_tablebook(self, BookNo = self$m$options$V_BookNo)
      invisible(self)
    },
    write_to_db = function() {
      self$assemble_crosstab_data()
      write_to_db_(
        self,
        dsn = self$m$params$database_dsn,
        errors = list(self$log$error),
        warns = list(self$log$warn),
        book_no = self$m$options$V_BookNo,
        questno = self$p$Abbreviation,
        is_first = self$m$options$is_first
      )
      invisible(self)
    }
  )
)
