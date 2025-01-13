#' Questions row class
#'
#' @field p parameters
#' @field m `Tabula` object
#' @field qtabs list of `Qtabs` objects
#' @field log log entries
#'
#' @export
#'
#' @examples
#' # see `?Tabula`
Qrow <- R6::R6Class(
  "Qrow",
  public = list(
    p = list(),
    m = list(),
    qtabs = tibble::tibble(),
    log = list(warn = NULL, error = NULL),
    crosstabs = NULL,
    #' @param df_qrow row of the Questions dataframe
    #' @param mapping `Mapping` object
    #' @param ... perhaps not used at the moment of writing :)
    initialize = function(df_qrow,
                          mapping,
                          ...) {
      self$p <- process_qrow_params(df_qrow, mapping)
      self$m <- mapping

      params <- gen_qtabs_params(self$p, self$m)

      verbose <- mapping$opts$da$verbose
      self$qtabs <- if (self$m$opts$da$error_out == "unsafe") {
        params |>
          lapply(\(x) Qtab$new(x, mapping))
      } else {
        tryCatch(
          error = function(e) {
            if (mapping$opts$da$debug) browser()
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
    },
    prep_tab_row_val = function() {
      self$crosstabs <- prep_tab_row_val_(self)
      prepare_row_table_tb(self)
      prepare_tab_table_tb(self)
      prepare_val_table_tb(self)
      invisible(self)
    }
  )
)
