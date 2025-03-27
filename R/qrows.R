#' Questions row class
#'
#' This is not supposed to be used directly.
#' When creating a "Tabula" object,
#' this will generate a list of `Qrow` objects in its `$qrows` field.
#'
#' @field p parameters extracted from `df_qrow`
#' @field m `Tabula` object
#' @field qtabs list of `Qtabs` objects
#' @field log log entries
#' @field ditw This is the "dust in the wind" list object field
#'   that stores data that didn't make it into their own field.
#'   For developers only!
#'   For reproducible code you should NEVER rely on this field
#'   as it might be subject to change without any warning.
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
    ditw = list(ct = NULL),
    #' @param df_qrow row of the Questions dataframe
    #' @param mapping `Tabula` object
    #' @param ... Not used at the moment.
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
          withCallingHandlers(
            {
              params |>
                lapply(\(x) Qtab$new(x, mapping))
            },
            warning = function(w) {
              self$log$warn <- utils::capture.output(w)[-1] |> paste(collapse = "\n")
              if (verbose) w |> conditionMessage() |> message()
              tryInvokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            if (mapping$opts$da$debug) browser()
            self$log$error <- e |> paste(collapse = "\n")
            if (verbose) e |> conditionMessage() |> message()
            obj <- list(NULL)
          }
        )
      }
    }
  ),
  private = list(
    prep_tab_row_val = function() {
      self$ditw$ct$crosstabs <- prep_tab_row_val_(self)
      prepare_row_table_tb(self)
      prepare_tab_table_tb(self)
      prepare_val_table_tb(self)
      invisible(self)
    }
  )
)
