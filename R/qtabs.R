new_qtabs <- function(qrow_params, mapping) {
  qrow_params |>
    purrr::map(\(x) new_qtab_type(x, mapping))
}

# S3 Subclass (qtab_type_... cat, mw, mcg or mdg) of Qtab R6 Class:
new_qtab_type <- function(params, mapping) {
  qtab <- Qtab$new(params, mapping)
  class(qtab) <- c(paste0("qtab_type_", params$Type), class(qtab))
  qtab
}

#' Qtab
#' @description Qtab
#' @field p parameters
#' @field d data
#' @field m parent mapping
#'
#' @examples
#' "hello"
#' @export
Qtab <- R6::R6Class("Qtab",
  public = list(
    p = list(),
    d = list(),
    m = list(),
    #' @description todo
    initialize = function(params,
                          mapping,
                          ...) {
      self$p <- params |>
        new_qtab_params() |>
        # TODO: think if it's better to separate the parts of the parameters
        # from the Tabula / Qrow more...!
        set_qtab_params(mapping)

      self$m <- mapping

      self$d$dat_mod  <- mapping$dat_mod
      self$d$head_table <- mapping$qsheet$head_table
      self$d$col_table <- mapping$qsheet$col_table
      self$d$tab_table <- gen_tab_table(self$p)
    },
    #' @description todo
    calc_qtab = function() {
      calc_qtab(self)
      self$wide_tab()
      invisible(self)
    },
    #' @description todo
    long_tab = function() {
      self$d$long_tab <- self$d[
        c("row_table", "col_table", "val_table", "head_table", "tab_table")
      ] |>
        purrr::reduce(merge, all.x = TRUE) |>
        tibble::as_tibble()
      invisible(self)
    },
    #' @description todo
    wide_tab = function() {
      if (is.null(self$d$long_tab)) {
        self$long_tab()
      }
      wide_tab(self)
      invisible(self)
    },
    #' @description print
    print = function(...) {
      self |> print()
      invisible(self)
    }
  )
)
calc_qtab <- function(qtab) {
  df <- get_raw_data(qtab)
  if (nrow(df) == 0) {
    return(NULL)
  }
  qtab$d$raw_data <- df

  pivot_table_data(qtab)
  calc_stats_rows(qtab)
  calc_stat_fun(qtab)
  calc_detail_freqs(qtab)
  qtab$d$catrec_freqs <- calc_catrec_freqs(qtab)
  qtab$d$percentages <- calc_percentages(qtab)
  qtab$d$invalid_percentages <- calc_invalid_percentages(qtab)
  qtab$d$vc_percentages <- calc_valid_counts_percentages(qtab)
  qtab$d$tab_values <- rbind_table_numbers(qtab)
  qtab$d$row_table <- gen_row_table(qtab)
  post_process(qtab)
  qtab$d$val_table <- gen_val_table(qtab)
}
wide_tab <- function(qtab) {
  cols_for_wide_tab <- c(
    "RowNo", "ColNo", "value"#,
    # "RowWeighted", "RowTitle1", "RowTitle2", "RowTitle3",
    # "RowVariable", "RowValue"#,
    # "ColTitle1", "ColTitle2", "ColVariable", "ColValue"
  )
  long_tab <- qtab$d$long_tab
  if (nrow(long_tab) == 0) {
    return(NULL)
  }

  qtab$d$wide_tab <- long_tab |>
    dplyr::select(dplyr::all_of(cols_for_wide_tab)) |>
    # correct ordering in result:
    # TODO: find cleaner way!...
    dplyr::arrange(ColNo) |>
    tidyr::pivot_wider(
      names_from = dplyr::matches("Col"),
      values_from = value
    ) |>
    dplyr::arrange(RowNo)
}

# TODO: keep in mind me the
# idea to print all the parameters in a wide tibble...

post_process <- function(qtab) {
  if (!is.null(qtab$p$sort_params) && qtab$p$sort_params$key_count) {
    order_by_counts(qtab)
  }
}
order_by_counts <- function(qtab) {
  UseMethod("order_by_counts")
}
order_by_counts.default <- function(qtab) {
  warning("Not yet implemented for this qtab type")
}
order_by_counts.qtab_type_mcg <- function(qtab) {
  # TODO: Wolf fragen ob bei SPSS ORDER=A nicht funzt ???
  # TODO: Wolf sagen dass es nur für gültige Werte geschieht...
  # TODO: gucken ob das auch für andere qtab types funktioniert!...:
  val_table_counts <- qtab$d$detail_freqs[
    qtab$d$detail_freqs$colvar == "DC#STICHPROBE"
  ,][c("value", "rowval")]

  row_table <- qtab$d$row_table
  row_table_detail_lgl <- row_table$RowContent == "Detail"
  row_table_detail_sorted <- row_table[row_table_detail_lgl,] |>
    dplyr::full_join(val_table_counts, by = c(RowValue = "rowval")) |>
    dplyr::arrange(value * (-1) ^ qtab$p$sort_params$order_d)
  row_table_detail_sorted$RowNo <- min(row_table_detail_sorted$RowNo):max(row_table_detail_sorted$RowNo)
  row_table_detail_sorted$value <- NULL

  qtab$d$row_table[row_table_detail_lgl,] <- row_table_detail_sorted
}
