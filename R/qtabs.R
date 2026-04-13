# TODO: use methods instead of passing fields around / using helper functions...!
# ee.g. for row_table & col_table where we want to acces either the whole table, i.e.
# - not using rm_header_footer()`` / referencing col_table_all
# or only the rows referencing labels, i.e.
# - using rm_header_footer()`` / referencing col_table
# => use methods Qtab$row_table() / Qtab$col_table() with one optional argument `rm_labels = TRUE/FALSE` instead
# something like this probably makes sense in a lot more places!

#' Qtab class
#'
#' This is not supposed to be used directly.
#' When creating a "Tabula" object,
#' this will generate a list of `Qrow` objects in its `$qrows` field,
#' themselves each containing a list of `Qtab` objects in their `$qtabs` fields.
#'
#' `Qtab` objects have a custom print method `print.Qtab`
#' (see examples in `?Tabula`).
#'
#' @field p parameters
#' @field d data
#' @field m `Tabula` object
#'
#' @export
#'
#' @examples
#' # see `?Tabula`
Qtab <- R6::R6Class("Qtab",
  public = list(
    p = list(),
    d = list(),
    m = list(),
    #' @param params Parameters from `Qrow` object
    #' @param mapping `Tabula` object
    #' @param ... Not used at the moment.
    initialize = function(params,
                          mapping,
                          ...) {
      self$p <- params |>
        qtab_params(mapping)

      self$m <- mapping

      self$d$head_table <- mapping$ditw$ct$db_tables$head_table
      self$d$col_table <- mapping$ditw$ct$db_tables$col_table
      self$d$col_table_all <- mapping$ditw$ct$db_tables$col_table_all

      self$d$tab_table <- gen_tab_table(self$p)

      check_tab(self)

      class(self) <- c(paste0("qtab_type_", params$Type), class(self))
      private$calc_qtab()
    }
  ),
  private = list(
    calc_qtab = function() {
      calc_qtab_(self)
      private$gen_wide_tab()
      invisible(self)
    },
    gen_long_tab = function() {
      self$d$row_table_values <- self$d$row_table |> rm_header_footer()
      self$d$long_tab <- self$d[
        c("row_table_values", "col_table", "val_table", "head_table", "tab_table")
      ] |>
        purrr::reduce(merge, all.x = TRUE) |>
        tibble::as_tibble()
      invisible(self)
    },
    gen_wide_tab = function() {
      if (is.null(self$d$long_tab)) {
        private$gen_long_tab()
      }
      gen_wide_tab_(self)
      invisible(self)
    },
    get_row_labels = function() {
      vallabs <- self$m$ditw$ct$dat_tab[self$p$rowvars_qtab] |>
        lapply(\(x) attr(x, "labels")) |>
        purrr::reduce(c)
      return(vallabs[unique(names(vallabs))])
    }
  )
)
calc_qtab_ <- function(qtab) {
  calc_qtab_elements(qtab)
  tab_values <- rbind_table_numbers(qtab)

  if (qtab$p$Unwgt) {
    qtab_unweighted <- qtab$clone()
    qtab_unweighted$p$Weight <- list(NULL)
    qtab_unweighted$p$Unwgt <- FALSE
    qtab_unweighted$p$long_weight <- character()
    calc_qtab_elements(qtab_unweighted)
    tab_values_unweighted <- rbind_table_numbers(qtab_unweighted)
    tab_values$RowWeighted <- "Weighted"
    tab_values_unweighted$RowWeighted <- "Unweighted"
    tab_values <- rbind(tab_values, tab_values_unweighted)
  }

  qtab$d$tab_values <- tab_values
  qtab$d$row_table <- gen_row_table(qtab)
  post_process(qtab)
  qtab$d$val_table <- gen_val_table(qtab)
  # At the moment RowContent is used to merge RowNo and ColNo to val_table in gen_val_table().
  # Therefore, we need to also add RowContent columns to all the data.frames used by rbind_table_numbers().
  # Thus, we can only set RowContent to "Filter" after calling gen_val_table().
  # TODO: find cleaner solution...:
  set_row_content_to_filter(qtab)
}
calc_qtab_elements <- function(qtab) {
  qtab$d$raw_data <- get_raw_data(qtab)

  qtab$d$df_rowvar_long <- pivot_rowvar_data(qtab)
  qtab$d$long_data <- now_do_colvar(qtab)

  calc_stats_rows(qtab)
  calc_stat_fun(qtab)
  calc_detail_freqs(qtab)
  qtab$d$catrec_freqs <- calc_catrec_freqs(qtab)
  qtab$d$percentages <- calc_percentages(qtab)
  qtab$d$invalid_percentages <- calc_invalid_percentages(qtab)
  qtab$d$vc_percentages <- calc_valid_counts_percentages(qtab)
  qtab$d$no_entry_percentages <- calc_no_entry_percentages(qtab)

}
gen_wide_tab_ <- function(qtab) {
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

  qtab$d$wide_tab <- long_tab[order(long_tab$ColNo), cols_for_wide_tab] |>
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
  # TODO: tell Wolf that only valid values are sorted...
  # TODO: find out if that alse works for other table types than mcg:
  val_table_counts <- qtab$d$detail_freqs[
    qtab$d$detail_freqs$colvar == "DC#TOTAL"
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

set_row_content_to_filter <- function(qtab) {
  row_table <- qtab$d$row_table
  is_filter <- row_table$RowContent == "Missing" & row_table$RowTitle1 == qtab$m$opts$ct$l_lexikon[["cTabFilter"]]
  qtab$d$row_table$RowContent[is_filter] <- "Filter"
}
