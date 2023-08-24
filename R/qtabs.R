new_qtabs <- function(qsheet_processed, mapping) {
  global_options <- mapping$options$l_macro_scenario
  params <- qsheet_processed |>
    purrr::transpose() |>
    purrr::map(\(x) x[!is.na(x)]) |>
    purrr::map(\(x) add_global_options(x, global_options))
  qtabs <- params |>
    purrr::map(\(x) new_qtab(x, mapping))
  qtabs |>
    purrr::walk(\(x) add_type_specific_params(x))


  class(qtabs) <- c("crosstabser_tabs", class(qtabs))
  qtabs

}

new_qtab <- function(params, mapping) {
  res <- Qtab$new(params, mapping)
  class(res) <- c(paste0("qtab_type_", params$Type), class(res))
  res
}

add_type_specific_params <- function(qtab) {
  UseMethod("add_type_specific_params")
}

add_type_specific_params.default <- function(qtab) {
  NULL
}
add_type_specific_params.qtab_type_mdg <- function(qtab) {
  mdg_val <- qtab$p$MdgVal %||% 1
  qtab$p$MdgVal <- as.numeric(mdg_val)
}
add_type_specific_params.qtab_type_mw <- function(qtab) {
  stat_fun <- qtab$p$ZsfgMW
  if (length(stat_fun) == 0) {
    stat_fun = "mean"
  }

  qtab$p$stat_fun <- stat_fun
}
add_type_specific_params.qtab_type_cat <- function(qtab) {
  if (!is.null(qtab$p$MetrMac)) {
    qtab$p$df_stat_funs <- process_metr_mac(qtab)
  }
}

df_metr_mac <- data.frame(
  shortcut = c("E", "M", "S", "P", "I", "A", "T"),
  fun = c("se", "median", "mean", "percentile", "min", "max", "sum"),
  # TODO: Wolf fragen wo das steht???
  ctab_entry = c("cTabStdErr", "cTabMedian", "cTabMean", "wo_steht???", "wo_steht2???", "wo_steht3???", "wo_steht4???")
)

process_metr_mac <- function(qtab) {
  l <- stringr::str_extract_all(qtab$p$MetrMac, "[A-Z]\\d+")[[1]] |>
    stringr::str_split("(?=\\d)")

  df_stat_funs <- data.frame(shortcut = l |> purrr::map_chr(1)) |>
    dplyr::mutate(
      fun = df_metr_mac$fun[match(shortcut, df_metr_mac$shortcut)],
      decimals = as.numeric(l |> purrr::map_chr(\(x) x[length(x)])),
      row_title = qtab$m$options$l_lexikon[
        df_metr_mac$ctab_entry[match(shortcut, df_metr_mac$shortcut)]
      ] |> unname()
    )
  df_stat_funs$quantile_val <- NA_real_
  df_stat_funs[df_stat_funs$shortcut == "P",]$quantile_val <-
    as.numeric(
      l[df_stat_funs$shortcut == "P"] |>
        purrr::map_chr(
          \(x) x[2:3] |>
            paste(collapse = ""))
    ) / 100
  df_stat_funs
}

Qtab <- R6::R6Class("Qtab",
  public = list(
    p = list(),
    d = list(),
    m = list(),
    initialize = function(params,
                          mapping,
                          ...) {
      self$p <- params
      self$m <- mapping
      self$p$l_lexikon <- mapping$options$l_lexikon
      self$d$dat_mod  <- mapping$dat_mod
      self$d$tab_table <- mapping$qsheet$tab_table[
        mapping$qsheet$tab_table$TabNo == self$p$TabNo,
      ]
      self$d$head_table <- mapping$qsheet$head_table
      self$d$col_table <- mapping$qsheet$col_table
    },
    calc_qtab = function() {
      calc_qtab(self)
      invisible(self)
    },
    long_tab = function() {
      self$d$long_tab <- self$d[
        c("val_table", "row_table", "col_table", "head_table", "tab_table")
      ] |>
        purrr::reduce(merge) |>
        tibble::as_tibble()
      invisible(self)
    },
    wide_tab = function() {
      if (is.null(self$d$long_tab)) {
        self$long_tab()
      }
      wide_tab(self)
      invisible(self)
    }
  )
)
calc_qtab <- function(qtab) {
  qtab$d$raw_data <- get_raw_data(qtab)
  pivot_table_data(qtab)
  calc_stats_rows(qtab)
  calc_stat_fun(qtab)
  qtab$d$detail_freqs <- calc_detail_freqs(qtab)
  calc_percentages(qtab)
  calc_valid_counts_percentages(qtab)
  qtab$d$tab_values <- merge_table_parts(qtab)
  qtab$d$row_table <- gen_row_table(qtab)
  qtab$d$val_table <- gen_val_table(qtab)
}
wide_tab <- function(qtab) {
  cols_for_wide_tab <- c(
    "RowNo", "ColNo", "value"#,
    # "RowWeighted", "RowTitle1", "RowTitle2", "RowTitle3",
    # "RowVariable", "RowValue"#,
    # "ColTitle1", "ColTitle2", "ColVariable", "ColValue"
  )
  qtab$d$wide_tab <- qtab$d$long_tab |>
    dplyr::select(dplyr::all_of(cols_for_wide_tab)) |>
    tidyr::drop_na(value) |>
    tidyr::pivot_wider(
      names_from = dplyr::matches("Col"),
      values_from = value
    ) |>
    dplyr::arrange(RowNo)
}
#' @export
print.crosstabser_tabs <- function(x,
                                   # unnest fields:
                                   uf = c("d"),
                                   n = 30,
                                   ...) {
  field_list <- x |> purrr::map(get_r6_fields)
  df <- tibble::tibble(x = field_list) |>
    tidyr::unnest_wider(x)
  if (length(uf) > 0) {
    df <- df |>
      tidyr::unnest_wider(uf)
    # |>
    #   dplyr::select(-matches("^Col[A-Z]$"))
  }
  print(df, n = n, ...)
}

get_r6_fields <- function(r6_obj) {
  r6_list <- as.list(r6_obj)
  r6_list[r6_list |> purrr::map_lgl(\(x) !is.environment(x) && !is.function(x))]

}
