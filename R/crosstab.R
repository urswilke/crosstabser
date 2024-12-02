rbind_table_numbers <- function(qtab) {
  # TODO: replaced rbind -> check if the dataframe parts can by simplified (e.g.
  # setting a column to NA shouldn't be necessary anymore...):
  dplyr::bind_rows(
    qtab$d$stats_rows$total,
    qtab$d$stats_rows$sum_of_valid,
    qtab$d$fun_stats,
    qtab$d$detail_freqs,
    qtab$d$catrec_freqs,
    qtab$d$percentages,
    qtab$d$vc_percentages,
    qtab$d$stats_rows$n_valid,
    qtab$d$invalid_freqs,
    qtab$d$invalid_percentages,
    qtab$d$stats_rows$no_entry,
    qtab$d$no_entry_percentages
  )
}

calc_stat_fun <- function(qtab) {
  UseMethod("calc_stat_fun")
}
calc_stat_fun.default <- function(qtab) {
  NULL
}
# TODO: add tests for stat_fun = median, sum, ...:
calc_stat_fun.qtab_type_mw <- function(qtab) {
  invalid_vals <- c(qtab$p[["Unguelt"]], qtab$p[["UngueltMW"]])
  long_data <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]

  if (!is.null(qtab$p$MWRec)) {
    long_data$rowval <- mw_rec_helper(long_data$rowval, qtab$p$MWRec)
  }
  stat_fun <- qtab$p$df_stat_funs$fun %||% "mean"
  res <- long_data |>
    summarize_stats(
      "rowval",
      wt = qtab$p$Weight[[1]],
      stat_fun = stat_fun,
      .by = c("rowvar", "colvar", "colval")
    )
  # TODO: With the refactoring, RowStatFun needs to be added here
  # and I don't understand why...
  # perhaps we need qtab$p$df_stat_funs$row_title instead (?)
  res$RowStatFun <- stat_fun
  res <- res |> add_missing_cases(qtab)

  res$RowContent <- "MStatistics"
  res$rowval <- NA_real_
  res$RowAbsPercent <- "Percent"
  qtab$d$fun_stats <- res
}
add_missing_cases <- function(df, qtab) {
  df |> tidyr::complete(
    rowvar = qtab$p$rowvars_string,
    tidyr::nesting(
      colvar = qtab$d$col_table$ColVariable,
      colval = qtab$d$col_table$ColValue
    ),
    RowStatFun = qtab$p$df_stat_funs$row_title,
  )
}
calc_stat_fun.qtab_type_cat <- function(qtab) {
  if (is.null(qtab$p$MetrMac)) {
    return(NULL)
  }
  invalid_vals <- c(qtab$p[["Unguelt"]], qtab$p[["UngueltMW"]])
  long_data <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]
  if (nrow(long_data) == 0) {
    return()
  }
  if (!is.null(qtab$p$MWRec)) {
    long_data$rowval <- mw_rec_helper(long_data$rowval, qtab$p$MWRec)
  }

  df_stat_funs <- qtab$p$df_stat_funs
  l <- df_stat_funs |>
    split(seq_len(nrow(df_stat_funs))) |>
    purrr::set_names(df_stat_funs$row_title)
  res <- l |>
    purrr::map_dfr(
      \(x) long_data |>
        summarize_stats(
          "rowval",
          wt = qtab$p$Weight[[1]],
          stat_fun = x$fun,
          probs = x$quantile_val[[1]],
          .by = c("rowvar", "colvar", "colval")
        ),
      .id = "RowStatFun"
    ) |> add_missing_cases(qtab)
  res$RowContent <- "Statistics"
  res$rowval <- 100
  res$RowAbsPercent <- "Percent"
  qtab$d$fun_stats <- res
}

calc_detail_freqs <- function(qtab) {
  UseMethod("calc_detail_freqs")
}
calc_detail_freqs.qtab_type_cat <- function(qtab) {
  long_data <- qtab$d$long_data[!is.na(qtab$d$long_data$rowval),]
  if (nrow(long_data) == 0) {
    return()
  }
  all_counts <- long_data |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowAbsPercent <- "Abs"

  if (!is.null(qtab$p$Einzelauspraegung) && qtab$p$Einzelauspraegung %in% c("0", "FALSE")) {
    detail_freqs <- NULL
  } else {
    detail_freqs <- all_counts[!all_counts$rowval %in% qtab$p[["Unguelt"]],]
    detail_freqs$RowContent <- "Detail"
  }

  invalid_freqs <- all_counts[all_counts$rowval %in% qtab$p[["Unguelt"]],]
  invalid_freqs$RowContent <- "Missing"

  qtab$d$detail_freqs <- detail_freqs
  qtab$d$invalid_freqs <- invalid_freqs
}

calc_stats_rows <- function(qtab) {
  UseMethod("calc_stats_rows")
}
calc_stats_rows.default <- function(qtab) {
  NULL
}
calc_stats_rows.qtab_type_cat <- function(qtab) {
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#TOTAL" <- 1

  df_cols <- df[c(qtab$p$raw_data_colvars, qtab$p$long_weight)]

  rowvars <- rv(qtab$p$l_selvar$valid %||% qtab$p$rowvars_qtab)
  df_cols$n_valid <- !df[[rowvars]] %in% c(qtab$p[["Unguelt"]], NA)
  df_cols$total <- !is.na(df[[rowvars]])

  # TODO: find better organisation (redundant code with calc_stats_rows.qtab_type_mdg):
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("total", "n_valid")
  df_stats_rows <- df_cols_long |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )

  l_row_types <- row_types |>
    purrr::set_names() |>
    lapply(\(x) {
      res <- df_stats_rows[c("colvar", "colval", x)]
      names(res)[3] <- "value"
      res$rowval <- 1
      res$rowvar <- qtab$p$rowvars_string
      res
    })
  l_row_types$total$RowContent <- "Total"
  l_row_types$total$RowAbsPercent <- "Abs"
  l_row_types$n_valid$RowContent <- "Valid"
  l_row_types$n_valid$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- l_row_types
}

calc_detail_freqs.qtab_type_mdg <- function(qtab) {
  long_data <- qtab$d$long_data
  if (nrow(long_data) == 0) {
    return()
  }
  all_counts <- long_data[long_data$val_to_count,] |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowAbsPercent <- "Abs"

  is_valid <- all_counts$rowvar %in% (qtab$p$l_selvar$invalid %||% qtab$p[["Unguelt"]])
  detail_freqs <- all_counts[!is_valid,]

  if (!is.null(qtab$p$MdgMissValid) && qtab$p$MdgMissValid == "TRUE") {
    valid_no_entry <- qtab$d$stats_rows$valid_no_entry
    valid_no_entry$rowvar <- "valid_no_entry"
    valid_no_entry$RowAbsPercent <- "Abs"
    detail_freqs <- rbind(detail_freqs, valid_no_entry[valid_no_entry$value > 0,])
  }

  detail_freqs$RowContent <- "Detail"

  # TODO: fix counting as done when Exclusive is set...:
  invalid_freqs <- all_counts[is_valid,]
  invalid_freqs$RowContent <- "Missing"

  qtab$d$detail_freqs <- detail_freqs
  qtab$d$invalid_freqs <- invalid_freqs
}
calc_stats_rows.qtab_type_mdg <- function(qtab) {
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#TOTAL" <- 1

  mdg_val <- qtab$p$MdgVal

  df_cols <- df[c(qtab$p$raw_data_colvars, qtab$p$long_weight)]
  rowvars <- rv(qtab$p$l_selvar$valid %||% qtab$p$rowvars_valid_qtab)
  df_rows <- df[rowvars]

  df_cols$total <- rowSums(is.na(df_rows)) < ncol(df_rows)
  sum_of_valid <- rowSums(df_rows == mdg_val, na.rm = TRUE)
  df_cols$sum_of_valid <- sum_of_valid
  df_cols$n_valid <- sum_of_valid >= 1
  # base R way to do:
  # df_cols$invalid_cts <- rowSums((df |> select(any_of(paste0("rowvar_", qtab$p[["Unguelt"]])))) == mdg_val, na.rm = TRUE) != 0
  invalid_colnames <- rv(qtab$p$l_selvar$invalid %||% qtab$p[["Unguelt"]])
  df_cols$invalid_cts <- rowSums(df[invalid_colnames] == mdg_val, na.rm = TRUE) != 0
  df_cols$no_entry <- as.numeric(sum_of_valid + df_cols$invalid_cts == 0)
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("total", "n_valid", "no_entry")
  if (!is.null(qtab$p$MdgMissValid) && qtab$p$MdgMissValid == "TRUE") {
    df_cols_long <- df_cols_long |>
      dplyr::mutate(
        n_valid = n_valid | no_entry,
        valid_no_entry = no_entry,
        no_entry = 0
      )
    row_types <- row_types |> append("valid_no_entry")
  }

  df_stats_rows <- df_cols_long |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )

  l_row_types <- row_types |>
    purrr::set_names() |>
    lapply(\(x) {
      res <- df_stats_rows[c("colvar", "colval", x)]
      names(res)[3] <- "value"
      res$rowval <- 1
      res$rowvar <- qtab$p$rowvars_string
      res
    })

  # VERY HACKY patch for sum of valid to work with Exclusive:
  df_long <- qtab$d$long_data
  invalid_vals <- qtab$p[["Unguelt"]]
  sum_of_valid <- df_long[!(df_long$rowvar %in% invalid_vals) & df_long$val_to_count,] |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("colvar", "colval")
    )
  if (!is.null(qtab$p$MdgMissValid) && qtab$p$MdgMissValid == "TRUE") {
    sum_of_valid = sum_of_valid |>
      dplyr::full_join(
        df_stats_rows[c("colvar", "colval", "valid_no_entry")],
        by = c("colvar", "colval")
      ) |>
      tidyr::replace_na(list(value = 0, valid_no_entry = 0)) |>
      dplyr::mutate(value = value + valid_no_entry) |>
      dplyr::select(-valid_no_entry)
  }
  sum_of_valid$RowContent <- "SumOfValid"
  sum_of_valid$RowAbsPercent <- "Abs"
  sum_of_valid$rowvar <- qtab$p$rowvars_string
  sum_of_valid$rowval <- 1
  l_row_types$sum_of_valid <- sum_of_valid

  l_row_types$total$RowContent <- "Total"
  l_row_types$no_entry$RowContent <- "Missing"
  l_row_types$total$RowAbsPercent <- "Abs"
  l_row_types$no_entry$RowAbsPercent <- "Abs"
  l_row_types$no_entry <- l_row_types$no_entry[l_row_types$no_entry$value > 0,]

  l_row_types$n_valid$RowContent <- "Valid"
  l_row_types$n_valid$RowAbsPercent <- "Abs"

  if (all(l_row_types$n_valid$value == 0)) l_row_types$n_valid <- NULL

  qtab$d$stats_rows <- l_row_types
}

calc_stats_rows.qtab_type_mw <- function(qtab) {
  invalid_vals <- c(qtab$p[["Unguelt"]], NA)
  df <- qtab$d$raw_data
  # for TOTAL column:
  df$"colvar_DC#TOTAL" <- 1

  df_cols <- df[c(qtab$p$raw_data_colvars, qtab$p$long_weight)]

  rowvars <- rv(qtab$p$l_selvar$valid %||% qtab$p$rowvars_qtab)
  df_rows <- df[rowvars]

  df_cols$n_valid <- rowSums(sapply(df_rows, Negate(`%in%`), invalid_vals)) >= 1
  df_cols_long <- df_cols |>
    pivot_cols()

  row_types <- c("n_valid")
  df_stats_rows <- df_cols_long |>
    summarize_stats(
      row_types,
      wt = qtab$p$Weight[[1]],
      stat_fun = "sum",
      .by = c("colvar", "colval")
    )

  df_stats_rows$rowval <- 1
  df_stats_rows$rowvar <- qtab$p$rowvars_string

  df_stats_rows$RowContent <- "Valid"
  df_stats_rows$RowAbsPercent <- "Abs"

  qtab$d$stats_rows <- list(n_valid = df_stats_rows)
}

calc_stats_rows.qtab_type_mcg <- function(qtab) {
  invalid_vals <- qtab$p[["Unguelt"]]

  df_long <- qtab$d$long_data
  df_long_valid <- df_long[!(df_long$rowval %in% invalid_vals) & df_long$val_to_count,]
  group_variables <- c("colvar", "colval")
  # the next is equivalent to (for unweighted):
  # total <- df_long |> dplyr::summarise(value = dplyr::n_distinct(i), .by = c("colvar", "colval"))
  total <- df_long |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("i", group_variables, qtab$p$long_weight)))) |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)
  sum_of_valid <- df_long_valid |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)
  n_valid <- df_long_valid |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("i", group_variables, qtab$p$long_weight)))) |>
    summarize_stats(NULL, qtab$p$Weight[[1]], .by = group_variables)


  total$RowContent <- "Total"
  total$RowAbsPercent <- "Abs"
  total$rowvar <- df_long$rowvar[1]
  total$rowval <- 1
  sum_of_valid$RowContent <- "SumOfValid"
  sum_of_valid$RowAbsPercent <- "Abs"
  sum_of_valid$rowvar <- df_long$rowvar[1]
  sum_of_valid$rowval <- 1
  n_valid$RowContent <- "Valid"
  n_valid$RowAbsPercent <- "Abs"
  n_valid$rowvar <- df_long$rowvar[1]
  n_valid$rowval <- 1


  qtab$d$stats_rows <- list(
    total = total,
    sum_of_valid = sum_of_valid,
    n_valid = n_valid
  )
}

calc_catrec_freqs <- function(qtab) {
  UseMethod("calc_catrec_freqs")
}
calc_catrec_freqs.default <- function(qtab) {
  NULL
}

calc_catrec_freqs.qtab_type_cat <- function(qtab) {
  if (is.null(qtab$p$CatRec)) {
    return(NULL)
  }
  invalid_vals <- qtab$p[["Unguelt"]]

  df_long <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,]
  if (nrow(df_long) == 0) {
    return()
  }

  catrec_strings <- strsplit(qtab$p$CatRec, "\\|")[[1]]
  all_counts <- catrec_strings |>
    lapply(\(x) summarise_catrec(df_long, x, qtab$p$Weight[[1]])) |>
    # TODO: tell Wolf about new parameter "i_catrec"!...:
    dplyr::bind_rows(.id = "i_catrec")
  all_counts$RowContent <- "Summary"
  all_counts$RowAbsPercent <- "Abs"
  all_counts
}
summarise_catrec <- function(df_long, catrec_string, wt) {
  catrec_sum_string <- catrec_string |> stringr::str_extract("(?<=\\{).*(?=\\})")
  df_long$rowval <- mw_rec_helper(df_long$rowval, catrec_string)
  df_long$rowvar <- paste0(df_long$rowvar, "__summary")
  non_recoded_idx <- is.na(df_long$rowval)

  res <- df_long |>
    summarize_stats(
      NULL,
      wt = wt,
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  if (is.na(catrec_sum_string)) {
    return(res)
  }
  df_user_expr <- parse_catrec_sum_expr(res, catrec_sum_string)
  rbind(res, df_user_expr)
}
parse_catrec_sum_expr <- function(df_summary, catrec_sum_string) {
  expr_string <- catrec_sum_string |> stringr::str_remove("=.*")
  translate_user_expression <- function(expr_string) {
    expr_translated <- expr_string |>
      stringr::str_replace_all("(\\[)(\\d+)(\\])", "value[rowval == \\2]")
    rlang::parse_expr(expr_translated)
  }
  df_user_expr <- df_summary |>
    tidyr::complete(
      rowvar, rowval,
      tidyr::nesting(colvar, colval),
      fill = list(value = 0)
    ) |>
    dplyr::summarise(
      value = !!translate_user_expression(expr_string),
      .by = c("rowvar", "colvar", "colval")
    )
  df_user_expr$rowval <- max(df_summary$rowval) + 1
  df_user_expr
}

calc_detail_freqs.qtab_type_mw <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  # TODO: check all occurrences of qtab$p[["Unguelt"]] (a lot) if NA also has to be included!
  # (except mdg of course)
  invalid_vals <- c(NA, qtab$p[["Unguelt"]])

  res <- qtab$d$long_data[!qtab$d$long_data$rowval %in% invalid_vals,] |>
    summarize_stats(
      "rowval",
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "colvar", "colval")
    )

  res$RowContent <- "MValid"
  res$RowAbsPercent <- "Abs"
  qtab$d$detail_freqs <- res
}
calc_detail_freqs.qtab_type_mcg <- function(qtab) {
  weight <- qtab$p$Weight[[1]]
  long_data <- qtab$d$long_data
  long_data[["i"]] <- NULL

  all_counts <- long_data[long_data$val_to_count,] |>
    summarize_stats(
      NULL,
      wt = qtab$p$Weight[[1]],
      .by = c("rowvar", "rowval", "colvar", "colval")
    )

  all_counts$RowAbsPercent <- "Abs"

  detail_freqs <- all_counts[!all_counts$rowval %in% qtab$p[["Unguelt"]],]
  detail_freqs$RowContent <- "Detail"

  invalid_freqs <- all_counts[all_counts$rowval %in% qtab$p[["Unguelt"]],]
  invalid_freqs$RowContent <- "Missing"

  qtab$d$detail_freqs <- detail_freqs
  qtab$d$invalid_freqs <- invalid_freqs
}

calc_percentages <- function(qtab) {
  UseMethod("calc_percentages")
}
calc_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_percentages.default <- function(qtab) {
  cts <- dplyr::bind_rows(qtab$d$detail_freqs, qtab$d$catrec_freqs)
  vc <- qtab$d$stats_rows$n_valid
  calc_percentage_helper(cts, vc)
}
calc_invalid_percentages <- function(qtab) {
  UseMethod("calc_invalid_percentages")
}
calc_invalid_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_invalid_percentages.default <- function(qtab) {
  cts <- qtab$d$invalid_freqs
  tc <- qtab$d$stats_rows$total
  calc_percentage_helper(cts, tc)
}
calc_percentage_helper <- function(cts, divider_cts) {
  if (length(cts) == 0) {
    return(NULL)
  }
  percentages <- cts
  percentages$value <- as.numeric(percentages$value)
  # avoid to divide by zero:
  divider_cts$value[divider_cts$value == 0] <- NA_integer_
  # corresponding indices of percentages values in divider_cts:
  idx <- match(
    paste(percentages$colvar, percentages$colval),
    paste(divider_cts$colvar, divider_cts$colval)
  )
  percentages$value <- 100 *percentages$value / divider_cts$value[idx]
  percentages$RowAbsPercent <- "Percent"
  percentages
}
calc_valid_counts_percentages <- function(qtab) {
  UseMethod("calc_valid_counts_percentages")
}
calc_valid_counts_percentages.qtab_type_mw <- function(qtab) {
  NULL
}
calc_valid_counts_percentages.default <- function(qtab) {
  total_cts <- qtab$d$stats_rows$total
  valid_cts <- qtab$d$stats_rows$n_valid
  # for mdg, when there aren't any valid counts, they aren't written...:
  if (is.null(valid_cts)) return(NULL)

  # TODO: check if this can be changed to include implicit missing values in the
  # raw_data or rather the counts (make them explicit...)
  # valid_cts$value <- 100 * valid_cts$value / total_cts$value
  # this doesn't work if the number of valid counts is not equal to the number of total counts...
  res <- total_cts |> dplyr::select(-RowContent, value_tot = value) |> merge(valid_cts, all.x = TRUE)
  raw_value <- 100 * res$value / res$value_tot
  # TODO: check with Wolf if this shouldn't be NA instead of 0:
  raw_value[res$value_tot == 0] <- 0
  res$value <- raw_value
  res$value_tot <- NULL
  res$RowAbsPercent <- "Percent"

  res
}


calc_no_entry_percentages <- function(qtab) {
  UseMethod("calc_no_entry_percentages")
}
calc_no_entry_percentages.default <- function(qtab) {
  NULL
}
calc_no_entry_percentages.qtab_type_mdg <- function(qtab) {
  if (nrow(qtab$d$stats_rows$no_entry) == 0) {
    return(NULL)
  }
  total_cts <- qtab$d$stats_rows$total
  no_entry_cts <- qtab$d$stats_rows$no_entry
  res <- total_cts |> dplyr::select(-RowContent, value_tot = value) |> merge(no_entry_cts, all.x = TRUE)
  res$value <- 100 * res$value / res$value_tot
  res$value_tot <- NULL
  res$RowAbsPercent <- "Percent"

  res

}
