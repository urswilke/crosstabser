# TODO: refactor (!?), e.g. to rely more on pre-calculated fields,
# e.g. qtab$d$tab_values instead of calculating quantities from raw data from scratch

gen_row_table <- function(qtab) {
  # TODO: replaced rbind -> check if the dataframe parts can by simplified (e.g.
  # setting a column to NA shouldn't be necessary anymore...):

  row_table_raw <- dplyr::bind_rows(
    row_table_total_line(qtab),
    row_table_valid_mw(qtab),
    row_table_valid_answers_line(qtab),
    row_table_body(qtab),
    row_table_summary(qtab),
    row_table_stats(qtab),
    row_table_valid_cases(qtab),
    row_table_invalid_vals(qtab),
    row_table_no_entry(qtab)
  )
  # This column is for ordering and will be overwritten in the final results:
  row_table_raw$RowNo <- seq_len(nrow(row_table_raw))

  row_table_weighted <- NULL
  row_table_unweighted <- NULL
  weight <- qtab$p$Weight[[1]]
  unweight <- qtab$p$Unwgt
  if (is.null(weight) | unweight) {
    row_table_unweighted <- row_table_raw
    row_table_unweighted$RowWeighted <- "Unweighted"
  }
  if (!is.null(weight)) {
    row_table_weighted <- row_table_raw
    row_table_weighted$RowWeighted <- "Weighted"

    weight_sign <- qtab$m$options$l_lexikon["cTabWeighted"]
    row_table_weighted$RowTitle3 <- paste(row_table_weighted$RowTitle3, weight_sign)
    is_stat_row <- row_table_weighted$RowContent == "Statistics"
    row_table_weighted[is_stat_row, ]$RowTitle1 <- paste(row_table_weighted[is_stat_row, ]$RowTitle1, weight_sign)
    row_table_weighted[is_stat_row, ]$RowTitle2 <- paste(row_table_weighted[is_stat_row, ]$RowTitle2, weight_sign)
  }
  row_table_values <- rbind(
    row_table_unweighted,
    row_table_weighted
  )
  row_table_values <- row_table_values[order(row_table_values$RowNo),]
  row_table <- dplyr::bind_rows(
    # TODO: find more elegant way to define column order...!
    # (done in order to keep the csv snapshot more stable...):
    empty_row_table(),
    row_table_beginning(qtab),
    row_table_values,
    row_table_end()
  )

  row_table$TabNo <- qtab$p$i_tab
  row_table$RowNo <- seq_len(nrow(row_table))
  row_table$QuestNo <- qtab$p$Abbreviation

  row_table
}
empty_row_table <- function() {
  c("RowNo", "RowContent", "RowAbsPercent", "RowWeighted", "TabNo", "RowTitle1", "RowTitle2", "RowTitle3", "RowFormat", "RowDecimals", "RowVariable", "RowValue")


  tibble::tibble(
    RowNo = integer(),
    RowContent = character(),
    RowAbsPercent = character(),
    RowWeighted = character(),
    TabNo = integer(),
    # RowMerge1 = integer(),
    # RowMerge2 = integer(),
    RowTitle1 = character(),
    RowTitle2 = character(),
    RowTitle3 = character(),
    RowFormat = character(),
    RowDecimals = integer(),
    RowVariable = character(),
    RowValue = double()
  )
}

row_table_beginning <- function(qtab) {
  RowBegin <- data.frame(RowContent = c("Title", "Header", "Header"))
  RowBegin$RowTitle3 <- ""
  RowBegin$RowTitle2 <- ""
  RowBegin$RowTitle1 <- NA_character_
  RowBegin$RowTitle1[1] <- paste(qtab$p$Title, collapse = "\n")
  RowBegin$RowWeighted <- ""
  RowBegin$RowAbsPercent <- ""
  RowBegin$RowVariable <- NA_character_
  if (!is.null(qtab$p$Fussnote)) {
    RowBegin$RowTitle1[2:3] <- qtab$p$Fussnote
  }
  RowBegin
}

row_table_end <- function() {
  RowEnd <- data.frame(RowContent = c("Empty"))
  RowEnd$RowTitle3 <- ""
  RowEnd$RowTitle2 <- ""
  RowEnd$RowTitle1 <- ""
  RowEnd$RowWeighted <- ""
  RowEnd$RowAbsPercent <- ""
  RowEnd$RowVariable <- NA_character_
  RowEnd
}



row_table_total_line <- function(qtab) {
  UseMethod("row_table_total_line")
}

row_table_total_line.default <- function(qtab) {
  row_table <- empty_row_table()
  total_row_text <- qtab$m$options$l_lexikon["cTabGesamt"]
  abs_text <- qtab$m$options$l_lexikon[["cTabAbs"]]
  row_variable <- qtab$p$rowvars_string
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowVariable", "RowValue")] <- list("Total", "Abs", total_row_text, total_row_text, abs_text, 0L, row_variable, 1)
  row_table
}
row_table_total_line.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_valid_mw <- function(qtab) {
  UseMethod("row_table_valid_mw")
}

row_table_valid_mw.default <- function(qtab) {
  NULL
}
row_table_valid_mw.qtab_type_mw <- function(qtab) {
  row_table <- empty_row_table()
  valid_mw_text <- qtab$m$options$l_lexikon["cTabGesamtMW"]
  abs_text <- qtab$m$options$l_lexikon[["cTabAbs"]]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowValue")] <- list("Valid", "Abs", valid_mw_text, valid_mw_text, abs_text, 0L, 1)
  row_table$RowVariable <- qtab$p$rowvars_string
  row_table
}


row_table_valid_answers_line <- function(qtab) {
  UseMethod("row_table_valid_answers_line")
}

row_table_valid_answers_line.qtab_type_mcg <- row_table_valid_answers_line.qtab_type_mdg <- function(qtab) {
  row_table <- empty_row_table()
  valid_answers_row_text <- qtab$m$options$l_lexikon["cTabGesamtMFA"]
  abs_text <- qtab$m$options$l_lexikon[["cTabAbs"]]
  row_table[
    1,
    c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowValue", "RowDecimals")
  ] <- list(
    "SumOfValid", "Abs", valid_answers_row_text, valid_answers_row_text, abs_text, 1, 0
  )
  row_table$RowVariable <- qtab$p$rowvars_string
  row_table
}
row_table_valid_answers_line.qtab_type_cat <- row_table_valid_answers_line.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_body <- function(qtab) {
  UseMethod("row_table_body")
}
row_table_body.qtab_type_mcg <- row_table_body.qtab_type_cat <- function(qtab) {
  # TODO: calc_detail_freqs.qtab_type_cat() & calc_percentages.default() could also be skipped
  if (!is.null(qtab$p$Einzelauspraegung) && qtab$p$Einzelauspraegung %in% c("0", "FALSE")) {
    return(NULL)
  }
  occuring_vals <- qtab$m$dat_mod[qtab$p$rowvars_qtab] |> unlist(use.names = FALSE) |> unique()
  invalid_vals <- qtab$p[["Unguelt"]]
  vallabs <- attr(qtab$m$dat_mod[[qtab$p$rowvars_qtab[1]]], "labels")

  # the following is equivalent to (but faster with base R):
  # vallab_table <- vallabs |>
  #   tibble::enframe("vallab", "val") |>
  #   dplyr::full_join(tibble(val = occuring_vals), by = "val") |>
  #   dplyr::filter(!val %in% invalid_vals) |>
  #   dplyr::slice(rep(seq_len(n_vals), each = 2))

  # the following is the same as:
  # all_valid_vals <- vallabs |> c(occuring_vals) |> dplyr::setdiff(invalid_vals)
  # but keeping the names (setdiff removes the names)
  # TODO: move to post_process()!
  all_valid_vals <- c(vallabs, occuring_vals)
  do_sort <- qtab$p$Sort %in% "ORDER=D"
  if (length(do_sort) == 0) {
    do_sort <- FALSE
  }
  all_valid_vals <- all_valid_vals[!duplicated(all_valid_vals) & !all_valid_vals %in% invalid_vals] |>
    # TODO: Wolf fragen was es alles gibt:
    sort(decreasing = do_sort)

  vallab_table <- all_valid_vals |>
    tibble::enframe("vallab", "val") |>
    dplyr::mutate(vallab = ifelse(vallab == "" | is.numeric(vallab), as.character(val), vallab))
  n_vals <- nrow(vallab_table)

  vallab_table <- vallab_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- vallab_table$val
  row_table$RowTitle1 <- vallab_table$vallab
  row_table$RowTitle2 <- vallab_table$vallab
  row_table$RowTitle3 <- c(
    qtab$m$options$l_lexikon[["cTabAbs"]],
    qtab$m$options$l_lexikon[["cTabProz"]]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowValue <- strip_attributes(vallab_table$val)
  row_table$RowVariable <- qtab$p$rowvars_string
  row_table$RowContent <- "Detail"
  row_table
}

row_table_body.qtab_type_mdg <- function(qtab) {
  rowvars <- qtab$p$l_selvar$rowvars[[1]] %||% qtab$p$rowvars_valid_qtab
  l_varlabs <- qtab$m$dat_mod[rowvars] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  no_varlab_idx <- l_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    l_varlabs[no_varlab_idx] <- names(l_varlabs[no_varlab_idx])
    warning(
      "There is no variable label for these mdg variable(s): ",
      no_varlab_idx[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  label_table <- data.frame(
    var = rowvars,
    label = unlist(l_varlabs, use.names = FALSE)
  ) |>
    dplyr::mutate(label = dplyr::coalesce(label, var))
  n_vals <- nrow(label_table)

  label_table <- label_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  mdg_val <- qtab$p$MdgVal
  row_table[seq_len(n_vals * 2),]$RowValue <- mdg_val
  row_table$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    qtab$m$options$l_lexikon[["cTabAbs"]],
    qtab$m$options$l_lexikon[["cTabProz"]]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowContent <- "Detail"
  row_table$RowVariable <- qtab$p$l_selvar$valid %||% qtab$p$rowvars_valid_qtab |> rep(each = 2)
  row_table
}

row_table_body.qtab_type_mw <- function(qtab) {
  rowvars <- qtab$p$l_selvar$rowvars[[1]] %||% qtab$p$rowvars_qtab
  l_varlabs <- qtab$m$dat_mod[rowvars] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  no_varlab_idx <- l_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    l_varlabs[no_varlab_idx] <- names(l_varlabs[no_varlab_idx])
    warning(
      "There is no variable label for these mw variable(s): ",
      no_varlab_idx[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  label_table <- data.frame(
    var = rowvars,
    label = unlist(l_varlabs, use.names = FALSE)
  ) |>
    dplyr::mutate(label = dplyr::coalesce(label, var))
  n_vals <- nrow(label_table)

  label_table <- label_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label

  ctab_entry <- df_metr_mac[df_metr_mac$fun == qtab$p$stat_fun, "ctab_entry"]
  row_title3 <- qtab$m$options$l_lexikon[[ctab_entry]]
  if (!is.null(qtab$p$repov_names)) {
    row_title3 <- qtab$p$repov_names
  }
  row_table$RowTitle3 <- c(
    # TODO: generalize for media std err etc.:
    row_title3,
    qtab$m$options$l_lexikon[["cTabGueltig"]]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Percent", "Abs") |> rep(n_vals)
  row_table$RowContent <- c(
    "MStatistics",
    "MValid"
  ) |> rep(n_vals)
  row_table$RowDecimals <- c(
    1L,
    0L
  ) |> rep(n_vals)
  row_table$RowVariable <- (qtab$p$l_selvar$valid %||% rowvars) |> rep(each = 2)
  row_table
}



row_table_valid_cases <- function(qtab) {
  UseMethod("row_table_valid_cases")
}
row_table_valid_cases.default <- function(qtab) {
  row_table <- empty_row_table()
  valid_cases_text <- qtab$m$options$l_lexikon[["cTabGueltig"]]
  abs_text <- qtab$m$options$l_lexikon[["cTabAbs"]]
  percent_text <- qtab$m$options$l_lexikon[["cTabProz"]]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowValue")] <- list("Valid", "Abs", valid_cases_text, valid_cases_text, abs_text, 0, 1)
  row_table[2, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowValue")] <- list("Valid", "Percent", valid_cases_text, valid_cases_text, percent_text, 1, 1)
  row_table$RowVariable <- qtab$p$rowvars_string
  row_table
}
row_table_valid_cases.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_summary <- function(qtab) {
  UseMethod("row_table_summary")
}
row_table_summary.default <- function(qtab) {
  NULL
}
row_table_summary.qtab_type_cat <- function(qtab) {
  cat_rec_string <- qtab$p$CatRec
  if (is.null(cat_rec_string)) {
    return(NULL)
  }
  cat_lab_string <- strsplit(qtab$p$CatLab, "\\|")[[1]]
  cat_rec_string <- strsplit(qtab$p$CatRec, "\\|")[[1]]
  row_table <- purrr::map2(
    cat_lab_string,
    cat_rec_string,
    catlab_helper
  ) |>
    dplyr::bind_rows(.id = "i_catrec")

  n_vals <- nrow(row_table) / 2

  row_table$RowTitle1 <- paste0(qtab$m$options$l_lexikon[["cTabZsfg"]], " ")
  row_table[row_table$i_catrec > 1,]$RowTitle1 <- paste0(
    row_table[row_table$i_catrec > 1,]$RowTitle1,
    row_table[row_table$i_catrec > 1,]$i_catrec
  )

  row_table$RowTitle3 <- c(
    qtab$m$options$l_lexikon[["cTabAbs"]],
    qtab$m$options$l_lexikon[["cTabProz"]]
  ) |> rep(n_vals)
  row_table$RowVariable <- paste0(qtab$p$l_selvar$valid %||% qtab$p$rowvars_qtab, "__summary")
  row_table
}
catlab_helper <- function(cat_lab_string, catrec_string) {
  cat_lab_splits <- split_cat_lab_string(cat_lab_string)
  catrec_sum_string <- catrec_string |> stringr::str_extract("(?<=\\{).*(?=\\})")
  if (!is.na(catrec_sum_string)) {
    catrec_sum_label <- catrec_sum_string |>
      stringr::str_remove(".*=") |>
      stringr::str_squish() |>
      stringr::str_remove_all("^'|'$")
    cat_lab_splits <- c(cat_lab_splits, (max(cat_lab_splits) + 1) |> purrr::set_names(catrec_sum_label))
  }
  row_table <- empty_row_table()
  n_vals <- length(cat_lab_splits)
  row_table[seq_len(n_vals * 2),]$RowValue <- unname(cat_lab_splits) |> rep(each = 2)
  row_table$RowContent <- "Summary"
  row_table$RowTitle2 <- names(cat_lab_splits) |> rep(each = 2)
  row_table$RowDecimals <- c(0L, 1L) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table
}


row_table_stats <- function(qtab) {
  UseMethod("row_table_stats")
}
row_table_stats.default <- function(qtab) {
  NULL
}
row_table_stats.qtab_type_cat <- function(qtab) {
  if (is.null(qtab$p$MetrMac)) {
    return(NULL)
  }

  df_stat_funs <- qtab$p$df_stat_funs

  row_table <- empty_row_table()
  row_table[seq_along(df_stat_funs$shortcut), c("RowTitle1", "RowTitle2", "RowTitle3")] <-
    list(df_stat_funs$row_title) |> rep(3)
  row_table$RowValue <- 100
  row_table$RowAbsPercent <- "Percent"

  row_table$RowContent <- "Statistics"
  row_table$RowDecimals <- df_stat_funs$decimals
  row_table$RowVariable <- qtab$p$l_selvar$valid %||% qtab$p$rowvars_qtab
  # TODO: tell Wolf that I needed this to properly merge to tab_values when
  # there multiple rows with MStatistics:
  row_table$RowStatFun <- df_stat_funs$fun
  row_table
}

# TODO: source out common functionality with row_table_body!
row_table_invalid_vals <- function(qtab) {
  UseMethod("row_table_invalid_vals")
}
row_table_invalid_vals.qtab_type_mcg <- row_table_invalid_vals.qtab_type_cat <- function(qtab) {
  occuring_vals <- qtab$d$tab_values$rowval |> unique()
  invalid_vals <- qtab$p[["Unguelt"]]
  if (all(!occuring_vals %in% invalid_vals)) {
    return(NULL)
  }
  vallabs <- attr(qtab$m$dat_mod[[qtab$p$rowvars_qtab[1]]], "labels")

  occuring_invalid_vals <- intersect(invalid_vals, occuring_vals)
  all_invalid_vals <- c(vallabs, occuring_invalid_vals)
  all_invalid_vals <- all_invalid_vals[!duplicated(all_invalid_vals) & all_invalid_vals %in% occuring_invalid_vals]
  sorted_invalid_vals <- all_invalid_vals[order(match(all_invalid_vals, invalid_vals))]

  vallab_table <- sorted_invalid_vals |>
    tibble::enframe("vallab", "val") |>
    dplyr::mutate(vallab = dplyr::coalesce(vallab, as.character(val)))
  n_vals <- nrow(vallab_table)

  vallab_table <- vallab_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- vallab_table$val
  row_table$RowTitle1 <- vallab_table$vallab
  row_table$RowTitle2 <- vallab_table$vallab
  row_table$RowTitle3 <- c(
    qtab$m$options$l_lexikon[["cTabAbs"]],
    qtab$m$options$l_lexikon[["cTabProz"]]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(0L, 1L) |> rep(n_vals)
  row_table$RowValue <- strip_attributes(vallab_table$val)
  row_table$RowVariable <- qtab$p$rowvars_string
  row_table$RowContent <- "Missing"

  row_table
}

# TODO: add lines with cTabNoEntry ("No entry in the respective variables") if present...!
row_table_invalid_vals.qtab_type_mdg <- function(qtab) {
  # TODO: clean up MESS by trying to hack multi selvar unguelt...:
  if (is.null(qtab$p[["Unguelt"]])) {
    return(NULL)
  }
  invalids_present <- qtab$p$l_selvar$invalid %||% qtab$p[["Unguelt"]] |>
    lapply(\(x) x %in% qtab$d$tab_values$rowvar) |>
    unlist()
  if (sum(invalids_present) == 0) {
    return(NULL)
  }
  invalid_vals <- qtab$p$l_selvar$rowvars_inv[[1]] %||% qtab$p[["Unguelt"]]
  l_varlabs <- qtab$m$dat_mod[invalid_vals] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  mdg_val <- qtab$p$MdgVal

  names(l_varlabs) <- qtab$p$l_selvar$invalid %||% names(l_varlabs)
  l_varlabs <- l_varlabs[invalids_present]
  no_varlab_idx <- l_varlabs |> sapply(is.null)
  if (sum(no_varlab_idx) > 0) {
    l_varlabs[no_varlab_idx] <- names(l_varlabs[no_varlab_idx])
    warning(
      "There is no variable label for these mdg variable(s): ",
      no_varlab_idx[no_varlab_idx] |> names() |> paste(collapse = ", ")
    )
  }
  label_table <- data.frame(
    var = names(l_varlabs),
    label = unlist(l_varlabs, use.names = FALSE)
  )
  n_vals <- nrow(label_table)

  label_table <- label_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- mdg_val
  row_table$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    qtab$m$options$l_lexikon[["cTabAbs"]],
    qtab$m$options$l_lexikon[["cTabProz"]]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowVariable <- label_table$var
  row_table$RowContent <- "Missing"
  row_table
}
row_table_invalid_vals.qtab_type_mw <- function(qtab) {
  NULL
}

row_table_no_entry <- function(qtab) {
  UseMethod("row_table_no_entry")
}
row_table_no_entry.default <- function(qtab) {
  NULL
}
row_table_no_entry.qtab_type_mdg <- function(qtab) {
  if (nrow(qtab$d$stats_rows$no_entry) == 0) {
    return(NULL)
  }
  row_table <- empty_row_table()
  no_entry_text <- qtab$m$options$l_lexikon["cTabNoEntry"]
  abs_text <- qtab$m$options$l_lexikon[["cTabAbs"]]
  percent_text <- qtab$m$options$l_lexikon[["cTabProz"]]
  # TODO: use all variables, not only valid ones...!
  # (also needs to be done in qtab$d$stats_rows$no_entry...):
  row_variable <- qtab$p$rowvars_string
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowVariable", "RowValue")] <- list("Missing", "Abs", no_entry_text, no_entry_text, abs_text, 0, row_variable, 1)
  row_table[2, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals", "RowVariable", "RowValue")] <- list("Missing", "Percent", no_entry_text, no_entry_text, percent_text, 1, row_variable, 1)
  row_table

}
