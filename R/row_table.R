gen_row_table <- function(df_row, mapping) {
  row_table <- rbind(
    row_table_title_lines(df_row),
    row_table_total_line(df_row, mapping),
    row_table_valid_mw(df_row, mapping),
    row_table_valid_answers_line(df_row, mapping),
    row_table_body(df_row, mapping),
    row_table_summary(df_row, mapping),
    row_table_stats(df_row, mapping),
    row_table_valid_cases(df_row, mapping),
    row_table_invalid_vals(df_row, mapping),
    row_table_empty_row()
  )

  #TODO: Wolf fragen wie nummerieren?? :
  row_table$TabNo <- df_row$row
  row_table$RowNo <- seq_len(nrow(row_table))
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
row_table_title_lines <- function(df_row) {
  row_table <- empty_row_table()

  row_table[1, c("RowContent", "RowTitle1")] <- list("Title", paste(df_row$Title[[1]], collapse = "\n"))
  row_table[2, c("RowContent")] <- list("Header")
  row_table[3, c("RowContent")] <- list("Header")
  row_table
}
row_table_total_line <- function(df_row, mapping) {
  UseMethod("row_table_total_line")
}

row_table_total_line.default <- function(df_row, mapping) {
  row_table <- empty_row_table()
  total_row_text <- mapping$options$l_lexikon["cTabGesamt"]
  abs_text <- mapping$options$l_lexikon["cTabAbs"]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals")] <- list("Total", "Abs", total_row_text, total_row_text, abs_text, 0L)
  row_table
}
row_table_total_line.tab_type_mw <- function(df_row, mapping) {
  NULL
}

row_table_valid_mw <- function(df_row, mapping) {
  UseMethod("row_table_valid_mw")
}

row_table_valid_mw.default <- function(df_row, mapping) {
  NULL
}
row_table_valid_mw.tab_type_mw <- function(df_row, mapping) {
  row_table <- empty_row_table()
  valid_mw_text <- mapping$options$l_lexikon["cTabGesamtMW"]
  abs_text <- mapping$options$l_lexikon["cTabAbs"]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals")] <- list("Valid", "Abs", valid_mw_text, valid_mw_text, abs_text, 0L)
  row_table
}


row_table_valid_answers_line <- function(df_row, mapping) {
  UseMethod("row_table_valid_answers_line")
}

row_table_valid_answers_line.tab_type_mdg <- function(df_row, mapping) {
  row_table <- empty_row_table()
  valid_answers_row_text <- mapping$options$l_lexikon["cTabGesamtMFA"]
  abs_text <- mapping$options$l_lexikon["cTabAbs"]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3")] <- list("Total", "Abs", valid_answers_row_text, valid_answers_row_text, abs_text)
  row_table
}
row_table_valid_answers_line.tab_type_mcg <- row_table_valid_answers_line.tab_type_mdg
row_table_valid_answers_line.tab_type_mw <- function(df_row, mapping) {
  NULL
}
row_table_valid_answers_line.tab_type_cat <- row_table_valid_answers_line.tab_type_mw

row_table_body <- function(df_row, mapping) {
  UseMethod("row_table_body")
}
row_table_body.tab_type_mcg <- row_table_body.tab_type_cat <- function(df_row, mapping) {
  occuring_vals <- mapping$dat_mod[df_row$RowVar[[1]]] |> unlist(use.names = FALSE) |> unique()
  invalid_vals <- df_row$Unguelt[[1]]
  if (is.na(invalid_vals[1])) {
    invalid_vals <- mapping$options$l_macro_scenario$Unguelt
  }
  vallabs <- attr(mapping$dat_mod[[df_row$RowVar[[1]][1]]], "labels")

  # the following is equivalent to (but faster with base R):
  # vallab_table <- vallabs |>
  #   tibble::enframe("vallab", "val") |>
  #   dplyr::full_join(tibble(val = occuring_vals), by = "val") |>
  #   dplyr::filter(!val %in% invalid_vals) |>
  #   dplyr::slice(rep(seq_len(n_vals), each = 2))

  # the following is the same as:
  # all_valid_vals <- vallabs |> c(occuring_vals) |> dplyr::setdiff(invalid_vals)
  # but keeping the names (setdiff removes the names)
  all_valid_vals <- c(vallabs, occuring_vals)
  all_valid_vals <- all_valid_vals[!duplicated(all_valid_vals) & !all_valid_vals %in% invalid_vals] |>
    # TODO: Wolf fragen was es alles gibt:
    sort(decreasing = df_row$Sort %in% "ORDER=D")

  vallab_table <- all_valid_vals |>
    tibble::enframe("vallab", "val")
  n_vals <- nrow(vallab_table)

  vallab_table <- vallab_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- vallab_table$val
  row_table$RowTitle1 <- vallab_table$vallab
  row_table$RowTitle2 <- vallab_table$vallab
  row_table$RowTitle3 <- c(
    mapping$options$l_lexikon["cTabAbs"],
    mapping$options$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowValue <- strip_attributes(vallab_table$val)
  row_table$RowVariable <- df_row$RowVar[[1]] |> paste(collapse = ", ")
  row_table$RowContent <- "Detail"
  row_table
}

row_table_body.tab_type_mdg <- function(df_row, mapping) {
  l_varlabs <- mapping$dat_mod[df_row$RowVar[[1]]] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
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
  row_table[seq_len(n_vals * 2),]$RowValue <- dplyr::coalesce(
    df_row$MdgVal |> as.numeric(),
    1
  )
  row_table$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    mapping$options$l_lexikon["cTabAbs"],
    mapping$options$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowContent <- "Detail"
  row_table$RowVariable <- label_table$var
  row_table
}

row_table_body.tab_type_mw <- function(df_row, mapping) {
  l_varlabs <- mapping$dat_mod[df_row$RowVar[[1]]] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
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
  row_table[seq_len(n_vals * 2),]$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    # TODO: generalize for media std err etc.:
    mapping$options$l_lexikon["cTabMean"],
    mapping$options$l_lexikon["cTabGueltig"]
  ) |> rep(n_vals)
  row_table$RowContent <- c(
    "MStatistics",
    "MValid"
  ) |> rep(n_vals)
  row_table$RowDecimals <- c(
    1L,
    0L
  ) |> rep(n_vals)
  row_table$RowVariable <- label_table$var
  row_table
}



row_table_valid_cases <- function(df_row, mapping) {
  UseMethod("row_table_valid_cases")
}
row_table_valid_cases.default <- function(df_row, mapping) {
  row_table <- empty_row_table()
  valid_cases_text <- mapping$options$l_lexikon["cTabGueltig"]
  abs_text <- mapping$options$l_lexikon["cTabAbs"]
  percent_text <- mapping$options$l_lexikon["cTabProz"]
  row_table[1, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals")] <- list("Valid", "Abs", valid_cases_text, valid_cases_text, abs_text, 0)
  row_table[2, c("RowContent", "RowAbsPercent", "RowTitle1", "RowTitle2", "RowTitle3", "RowDecimals")] <- list("Valid", "Percent", valid_cases_text, valid_cases_text, percent_text, 1)
  row_table
}
row_table_valid_cases.tab_type_mw <- function(df_row, mapping) {
  NULL
}

row_table_summary <- function(df_row, mapping) {
  #TODO
  NULL
}
row_table_stats <- function(df_row, mapping) {
  #TODO
  NULL
}
# TODO: source out common functionality with row_table_body!
row_table_invalid_vals <- function(df_row, mapping) {
  UseMethod("row_table_invalid_vals")
}
row_table_invalid_vals.tab_type_mcg <- row_table_invalid_vals.tab_type_cat <- function(df_row, mapping) {
  occuring_vals <- mapping$dat_mod[df_row$RowVar[[1]]] |> unlist(use.names = FALSE) |> unique()
  invalid_vals <- df_row$Unguelt[[1]]
  if (is.na(invalid_vals[1])) {
    invalid_vals <- mapping$options$l_macro_scenario$Unguelt
  }
  if (all(!occuring_vals %in% invalid_vals)) {
    return(NULL)
  }
  vallabs <- attr(mapping$dat_mod[[df_row$RowVar[[1]][1]]], "labels")

  occuring_invalid_vals <- intersect(invalid_vals, occuring_vals)
  all_invalid_vals <- c(vallabs, occuring_invalid_vals)
  all_invalid_vals <- all_invalid_vals[!duplicated(all_invalid_vals) & all_invalid_vals %in% occuring_invalid_vals]

  vallab_table <- all_invalid_vals |>
    tibble::enframe("vallab", "val")
  n_vals <- nrow(vallab_table)

  vallab_table <- vallab_table[rep(seq_len(n_vals), each = 2),]

  row_table <- empty_row_table()
  row_table[seq_len(n_vals * 2),]$RowValue <- vallab_table$val
  row_table$RowTitle1 <- vallab_table$vallab
  row_table$RowTitle2 <- vallab_table$vallab
  row_table$RowTitle3 <- c(
    mapping$options$l_lexikon["cTabAbs"],
    mapping$options$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(0L, 1L) |> rep(n_vals)
  row_table$RowValue <- strip_attributes(vallab_table$val)
  row_table$RowVariable <- df_row$RowVar[[1]] |> paste(collapse = ", ")
  row_table$RowContent <- "Valid"

  row_table
}
row_table_invalid_vals.tab_type_mdg <- function(df_row, mapping) {
  l_varlabs <- mapping$dat_mod[df_row$Unguelt[[1]]] |> purrr::map(\(x) attr(x, "label", exact = TRUE))
  invalids_present <- mapping$dat_mod[names(l_varlabs)] |>
    purrr::map_lgl(\(x) dplyr::coalesce(df_row$MdgVal |> as.numeric(), 1) %in% x)
  if (sum(invalids_present) == 0) {
    return(NULL)
  }
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
  row_table[seq_len(n_vals * 2),]$RowValue <- dplyr::coalesce(
    df_row$MdgVal |> as.numeric(),
    1
  )
  row_table$RowTitle1 <- label_table$label
  row_table$RowTitle2 <- label_table$label
  row_table$RowTitle3 <- c(
    mapping$options$l_lexikon["cTabAbs"],
    mapping$options$l_lexikon["cTabProz"]
  ) |> rep(n_vals)
  row_table$RowAbsPercent <- c("Abs", "Percent") |> rep(n_vals)
  row_table$RowDecimals <- c(
    0L,
    1L
  ) |> rep(n_vals)
  row_table$RowVariable <- label_table$var
  row_table$RowContent <- "Detail"
  row_table
}
row_table_invalid_vals.tab_type_mw <- function(df_row, mapping) {
  #TODO
  NULL
}
row_table_empty_row <- function() {
  row_table <- empty_row_table()
  row_table[1,]$RowContent <- "Empty"
  row_table
}
