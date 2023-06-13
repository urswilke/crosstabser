read_qsheet <- function(mapping) {
  mapping$qsheet$qsheet_raw <- read_qsheet_raw(mapping$mapping_file)
  mapping$qsheet$qsheet_processed <- process_qsheet(mapping)
}

read_qsheet_raw <- function(mapping_file, sheet = "Questions") {
  # TODO: only use English column names
  df_questions <- readxl::read_excel(
    mapping_file, sheet = sheet,
    col_types = "text"
  )
  df_questions |>
    dplyr::mutate(row = dplyr::row_number(), .before = 1) |>
    # https://stackoverflow.com/a/66136167
    dplyr::filter(dplyr::if_any(-row, ~ !is.na(.x)))
  # |>
  #   make_clean_names()
}

process_qsheet <- function(mapping) {
  mapping$qsheet$qsheet_raw |>
    tidyr::drop_na(Type) |>
    dplyr::mutate(
      Title = as.list(Title),
      RowVars = RowVars |> split_cell(" "),
      Invalid = split_cell(Invalid),
      Invalid = purrr::map_if(Invalid, Type %in% c("cat", "mcg", "mw"), as.numeric, .else = ~.x),
      Type = as.list(Type),
      Filter = split_cell(Filter) |> purrr::map(\(x) x[!is.na(x)]),
      SelVar = split_cell(SelVar),
      SelVal = split_cell(SelVal) |> purrr::map(as.numeric),
      RemoveEmpty = stringr::str_trim(RemoveEmpty) == "EXCLUDE"
    ) |>
    add_options_column(mapping) |>
    unnest_qsheet_rows(mapping)

  # |>
  #   tidyr::unnest(SelVal, keep_empty = TRUE)
}

add_options_column <- function(qsheet, mapping) {
  qsheet$options <- rep(list(mapping$options$l_macro_scenario), nrow(qsheet))
  qsheet
}

unnest_qsheet_rows <- function(df_qsheet, mapping) {
  df_qsheet |>
    row_split() |>
    purrr::map_dfr(\(row) unnest_selvar(row, mapping)) |>
    row_split() |>
    purrr::map_dfr(\(row) unnest_mw_rows(row, mapping))
}
unnest_mw_rows <- function(df_row, mapping) {
  if (df_row$Type != "mw") {
    return(df_row)
  }
  mw_label <- dplyr::coalesce(df_row$MWLabel, mapping$options$l_lexikon[["cTabMeanOV"]])
  df_row$Title[[1]][2] <- mw_label
  if (df_row$Freq %in% "0") {
    return(df_row)
  }
  n_rowvar <- length(df_row$RowVars[[1]])
  res <- df_row[rep(1, n_rowvar + 1),]
  res$Type <- list("mw") |> append(as.list(rep("cat", n_rowvar)))
  res$RowVars <- df_row$RowVars |> append(as.list(unlist(df_row$RowVars)))
  res$Title[-1] <- purrr::map2(
    res$Title[-1],
    res$RowVars[-1],
    \(title, rowvar){
      title[2] <- attr(mapping$dat_mod[[rowvar]], "label", exact = TRUE);
      title
    }
  )
  res
}
unnest_selvar <- function(df_row, mapping) {
  if (is.na(df_row$SelVar[[1]])) {
    return(df_row)
  }
  n_selval <- length(df_row$SelVal[[1]])
  res <- df_row[rep(1, n_selval),]
  # TODO: generalise for selval expressions like "1-3:new_label"
  res$Filter <- purrr::map2(
    res$Filter,
    df_row$SelVal[[1]],
    \(filt, selval) append(filt, paste0(df_row$SelVar, " == ", selval))
  )
  res
}
