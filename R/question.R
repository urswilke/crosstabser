read_qsheet <- function(mapping) {
  mapping$qsheet$qsheet_raw <- read_qsheet_raw(mapping$mapping_file)
  mapping$qsheet$df <- process_qsheet(mapping)
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
      Invalid = purrr::map_if(Invalid, Type %in% c("cat", "mcg", "mw"), ~as.numeric(.x), .else = ~.x),
      Type = as.list(Type),
      SelVar = split_cell(SelVar),
      SelVal = split_cell(SelVal) |> purrr::map(as.numeric),
      RemoveEmpty = stringr::str_trim(RemoveEmpty) == "EXCLUDE"
    ) |>
    unnest_qsheet_rows()

  # |>
  #   tidyr::unnest(SelVal, keep_empty = TRUE)
}

unnest_qsheet_rows <- function(df_qsheet) {
  split(df_qsheet, seq_len(nrow(df_qsheet))) |>
    purrr::map_dfr(unnest_mw_rowvar)
}
unnest_mw_rowvar <- function(df_row) {
  n_rowvar <- length(df_row$RowVars[[1]])
  res <- df_row[rep(1, n_rowvar + 1),]
  res$Type <- list("mw") |> append(as.list(rep("cat", n_rowvar)))
  res$RowVars <- df_row$RowVars |> append(as.list(unlist(df_row$RowVars)))
  res
}
