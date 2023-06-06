read_qsheet <- function(mapping_file) {
  qsheet_raw <- read_qsheet_raw(mapping_file)
  df <- process_qsheet(qsheet_raw)
  tibble::lst(
    qsheet_raw,
    df
  )
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

process_qsheet <- function(qsheet_raw) {
  qsheet_raw |>
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
    )
  # |>
  #   tidyr::unnest(SelVal, keep_empty = TRUE)
}
