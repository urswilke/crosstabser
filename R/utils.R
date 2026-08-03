split_cell <- function(x, sep = "[,; ]+") {
  x |>
    stringr::str_squish() |>
    stringr::str_split(sep)
}

strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

`%||%` <- rlang::`%||%`

spss_to_r <- function(ex) {
  ex |>
    # replace equal sign not preceeded by !, <, > OR = AND not succeeded by = :
    stringr::str_replace_all("(?<![!<=>])=(?!=)", "==") |>
    stringr::str_replace_all(" NE ", " != ") |>
    stringr::str_replace_all(" GT ", " > ") |>
    stringr::str_replace_all(" GE ", " >= ") |>
    stringr::str_replace_all(" LE ", " <= ") |>
    stringr::str_replace_all(" LT ", " < ") |>
    stringr::str_replace_all(" AND ", " & ") |>
    stringr::str_replace_all(" OR ", " | ") |>
    stringr::str_replace_all(" <> ", " != ")
}

add_prefix <- function(x, prefix) {
  if (length(x) == 0) {
    return(NULL)
  }
  paste0(prefix, x)
}
rv <- function(x) add_prefix(x, prefix = "rowvar_")
cv <- function(x) add_prefix(x, prefix = "colvar_")

extract_rowvars <- function(x, data) {
  if (stringr::str_detect(x, "^ts: *")) {
    return(
      select_loc(
        data,
        stringr::str_remove(x, "ts: *")
      )
    )
  }
  # before introducing this new functionality above, split_cell was operating on
  # the whole RowVar column. As it's now operating cellwise, we have to extract
  # the list (...[[1]]):
  split_cell(x, " ")[[1]]
}

# see here: https://tidyselect.r-lib.org/reference/language.html
# and here: https://tidyselect.r-lib.org/articles/syntax.html
select_loc <- function(data, ...) {
  tidyselect::eval_select(rlang::parse_expr(c(...)), data) |> names()
}

rm_header_footer <- function(row_table) {
  row_table[-c(1:3, nrow(row_table)),]
}

add_exclusive_info <- function(df_rows_long, exclusives, choice_col) {
  occurring_vals <- df_rows_long[[choice_col]] |> unique() |> sort()

  non_exclusives <- occurring_vals |> setdiff(exclusives)
  ordered_vals <- non_exclusives |> c(exclusives)
  res <- df_rows_long |>
    dplyr::mutate(
      index = match(.data[[choice_col]], ordered_vals),
      lowest_index = index |> min(),
      val_to_count = index == lowest_index | ordered_vals[index] %in% non_exclusives,
      .by = "i"
    )
  res$index <- NULL
  res$lowest_choice <- ordered_vals[res$lowest_index]
  res$lowest_index <- NULL
  res
}

line_break <- if (Sys.info()[['sysname']] == "Windows") "\r\n" else "\n"

extract_var_names <- function(x) {
  if (length(x) == 0) {
    return(c())
  }
  x |>
    # without keep.source = TRUE, the tests were passing in the console but not in the rstudio build pane...:
    # https://forum.posit.co/t/getparsedata-returns-null-when-published-in-a-shiny-app-to-rstudio-connect/41353/5
    parse(text = _, keep.source = TRUE) |>
    getParseData() |>
    dplyr::filter(token == "SYMBOL") |>
    dplyr::pull(text) |>
    unique() |>
    purrr::set_names()
}
