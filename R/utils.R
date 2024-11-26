split_cell <- function(x, sep = "[,; ]+") {
  x |>
    stringr::str_squish() |>
    stringr::str_split(sep)
}

strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

all_true <- function (l) {
  Reduce("&", l)
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

get_tabulated_invalid_vals <- function(qtab) {
  raw_data <- qtab$d$raw_data
  df <- raw_data[names(raw_data) |> stringr::str_subset("^rowvar")]

  all_vals <- df |> unlist() |> unique() |> sort()
  invalid_vals <- qtab$p[["Unguelt"]]
  ordered_vals <- all_vals |> setdiff(invalid_vals) |> c(invalid_vals)
  df$i <- seq_len(nrow(df))
  df_long <- df |>
    tidyr::pivot_longer(-i) |>
    dplyr::mutate(ii = match(value, ordered_vals) |> min(), .by = "i")
  indices <- df_long$ii |>
    unique()
  ordered_vals[indices]
}
