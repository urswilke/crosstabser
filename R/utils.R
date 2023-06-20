split_cell <- function(x, sep = "[,; ]+") {
  x |>
    stringr::str_squish() |>
    stringr::str_split(sep)
  # |>
  #   purrr::map(~.x[!is.na(.x)])
}

strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

row_split <- function(df) {
  df |>
    dplyr::rowwise() |>
    dplyr::group_split()
}

all_true <- function (l) {
  Reduce("&", l)
}
any_true <- function (l) {
  Reduce("|", l)
}

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
